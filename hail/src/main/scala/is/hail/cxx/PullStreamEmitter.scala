package is.hail.cxx

import is.hail.expr.types._
import is.hail.expr.types.physical._
import is.hail.utils._

abstract class Continuation(
  val setup: String,
  val numArgs: Int
) { self =>

  def call(args: Seq[Code]): Code

  def apply(args: Code*): Code = {
    assert(args.length == numArgs)
    call(args)
  }

  def mapArgs(f: Seq[Code] => (Code, Seq[Code])) = new Continuation(setup, numArgs) {
    def call(args: Seq[Code]) = {
      val (setup, newArgs) = f(args)
      s"""{
         |$setup
         |${self.call(newArgs)}
         |}
       """.stripMargin
    }
  }
}

class AssignAndJump(val label: String, val vars: IndexedSeq[Variable])
    extends Continuation(Code.sequence(vars.map(_.define)), vars.length) {

  def call(args: Seq[Code]): Code =
    s"""
       |${Code.sequence(args.zipWithIndex.map { case (arg, i) => s"${vars(i)} = $arg;" })}
       |goto $label;
     """.stripMargin
}

object Continuation {
  def apply(fb: FunctionBuilder, namePrefix: String, args: (String, PType)*): AssignAndJump =
    new AssignAndJump(
      fb.genSym(namePrefix),
      args.toIndexedSeq.map { case (name, typ) => fb.variable(name, typeToCXXType(typ)) })
}

abstract class PullStreamEmitter(
  val fb: FunctionBuilder,
  val pType: PStream,
  val setup: Code,
  val m: Code
) { self =>

  val elemType = pType.elementType

  def init(elem: Continuation, eos: Continuation): Code
  def step(elem: Continuation, eos: Continuation): Code

  def consume(f: (Code, Code) => Code, iterSetup: Code): Code = {
    val loop = Continuation(fb, "loop", "eltm" -> PBooleanRequired, "eltv" -> elemType)
    val eltm = loop.vars(0)
    val eltv = loop.vars(1)
    val done = Continuation(fb, "done")
    s"""
       |${loop.setup}
       |${done.setup}
       |{
       |  $iterSetup
       |  ${init(loop, done)}
       |}
       |${loop.label}:
       |{
       |  $iterSetup
       |  ${f(eltm.toString(), eltv.toString())}
       |  ${step(loop, done)}
       |}
       |${done.label}:;
     """.stripMargin
  }

  def toArrayEmitter(arrayRegion: EmitRegion, sameRegion: Boolean) =
    new PullStreamEmitter.ToArrayEmitter(this, arrayRegion, sameRegion)

  def map(f: (Code, Code) => EmitTriplet): PullStreamEmitter = {
    val vm = fb.variable("eltm", "bool")
    val vv = fb.variable("eltv", typeToCXXType(elemType))
    val body = f(vm.toString, vv.toString)
    new PullStreamEmitter(fb, PStream(body.pType, pType.required), setup, m) {
      def mapElemCont(c: Continuation): Continuation =
        c.mapArgs { args =>
          val setup =
            s"""
             |${vm.define}
             |${vv.define}
             |$vm = ${args(0)};
             |if(!$vm)
             |  $vv = ${args(1)};
             |${body.setup}
           """.stripMargin
          (setup, Seq(body.m, body.v))
        }

      def init(elem: Continuation, eos: Continuation): Code = self.init(mapElemCont(elem), eos)
      def step(elem: Continuation, eos: Continuation): Code = self.step(mapElemCont(elem), eos)
    }
  }
}

object PullStreamEmitter {

  class ToArrayEmitter(stream: PullStreamEmitter, arrayRegion: EmitRegion, sameRegion: Boolean)
      extends ArrayEmitter(stream.pType, stream.setup, stream.m, "", None, arrayRegion) {

    def consume(f: (Code, Code) => Code): Code =
      stream.consume(f, arrayRegion.defineIfUsed(sameRegion))

    override def map(fb: FunctionBuilder)(f: EmitTriplet => EmitTriplet): ToArrayEmitter =
      stream.map { (m, v) =>
        f(EmitTriplet(stream.elemType, "", m, v, arrayRegion))
      }.toArrayEmitter(arrayRegion, sameRegion)
  }

  class Empty(fb: FunctionBuilder, pType: PStream)
      extends PullStreamEmitter(fb, pType, "", m = "false") {
    def init(elem: Continuation, eos: Continuation): Code = eos()
    def step(elem: Continuation, eos: Continuation): Code = eos()
  }

  def fromTriplets(
    fb: FunctionBuilder,
    pType: PStream,
    _triplets: Seq[EmitTriplet]
  ): PullStreamEmitter =
    if (_triplets.isEmpty)
      new Empty(fb, pType)
    else {
      assert(_triplets.forall(_.pType isOfType pType.elementType))
      val triplets = _triplets.map(_.memoize(fb))
      val i = fb.variable("i", "int")
      val setup = Code.sequence(i.define +: triplets.map(_.setup))
      new PullStreamEmitter(fb, pType, setup, "false") {
        def init(elem: Continuation, eos: Continuation): Code =
          s"""
             |$i = 0;
             |${elem(triplets(0).m, triplets(0).v)}
           """.stripMargin

        def step(elem: Continuation, eos: Continuation): Code = {
          val cases = Code.sequence(
            triplets.tail.zipWithIndex.map { case (et, idx) =>
              s"case $idx: ${elem(et.m, et.v)}"
            })
          s"""
             |switch($i++) {
             |${cases}
             |default:
             |  ${eos()}
             |}
           """.stripMargin
        }
      }
    }

  def range(fb: FunctionBuilder, _len: EmitTriplet) = {
    assert(_len.pType isOfType PInt32())
    val i = fb.variable("i", "int")
    val len = _len.memoize(fb)
    val setup = Code(i.define, len.setup)
    val m = len.m
    new PullStreamEmitter(fb, PStream(PInt32Required, len.pType.required), setup, m) {
      def init(elem: Continuation, eos: Continuation): Code =
        s"""
           |if (${len.v} > 0) {
           |  $i = 0;
           |  ${elem("false", "0")}
           |} else {
           |  ${eos()}
           |}
         """.stripMargin

      def step(elem: Continuation, eos: Continuation): Code =
        s"""
           |$i++;
           |if ($i < ${len.v}) {
           |  ${elem("false", i.toString())}
           |} else {
           |  ${eos()}
           |}
         """.stripMargin
    }
  }

  def fromContainer(fb: FunctionBuilder, container: EmitTriplet): PullStreamEmitter = {
    val a = container.memoize(fb)
    val atyp = coerce[PContainer](container.pType)
    val len = atyp.cxxLoadLength(a.v)
    val lent = EmitTriplet(PInt32(atyp.required), a.setup, a.m, len, region = null)
    range(fb, lent).map { (_, i) =>
      EmitTriplet(atyp.elementType,
        "",
        atyp.cxxIsElementMissing(a.v, i),
        loadIRIntermediate(atyp.elementType, atyp.cxxElementAddress(a.v, i)),
        region = null)
    }
  }
}

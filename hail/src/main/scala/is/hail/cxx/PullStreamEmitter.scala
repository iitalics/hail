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

  def mapArgs(f: Seq[Code] => Seq[Code]) = new Continuation(setup, numArgs) {
    def call(args: Seq[Code]) = self.call(f(args))
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

  def consume(f: (Code, Code) => Code): Code = {
    val loop = Continuation(fb, "loop", "eltm" -> PBooleanRequired, "eltv" -> elemType)
    val eltm = loop.vars(0)
    val eltv = loop.vars(1)
    val done = Continuation(fb, "done")
    s"""
       |${loop.setup}
       |${done.setup}
       |${init(loop, done)}
       |${loop.label}:
       |${f(eltm.toString(), eltv.toString())}
       |${step(loop, done)}
       |${done.label}:;
     """.stripMargin
  }

  def toArrayEmitter(arrayRegion: EmitRegion, sameRegion: Boolean): ArrayEmitter =
    new ArrayEmitter(pType, setup, m, "", None, arrayRegion) {
      def consume(f: (Code, Code) => Code): Code =
        self.consume { (m, v) =>
          s"""
             |${arrayRegion.defineIfUsed(sameRegion)}
             |${f(m, v)}
           """.stripMargin
        }
    }
}

object PullStreamEmitter {
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
}

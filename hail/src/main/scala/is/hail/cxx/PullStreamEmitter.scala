package is.hail.cxx

import is.hail.expr.types._
import is.hail.expr.types.physical._
import is.hail.utils._

case class EmitPullStream(
  setup: Code,
  m: Code,
  init: Code,
  step: Code
)

abstract class PullStreamEmitter(val fb: FunctionBuilder, val pType: PStream) { self =>

  def emit(elem: Label, eos: Label): EmitPullStream

  def elemType = pType.elementType

  def map(f: (Code, Code) => EmitTriplet): PullStreamEmitter = {
    val vm = fb.variable("vm", "bool")
    val vv = fb.variable("vv", typeToCXXType(elemType))
    val bodyt = f(vm.toString, vv.toString).memoize(fb)
    val newType = coerce[PStream](pType.copyStreamable(bodyt.pType))
    new PullStreamEmitter(fb, newType) {
      def emit(elem: Label, eos: Label): EmitPullStream = {
        /*
        val (mapElem, defineMapElemL) = lb.createWithMissingness("map", self.elemType)
        defineMapElemL { case Seq(m, v) =>
          s"""
             |${vm.define}
             |${vv.define}
             |$vm = $m;
             |$vv = $v;
             |${bodyt.setup}
             |${elem(bodyt.m, bodyt.v)}
           """.stripMargin
        }
        self.emit(lb, mapElem, eos)
         */
        ???
      }
    }
  }

  def filter(f: (Code, Code) => EmitTriplet): PullStreamEmitter = {
    val vm = fb.variable("vm", "bool")
    val vv = fb.variable("vv", typeToCXXType(elemType))
    val condt = f(vm.toString, vv.toString)
    assert(condt.pType isOfType PBoolean())
    new PullStreamEmitter(fb, pType) {
      def emit(elem: Label, eos: Label): EmitPullStream = {
        /*
        val (filterElem, defineFilterElemL) = lb.createWithMissingness("filter", self.elemType)
        val (step, defineStepL) = lb.createWithMissingness("step")
        val stream = self.emit(lb, filterElem, eos)
        defineFilterElemL { case Seq(m, v) =>
          s"""
             |${vm.define}
             |${vv.define}
             |$vm = $m;
             |$vv = $v;
             |${condt.setup}
             |if(!${condt.m} && ${condt.v}) {
             |  ${elem(m, v)}
             |} else {
             |  ${step()}
             |}
           """.stripMargin
        }
        // create join point for step so we don't repeat the step code
        defineStepL { _ => stream.step }
        EmitPullStream(stream.setup, stream.m, stream.init, step())
         */
        ???
      }
    }
  }

  def toArrayEmitter(arrayRegion: EmitRegion, sameRegion: Boolean): ArrayEmitter =
    PullStreamToAE(this, arrayRegion, sameRegion)
}

object PullStreamEmitter {
  def range(fb: FunctionBuilder, len: EmitTriplet): PullStreamEmitter = {
    assert(len.pType isOfType PInt32())
    new PullStreamEmitter(fb, PStream(PInt32Required, len.pType.required)) {
      def emit(elem: Label, eos: Label): EmitPullStream = {
        val i = fb.variable("i", "int")
        val n = fb.variable("n", "int")
        val setup = Code(i.define, n.define, len.setup)
        val init =
          s"""
             |$n = ${len.v};
             |if ($n > 0) {
             |  $i = 0;
             |  ${elem("false", "0")}
             |} else {
             |  ${eos()}
             |}
           """.stripMargin
        val step =
          s"""
             |++$i;
             |if($i < $n) {
             |  ${elem("false", i.toString)}
             |} else {
             |  ${eos()}
             |}
           """.stripMargin
        EmitPullStream(setup, len.m, init, step)
      }
    }
  }

  def empty(fb: FunctionBuilder, pType: PStream) =
    new PullStreamEmitter(fb, pType) {
      def emit(elem: Label, eos: Label): EmitPullStream =
        EmitPullStream("", "false", eos(), eos())
    }

  def fromTriplets(
    fb: FunctionBuilder,
    triplets: Seq[EmitTriplet],
    pType: PStream
  ): PullStreamEmitter =
    if (triplets.isEmpty)
      empty(fb, pType)
    else {
      assert(triplets.forall(_.pType isOfType pType.elementType))
      new PullStreamEmitter(fb, pType) {
        def emit(elem: Label, eos: Label): EmitPullStream = {
          val ts = triplets.map(_.memoize(fb))
          val i = fb.variable("i", "int")
          val init =
            s"""
               |$i = 0;
               |${elem(ts(0).m, ts(0).v)}
             """.stripMargin
          val step =
            s"""
               |switch (${i}++) {
               |${Code.sequence(ts.tail.zipWithIndex.map { case (t, idx) =>
                   s"case $idx: ${elem(t.m, t.v)}" }) }
               |default:
               |  ${eos()}
               |}
             """.stripMargin
          EmitPullStream(Code.sequence(i.define +: ts.map(_.setup)), "false", init, step)
        }
      }
    }
}

class PullStreamToAE(
  val base: PullStreamEmitter,
  arrayRegion: EmitRegion,
  sameRegion: Boolean,
  stream: EmitPullStream,
  elemm: Variable,
  elemv: Variable,
  elemLabel: Label,
  eosLabel: Label
) extends ArrayEmitter(
  base.pType,
  Code(elemm.define, elemv.define, stream.setup),
  stream.m,
  "", None, arrayRegion) {

  def consume(f: (Code, Code) => Code): Code =
    Code(
      /* TODO: initialize per-element region here */
      stream.init,
      elemLabel.define(
        s"""
           |${f(elemm.toString, elemv.toString)}
           |${/* TODO: initialize per-element region here */""}
           |${stream.step}
         """.stripMargin
      ),
      eosLabel.define(""))

  override def map(fb: FunctionBuilder)(f: EmitTriplet => EmitTriplet): ArrayEmitter =
    base.map { (m, v) => f(EmitTriplet(base.elemType, "", m, v, arrayRegion)) }
      .toArrayEmitter(arrayRegion, sameRegion)

  override def filter(fb: FunctionBuilder)(f: EmitTriplet => EmitTriplet): ArrayEmitter =
    base.filter { (m, v) => f(EmitTriplet(PBoolean(), "", m, v, arrayRegion)) }
      .toArrayEmitter(arrayRegion, sameRegion)
}

object PullStreamToAE {
  def apply(base: PullStreamEmitter, arrayRegion: EmitRegion, sameRegion: Boolean): PullStreamToAE = {
    val elemm = base.fb.variable("elem_m", "bool")
    val elemv = base.fb.variable("elem_v", typeToCXXType(base.elemType))
    val elemLabel = Label(base.fb.genSym("elem"), Seq(elemm, elemv))
    val eosLabel = Label(base.fb.genSym("eos"), Seq())
    val stream = base.emit(elemLabel, eosLabel)
    new PullStreamToAE(base, arrayRegion, sameRegion, stream,
      elemm, elemv, elemLabel, eosLabel)
  }
}

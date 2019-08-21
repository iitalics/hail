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

  def emit(lb: LabelBuilder, elem: Label, eos: Label): EmitPullStream

  def elemType = pType.elementType

  def map(f: (Code, Code) => EmitTriplet): PullStreamEmitter = {
    val vm = fb.variable("vm", "bool")
    val vv = fb.variable("vv", typeToCXXType(elemType))
    val bodyt = f(vm.toString, vv.toString).memoize(fb)
    val newType = coerce[PStream](pType.copyStreamable(bodyt.pType))
    new PullStreamEmitter(fb, newType) {
      def emit(lb: LabelBuilder, elem: Label, eos: Label): EmitPullStream = {
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
      }
    }
  }

  def filter(f: (Code, Code) => EmitTriplet): PullStreamEmitter = {
    val vm = fb.variable("vm", "bool")
    val vv = fb.variable("vv", typeToCXXType(elemType))
    val condt = f(vm.toString, vv.toString)
    assert(condt.pType isOfType PBoolean())
    new PullStreamEmitter(fb, pType) {
      def emit(lb: LabelBuilder, elem: Label, eos: Label): EmitPullStream = {
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
      def emit(lb: LabelBuilder, elem: Label, eos: Label): EmitPullStream = {
        val i = fb.variable("i", "int")
        val n = fb.variable("n", "int")
        val setup = Code(i.define, n.define, len.setup)
        val (step, defineStepL) = lb.create("step")
        defineStepL { _ =>
          s"""
             |++$i;
             |if($i < $n) {
             |  ${elem("false", i.toString)}
             |} else {
             |  ${eos()}
             |}
           """.stripMargin
        }
        val init =
          s"""
             |$n = ${len.v};
             |$i = -1;
             |${step()}
           """.stripMargin
        EmitPullStream(setup, len.m, init, step())
      }
    }
  }

  def empty(fb: FunctionBuilder, pType: PStream) =
    new PullStreamEmitter(fb, pType) {
      def emit(lb: LabelBuilder, elem: Label, eos: Label): EmitPullStream =
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
        def emit(lb: LabelBuilder, elem: Label, eos: Label): EmitPullStream = {
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
  lb: LabelBuilder,
  defineElemL: LabelBuilder.DefineF
) extends ArrayEmitter(base.pType, Code(lb.defineVars, stream.setup), stream.m, "", None, arrayRegion) {

  def consume(f: (Code, Code) => Code): Code = {
    defineElemL { case Seq(m, v) =>
      s"""
         |${f(m, v)}
         |${/* TODO: make per-element region here */""}
         |${stream.step}
       """.stripMargin
    }
    s"""
       |${/* TODO: make per-element region here */""}
       |${stream.init}
       |${lb.defineLabels}
     """.stripMargin
  }

  override def map(fb: FunctionBuilder)(f: EmitTriplet => EmitTriplet): ArrayEmitter =
    base.map { (m, v) => f(EmitTriplet(base.elemType, "", m, v, arrayRegion)) }
      .toArrayEmitter(arrayRegion, sameRegion)

  override def filter(fb: FunctionBuilder)(f: EmitTriplet => EmitTriplet): ArrayEmitter =
    base.filter { (m, v) => f(EmitTriplet(PBoolean(), "", m, v, arrayRegion)) }
      .toArrayEmitter(arrayRegion, sameRegion)
}

object PullStreamToAE {
  def apply(base: PullStreamEmitter, arrayRegion: EmitRegion, sameRegion: Boolean): PullStreamToAE = {
    val lb = new LabelBuilder(base.fb)
    val (elem, defineElemL) = lb.createWithMissingness("elem", base.elemType)
    val stream = base.emit(lb, elem, lb.exitLabel)
    new PullStreamToAE(base, arrayRegion, sameRegion, stream, lb, defineElemL)
  }
}

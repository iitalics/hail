package is.hail.cxx

import is.hail.expr.types._
import is.hail.expr.types.physical._
import is.hail.utils._

case class EmitPullStream(
  defineVars: Code,
  defineLabels: Code,
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
    val mapElem = Label(fb.genSym("map"), Seq(vm, vv))

    new PullStreamEmitter(fb, coerce[PStream](pType.copyStreamable(bodyt.pType))) {
      def emit(elem: Label, eos: Label): EmitPullStream = {
        val es = self.emit(mapElem, eos)
        val defineVars = Code(es.defineVars, vm.define, vv.define)
        val defineLabels = Code(es.defineLabels,
          mapElem.define(
            s"""
               |${bodyt.setup}
               |${elem(bodyt.m, bodyt.v)}
             """.stripMargin)
        )
        EmitPullStream(defineVars, defineLabels, es.setup, es.m, es.init, es.step)
      }
    }
  }

  def filter(f: (Code, Code) => EmitTriplet): PullStreamEmitter = {
    val vm = fb.variable("vm", "bool")
    val vv = fb.variable("vv", typeToCXXType(elemType))
    val condt = f(vm.toString, vv.toString)
    assert(condt.pType isOfType PBoolean())
    val filterElem = Label(fb.genSym("filter"), Seq(vm, vv))
    val step = Label(fb.genSym("step"), Seq())

    new PullStreamEmitter(fb, pType) {
      def emit(elem: Label, eos: Label): EmitPullStream = {
        val es = self.emit(filterElem, eos)
        val defineVars = Code(es.defineVars, vm.define, vv.define)
        val defineLabels = Code(es.defineLabels,
          filterElem.define(
            s"""
               |${condt.setup}
               |if(!${condt.m} && ${condt.v}) {
               |  ${elem(vm.toString, vv.toString)}
               |} else {
               |  ${step()}
               |}
             """.stripMargin),
          // create label for step so we don't repeat its code
          step.define(es.step)
        )
        EmitPullStream(es.defineVars, es.defineLabels, es.setup, es.m, es.init, step())
      }
    }
  }

  def flatMap(f: (Code, Code) => PullStreamEmitter): PullStreamEmitter = {
    val vm = fb.variable("vm", "bool")
    val vv = fb.variable("vv", typeToCXXType(elemType))
    val innerEmitter = f(vm.toString, vv.toString)
    val initInner = Label(fb.genSym("init_inner"), Seq(vm, vv))
    val stepOuter = Label(fb.genSym("step_outer"), Seq())

    new PullStreamEmitter(fb, coerce[PStream](pType.copyStreamable(innerEmitter.elemType))) {
      def emit(elem: Label, eos: Label): EmitPullStream = {
        val outer = self.emit(initInner, eos)
        val inner = innerEmitter.emit(elem, stepOuter)
        val defineVars = Code(outer.defineVars, inner.defineVars, vm.define, vv.define)
        val defineLabels = Code(outer.defineLabels, inner.defineLabels,
          initInner.define(
            s"""
               |${inner.setup}
               |if (${inner.m}) {
               |  ${stepOuter()}
               |} else {
               |  ${inner.init}
               |}
             """.stripMargin),
          stepOuter.define(outer.step)
        )
        EmitPullStream(defineVars, defineLabels, outer.setup, outer.m, outer.init, inner.step)
      }
    }
  }

  def toArrayEmitter(arrayRegion: EmitRegion, sameRegion: Boolean): ArrayEmitter =
    PullStreamToAE(this, arrayRegion, sameRegion)
}

object PullStreamEmitter {
  def range(fb: FunctionBuilder, len: EmitTriplet): PullStreamEmitter = {
    val incr = Label(fb.genSym("incr"), Seq())
    assert(len.pType isOfType PInt32())
    new PullStreamEmitter(fb, PStream(PInt32Required, len.pType.required)) {
      def emit(elem: Label, eos: Label): EmitPullStream = {
        val i = fb.variable("i", "int")
        val n = fb.variable("n", "int")
        val defineVars = Code(i.define, n.define)
        val defineLabels =
          incr.define(
            s"""
             |++$i;
             |if($i < $n) {
             |  ${elem("false", i.toString)}
             |} else {
             |  ${eos()}
             |}
             """.stripMargin)
        val init =
          s"""
             |$n = ${len.v};
             |$i = -1;
             |${incr()}
           """.stripMargin
        EmitPullStream(defineVars, defineLabels, len.setup, len.m, init, incr())
      }
    }
  }

  def fromTriplets(
    fb: FunctionBuilder,
    triplets: Seq[EmitTriplet],
    pType: PStream
  ): PullStreamEmitter = {
    assert(triplets.forall(_.pType isOfType pType.elementType))
    new PullStreamEmitter(fb, pType) {
      def emit(elem: Label, eos: Label): EmitPullStream = {
        val i = fb.variable("i", "int")
        val step = Label(fb.genSym("step"), Seq())
        EmitPullStream(
          i.define,
          step.define(
            s"""
               |switch($i++) {
               |${Code.sequence(triplets.zipWithIndex.map { case (t, idx) =>
                    s"case $idx: { ${t.setup} ${elem(t.m, t.v)} }"
                  })}
               |default:
               |  ${eos()}
               |}
             """.stripMargin),
          "",
          "false",
          s"""
             |$i = 0;
             |${step()}
             """.stripMargin,
          step())
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
  Code(elemm.define, elemv.define, stream.defineVars, stream.setup),
  stream.m,
  "", None, arrayRegion) {

  def consume(f: (Code, Code) => Code): Code =
    Code(
      /* TODO: initialize per-element region here */
      stream.init,
      stream.defineLabels,
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

  override def flatMap(fb: FunctionBuilder)(f: EmitTriplet => ArrayEmitter): ArrayEmitter =
    base.flatMap { (m, v) =>
      val t = EmitTriplet(base.elemType, "", m, v, arrayRegion)
      f(t) match {
        case x: PullStreamToAE => x.base
        case _ => fatal("cannot flatmap")
      }
    }.toArrayEmitter(arrayRegion, sameRegion)
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

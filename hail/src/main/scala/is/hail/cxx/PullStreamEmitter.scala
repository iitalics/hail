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
  def range(
    fb: FunctionBuilder,
    startt: EmitTriplet,
    stopt: EmitTriplet,
    stept: EmitTriplet,
    formatError: String => String = x => x
  ): PullStreamEmitter = {
    assert(Seq(startt, stopt, stept).forall(_.pType isOfType PInt32()))
    new PullStreamEmitter(fb, PStream(PInt32())) {
      def emit(elem: Label, eos: Label): EmitPullStream = {
        val v = fb.variable("v", "int")
        val stopv = fb.variable("stopv", "int")
        val stepv = fb.variable("stepv", "int")
        val i = fb.variable("i", "int")
        val len = fb.variable("len", "int")
        val llen = fb.variable("llen", "long")
        EmitPullStream(
          Code(stepv.define, len.define, i.define, v.define),
          "",
          s"${startt.setup} ${stopt.setup} ${stept.setup}",
          s"(${startt.m} || ${stopt.m} || ${stept.m})",
          s"""
             |${stopv.define}
             |${llen.define}
             |${v} = ${startt.v};
             |${stopv} = ${stopt.v};
             |${stepv} = ${stept.v};
             |if ($stepv == 0) {
             |  ${ fb.nativeError(formatError("Array range step size cannot be 0.")) }
             |} else if ($stepv < 0)
             |  $llen = ($v <= $stopv) ? 0l : ((long)$v - (long)$stopv - 1l) / (long)(-$stepv) + 1l;
             |else
             |  $llen = ($v >= $stopv) ? 0l : ((long)$stopv - (long)$v - 1l) / (long)$stepv + 1l;
             |if ($llen > INT_MAX) {
             |  ${ fb.nativeError(formatError("Array range cannot have more than INT_MAX elements")) }
             |} else {
             |  $len = ($llen < 0) ? 0 : (int)$llen;
             |}
             |if($len == 0) {
             |  ${eos()}
             |} else {
             |  $i = 0;
             |  ${elem("false", v.toString)}
             |}
           """.stripMargin,
          s"""
           |$i++;
           |$v += $stepv;
           |if($i < $len) {
           |  ${elem("false", v.toString)}
           |} else {
           |  ${eos()}
           |}
           """.stripMargin)
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

  def fromContainer(fb: FunctionBuilder, a: EmitTriplet): PullStreamEmitter = {
    assert(a.pType.isInstanceOf[PContainer])
    val aType = coerce[PContainer](a.pType)
    new PullStreamEmitter(fb, PStream(aType.elementType)) {
      def emit(elem: Label, eos: Label): EmitPullStream = {
        val array = fb.variable("array", typeToCXXType(aType))
        val len = aType.cxxLoadLength(array.toString)
        val elementEmitter = range(fb,
          EmitTriplet(PInt32(), "", "false", "0", null),
          EmitTriplet(PInt32(), "", "false", len, null),
          EmitTriplet(PInt32(), "", "false", "1", null)
        )
          .map { (_, i) =>
            EmitTriplet(elemType, "",
              aType.cxxIsElementMissing(array.toString, i),
              loadIRIntermediate(elemType, aType.cxxElementAddress(array.toString, i)),
              null)
          }
        val es = elementEmitter.emit(elem, eos)
        EmitPullStream(
          s"${es.defineVars} ${array.define}",
          es.defineLabels,
          a.setup,
          a.m,
          s"""
             |$array = ${a.v};
             |${es.init}
           """.stripMargin,
          es.step)
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

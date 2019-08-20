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

abstract class PullStreamEmitter(fb: FunctionBuilder, pType: PStream) { self =>

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

  def toArrayEmitter(arrayRegion: EmitRegion, sameRegion: Boolean): ArrayEmitter = {
    val lb = new LabelBuilder(fb)
    val (elem, defineElemL) = lb.createWithMissingness("elem", elemType)
    val EmitPullStream(setup, m, init, step) = emit(lb, elem, lb.exitLabel)
    new ArrayEmitter(pType, Code(lb.defineVars, setup), m, "", None, arrayRegion) {
      def consume(f: (Code, Code) => Code): Code = {
        defineElemL { case Seq(m, v) =>
          s"""
             |${f(m, v)}
             |${/* TODO: make per-element region here */""}
             |${step}
           """.stripMargin
        }
        s"""
           |${/* TODO: make per-element region here */""}
           |${init}
           |${lb.defineLabels}
         """.stripMargin
      }

      override def map(fb: FunctionBuilder)(f: EmitTriplet => EmitTriplet): ArrayEmitter =
        self.map { (m, v) => f(EmitTriplet(elemType, "", m, v, arrayRegion)) }
          .toArrayEmitter(arrayRegion, sameRegion)
    }
  }
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
}

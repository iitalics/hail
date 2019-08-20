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

  def toArrayEmitter(
    arrayRegion: EmitRegion,
    sameRegion: Boolean
  ): ArrayEmitter = {
    val lb = new LabelBuilder(fb)
    val (elemL, defineElemL) = lb.create("label", "m" -> "bool", "v" -> typeToCXXType(pType.elementType))

    val EmitPullStream(setup, m, init, step) = emit(lb, elemL, lb.exitLabel)

    new ArrayEmitter(pType, Code(lb.defineVars, setup), m, "", None, arrayRegion) {
      def consume(f: (Code, Code) => Code): Code = {
        defineElemL { case Seq(m, v) =>
          s"""
             |${f(m, v)}
             |${step}
           """.stripMargin
        }
        s"""
           |${init}
           |${lb.defineLabels}
         """.stripMargin
      }
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

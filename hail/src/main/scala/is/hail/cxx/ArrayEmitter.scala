package is.hail.cxx

import is.hail.expr.ir
import is.hail.expr.types._
import is.hail.expr.types.physical._
import is.hail.utils._

abstract class ArrayEmitter(
  val pType: PStream,
  val setup: Code,
  val m: Code,
  val setupLen: Code,
  val length: Option[Code],
  val arrayRegion: EmitRegion
) { self =>

  def consume(f: (Code, Code) => Code): Code

  private def elemType = pType.elementType

  def withSetup(pre: Code): ArrayEmitter =
    new ArrayEmitter(pType, Code(pre, setup), m, setupLen, length, arrayRegion) {
      def consume(f: (Code, Code) => Code) = self.consume(f)
    }

  def map(fb: FunctionBuilder)(f: EmitTriplet => EmitTriplet): ArrayEmitter = {
    val vm = fb.variable("m", "bool")
    val vv = fb.variable("v", typeToCXXType(elemType))
    val bodyt = f(EmitTriplet(elemType, "", vm.toString, vv.toString, arrayRegion))
    val newType = coerce[PStream](pType.copyStreamable(bodyt.pType))
    new ArrayEmitter(newType, setup, m, setupLen, length, arrayRegion) {
      def consume(f: (Code, Code) => Code): Code =
        self.consume { (m2: Code, v2: Code) =>
          s"""{
             |  ${ vm.define }
             |  ${ vv.define }
             |  $vm = $m2;
             |  if (!$vm)
             |    $vv = $v2;
             |  ${ bodyt.setup }
             |  ${ f(bodyt.m, bodyt.v) }
             |}
             |""".stripMargin
        }
    }
  }

  def filter(fb: FunctionBuilder)(f: EmitTriplet => EmitTriplet): ArrayEmitter = {
    val vm = fb.variable("m", "bool")
    val vv = fb.variable("v", typeToCXXType(elemType))
    val condt = f(EmitTriplet(elemType, "", vm.toString, vv.toString, arrayRegion))
    assert(condt.pType.isOfType(PBoolean()))
    new ArrayEmitter(pType, setup, m, setupLen, None, arrayRegion) {
      def consume(f: (Code, Code) => Code): Code =
        self.consume { (m2: Code, v2: Code) =>
          s"""{
             |  ${ vm.define }
             |  ${ vv.define }
             |  $vm = $m2;
             |  if (!$vm)
             |    $vv = $v2;
             |  ${ condt.setup }
             |  if (!${ condt.m } && ${ condt.v }) {
             |    ${ f(vm.toString, vv.toString) }
             |  }
             |}
             |""".stripMargin
        }
    }
  }

  def flatMap(fb: FunctionBuilder)(f: EmitTriplet => ArrayEmitter): ArrayEmitter = {
    val vm = fb.variable("m", "bool")
    val vv = fb.variable("v", typeToCXXType(elemType))
    val bodyae = f(EmitTriplet(elemType, "", vm.toString, vv.toString, arrayRegion))
    new ArrayEmitter(bodyae.pType, setup, m, setupLen, None, bodyae.arrayRegion) {
      def consume(f: (Code, Code) => Code): Code =
        self.consume { (m2: Code, v2: Code) =>
          s"""{
             |  ${ vm.define }
             |  ${ vv.define }
             |  $vm = $m2;
             |  if (!$vm)
             |    $vv = $v2;
             |  ${ bodyae.setup }
             |  if (!${ bodyae.m }) {
             |    ${ bodyae.setupLen }
             |    ${ bodyae.consume(f) }
             |  }
             |}
             |""".stripMargin
        }
    }
  }
}

object ArrayEmitter {

  def fromTriplets(
    fb: FunctionBuilder,
    arrayRegion: EmitRegion,
    sameRegion: Boolean,
    elemType: PType,
    triplets: Seq[EmitTriplet]): ArrayEmitter = {
    assert(triplets.forall(_.pType.isOfType(elemType)))
    new ArrayEmitter(PStream(elemType), "", "false", "", Some(triplets.length.toString), arrayRegion) {
      def consume(f: (Code, Code) => Code): Code = {
        val sb = new ArrayBuilder[Code]
        val m = fb.variable("argm", "bool")
        val v = fb.variable("argv", typeToCXXType(elemType))
        val cont = f(m.toString, v.toString)
        triplets.foreach { argt =>
          sb +=
            s"""{
               |  ${ arrayRegion.defineIfUsed(sameRegion) }
               |  ${ argt.setup }
               |  ${ m.defineWith(argt.m) }
               |  ${ v.defineWith(argt.v) }
               |  $cont
               |}
               |""".stripMargin
        }
        sb.result().mkString
      }
    }
  }

  def fromContainer(
    fb: FunctionBuilder,
    arrayRegion: EmitRegion,
    sameRegion: Boolean,
    aet: EmitTriplet): ArrayEmitter = {
    val atyp = coerce[PContainer](aet.pType)
    val a = fb.variable("a", "const char *", aet.v)
    val len = fb.variable("len", "int", atyp.cxxLoadLength(a.toString))
    val setupLen =
      s"""
         |${ a.define }
         |${ len.define }
         |""".stripMargin
    new ArrayEmitter(PStream(atyp.elementType), aet.setup, aet.m, setupLen, Some(len.toString), arrayRegion) {
      def consume(f: (Code, Code) => Code): Code = {
        val i = fb.variable("i", "int", "0")
        val cont = f(atyp.cxxIsElementMissing(a.toString, i.toString),
          loadIRIntermediate(atyp.elementType, atyp.cxxElementAddress(a.toString, i.toString)))
        s"""
           |for (${ i.define } $i < $len; ++$i) {
           |  ${ arrayRegion.defineIfUsed(sameRegion) }
           |  $cont
           |}
           |""".stripMargin
      }
    }
  }

  def range(
    fb: FunctionBuilder,
    arrayRegion: EmitRegion,
    sameRegion: Boolean,
    origIR: ir.IR,
    startt: EmitTriplet,
    stopt: EmitTriplet,
    stept: EmitTriplet): ArrayEmitter = {
    fb.translationUnitBuilder().include("<limits.h>")
    val startv = fb.variable("start", "int", startt.v)
    val stopv = fb.variable("stop", "int", stopt.v)
    val stepv = fb.variable("step", "int", stept.v)
    val len = fb.variable("len", "int")
    val llen = fb.variable("llen", "long")
    val s = StringEscapeUtils.escapeString(ir.Pretty.short(origIR))
    new ArrayEmitter(PStream(PInt32Required),
      s"""
         |${ startt.setup }
         |${ stopt.setup }
         |${ stept.setup }
         |""".stripMargin,
      s"(${ startt.m } || ${ stopt.m } || ${ stept.m })",
      s"""
         |${ startv.define }
         |${ stopv.define }
         |${ stepv.define }
         |${ len.define }
         |${ llen.define }
         |if ($stepv == 0) {
         |  ${ fb.nativeError("Array range step size cannot be 0.  IR: %s".format(s)) }
         |} else if ($stepv < 0)
         |  $llen = ($startv <= $stopv) ? 0l : ((long)$startv - (long)$stopv - 1l) / (long)(-$stepv) + 1l;
         |else
         |  $llen = ($startv >= $stopv) ? 0l : ((long)$stopv - (long)$startv - 1l) / (long)$stepv + 1l;
         |if ($llen > INT_MAX) {
         |  ${ fb.nativeError("Array range cannot have more than INT_MAX elements.  IR: %s".format(s)) }
         |} else
         |  $len = ($llen < 0) ? 0 : (int)$llen;
         |""".stripMargin,
      Some(len.toString),
      arrayRegion
    ) {
      def consume(f: (Code, Code) => Code): Code = {
        val i = fb.variable("i", "int", "0")
        val v = fb.variable("v", "int", startv.toString)
        s"""
           |${ v.define }
           |for (${ i.define } $i < $len; ++$i) {
           |  ${ arrayRegion.defineIfUsed(sameRegion) }
           |  ${ f("false", v.toString) }
           |  $v += $stepv;
           |}
           |""".stripMargin
      }
    }
  }

}

package is.hail.expr.ir.agg

import is.hail.annotations.{Region, StagedRegionValueBuilder}
import is.hail.asm4s._
import is.hail.expr.ir.{EmitTriplet, EmitFunctionBuilder}
import is.hail.expr.types.physical._
import is.hail.io.{BufferSpec, InputBuffer, OutputBuffer}

object MkStringAggregator extends StagedAggregator {

  val stateType = PStruct(
    "accum" -> PBinaryRequired
      // TODO:
      // "delim" -> PBinaryRequired
      // "insertDelim" -> PBooleanRequired
  )

  class State(
    fb: EmitFunctionBuilder[_]
  ) extends TypedRegionBackedAggState(stateType, fb) {
    override val regionSize: Int = Region.SMALL

    def initialize(): Code[Unit] =
      initF.invoke(region)
    private lazy val initF = {
      val mb = fb.newMethod("ropeInit", Array[TypeInfo[_]](typeInfo[Region]), typeInfo[Unit])
      mb.emit {
        val region = mb.getArg[Region](1).load
        val boff = mb.newLocal[Long]
        Code(
          boff := PBinary.allocate(region, 0),
          StagedRegionValueBuilder.deepCopy(fb, region, PBinaryRequired, boff,
            stateType.fieldOffset(off, "accum")))
      }
      mb
    }

    def append(bytes: Code[Long]): Code[Unit] =
      appendF.invoke(region, bytes)
    private lazy val appendF = {
      val mb = fb.newMethod("ropeAppend", Array[TypeInfo[_]](typeInfo[Region], typeInfo[Long]), typeInfo[Unit])
      mb.emit {
        val region = mb.getArg[Region](1).load
        val boff2 = mb.getArg[Long](2).load
        val boff1 = mb.newLocal[Long]
        val boff3 = mb.newLocal[Long]
        val len1 = mb.newLocal[Int]
        val len2 = mb.newLocal[Int]
        Code(
          boff1 := stateType.loadField(region, off, "accum"),
          len1 := PBinary.loadLength(boff1),
          len2 := PBinary.loadLength(boff2),
          boff3 := PBinary.allocate(region, len1 + len2),
          Region.copyFrom(PBinary.bytesOffset(boff1), PBinary.bytesOffset(boff3), len1.toL),
          Region.copyFrom(PBinary.bytesOffset(boff2), PBinary.bytesOffset(boff3) + len1.toL, len2.toL),
          Region.storeAddress(boff3, stateType.fieldOffset(off, "accum")))
      }
      mb
    }

    lazy val accumulator: Code[Long] =
      stateType.loadField(region, off, "accum")
  }

  def resultType: PType = PString()

  def createState(fb: EmitFunctionBuilder[_]): State = new State(fb)

  def initOp(state: State, init: Array[EmitTriplet], dummy: Boolean): Code[Unit] =
    state.initialize()

  def seqOp(state: State, seq: Array[EmitTriplet], dummy: Boolean): Code[Unit] = {
    val elt = seq(0)
    (!elt.m).orEmpty(
      state.append(elt.value))
  }

  def combOp(state: State, other: State, dummy: Boolean): Code[Unit] = {
    val mb = state.fb.newMethod("mkStringComb", Array[TypeInfo[_]](), typeInfo[Unit])
    mb.emit(state.append(other.accumulator))
    mb.invoke()
  }

  def result(state: State, srvb: StagedRegionValueBuilder, dummy: Boolean): Code[Unit] = {
    val mb = state.fb.newMethod("mkStringResult", Array[TypeInfo[_]](), typeInfo[Unit])
    mb.emit(srvb.addWithDeepCopy(PBinaryRequired, state.accumulator))
    mb.invoke()
  }
}

package is.hail.expr.ir

import is.hail.{ExecStrategy, HailSuite}
import is.hail.expr.types.virtual._
import is.hail.TestUtils._
import is.hail.utils._
import org.apache.spark.sql.Row
import org.testng.annotations.Test

class ArrayDeforestationSuite extends HailSuite {
  implicit val execStrats = ExecStrategy.values

  def primitiveArrayNoRegion(len: IR): IR =
    ArrayMap(
      ArrayRange(0, len, 1),
      "x1",
      Ref("x1", TInt32()) + 5)

  def arrayWithRegion(len: IR): IR =
    ArrayMap(
      ArrayRange(0, len, 1),
      "x2",
      MakeStruct(FastSeq[(String, IR)]("f1" -> (Ref("x2", TInt32()) + 1), "f2" -> 0)))

  def primitiveArrayWithRegion(len: IR): IR = {
    val array = arrayWithRegion(len)
    ArrayMap(
      array,
      "x3",
      GetField(Ref("x3", coerce[TArray](array.typ).elementType), "f1"))
  }

  def arrayFoldWithStructWithPrimitiveValues(len: IR, max1: Int, max2: Int): IR = {
    val zero = MakeStruct(FastSeq[(String, IR)]("max1" -> max1, "max2" -> max2))
    val accum = Ref(genUID(), zero.typ)
    val value = Ref(genUID(), TInt32())
    ArrayFold(
      primitiveArrayWithRegion(len),
      zero,
      accum.name, value.name,
      If(value > GetField(accum, "max1"),
        MakeStruct(FastSeq("max1" -> value, "max2" -> GetField(accum, "max1"))),
        If(value > GetField(accum, "max2"),
          MakeStruct(FastSeq("max1" -> GetField(accum, "max1"), "max2" -> value)),
          accum)))
  }

  def arrayFoldWithStruct(len: IR, v1: Int, v2: Int): IR = {
    val zero = MakeTuple.ordered(FastSeq(
      MakeStruct(FastSeq[(String, IR)]("f1" -> v1, "f2" -> v2)),
      MakeStruct(FastSeq[(String, IR)]("f1" -> v1, "f2" -> v2))))
    val array = arrayWithRegion(len)
    val accum = Ref(genUID(), zero.typ)
    val value = Ref(genUID(), coerce[TArray](array.typ).elementType)
    ArrayFold(
      array,
      zero,
      accum.name, value.name,
      MakeTuple.ordered(FastSeq(GetTupleElement(accum, 1), value)))
  }

  @Test def testArrayFold() {
    assertEvalsTo(arrayFoldWithStructWithPrimitiveValues(5, -5, -6), Row(5, 4))
    assertEvalsTo(arrayFoldWithStruct(5, -5, -6), Row(Row(4, 0), Row(5, 0)))
  }

  @Test def testStreamifyNestedSort() {
    val t = TInt32()
    val x = Ref("x", t)
    assertEvalsTo(
      ArrayMap(
        ArraySort(
          MakeArray(
            Seq(I32(1), I32(2), I32(3)),
            TArray(t))),
        "x",
        x * 5),
      IndexedSeq(5, 10, 15))
  }

  @Test def testArrayLeftJoin() {
    val l = Ref(genUID(), TInt32())
    val r = Ref(genUID(), TInt32())
    val left = ArrayRange(0, 10, 1)
    val right = MakeArray(Seq(2, 5, 8), TArray(TInt32()))
    assertEvalsTo(
      ArrayLeftJoinDistinct(
        left, right,
        l.name, r.name,
        l - r,
        If(IsNA(r), l, 0)),
      IndexedSeq(0, 1, 0, 3, 4, 0, 6, 7, 0, 9))
  }
}

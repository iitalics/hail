package is.hail.expr.ir

import is.hail.{ExecStrategy, HailSuite}
import is.hail.expr.types.virtual._
import is.hail.TestUtils._
import is.hail.utils._
import org.apache.spark.sql.Row
import org.testng.annotations.Test

class ArrayDeforestationSuite extends HailSuite {
  implicit val execStrats = ExecStrategy.values

  // primitiveArrayNoRegion(len) = "[5, 6, ..., len+4]"
  def primitiveArrayNoRegion(len: IR): IR =
    ArrayMap(
      ArrayRange(0, len, 1),
      "x1",
      Ref("x1", TInt32()) + 5)

  // arrayWithRegion(len) = "[{f1=1,f2=0}, {f1=2,f2=0}, ..., {f1=len,f2=0}]"
  def arrayWithRegion(len: IR): IR =
    ArrayMap(
      ArrayRange(0, len, 1),
      "x2",
      MakeStruct(FastSeq[(String, IR)]("f1" -> (Ref("x2", TInt32()) + 1), "f2" -> 0)))

  // primitiveArrayWithRegion(len) = "[1, 2, ..., len]"
  def primitiveArrayWithRegion(len: IR): IR = {
    val array = arrayWithRegion(len)
    ArrayMap(
      array,
      "x3",
      GetField(Ref("x3", coerce[TArray](array.typ).elementType), "f1"))
  }

  def max2(array: IR): IR = {
    val zero = MakeStruct(FastSeq[(String, IR)]("max1" -> -999, "max2" -> -999))
    val accum = Ref(genUID(), zero.typ)
    val value = Ref(genUID(), TInt32())
    ArrayFold(array,
      zero,
      accum.name, value.name,
      If(value > GetField(accum, "max1"),
        MakeStruct(FastSeq("max1" -> value, "max2" -> GetField(accum, "max1"))),
        If(value > GetField(accum, "max2"),
          MakeStruct(FastSeq("max1" -> GetField(accum, "max1"), "max2" -> value)),
          accum)))
  }

  def last2(array: IR): IR = {
    val elemType = array.typ.asInstanceOf[TArray].elementType
    val zero = MakeTuple.ordered(FastSeq(NA(elemType), NA(elemType)))
    val accum = Ref(genUID(), zero.typ)
    val value = Ref(genUID(), elemType)
    ArrayFold(array,
      zero,
      accum.name, value.name,
      MakeTuple.ordered(FastSeq(GetTupleElement(accum, 1), value)))
  }

  @Test def testArrayFoldMax2() {
    assertEvalsTo(max2(primitiveArrayNoRegion(5)), Row(9, 8))
    assertEvalsTo(max2(primitiveArrayWithRegion(5)), Row(5, 4))
  }

  @Test def testArrayFoldLast2() {
    assertEvalsTo(last2(primitiveArrayNoRegion(5)), Row(8, 9))
    assertEvalsTo(last2(arrayWithRegion(5)), Row(Row(4, 0), Row(5, 0)))
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
}

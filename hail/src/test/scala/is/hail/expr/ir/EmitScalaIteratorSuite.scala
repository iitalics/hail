package is.hail.expr.ir

import org.testng.annotations.Test
import is.hail.asm4s._
import is.hail.asm4s.joinpoint._
import is.hail.expr.types.physical._
import is.hail.annotations.{Region, RegionValue, SafeRow}
import is.hail.{ExecStrategy, HailSuite}
import is.hail.utils._

class EmitScalaIteratorSuite extends HailSuite {
  import EmitScalaIterator._

  def evalSRVI(t: PType, mk: EmitFunctionBuilder[_] => StagedRegionValueIterator): IndexedSeq[Any] = {
    val ab = new ArrayBuilder[Any](16)
    Region.scoped { r =>
      val fb = buildIterator(mk)
      val f = fb.resultWithIndex()(0, r)
      for (rv <- iterator(f))
        ab += SafeRow(PTuple(t), rv)(0)
    }
    ab.result()
  }

  object StagedEmpty extends StagedRegionValueIterator {
    def init(mb: MethodBuilder): Code[Unit] = Code._empty
    def step(mb: MethodBuilder, jb: JoinPointBuilder)(k: Step => Code[Ctrl]): Code[Ctrl] = k(EOI)
  }

  class StagedRange(
    fb: EmitFunctionBuilder[_],
    start: Code[Int], stop: Code[Int]
  ) extends StagedRegionValueIterator {
    val idx = fb.newField[Int]
    val r = fb.partitionRegion.load

    def init(mb: MethodBuilder): Code[Unit] =
      idx := start

    def step(mb: MethodBuilder, jb: JoinPointBuilder)(k: Step => Code[Ctrl]): Code[Ctrl] =
      (idx >= stop).mux(k(EOI), {
        val off = mb.newLocal[Long]
        Code(
          off := PTuple(PInt32Required).allocate(r),
          Region.storeInt(off, idx),
          idx := idx + 1,
          k(YieldRV(r, off)))
      })
  }

  @Test def testEmpty() = {
    assert(evalSRVI(PInt32Required, _ => StagedEmpty) == IndexedSeq.empty)
  }

  @Test def testRange() = {
    for {
      i <- 0 to 5
      j <- 0 to 10
    }
      assert(evalSRVI(PInt32Required, new StagedRange(_, i, j))
        == IndexedSeq.range(i, j),
        s"range($i, $j)")
  }
}

package is.hail.cxx

import is.hail.TestUtils._
import is.hail.utils._
import is.hail.expr.types.physical._
import is.hail.cxx.{ArgumentPack => P}

import org.scalatest.testng.TestNGSuite
import org.testng.annotations.Test

class StreamEmitterSuite extends TestNGSuite {
  import CompileUtils._

  def id[A](fb: FunctionBuilder, pack: ArgumentPack[A]) =
    StagedParameterizedStream.id(fb, pack)

  def prange(fb: FunctionBuilder) =
    StagedParameterizedStream.range(fb)

  def range(fb: FunctionBuilder, len: Code) =
    StagedParameterizedStream.range(fb)(len)

  def stagedSum(stream: StagedStream[Code]): Code =
    EmitLabel.withReturnCont(stream.fb, P.int64) { kRet =>
      stream.consume[Code](
        P.int64,
        "(long) 0",
        (sum, elt, k) => k(s"($sum + (long) $elt /*--FOLD EXPR--*/)"),
        kRet(_))
    }

  @Test def testSumRange() {
    val sum = compileL1 { (fb, arg) =>
      stagedSum(range(fb, arg))
    }
    for (i <- 1 to 50) {
      val ans = i*(i-1)/2
      assert(sum(i) == ans, s"sum($i)")
    }
  }

  @Test def testSumMapRange() {
    val sum = compileL1 { (fb, arg) =>
      stagedSum(range(fb, arg).map(P.int32) { v => s"($v * $v /*--MAP EXPR--*/)" })
    }
    for (i <- 1 to 50) {
      val ans = i*(i-1)*(2*i-1)/6
      assert(sum(i) == ans, s"sum(map(sqr, $i))")
    }
  }

  @Test def testSumZip() {
    val sum = compileL1 { (fb, arg) =>
      val len1 = s"($arg * 2)"
      val len2 = s"($arg + 3)"
      stagedSum(
        range(fb, len1)
          .zip(range(fb, len2))
          .map(P.int32) { case (n, m) => s"($n + $m)" }
      )
    }
    assert(sum(2) == 2*(0+1+2+3), "sum(range(4) + range(5))")
    assert(sum(5) == 2*(0+1+2+3+4+5+6+7), "sum(range(10) + range(8))")
  }

  @Test def testSumFlatMap() {
    val sum = compileL1 { (fb, arg) =>
      stagedSum(range(fb, arg).flatMap(prange(fb)))
    }
    assert(sum(5) == 0+(0+1)+(0+1+2)+(0+1+2+3), "sum(range(5).flatMap(range))")
  }

  @Test def testSumFlatMapZip() {
    val f = compileL1 { (fb, arg) =>
      val len1 = arg
      val len2 = s"($arg * 2)"
      stagedSum(
        range(fb, len1)
          .flatMap(prange(fb))
          .zip(range(fb, len2))
          .map(P.int32) { case (n,m) => s"($n * $m /*--MAP EXPR--*/)" }
      )
    }
    for (i <- 1 to 50) {
      val ans = ((0 until i)
        .flatMap(0 until _)
        .zip(0 until (i * 2))
        .foldLeft(0) { case (a,(n,m)) => a + n*m  })
      assert(f(i) == ans, s"f($i)")
    }
  }

  @Test def testSumFlatMapZip2() {
    val f = compileL1 { (fb, arg) =>
      val len1 = arg
      val len2 = s"($arg * 2)"
      stagedSum(
        range(fb, len2)
          .zip(range(fb, len1).flatMap(prange(fb)))
          .map(P.int32) { case (m,n) => s"($n * $m /*--MAP EXPR--*/)" }
      )
    }
    for (i <- 1 to 50) {
      val ans = ((0 until i)
        .flatMap(0 until _)
        .zip(0 until (i * 2))
        .foldLeft(0) { case (a,(n,m)) => a + n*m  })
      assert(f(i) == ans, s"f($i)")
    }
  }

  @Test def testFlatMapMissingInnerStream() {
    val f = compileL1 { (fb, arg) =>
      stagedSum(
        range(fb, arg)
          .flatMap(prange(fb)
            .guard(P.int32, { i =>
              val j = fb.variable("j", "int", i)
              (j.define, s"($j == 3)", s"($j * 2 /*--GUARD EXPR--*/)")
            }))
      )
    }
    for (i <- 1 to 50) {
      val ans = (0 until i)
        .filter(_ != 3)
        .map(_ * 2)
        .flatMap(0 until _)
        .foldLeft(0)(_ + _)
      assert(f(i) == ans, s"f($i)")
    }
  }

}

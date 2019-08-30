package is.hail.cxx

import is.hail.TestUtils._
import is.hail.utils._
import is.hail.expr.types.physical._

import org.scalatest.testng.TestNGSuite
import org.testng.annotations.Test

class ArgumentPackSuite extends TestNGSuite {
  import CompileUtils._
  import is.hail.cxx.{ArgumentPack => P}

  @Test def testControlFlow() {
    val f = compileL1 { (fb, arg) =>
      EmitLabel.withReturnCont(fb, P.int64) { k =>
        val lb = new LabelBuilder(fb)
        val loop = lb.label("loop", P.tuple2(P.int64, P.int64))
        lb.define(loop) { case (acc, i) =>
          s"""
             |if ($i <= 0) {
             |  ${k(acc)}
             |} else {
             |  ${loop((s"$acc * $i", s"$i - 1"))}
             |}
           """.stripMargin
        }
        s"""
           |${lb.end()}
           |${loop(("1", arg))}
         """.stripMargin
      }
    }

    assert(f(1) == 1)
    assert(f(2) == 2)
    assert(f(3) == 6)
    assert(f(4) == 24)
    assert(f(5) == 120)
    assert(f(6) == 720)
  }
}

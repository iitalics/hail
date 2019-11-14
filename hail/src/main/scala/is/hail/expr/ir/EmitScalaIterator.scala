package is.hail.expr.ir

import is.hail.asm4s._
import is.hail.asm4s.joinpoint._
import is.hail.annotations.{Region, RegionValue}
import is.hail.expr.types.physical.PType
import java.io.PrintWriter

object EmitScalaIterator {

  trait AsmIteratorFunction[E] extends AsmFunction0[Boolean] {
    def element: E
  }

  def iterator[E](f: AsmIteratorFunction[E]): Iterator[E] = new Iterator[E] {
    var _memo = false
    var _next = false

    def hasNext: Boolean = {
      if (!_memo)
        _next = f.apply()
      _memo = true
      _next
    }

    def next(): E = {
      if (!hasNext)
        return Iterator.empty.next()
      _memo = false
      f.element
    }
  }

  sealed trait Step
  case object EOI extends Step
  case class YieldRV(region: Code[Region], off: Code[Long]) extends Step

  trait StagedRegionValueIterator {
    def init(mb: MethodBuilder): Code[Unit]
    def step(mb: MethodBuilder, jb: JoinPointBuilder)(k: Step => Code[Ctrl]): Code[Ctrl]
  }

  def buildIterator(
    mkSRVI: EmitFunctionBuilder[_] => StagedRegionValueIterator
  ): EmitFunctionBuilder[AsmIteratorFunction[RegionValue]] = {
    val fb = new EmitFunctionBuilder[AsmIteratorFunction[RegionValue]](Array(), GenericTypeInfo[Boolean])
    val srvi = mkSRVI(fb)

    val elem = fb.newField[RegionValue]("elem")
    val elemF = new MethodBuilder(fb, "element", Array(), typeInfo[java.lang.Object])
    elemF.emit(elem.load)
    fb.methods += elemF

    val initF = fb.newMethod("init", Array[TypeInfo[_]](), typeInfo[Unit])
    initF.emit(srvi.init(initF))
    fb.addInitInstructions(Code(
      elem := Code.invokeScalaObject[RegionValue](RegionValue.getClass, "apply"),
      initF.invoke[Unit]()))

    val stepF = fb.apply_method
    stepF.emit(JoinPoint.CallCC[Code[Boolean]] { (jb, ret) =>
      srvi.step(stepF, jb) {
        case EOI => ret(false)
        case YieldRV(r, off) =>
          Code(elem.invoke[Region, Long, Unit]("set", r, off), ret(true))
      }
    })

    fb
  }
}

package is.hail.cxx

import is.hail.expr.types.physical._
import scala.language.implicitConversions


object VariablePack {
  def typed(fb: FunctionBuilder, pType: PType, prefix: String): VariablePack[Code] =
    TypedVariablePack(fb.variable(prefix, typeToCXXType(pType)))
}

abstract class VariablePack[A] { self =>
  def define: Code
  def load: A
  def store(a: A): Code

  def map[B](from: B => A, to: A => B): VariablePack[B] = new VariablePack[B] {
    def define: Code = self.define
    def load: B = to(self.load)
    def store(b: B): Code = self.store(from(b))
  }
}

case class TypedVariablePack(vr: Variable) extends VariablePack[Code] {
  def define = vr.define
  def load = vr.name
  def store(rhs: Code): Code = s"${vr.name} = $rhs;"
}


object ArgumentPack {

  def typed(pType: PType) = new ArgumentPack[Code] {
    def variables(fb: FunctionBuilder, prefix: String) =
      VariablePack.typed(fb, pType, prefix)
  }

  val int32 = typed(PInt32())
  val int64 = typed(PInt64())
  val boolean = typed(PBoolean())

  val unit = new ArgumentPack[Unit] {
    def variables(fb: FunctionBuilder, prefix: String) = new VariablePack[Unit] {
      val define = ""
      val load = ()
      def store(u: Unit): Code = ""
    }
  }

  def tuple2[A,B](
    aPack: ArgumentPack[A],
    bPack: ArgumentPack[B]
  ) = new ArgumentPack[(A,B)] {
    def variables(fb: FunctionBuilder, prefix: String) = new VariablePack[(A,B)] {
      val aVars = aPack.variables(fb, prefix)
      val bVars = bPack.variables(fb, prefix)
      def define = Code(aVars.define, bVars.define)
      def load: (A,B) = (aVars.load, bVars.load)
      def store(t: (A,B)) = Code(aVars.store(t._1), bVars.store(t._2))
    }
  }

  def tuple3[A,B,C](
    aPack: ArgumentPack[A],
    bPack: ArgumentPack[B],
    cPack: ArgumentPack[C]
  ) = new ArgumentPack[(A,B,C)] {
    def variables(fb: FunctionBuilder, prefix: String) = new VariablePack[(A,B,C)] {
      val aVars = aPack.variables(fb, prefix)
      val bVars = bPack.variables(fb, prefix)
      val cVars = cPack.variables(fb, prefix)
      def define = Code(aVars.define, bVars.define, cVars.define)
      def load: (A,B,C) = (aVars.load, bVars.load, cVars.load)
      def store(t: (A,B,C)) = Code(aVars.store(t._1), bVars.store(t._2), cVars.store(t._3))
    }
  }
}

abstract class ArgumentPack[A] { self =>
  def variables(fb: FunctionBuilder, prefix: String): VariablePack[A]

  def memoize(fb: FunctionBuilder, a: A): (Code, A) = {
    val vars = variables(fb, "tmp")
    (Code(vars.define, vars.store(a)), vars.load)
  }
}

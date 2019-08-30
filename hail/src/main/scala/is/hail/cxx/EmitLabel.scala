package is.hail.cxx

import is.hail.expr.types.physical._
import is.hail.utils._
import scala.collection.mutable.{Map, ArrayBuilder}

object EmitLabel {

  def withReturnCont(fb: FunctionBuilder, pack: ArgumentPack[Code])(
    f: EmitLabel[Code] => Code
  ): Code = {
    val kont = EmitLabel(fb, pack, "k", "ret")
    val body = f(kont)
    s"""
       |({
       |  ${kont.args.define}
       |  { $body }
       |  $kont:
       |  ${kont.args.load};
       |})
     """.stripMargin
  }

  def apply[A](fb: FunctionBuilder, pack: ArgumentPack[A], name: String, varName: String): EmitLabel[A] =
    EmitLabel(fb.genSym(name), pack, pack.variables(fb, varName))
}

case class EmitLabel[A](name: String, pack: ArgumentPack[A], args: VariablePack[A]) {
  override def toString: String = name

  def apply(arg: A): Code =
    s"""
       |${args.store(arg)}
       |goto $name;
     """.stripMargin
}


class LabelBuilder(fb: FunctionBuilder) {

  private var _labels: List[EmitLabel[_]] = Nil
  private val _definitions = Map.empty[String, String]

  def label[A](namePrefix: String, pack: ArgumentPack[A]): EmitLabel[A] = {
    val l = EmitLabel[A](fb, pack, namePrefix, "in")
    _labels = l :: _labels
    l
  }

  def define[A](l: EmitLabel[A])(f: A => Code): Unit = {
    if (_definitions.get(l.name).isDefined)
      fatal(s"attempt to re-define label ${l.name}")

    val (setup, args) = l.pack.memoize(fb, l.args.load)
    val body = Code(setup, f(args))
    _definitions += l.name -> body
  }

  lazy val hopOver = fb.genSym("hop_over")

  def end(): Code = {
    for (l <- _labels) {
      if (_definitions.get(l.name).isEmpty)
        fatal(s"label created but never defined: ${l.name}")
    }
    s"""
       |${Code.sequence(_labels.map { l => l.args.define })}
       |goto $hopOver;
       |${Code.sequence(_labels.map { l =>
            s"$l: { ${_definitions(l.name)} }"
         })}
      |$hopOver:;
     """.stripMargin
  }

}

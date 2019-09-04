package is.hail.cxx

import is.hail.expr.types.physical._
import is.hail.utils._
import scala.collection.mutable.{Map, ArrayBuilder}

object EmitLabel {
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
  private lazy val _hopOver = fb.genSym("hop_over")

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

  def end(): Code =
    s"""
       |${Code.sequence(_labels.map { l => l.args.define })}
       |goto ${_hopOver};
       |${Code.sequence(_definitions.toSeq.map { case (l, body) =>
            s"$l: { $body }"
         })}
      |${_hopOver}:
     """.stripMargin

}

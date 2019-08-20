package is.hail.cxx

import scala.collection.mutable.{ArrayBuilder, Map}
import is.hail.expr.types.physical._

case class Label(
  val name: String,
  val vars: Seq[Variable]
) { self =>

  def numArgs = vars.length

  def call(args: Seq[Code]): Code = {
    assert(args.length == vars.length)
    Code.sequence(
      vars.zip(args).map { case (v, arg) => s"$v = $arg;" }
        :+ s"goto $name;")
  }

  def apply(args: Code*): Code = call(args)
}

object LabelBuilder {
  type DefineF = (Seq[Code] => Code) => Unit
}

class LabelBuilder(fb: FunctionBuilder) {
  import LabelBuilder._

  private var allVars = ArrayBuilder.make[Variable]
  private var labelDefinitions = Map.empty[Label, Code]
  private var _exitLabel: Option[Label] = None

  def exitLabel: Label = _exitLabel match {
    case Some(l) => l
    case None => {
      val l = create("exit")._1
      _exitLabel = Some(l)
      l
    }
  }

  def defineVars =
    Code.defineVars(allVars.result().toSeq)

  def defineLabels =
    Code.sequence(
      labelDefinitions.toSeq.map { case (l, body) =>
        s"""
           |${l.name}: {
           |$body
           |}
         """.stripMargin
      } :+ _exitLabel.map(_.name + ":;").getOrElse(""))

  def emit(entryPoint: Code): Code =
    s"""
       |$defineVars
       |$entryPoint
       |$defineLabels
     """.stripMargin

  def create(labelPrefix: String, args: (String, String)*): (Label, DefineF) = {
    val label = fb.genSym(labelPrefix)
    val vars = args.map { case (prefix, typ) => fb.variable(prefix, typ) }
    val cont = Label(label, vars)
    val define: DefineF = f => labelDefinitions += (cont -> f(vars.map(_.toString)))
    allVars ++= vars
    (cont, define)
  }

  def createWithMissingness(p: String, args: PType*): (Label, DefineF) =
    create(p, args.flatMap { t => Seq(s"m" -> "bool", s"v" -> typeToCXXType(t))}: _*)
}

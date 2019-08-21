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

  def define(body: Code): Code =
    s"""
       |${name}: {
       |  $body
       |}
     """.stripMargin
}

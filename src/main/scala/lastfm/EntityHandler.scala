package lastfm

import java.io.StringReader

import scala.util.parsing.combinator.JavaTokenParsers

/**
  * Created by erodriguez on 01/12/16.
  */
class EntityHandler extends JavaTokenParsers {

  def string: Parser[Any] =
    """".*" """.r

  def artist: Parser[Any] = "ARTIST"~string
  def album: Parser[Any] = "ALBUM"~string
  def entity: Parser[Any] = artist | album

  //def command: Parser[Any] = ("GET"|"ADD")~entity
  def command: Parser[Any] = "GET"~entity

  def operator: Parser[Any] = ("like" | "from" | "until" )~string
  def expr: Parser[Any] = command~opt(operator)

  def block : Parser[Any] = "{"~rep(command)~"}"
  def ifClause :Parser[Any] = "if"~"("~expr~")"~block
  def elseIfClause : Parser[Any] = "else"~ifClause
  def elseClause : Parser[Any] = "else"~block
  def ifElse : Parser[Any] = ifClause~opt(rep(elseIfClause))~opt(elseClause)

  def commandList = rep(command | ifElse)
}

object EntityHandlerTest extends EntityHandler {

  def main(args: Array[String]): Unit = {
    val cmd1 = """GET ARTIST "inxs" """
    val cmd2 = """GET ALBUM "Best Hits" """
    val cmd3 = """GET ALBUM "Best" from "1980" """
    val cmds = List(cmd1, cmd2, cmd3)
    cmds.map(cmd => println(parseAll(commandList, new StringReader(cmd))))
  }
}
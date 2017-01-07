package lastfm

import java.io.StringReader

import scala.util.parsing.combinator.JavaTokenParsers

/**
  * Created by erodriguez on 01/12/16.
  */
object LastFMAPI {
  def callAPI(endpoint: String): String = endpoint
  def getArtist(name: String): String = callAPI(s"/2.0/?method=artist.getinfo&artist=$name&api_key=YOUR_API_KEY&format=json")
  def getAlbum(artist: String, album:String): String = callAPI(s"/2.0/?method=album.getinfo&api_key=YOUR_API_KEY&artist=$artist&album=$album&format=json")
  def searchAlbum(name: String): String = callAPI(s" /2.0/?method=album.search&album=$name&api_key=YOUR_API_KEY&format=json")
}

sealed abstract class Command {
  def eval(): String
}

case class DirectCommand(entity: Entity) extends Command {
  override def eval(): String = entity match {
    case ARTIST(name) => LastFMAPI.getArtist(name)
    case ALBUM(name) => LastFMAPI.searchAlbum(name)
  }
}

object EntityHandler extends JavaTokenParsers {

  def artist: Parser[Entity] = "ARTIST"~stringLiteral ^^ { s => new ARTIST(s._2) }
  def album: Parser[Entity] = "ALBUM"~stringLiteral ^^ { s => new ALBUM(s._2) }
  def entity: Parser[Entity] = artist | album

  //def command: Parser[Any] = ("GET"|"ADD")~entity
  def command: Parser[Command] = "GET"~entity ^^ { e => DirectCommand(e._2) }

  def operator: Parser[Any] = ( "like" | "from" | "until" )~stringLiteral
  def expr: Parser[Any] = command ~ opt(operator)

  def block : Parser[Any] = "{" ~ rep(command) ~ "}"
  def ifClause :Parser[Any] = "if" ~ "(" ~ expr ~ ")" ~ block
  def elseIfClause : Parser[Any] = "else" ~ ifClause
  def elseClause : Parser[Any] = "else" ~ block
  def ifElse : Parser[Any] = ifClause ~ opt(rep(elseIfClause)) ~ opt(elseClause)

  def commandList = rep(expr | ifElse)

  /*def parse(s:String) = {
    val tokens = new lexical.Scanner(s)
    phrase(expr)(tokens)
  }*/

  /*def apply(s:String): Command = {
    parse(command, new StringReader(s)) match {
      case Success(tree, _) => tree
      case e: NoSuccess =>
        throw new IllegalArgumentException("Bad syntax: "+s)
    }
  }*/

  def main(args: Array[String]): Unit = {
    val cmd1 = """GET ARTIST "inxs" """
    val cmd2 = """GET ALBUM "Best Hits" """
    val cmd3 = """GET ALBUM "Best" from "1980" """
    val cmds = List(cmd1, cmd2, cmd3)
    /*cmds.map(cmd => parseAll(commandList, new StringReader(cmd))) match {
      case Success(t, c) => println("Tree: " + t)
      case e: NoSuccess => Console.err.println(e)
    }*/
    cmds.map(cmd => parseAll(command, new StringReader(cmd)) match {
      case Success(result, _) =>
        println(result)
        println(result.eval())
      case e: NoSuccess => Console.err.println(e)
    })
  }
}
package lastfm

/**
  * Created by erodriguez on 01/12/16.
  */
sealed trait Entity {
  val name: String
}

case class ARTIST(name: String) extends Entity
case class ALBUM(name: String) extends Entity

sealed trait LFMProgram[T]
case class Get(entity: Entity) extends LFMProgram[String]
case class Add(entity: Entity) extends LFMProgram[Unit]

//def string: Parse

object LastFMDSL {
  /*def getArtist(name: String): LFMProgram[String] = {
    for {
      artist <- Artist(name)
    } yield Get(artist)
  }*/

  val artists: List[ARTIST] = List(ARTIST("extreme"), ARTIST("inxs"))

  object Interpreter {
    def run[T](program: LFMProgram[T]): T = {
      program match {
        case Get(entity) => artists.filter(artist => artist.name == entity.name).head.name
        case Add(entity) => println(s"Added entity $entity")
      }
    }
  }

  def main(args: Array[String]): Unit = {
    //println(Interpreter.run(getArtist()))
  }
}

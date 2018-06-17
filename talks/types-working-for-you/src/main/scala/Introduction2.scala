package klarna

import cats.implicits._
import shapeless._, record._, ops.record._

object Introduction2 {

  case class Persona(id: Long, name: String)

  val persona = Persona(1337, "Darude Dude")

  val gen = Generic[Persona]

  // Int :: String :: HNil
  val repr = gen.to(persona)

  // This isn't very good
  println(repr)

  def show[H, T <: HList](xs: H :: T)(
    implicit
    f: H :: T => String
  ): String =
    ???

  implicit val HNilRep: HNil => String =
    _ => "HNil"

  implicit val LongRep: Long => String =
    _.toString

  implicit val StringRep: String => String =
    "\"" ++ _ ++ "\""

}

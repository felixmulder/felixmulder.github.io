package klarna

import cats.implicits._
import shapeless._, record._, ops.record._

object Introduction4 {

  case class Persona(id: Long, name: String)

  val persona = Persona(1337, "Darude Dude")

  val gen = Generic[Persona]

  // Int :: String :: HNil
  val repr = gen.to(persona)

  // This isn't very good
  println(repr)

  case class StringRep(value: String) extends AnyVal

  def show[H, T <: HList](xs: H :: T)(
    implicit
    f: H :: T => StringRep
  ): String =
    ???

  implicit val HNilRep: HNil => StringRep =
    _ => StringRep("HNil")

  implicit val LongRep: Long => StringRep =
    l => StringRep(l.toString)

  implicit val StringRepr: String => StringRep =
    s => StringRep("\"" ++ s ++ "\"")

  implicit def recurse[H, T <: HList](
    implicit
    hf: H => StringRep,
    tf: T => StringRep
  ): H :: T => StringRep =
    ???

}

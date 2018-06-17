package klarna

import cats.implicits._
import shapeless._, record._, ops.record._

object Introduction0 {

  case class Persona(id: Long, name: String)

  val persona = Persona(1337, "Darude Dude")

  val gen = Generic[Persona]

  // Int :: String :: HNil
  val repr = gen.to(persona)

  // This isn't very good
  println(repr)

}

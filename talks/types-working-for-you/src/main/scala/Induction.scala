package klarna

import cats.implicits._
import shapeless._, record._, ops.record._

object Induction extends App {

  case class Persona(id: Long, name: String)

  val persona = Persona(1337, "Person P")

}

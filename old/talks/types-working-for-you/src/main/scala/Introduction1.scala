package klarna

import shapeless._, labelled._

object Introduction1 {

  // Let's create an static serializer for JSON!

  case class Persona(id: Long, name: String)

  val persona = Persona(1337, "Darude Dude")

  val gen = Generic[Persona]

  // Int :: String :: HNil
  val repr = gen.to(persona)

  println(repr)

  // Let's print the representation in JSON:
  def json[XS <: HList](xs: XS): String =
    ???

  json(repr)
}

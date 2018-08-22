package klarna

import shapeless._, labelled._

object Introduction4 {

  // Let's create an static serializer for JSON!

  case class Persona(id: Long, name: String)

  val persona = Persona(1337, "Darude Dude")

  val gen = Generic[Persona]

  // Int :: String :: HNil
  val repr = gen.to(persona)

  println(repr)

  // Let's print the representation in JSON:
  def json[XS <: HList](xs: XS)(
    implicit
    json: Json[XS],
  ): String =
    json(xs)

  trait Json[A] {
    def apply(a: A): String
  }

  def encoder[A](f: A => String): Json[A] = new Json[A] {
    def apply(a: A) = f(a)
  }

  implicit val JsonHNil: Json[HNil] = encoder(_ => "")

  implicit val JsonString: Json[String] = encoder("\"" + _ + "\"")

  implicit val JsonLong: Json[Long] = encoder(_.toString)

  //json(repr)
}

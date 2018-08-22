package klarna

import shapeless._, labelled._

object Introduction9 {

  // Let's create an static serializer for JSON!

  case class Persona(id: Long, name: String)

  val persona = Persona(1337, "Darude Dude")

  val gen = LabelledGeneric[Persona]

  // Int :: String :: HNil
  val repr = gen.to(persona)

  println(repr)

  // Let's print the representation in JSON:
  def json[A <: Product, XS <: HList](xs: A)(
    implicit
    gen:  LabelledGeneric.Aux[A, XS],
    json: Json[XS],
  ): String =
    "{" + json(gen.to(xs)) + "}"

  trait Json[A] {
    def apply(a: A): String
  }

  def encoder[A](f: A => String): Json[A] = new Json[A] {
    def apply(a: A) = f(a)
  }

  implicit val JsonHNil: Json[HNil] = encoder(_ => "")

  implicit val JsonString: Json[String] = encoder("\"" + _ + "\"")

  implicit val JsonLong: Json[Long] = encoder(_.toString)

  implicit def jsonElems[H, K <: Symbol, T <: HList](
    implicit
    key:      Witness.Aux[K],
    headJson: Json[H],
    tailJson: Json[T],
  ): Json[FieldType[K, H] :: T] = encoder {
    case h :: HNil =>
      keyPair(key, headJson(h))

    case h :: t =>
      keyPair(key, headJson(h)) + ", " + tailJson(t)
  }

  def keyPair[A <: Symbol](k: Witness.Aux[A], rep: String) =
    s""""${k.value.name}": $rep"""

  println {
    json(persona)
  }
}



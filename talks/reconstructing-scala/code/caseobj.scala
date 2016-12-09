object Test {
  object Monday
  object Tuesday

  object Saturday
  object Sunday

  type Weekday = Monday.type | Tuesday.type
  type Weekend = Saturday.type | Sunday.type
  type AnyDay  = Weekday | Weekend

  def main(args: Array[String]) = {
    println(Monday.isInstanceOf[Weekend])
    println(Monday.isInstanceOf[Weekday])
    println(Saturday.isInstanceOf[Weekday])
  }
}

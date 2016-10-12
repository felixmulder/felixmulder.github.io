---
layout: "post"
title: "JSON Conversion for Supertypes using Spray-JSON"
---

When using JSON as the standard response for a project it can become a burden
to keep defining `implicit val`s when working with [spray-json](https://github.com/spray/spray-json).

In a current project we're basically always responding to an erroneous
request by issuing the following response structure:

```json
{
    "error": "<name of error>",
    "reason": "<string explaining error>"
}
```

When using [spray-json](https://github.com/spray/spray-json) you basically do
this to get be able to convert case classes to json-strings:

```scala
object JsonModels extends DefaultJsonProtocol {
    implicit val userFormatter = jsonFormat2(User)
}

// which allows you to do this:
user.toJson
```

This is fine when each class has a different apply function, but it gets
tedious when all of your objects are subclasses of an error trait that forces
two fields on the implementing class.

```scala
sealed trait Error {
  def error: String
  def reason: String
}

case object UserUpdateError extends Error {
  override def error  = "user_update_error"
  override def reason = "couldn't insert or update supplied user"
}

... more case objects extending Error
```

Instead of defining an `implicit val` for each subclass of `Error` what you can
do instead is define an object that extends `JsonFormat[A]` and define the `write`
and `read` functions yourself.

```scala
sealed trait Error { self =>
  def toJson: JsValue = Error.write(self)

  def error: String
  def reason: String
}

object ErrorFormat extends JsonFormat[Error] {
  def write(err: ErrorResponse) = JsObject(
    "error"  -> JsString(err.error)
    "reason" -> JsString(err.reason)
  )

  def write(value: JsValue) =
    deserializationError("not necessary since not converting from JSON")
}
```

As such, each type that extends Error will have `.toJson` available - but won't
need to define it.

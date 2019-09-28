---
layout: "post"
title: "Stacking Futures and Eithers using Monad transformers"
---

In a pet project I'm designing a simple
[CRUD](https://en.wikipedia.org/wiki/Create,_read,_update_and_delete)
application for an advert platform using [akka-http](http://akka.io/) and
[slick](http://slick.typesafe.com/) as the backend. Using Akka for the backend
is an interesting experiment, but my initial impression is that this is going
to be great.

The API for this application is extremely simple.

```bash
User makes API request -> API returns a JSON response
```

Always returning a type that can be converted to JSON would be fine, but, if an
error occurs far down the stack - we'd like this propagated to the API user.
Therefore, it makes sense to use something like Scala's `Either[+A,+B]` for the
`Response` type.

```scala
import scala.concurrent.Future
sealed trait ErrorResponse // converts to JSON

type Response[JsonResponse]  = Either[ErrorResponse, JsonResponse]
type ResponseF[JsonResponse] = Future[Response[JsonResponse]]
```

All of the interaction with the database occurs asynchronously. As such, these
functions return a `ResponseF[JsonResponse]`. If you're not familiar with
Scala's `Either`, below is an example of combining the use of `Either` with
that of `Future`s.

```scala
val x = Right(1)
val y: Left[String, Int] = Left("OH NO!")
val z: Left[String, Int] = Left("Later OH NO!")

for (a <- x.right; b <- y.right; c <- right) yield a + b + c
// result: Left("OH NO!")

val futX = Future.successful(x)
val futY = Future.successful(y)
val futZ = Future.successful(z)

for {
  eitherA <- futX
  eitherB <- futY
} yield for {
  a <- eitherA.right
  b <- eitherB.right
} yield a + b
// result: Left("OH NO!")
```

We can't actually `flatMap` over `Either`, we first have to choose a
projection, i.e. if we want the `left` or the `right` value. This poses an
issue to us since it is likely that we would want to perform a series of calls
dependent on each other's output. The resulting code would therefore be riddled
with chained for-comprehensions.

The solution would be if we could `flatMap` over these futures to get the
`right` value inside the `Either`. This would allow us to write very simple
code:

```scala
def signup(socialNetworkToken: String): ResponseF[AuthToken] =
  for {
    user     <- userInfoFromSocialNetwork(socialNetworkToken) // ResponseF[User]
    userInDb <- UserService.insertOrUpdate(user)              // ResponseF[User]
    token    <- getToken(userInDb)                            // ResponseF[Token]
  } yield token

  // Unfortunately, this happens:
  // error   : type mismatch;
  // found   : scala.util.Right[Error,User]
  // required: User
```

The trouble is that the value mapped to `user` is not of type `User` but rather
`Response[User]`. We're mixing monads, this bad. Enter `scalaz`.

Redefining the responses to:

```scala
import scalaz.{EitherT, \/}
import scala.concurrent.Future

type Response[JsonResponse]  = ErrorResponse \/ JsonResponse
type ResponseF[JsonResponse] = EitherT[Future, ErrorResponse, JsonResponse]
```

The `Either` is replaced with `scalaz`'s counterpart, `\/[+A,+B]`. We also
define our `ResponseF` as the monad `EitherT`. This monad's `flatMap` method
returns the value inside the future's `\/`, or if you're familiar with monads:
binds through the right of its disjunction. We can now write the code that we
originally wanted to write:

```scala
def signup(socialNetworkToken: String): ResponseF[AuthToken] =
  for {
    user     <- userInfoFromSocialNetwork(socialNetworkToken) // ResponseF[User]
    userInDb <- UserService.insertOrUpdate(user)              // ResponseF[User]
    token    <- getToken(userInDb)                            // ResponseF[Token]
  } yield token
```

If an error were to occur in the first line of the for-comprehension, the
following statements would be ignored and the error would propagate as expected.

So how do we create an `EitherT` from a future? Depending upon your need for
error handling it can be as simple as:

```scala
def insertOrUpdate(user: User): ResponseF[User] = EitherT {
  val query = db.run(insertOrUpdateQuery(user)).map(_.right)

  query recover {
    case _ => UserUpdateError.left
  }
}
```

Monads are not always simple to understand, but they allow for elegant code if
used correctly. In this case I would argue that the hour spent on understanding
`EitherT` will help us write elegant code throughout this project.

Checkout Andy's
[scala-errorhandling](https://github.com/ekroth/scala-errorhandling) repository
for a full implementation of the error handling - including collection support.

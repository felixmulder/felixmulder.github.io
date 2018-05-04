---
title:       Regaining Control
subtitle:    with Indexed Monads
author:      Felix Mulder
date:        flatMap(Oslo) 2018
classoption: "aspectratio=169"
---

## Who am I?
• Scala 2.12 Docs Compiler

• Scala 3 Compiler Engineer @ EPFL w/ Martin Odersky

• Software Engineer @ Klarna Bank

# Functional State

## The Canonical Example

```tut:invisible
import cats.implicits._
def rng(seed: Long): (Long, Long) =
  (seed * 6364136223846793005L + 1442695040888963407L, seed)
```

```tut:silent
type Seed = Long
```

```scala
def rng(seed: Seed): (Seed, Long)
```

```tut:silent
def rbg(seed: Seed): (Seed, Boolean) = {
  val (newSeed, rand) = rng(seed)
  (newSeed, rand > 0L)
}
```

## Adding Three Random Numbers

```tut:silent
val s0 = 0L

val (s1, r0) = rng(s0)
val (s2, r1) = rng(s1)
val (_,  r2) = rng(s2)
```
```tut:book
r0 + r1 + r2
```

## The Canonical Example
### Avoid passing the state?
### Get rid of boilerplate?

# `S => (S, A)`

## The State Monad
```tut:silent
case class State[S, A](run: S => (S, A)) extends AnyVal
```

## The State Monad
```tut:silent
val nextLong: State[Seed, Long] = State(seed => rng(seed))
```

## The State Monad
```scala
val nextBool: State[Seed, Boolean] = ???
```

## Map
We'd like to implement map in such a way that we do not affect `S`

```scala
State[S, A] => State[S, B]
```

## The State Monad
```tut:silent
case class State[S, A](run: S => (S, A)) extends AnyVal {

  def map[B](f: A => B): State[S, B] = State {
    s0 => {
      val (s1, a) = run(s0)
      (s1, f(a))
    }
  }

}
```

## The State Monad
```tut:invisible
val nextLong: State[Seed, Long] = State(seed => rng(seed))
```
```tut:silent
val nextBool: State[Seed, Boolean] = nextLong.map(_ > 0L)
```

## The State Monad
How do we get rid of the explicit state passing?

## The State Monad
We want to reason about the `A` value in `State[S, A]`

(without having to worry about `S`!)

## The State Monad
We sort of want to pull the value out, to *bind* it...

## The State Monad
```tut:silent
case class State[S, A](run: S => (S, A)) extends AnyVal {

  // ...

  def flatMap[B](f: A => State[S, B]): State[S, B] = State {
    s0 => {
      val (s1, a) = run(s0)
      f(a).run(s1)
    }
  }

}
```

## Adding Three Random Numbers
```tut:invisible
case class State[S, A](run: S => (S, A)) extends AnyVal {
  def map[B](f: A => B): State[S, B] = State {
    s0 => {
      val (s1, a) = run(s0)
      (s1, f(a))
    }
  }

  def flatMap[B](f: A => State[S, B]): State[S, B] = State {
    s0 => {
      val (s1, a) = run(s0)
      f(a).run(s1)
    }
  }
}
val nextLong: State[Seed, Long] = State(rng _)
val nextBool: State[Seed, Boolean] = nextLong.map(_ > 0L)
```
```tut:silent
val addition: State[Seed, Long] = for {
  r0 <- nextLong
  r1 <- nextLong
  r2 <- nextLong
} yield r0 + r1 + r2
```
```tut:book
addition.run(0L)
```

## Cooler stuff
```tut:silent
case class Customer(id: Long, debt: Long, name: String)

val randomCustomer: State[Seed, Customer] =
  for {
    id      <- nextLong
    debt    <- nextLong
    isHuman <- nextBool
    name    =  if (isHuman) "Kim" else "Mark Zuckerberg"
  } yield Customer(id, debt, name)
```
```tut:book
randomCustomer.run(1L)._2
```

# Are we there yet?

# Stack safety?

# What about effects?

## What about effects?
```tut:silent
import cats.effect.IO

def getNonce(seed: Seed): IO[(Seed, Long)] =
  IO(rng(seed))
```

```tut:nofail:book
val nextNonce: State[Seed, Long] = State(seed => getNonce(seed))
```

## StateT
```tut:silent
case class StateT[F[_], S, A](val run: S => F[(S, A)])

val nextNonce: StateT[IO, Seed, Long] = StateT(seed => getNonce(seed))
```

## Stack Safety
### Now depends on `F[_]`

## Requirements on `F[_]`
`Functor[F]` and `FlatMap[F]`

for `map` and `flatMap`

## State in Cats
```tut:silent
import cats.Eval

type State[S, A] = StateT[Eval, S, A]
```

# Where is my indexed Monad?

# Also, what are indexed Monads?

# `S => (S, A)`

# `I => (O, A)`

## Chaining State Transitions
```scala
(S1 => (S2, A)) =>
(S2 => (S3, A)) =>
(S3 => (S4, A)) ...
```

## Indexed State Monad
```tut:silent
case class IxState[I, O, A](run: I => (O, A))
```

## Yet Another Naive Implementation
```tut:silent
case class IxState[I, O, A](run: I => (O, A)) {

  def map[B](f: A => B): IxState[I, O, B] = IxState {
    i => {
      val (o, a) = run(i)
      (o, f(a))
    }
  }

}
```

## Yet Another Naive Implementation
```tut:silent
case class IxState[I, O, A](run: I => (O, A)) {

  // ...

  def flatMap[OO, B](f: A => IxState[O, OO, B]): IxState[I, OO, B] =
    IxState {
      i => {
        val (o, a) = run(i)
        f(a).run(o)
      }
    }
}
```

## Chained State Transitions
```scala
IxState[S1, S2, A] =>
IxState[S2, S3, B] =>
IxState[S3, S4, C] ...
```

## Now we can model state transitions!
```tut:silent
sealed trait OrderStatus
case class Initiated() extends OrderStatus
case class Received()  extends OrderStatus
case class Packed()    extends OrderStatus
case class Shipped()   extends OrderStatus
case class Delivered() extends OrderStatus
```

## Helper Functions
```tut:invisible
case class IxState[I, O, A](run: I => (O, A)) {

  def map[B](f: A => B): IxState[I, O, B] = IxState {
    i => {
      val (o, a) = run(i)
      (o, f(a))
    }
  }

  def flatMap[OO, B](f: A => IxState[O, OO, B]): IxState[I, OO, B] =
    IxState {
      i => {
        val (o, a) = run(i)
        f(a).run(o)
      }
    }
}; object IxState { // annoying hack
  def set[I, O](o: O): IxState[I, O, Unit] =
    IxState(_ => (o, ()))
}
```
```scala
object IxState {
  def set[I, O](o: O): IxState[I, O, Unit] =
    IxState(_ => (o, ()))
}
```

## Helper Functions
```tut:silent
def received: IxState[Initiated, Received, Unit] =
  IxState.set(Received())

def packed: IxState[Received, Packed, Unit] =
  IxState.set(Packed())

def shipped: IxState[Packed, Shipped, Unit] =
  IxState.set(Shipped())

def delivered: IxState[Shipped, Delivered, Unit] =
  IxState.set(Delivered())
```

## Usage
```tut:silent
val order = for {
  _ <- received
  _ <- packed
  _ <- shipped
  _ <- delivered
} yield ()
```
```tut:book
order.run(Initiated())
```

## Static errors!
```tut:nofail:book
for {
  _ <- delivered
  _ <- packed
} yield ()
```

## Cats
```tut:silent
class IndexedStateT[F[_], SA, SB, A](val runF: F[SA => F[(SB, A)]])
```

# Wait a minute, this looks familiar...

## StateT in Cats
```tut:silent
import cats.data.IndexedStateT

type StateT[F[_], S, A] = IndexedStateT[F, S, S, A]
```

# The Epiphany

## The Epiphany
![](brain1.png)

## The Epiphany
![](brain2.png)

## The Epiphany
![](brain3.png)

## The Epiphany
![](brain4.png)

## The Epiphany
![](brain5.png)

## The Epiphany
![](brain6.png)

## The Epiphany
![](brain7.png)

# Designing APIs Using `IndexedStateT`

## Our Order Status API
```tut:silent
sealed trait OrderStatus
case class Initiated() extends OrderStatus
case class Received()  extends OrderStatus
case class Packed()    extends OrderStatus
case class Shipped()   extends OrderStatus
case class Delivered() extends OrderStatus
```

## A Vanilla API
```tut:invisible
import cats.effect.IO
import org.http4s._
import org.http4s.dsl.io._
import cats.syntax.apply._
def createOrder(id: Int): IO[Unit] = IO.unit
def ship(id: Int): IO[Unit] = IO.unit
def pack(id: Int): IO[Unit] = IO.unit
def orderStatus(id: Int): IO[String] = IO.pure("")
```
```tut:silent
HttpService[IO] {
  case GET -> Root / "status" / IntVar(id) =>
    orderStatus(id).flatMap(Ok(_))

  case POST -> Root / "status" / IntVar(id) =>
    createOrder(id) *> Ok()

  case PATCH -> Root / "status" / "packAndShip" / IntVar(id) =>
    ship(id) *> pack(id) *> Ok()
}
```

## Designing APIs Using `IndexedStateT`
```tut:invisible
object UserApi {
  type OrderId = Long

  case class OrderInit(id: OrderId)

  def persist(info: OrderInit): IO[OrderId] = IO(info.id)

  def persist[S <: OrderStatus](s: S, id: OrderId): IO[S] = IO.pure(s)

  def getState(id: Int): IO[OrderStatus] = IO.pure(Initiated())

  implicit val entityEncoder: EntityEncoder[IO, OrderStatus] = EntityEncoder[IO, String].contramap(a => a.toString)

  def state[A <: OrderStatus](id: Long): IO[A] = ???
}
import UserApi._
```
```tut:silent
def createOrder(init: OrderInit): IndexedStateT[IO, Initiated, Received, OrderId] =
  IndexedStateT(_ => persist(init).map(id => (Received(), id)))

def packed(id: OrderId): IndexedStateT[IO, Received, Packed, Unit] =
  IndexedStateT.setF(persist(Packed(), id))

def shipped(id: OrderId): IndexedStateT[IO, Packed, Shipped, Unit] =
  IndexedStateT.setF(persist(Shipped(), id))

def delivered(id: OrderId): IndexedStateT[IO, Shipped, Delivered, Unit] =
  IndexedStateT.setF(persist(Delivered(), id))
```

## Using the API
```tut:silent
val orderId = 1L

val packAndShip = for {
  _ <- packed(orderId)
  _ <- shipped(orderId)
} yield ()
```
```tut:book
packAndShip.runS(Received()).unsafeRunSync()
```

## The non-vanilla API
```tut:invisible
import cats.effect.IO
import org.http4s._
import org.http4s.dsl.io._
import cats.implicits._
```
```tut:silent
HttpService[IO] {
  case GET -> Root / "status" / IntVar(id) =>
    getState(id).flatMap(Ok(_))

  case POST -> Root / "status" / IntVar(id) =>
    createOrder(OrderInit(id)).run(Initiated()) *> Ok()

  case PATCH -> Root / "status" / "packAndShip" / LongVar(id) =>
    for {
      r   <- state[Received](id)
      _   <- packAndShip.run(r)
      res <- Ok()
    } yield res
}
```

## Encode Any Protocol
• File Protocols

  ```scala
  def writeHeader(header: String): IxState[NoHeader, HeaderWritten, Array[Byte]]
  ```

• Session types

  ```scala
  def initSSL(ch: ClientHello): IxState[NoSession, ClientHello, Unit]
  ```

# Downsides to using `IndexedStateT`?

## Downsides
```tut:invisible
case class Unrelated0()
case class Unrelated1()
```
```tut:nofail
for {
  _ <- packed(orderId)
  _ <- IndexedStateT.set[IO, Unrelated0, Unrelated1](Unrelated1())
} yield ()
```

# Shapeless: "Hold my beer"

# HLists

## HLists
```scala
State[S1 :: R1 :: HNil] =>

State[S2 :: R1 :: HNil] =>

State[S2 :: R2 :: HNil]
```

## HLists
```tut:silent
case class Whatever()
```
```tut:invisible
import shapeless._
trait Once[Contains, I <: HList]
trait Output[O <: HList, Remove, Insert] {
  def out: O
}
implicit val in1  = new Once[Received, Received :: Whatever :: HNil] {}
implicit val out1 = new Output[Packed :: Whatever :: HNil, Received, Packed] {
  val out = Packed() :: Whatever() :: HNil
}
implicit val in2  = new Once[Packed, Packed :: Whatever :: HNil] {}
implicit val out2 = new Output[Shipped :: Whatever :: HNil, Packed, Shipped] {
  val out = Shipped() :: Whatever() :: HNil
}
```
```tut:silent
def packed[I <: HList, O <: HList](id: OrderId)(
  implicit
  I: Once[Received, I],
  O: Output[O, Received, Packed],
): IndexedStateT[IO, I, O, Unit] = IndexedStateT.set(O.out)
```
```tut:book
packed(0L).runS(Received() :: Whatever() :: HNil)
```

## HLists
```tut:silent
def shipped[I <: HList, O <: HList](id: OrderId)(
  implicit
  I: Once[Packed, I],
  O: Output[O, Packed, Shipped],
): IndexedStateT[IO, I, O, Unit] = IndexedStateT.set(O.out)


val packAndShip = for {
  _ <- packed(0L)
  _ <- shipped(0L)
} yield ()
```
```tut:book
packAndShip.runS(Received() :: Whatever() :: HNil)
```

# Should you do this?

# Probably not.

## Abstracting over `F[_]`
These structures allow you to stay generic. Don't commit too early.

• `F = Id`

• `F = Option`

• `F = OptionT[IO, ?]`

• `F = EitherT[IO, Throwable, ?]`

• `F = MonadError[Throwable, ?]`

## Conclusions
- Get rid of explicit state passing
- Finite State Machines, Protocols, Simple Session Types
- Delay choosing your `F[_]`
- Regain control over your API!

# Thank You!

## References
• [Cats State](https://typelevel.org/cats/datatypes/state.html) - Typelevel Cats Documentation

• [Control.Monad.State](http://hackage.haskell.org/package/mtl-2.2.2/docs/Control-Monad-State-Lazy.html) - Hackage

• [pandoc-include-code](https://github.com/owickstrom/pandoc-include-code) - Oskar Wickström // [\@owickstrom](https://twitter.com/owickstrom)

• [tut](https://github.com/tpolecat/tut) - doc/tutorial generator for scala // [\@tpolecat](https://twitter.com/tpolecat)

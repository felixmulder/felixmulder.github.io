---
title:    Regaining Control
subtitle: with Indexed Monads
author:   Felix Mulder
date:     May 2018
---

# Functional State

## The Canonical Example

```tut:invisible
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

## The Canonical Example
```scala
S => (S, A)
```

## The State Monad
```tut:silent
case class State[S, A](run: S => (S, A)) extends AnyVal

val nextLong: State[Seed, Long] = State(rng)

def nextBool: State[Seed, Boolean] = ???
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
val nextLong: State[Seed, Long] = State(rng)
```
```tut:silent
val nextBool: State[Seed, Boolean] = nextLong.map(_ > 0L)
```

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
val nextNonce: State[Seed, Long] = State(getNonce)
```

## StateT
```tut:silent
case class StateT[F[_], S, A](val run: S => F[(S, A)])

val nextNonce: StateT[IO, Seed, Long] = StateT(getNonce)
```

## Stack Safety
### Now depends on `F[_]`

## State in Cats
```tut:silent
import cats.Eval

type State[S, A] = StateT[Eval, S, A]
```

# Where is my indexed Monad?

# Also, what are indexed Monads?

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
}
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
```tut:silent
def received: IxState[Initiated, Received, Unit] =
  IxState(_ => (Received(), ()))

def packed: IxState[Received, Packed, Unit] =
  IxState(_ => (Packed(), ()))

def shipped: IxState[Packed, Shipped, Unit] =
  IxState(_ => (Shipped(), ()))

def delivered: IxState[Shipped, Delivered, Unit] =
  IxState(_ => (Delivered(), ()))
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

# The Epifani

## The Epifani
[](brain1.png)

## The Epifani
![](brain2.png)

## The Epifani
![](brain3.png)

## The Epifani
![](brain4.png)

## The Epifani
![](brain5.png)

## The Epifani
![](brain6.png)

## The Epifani
![](brain7.png)


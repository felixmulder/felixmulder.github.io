---
title:       Intro to Functional Programming
subtitle:    Equational Reasonging, Compositionality, and Effects
author:      Felix Mulder
date:        Scala Stockholm Meetup - May 30
classoption: "aspectratio=169"
---

## Who am I?
• Scala 2.12 Docs Compiler

• Scala 3 Compiler Engineer @ EPFL w/ Martin Odersky

• Software Engineer @ Klarna Bank

## Background
> "Constraints liberate, and liberties constrain."
>
> -- Runar Bjarnason

## Example
```tut:silent
def foo(a: Int): Int = ???
```

## Example
```tut:silent
def foo[A](a: A): A = ???
```

## Example
```tut:silent
def foo[A](a: A): A = a
```

## The purpose of Abstraction
> "The purpose of abstraction is not to be vague, but to create a new semantic
> level in which one can be absolutely precise"
>
> -- Edsger W. Dijkstra

# `Future[_]` vs `Actor`s

## The Actor API
```scala
def apply[A](a: A): Future[A]

def map[A, B](fa: Future[A])(f: A => B): Future[B]

def flatten[A](fa: Future[Future[A]]): Future[A]
```

## The Actor API
```scala
def receive: Any => Unit
```

## The Actor API
```scala
def become(behavior: Any => Unit, ...): Unit

def unbecome(): Unit
```

## Felix's Conjecture
> "By being able to do anything - we can assume nothing."
>
> -- Felix Mulder (and probably other people everywhere...all the time)

# Prefer `Future[_]` over `Actor`

## The Purpose of Abstraction
> "The purpose of abstraction is not to be vague, but to create a new semantic
> level in which one can be absolutely precise"
>
> -- Edsger W. Dijkstra

# How does Functional Programming fit into this?

# What is Functional Programming?

# Functional Programming is **not** about making things immutable.

# Functional Programming is **not** about first class functions.

# In extension - it is also **not** about higher-order functions.

# It is not about programming **without** I/O.

# So.

# Functional Programming is about programming with **functions**.

## Mathematical Functions
```
f(x) = x
```

# Referential Transparency

## Referential Transparency
>  An expression is said to be referentially transparent if it can be replaced
>  with its corresponding value without changing the program's behavior.

## Referential Transparency
```
x = 5
y = x + x
z = 2 * y + x
```

## Referential Transparency
```
x = 5
y = 5 + 5
z = 2 * y + x
```

## Referential Transparency
```
x = 5
y = 10
z = 2 * y + x
```

## Referential Transparency
```
x = 5
y = 10
z = 2 * 10 + 5
```

## Referential Transparency
```
x = 5
y = 10
z = 25
```

## Referential Transparency
```scala
val x = <expr>
(a, a)

// is equivalent to:

(<expr>, <expr>)

// ?
```

# When is something pure?

## When is something pure?
> When **only** the input of the function can change its outcome.

## Things that aren't pure
- I/O (reading files, time, interacting with DB)
- Constants that depend on the above
- Mutation

# Why should we care about purity?

# Equational Reasoning.

# Compositionality.

# What is equational reasoning, and compositionality? And how is it different from something that composes?

## Compositional example
```tut:silent
import cats.effect.Sync
import fs2.{Stream, text}
import java.nio.file.Paths

def fahrenheitToCelsius(f: Double): Double =
  (f - 32.0) * (5.0/9.0)

def converter[F[_]](file: Stream[F, Byte]): Stream[F, Byte] = {
  file
    .through(text.utf8Decode)
    .through(text.lines)
    .filter(s => !s.trim.isEmpty && !s.startsWith("//"))
    .map(line => fahrenheitToCelsius(line.toDouble).toString)
    .intersperse("\n")
    .through(text.utf8Encode)
}
```

## Definitions
> "[T]he meaning of a complex expression is determined by the meanings of its
> constituent expressions and the rules used to combine them."
>
> [Wikipedia - Principle of compositionality](https://en.wikipedia.org/wiki/Principle_of_compositionality)

---

# Composability v. Compositionality

## Composability v. compositionality
> A system that is composable has modular parts that can be put together to
> create a larger system.
>
> In compositional systems - additionally, the meaning of each component makes
> sense on its own.

# Formalisms

## Formalisms
<!-- Graph a -> b -> c -->

## Formalisms
<!-- Graph a -> b -> c with functions -->

## Formalisms
<!-- Graph a -> b -> c, a -> c, with functions and composed function -->

## Formalisms
<!-- Graph a -> b -> c, a -> c, with functions and composed function, and identity -->

# Functor

# Category Theory

## An unconstrained example
```tut:silent
def transform[A, B](fa: List[A])(f: A => B) =
  ???
```

## A constrained example
```tut:silent
import cats._, cats.implicits._

def transform[F[_]: Functor, A, B](fa: F[A])(f: A => B) =
  ???
```

## A constrained example
```tut:silent
import cats._, cats.implicits._

def transform[F[_]: Functor, A, B](fa: F[A])(f: A => B) =
  fa.map(f)
```

# Type Classes

## There are laws - you've already seen the two for functor!
- Identity
- Composition

## Formalisms
<!-- Graph a -> b -> c, a -> c, with functions and composed function, and identity -->

# Semigroup

## Semigroup
```
A |+| A === A
```

## Semigroup
```tut:silent
trait Semigroup[A] {
  def combine(a1: A, a2: A): A
}
```

## Semigroup Laws
- Associativity

```
(a |+| b) |+| c

<=>

a |+| (b |+| c)
```

## Associativity
```tut:silent
def associativity[A](a: A, b: A, c: A)(implicit S: Semigroup[A]): Boolean =
  S.combine(S.combine(a, b), c) == S.combine(a, S.combine(b, c))
```

## Semigroup instance for List
```tut:silent
implicit def listSemigroup[E] = new Semigroup[List[E]] {
  def combine(a1: List[E], a2: List[E]): List[E] =
    a1 ++ a2
}
```

# Because of the associativity law, there's only one way to implement this!

# Monoid

# A monoid is a semigroup that has a zero-element.


## Monoid
```tut:silent
trait Monoid[A] extends Semigroup[A] {
  def unit: A
}
```

## Monoid laws
- (Associativity)
- Left identity

  ```tut:silent
  def leftIdentity[A](m: A)(implicit M: Monoid[A]): Boolean =
    M.combine(M.unit, m) == m
  ```

- Right identity

  ```tut:silent
  def rightIdentity[A](m: A)(implicit M: Monoid[A]): Boolean =
    M.combine(m, M.unit) == m
  ```

## Monoid instance for List
```tut:silent
implicit def listMonoid[E] = new Monoid[List[E]] {
  def combine(a1: List[E], a2: List[E]): List[E] =
    a1 ++ a2

  def unit = List.empty
}
```

## Ever heard this one before?
> "A Monad is a Monoid in the category of endofunctors, so what's the problem?"
>
> -- ["A Brief, Incomplete, and Mostly Wrong History of Programming Languages"](http://james-iry.blogspot.com/2009/05/brief-incomplete-and-mostly-wrong.html), James Iry

# Effects

## Effects
> "Effects are good, side-effects are bugs."
>
> -- Rob Norris

# `Future[A]` and effects

## `Future[A]` and effects
```tut:book:reset
import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.DurationInt

val const5  = Future(5)
val const10 = const5.flatMap(x => const5.map(x + _))

Await.result(const10, 1.second)
```

## `Future[A]` and effects
```tut:book
def read = Future(io.StdIn.readInt())

val read2 = read.flatMap(x => read.map(x + _))

// > 10

// > 5

Await.result(read2, 5.seconds)
```

## `Future[A]` and effects
```tut:book
val read2 = Future(io.StdIn.readInt()).flatMap {
  x => Future(io.StdIn.readInt()).map(x + _)
}

// > 10

// > 5

Await.result(read2, 5.seconds)
```

## `Future[A]` and effects
```tut:book
val read = Future(io.StdIn.readInt())
val read2 = read.flatMap {
  x => read.map(x + _)
}

// > 10

Await.result(read2, 5.seconds)
```

## So what have we lost?
- Modularity
- Compositionality
- Concurrency
- Parametricity
- Equational Reasoning

## Effects
> "Effects are good, side-effects are bugs."
>
> -- Rob Norris

## Effects
> "Effects are good, side-effects are bugs *waiting to happen*."
>
> -- Rob Norris + Felix Mulder

# So, how do we actually separate side-effects from effects?

# Laziness\*

\*in scala

# `IO[A]`

## `IO[A]`
```tut:book:reset
import cats.effect.IO
import cats.implicits._

val read = IO { io.StdIn.readInt() }

(read, read).mapN(_ + _)
```

## `IO[A]`
```tut:book
(read, read).mapN(_ + _).unsafeRunSync() // > 1, > 2
```

# End of the World

# Beware! `IO[A]` suspends *all* side effects


## Lord Acton
> "Power tends to corrupt; absolute power corrupts absolutely"
>
> -- Lord Acton

# Separate your pure computations from `IO`!

## Separate your pure computations from `IO`!
```tut:book
import java.util.UUID

def isEqual(fst: UUID, snd: UUID): Boolean =
  fst.compareTo(snd) == 0 // yeah, not likely!

val randomUUID = IO { UUID.randomUUID() }

val calc = for {
  n1 <- randomUUID
  n2 <- randomUUID
} yield isEqual(n1, n2)
```

# Thank you!

## Acknowledgements
- [Functional Programming with Effects](https://www.youtube.com/watch?v=po3wmq4S15A) -- Rob Norris (\@tpolecat)
- [Constraints are Freedom](https://www.youtube.com/watch?v=GqmsQeSzMdw) -- Runar Bjarnason

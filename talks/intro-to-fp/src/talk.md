---
title:       Intro to Functional Programming
subtitle:    Equational Reasonging, Compositionality, and Effects
author:      Felix Mulder
date:        Scala Stockholm Meetup - May 30
classoption: "aspectratio=169"
---

## Who am I?
* Scala 2.12 Docs Compiler

* Scala 3 Compiler Engineer @ EPFL w/ Martin Odersky

* Software Engineer @ Klarna Bank

---

> "Constraints liberate, and liberties constrain."
>
> -- Runar Bjarnason

---

> "The purpose of abstraction is not to be vague, but to create a new semantic
> level in which one can be absolutely precise"
>
> -- Edsger W. Dijkstra

---

```tut:silent
def foo(a: Int): Int = ???
```

---

```tut:silent
def foo[A](a: A): A = ???
```

---

```tut:silent
def foo[A](a: A): A = a
```

---

# `Future[_]` vs `Actor`s

---

```scala
def apply[A](a: A): Future[A]

def map[A, B](fa: Future[A])(f: A => B): Future[B]

def flatten[A](fa: Future[Future[A]]): Future[A]
```

---

```scala
def receive: Any => Unit
```

---

```scala
def become(behavior: Any => Unit, ...): Unit

def unbecome(): Unit
```

---

> "By being able to do anything - we can assume nothing."
>
> -- Felix Mulder (and probably other people everywhere...all the time)

---

# Prefer `Future[_]` over `Actor`

---

> "The purpose of abstraction is not to be vague, but to create a new semantic
> level in which one can be absolutely precise"
>
> -- Edsger W. Dijkstra

---

# How does Functional Programming fit into this?

---

# What is Functional Programming?

---

# Functional Programming is **not** about making things immutable.

---

# Functional Programming is **not** about first class functions.

---

# In extension - it is also **not** about higher-order functions.

---

# It is not about programming **without** I/O.

---

# So.

---

# Functional Programming is about programming with **functions**.

---

\displaymath{
    f(x) = x
}

---

# Referential Transparency

---

## Referential Transparency
>  An expression is said to be referentially transparent if it can be replaced
>  with its corresponding value without changing the program's behavior.

---

\displaymath{
    x = 5
    y = x + x
    z = 2 * y + x
}

---

\displaymath{
    x = 5
    y = 5 + 5
    z = 2 * y + x
}

---

\displaymath{
    x = 5
    y = 10
    z = 2 * y + x
}

---

\displaymath{
    x = 5
    y = 10
    z = 2 * 10 + 5
}

---

\displaymath{
    x = 5
    y = 10
    z = 25
}

---

# When is something pure?

---

> When **only** the input of the function can change its outcome.

---

Things that aren't pure:

- I/O (reading files, time, interacting with DB)
- Constants that depend on the above
- Mutation

---

# Why should we care about purity?

---

# Equational Reasoning.

---

# Compositionality.

---

# What is equational reasoning, and compositionality? And how is it different from something that composes?

---

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

---

> "[T]he meaning of a complex expression is determined by the meanings of its
> constituent expressions and the rules used to combine them."
>
> [Wikipedia - Principle of compositionality](https://en.wikipedia.org/wiki/Principle_of_compositionality)

---

# Composability v. Compositionality

> A system that is composable has modular parts that can be put together to
> create a larger system.
>
> In compositional systems - additionally, the meaning of each component makes
> sense on its own.

---

# Formalisms

---

\usepackage[pdf]{graphviz}
\digraph{abc}{
    rankdir=LR;
    a -> b -> c;
}

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
apply: A => Future[A]

map: (A => B) => Future[A] => Future[B]

flatten: Future[Future[A]] => Future[A]
```

---

```scala
receive: Any => Unit
```

---

```scala
def become(behavior: Any => Unit, ...): Unit

def unbecome(): Unit
```

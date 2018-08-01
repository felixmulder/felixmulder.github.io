---
title:       Let the Types Work for You
subtitle:    Klarna Konferense
author:      Felix Mulder
date:        August 2018
classoption: "aspectratio=169"
---

## Agenda
- Functional Programming
- Type systems
- FP + Types == amazing!
- Profit!

## Bio
- Felix Mulder
- Software Engineer, IronBank
- Compiler Engineer, Scala 3 @ EPFL

---

> "Do you know that feeling of having to hold too many things in your head at
> once?"

# Functional Programming gets rid of that by definition.

## Referential Transparency
- Equational reasoning
- Compositionality

---

Referential Transparency + Types

==

Refactor All The Things! (without fear)

# Game over, OO. Right?

# What about the downsides?

## What if you could negate those downsides?
- Smarter inference
- Better compiler messages

# What if we used the types to derive the implementation?

---

![alt-center](https://pbs.twimg.com/media/DX4al-6WAAAY7By.jpg)


# Today we're exploring type-level induction and recursion

## What we're actually doing
Writing a compile-time serializer for data types - with no need for scary
runtime reflection.

# Coding time!

# Why are we so obsessed with parametricity?

## Felix's Conjecture
> "By being able to do anything, we can assume nothing"

---

> "The purpose of abstraction is not to be vague, but to create a new semantic
> level in which one can be absolutely precise"
>
> -- Edsger W. Dijkstra

# Constraints Liberate, and Liberties Constrain

# `Any => Unit`

## Constraints Liberate, and Liberties Constrain
```tut:silent
def foo(i: Int): Int = ???
```

## Constraints Liberate, and Liberties Constrain
```tut:silent
def foo[A](a: A): A= ???
```

## Constraints Liberate, and Liberties Constrain
```tut:silent
def foo[A](a: A): A = a
```

## Constraints Liberate, and Liberties Constrain
```tut:silent
def id[A](a: A): A = a
```

## In Closing
- Type level recursion for fun and profit!
- Built a type-level, compile-time JSON serializer
- You shouldn't work against the compiler, make it work for you!

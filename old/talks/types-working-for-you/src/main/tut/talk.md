---
title:       Let the Types Work for You
subtitle:    Klarna Konferense
author:      Felix Mulder
date:        August 2018
classoption: "aspectratio=169"
---

## Agenda
• Functional Programming

• Type systems

• FP + Types == amazing!

---

![alt-center](./fp-meme.png)

---

> "Do you know that feeling of having to hold **too many things** in your head at
> once?"

# Functional Programming gets rid of that by definition.

## Referential Transparency
  ```scala
  x = 5
  y = x + x
  z = y + x

  // ==>
  z = (5 + 5) + 5
  ```

## Referential Transparency
• Equational reasoning

• Compositionality

---

Referential Transparency + Types

==

Refactor All The Things! (without fear)

# Game over, OO. Right?

# What about the downsides?

# "Types get in the way of what I'm actually trying to do"

# "The compiler is all complaints"

# "I don't gain anything from the types"

# "I'd rather write tests for my entire code base"

## How do we negate that?
• Inference

• Smart type systems

• Better compilers

• and...

---

![alt-center](./from-the-types.jpg)

# Today we're exploring type-level induction and recursion

## What we're actually doing
• Compile-time JSON serializer

• NO runtime reflection

# Why?

# DISCLAIMER!

# Coding time!

## Felix's Conjecture
> "By being able to do anything, we can assume nothing" -- Felix's Conjecture

## Constraints Liberate, and Liberties Constrain
```tut:silent
def foo(i: Int): Int = ???
```

## Constraints Liberate, and Liberties Constrain
```tut:silent
def foo[A](a: A): A = ???
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
• Type level recursion for fun and profit!

• Built a type-level, compile-time JSON serializer

---

![alt-center](./exit-12.png)

## References (move to on slide)
• [Constraints Liberate, Liberties Constrain](https://www.youtube.com/watch?v=GqmsQeSzMdw) - Runar Bjarnason

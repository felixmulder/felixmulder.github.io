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

## The Canonical Example
```scala
S => (S, A)
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

---
title: How Stylish Haskell works
published: true
date: 2020-08-20
summary: >
  Stylish haskell is a code formatter for the Haskell language. In this post,
  we'll go through how the application works and give you a glimpse of what
  it's like working with the codebase.
---

# How stylish-haskell works
During the summer, our team at Klarna has been recruiting other teams to join
our Haskell monorepo. Getting more teams interested in joining us in doing
Haskell has always been our strategy - by doing it in a monorepo we feel that
we are able to make sure that all teams write their code in a similar way.

This poses a couple of challenges to us - only one of which we're dealing with
in this article!

There are the basics of how we build the applications and make sure that
tooling works great for everyone involved, to how the CI tests and deploys the
artifacts.

Then there are more subtle things: how we make sure that code reviews focus on
the right things - and that people don't introduce new patterns and ways of
doing things.

I believe that one of the most important things that you can do as a developer
is to make sure that the above is automated. Which leads us to one of my pet
peaves:

## Code formatting
When there's no (enforced) canonical way of formatting something in a language
- people tend to think up their own way of doing something. Let's take
something as simple as formatting `data` in Haskell. Here are a couple of
alternatives for how to format a record type:

```haskell
-- JS style:
data Car = Car {
  manufacturingYear :: Year,
  milesRun :: Natural
} deriving stock (Eq, Show)

-- Vertical alignment (my personal hell):
data Car = Car { manufacturingYear :: Year
               , milesRun          :: Natural
               }
               deriving stock (Eq, Show)

-- 2 space indent:
data Car = Car
  { manufacturingYear :: Year
  , milesRun :: Natural
  }
  deriving stock (Eq, Show)
```

The last one is my personal favorite as it minimizes git-diffs (yeah, I'm one
of those people).[^on-data-indentation]

This is just the tip of the iceberg though. Once we've agreed to how we format
data, then we have: newtypes, imports, module headers, docstrings, language
pragmas...

Keeping this in sync in a single team is _fine_, maybe not enjoyable, but fine.
In a monorepo with >50kLOC and multiple teams; yeah, that's not going to be
pleasant at all.

## Use a code formatter!
There are several options for Haskell:

- [ormolu](https://github.com/tweag/ormolu)
- [hindent](https://github.com/mihaimaruseac/hindent)
- [stylish-haskell](https://github.com/jaspervdj/stylish-haskell)

We chose to go with the last of the bunch. The reason simply being that we
wanted to be able to customize the way that our code is formatted and some
of our engineers are maintainers of the repo.[^lp-maintainers]

With stylish in tow, we added formatting to our CI and the world was a better
place, for a while.

## GHC 8.10.X
We like shiny things! Especially if that means low-latency garbage collection
and improved runtime - or a sane option for qualified imports, [post
qualified][post-qualified]!

```haskell
-- This let's you write:
import A.B.C qualified as C
import D.E.F qualified as F
import G.H.I (J)

-- Instead of having to do this to minimize diffs:
import qualified A.B.C as C
import qualified D.E.F as F
import           G.H.I (J)
```

We upgraded to 8.10 as soon as our friends at IOHK patched
[haskell.nix](https://github.com/input-output-hk/haskell.nix) to offer the
latest greatest version of GHC.

The drawback: enabling the post qualified imports broke stylish haskell 😢

## How stylish-haskell works
(Enough preamble!) Here's the recipe for how stylish-haskell formats your
haskell source code:

Stylish is able to go into your source file, look at a specific segment of your
code and apply a so called "step" to it. Each step modifies only a single
type of structure - an example being how to format language pragmas.

The step has the following definition, where `stepFilter` is the actual
functionality of the step:

```haskell
data Step = Step
  { stepName :: String
  , stepFilter :: Lines -> Module -> Lines
  }

-- where
type Lines = [String]
```

The `Module` is given to the step by parsing the source code with
[haskell-src-exts][haskell-src-exts]. This means that the `stepFilter` function
has both the original source code in terms of `Lines` as well as an AST
representation of said source.

Since the step returns `Lines` it's possible to compose several of these
together in order to format the entire file.

When it comes time for the `Step` to edit the `Lines`, stylish has the concept
of `Block` as well as editor functionality operating on blocks and lines called
`Change`.

```haskell
-- A block is defined as:
data Block = Block
  { blockStart :: Int
  , blockEnd :: Int
  }

-- and a change as:
data Change a = Change
  { changeBlock :: Block a
  , changeLines :: [a] -> [a]
  }
```

Here's a short example:

```haskell
deleteTrailingWhitespace :: Step
deleteTrailingWhitespace = makeStep \lines _module -> fmap lines stripWhitespace
  where
    stripWhitespace = reverse . dropWhile isSpace . reverse

-- alternatively using the changes API:
deleteTrailingWhitespace :: Step
deleteTrailingWhitespace =
  makeStep "Delete trailing whitespace" $
    \lines _module -> applyChanges lines (stripWhitespace <$> lineNumbers)
  where
    stripWhitespace i =
      changeLine i . reverse . dropWhile isSpace . reverse

    lineNumbers = -- elided for brevity

```

Both of these do the same thing - but it'll become important later that we're
able to modify the file in place, and for that - the editor functions really
come in hand.

The editor is very useful when combined with the positions from the
[haskell-src-exts][haskell-src-exts] lib. One example being formatting records.
A record is represented as a `Decl` which contains a `RecDecl`. We can get the
starting positions of the record from the `Decl` and then tell stylish to only
format what's between the start and end line of the record. If a `Decl` turns
out to not be a record, we can choose to emit no change. This means we get
preservation of all other parts of the source file - no need to preserve
comments or imports around the record, we can focus only on the thing we want
to change.

In short, stylish haskell does the following:

- Read file into `Lines`
- Check which `Step` to enable and how to configure each of them from the
  stylish config you as the user specified
- Use the editor functions to produce a number of `Change` datas in order to
  edit the file in place by either line numbers or by the concept of `Block`
- Compose each enabled and configured step one after the other
- Write the file to disk

## Haskell source extensions
The limitations of stylish rest fundamentally on the functionality of
[haskell-src-exts][haskell-src-exts]. This means that any new language feature
enabled by language pragma or other flag, needs to first gain support in this
dependency before stylish can make use of it.

The pros of using this library is that it's quite easy to manipulate the
resulting AST that you get from parsing. In contrast to most real compiler
parsers, it keeps a lot of source file information that normal parsers might
discard. Compiler parsers tend to discard things that aren't useful to
compilation - such as comments.[^scala-meta]

Other tooling projects in Haskell land have started using the GHC parser
directly instead as means to mitigate this limitiaton. The parser as well as
the AST is available in the [ghc-lib-parser][ghc-lib-parser] package, which is
the GHC API but usable as a library.

## Rewriting stylish
In order to fix our issue, I started re-writing the parts of stylish that I
thought relevant to my team's needs using [ghc-lib-parser][ghc-lib-parser]. The
result of this work is that this [PR branch][stylish-pr] now formats our
>50kLOC haskell monorepo on every PR and every branch build.

I think it might be interesting to write a separate article on how the GHC AST
works and how I adapted stylish to work with it. It contains a couple of
interesting things like a printer monad. I'll try to put something together
soon, hope this was an interesting read!

// Felix

[^on-data-indentation]:
    Some of you might argue that it does not minimize diffs. Well, it does when
    you structure co-products like this:

    ```haskell
    data Car = Car
      { manufacturingYear :: Year
      , milesRun :: Natural
      }
      deriving stock (Eq, Show)

    data Vehicle
      = MkCar Car
      | MkBicycle Bicycle
      deriving stock (Eq, Show)
    ```

    This has the added benefit that you can get precise types when you
    deconstruct the co-product.

[^lp-maintainers]: Funny story, they actually just wanted to see if they could
  get imports formatted according to our standards and after a few PRs they
  were made maintainers - after that we thought, well - looks like a well
  structured project that we can extend to our liking. Let's go for it!

[^scala-meta]: As a side note, [Eugene](https://twitter.com/eugene_burmako) and
  [Olaf](https://twitter.com/olafurpg) spent an inordinate amount of time
  getting things like comment positions just right for scalameta. Getting
  positions correct _is really damn difficult_. Making an AST easy to use
  while retaining this information is an art.

[post-qualified]: https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#extension-ImportQualifiedPost
[haskell-src-exts]: https://hackage.haskell.org/package/haskell-src-exts
[ghc-lib-parser]: https://hackage.haskell.org/package/ghc-lib-parser
[stylish-pr]: https://github.com/jaspervdj/stylish-haskell/pull/293

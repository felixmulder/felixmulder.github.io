---
title: Revisiting application structure
date: 2020-08-08
published: true
summary: >
  After more than two years of writing production code using Haskell at Klarna,
  we've learned a ton. Initially we leaned a lot on <code>ReaderT</code> but now
  we've gone back to the drawing board of how we structure our applications.
  </br></br>

  In this article, we'll go through how we went from a <code>ReaderT</code>
  based polymorphic approach to a MTL style approach that does not have the
  dreaded <code>n^2</code> instances issue.
---

# Revisiting application structure
After more than two years of writing production code using Haskell at Klarna.
We've learned a ton. Initially, we used a `ReaderT` pattern detailed in my
["Haskell in Production"][hip] mini-series. We've now transitioned into using
`MonadTrans` and `MonadTransControl` as means to write MTL without boilerplate.

In this post, we're going to review the `ReaderT` pattern we used, as well as
go through its shortcomings and our chosen solution to it. Since a lot of
people might only be interested in the solution, it is provided first.

## The solution
Each interface has a corresponding `class`:

```haskell
class Monad m => MonadLog m where
  -- | Print 'a' to the log with source code positions
  logLn :: (HasCallStack, Loggable a) => LogLevel -> a -> m ()
```

which acts as the interface and allows us to write code that's polymorphic in
`m`.

There's a pass-through instance that is able to take any transformer monad `t`
that has an instance for `MonadLog` on its base monad `m`:

```haskell
-- Pass-through instance for transformers
instance {-# OVERLAPPABLE #-}
  ( Monad (t m)
  , MonadTrans t
  , MonadLog m
  ) => MonadLog (t m) where
  logLn level msg = lift (logLn level msg)
```

Having the `OVERLAPPABLE` <span id="overlappable">pragma<span> on the
pass-through instance means that any other instance we define would be chosen
in preference to this one during instance resolution. This is described in the
[GHC user's guide][ghc-overlappable].

The instance above is used in order to provide instances for any transformer.
Getting us past the dreaded `n^2` issue! If you don't know what that is - don't
worry, it's explained [further down](#the-n2-issue).

Now it comes time to choose how to implement our effects. For each effect,
there's a newtype that constitutes the effect:

```haskell
-- | Newtype for disabling logging
newtype NoLoggingT m a
  = NoLoggingT { runNoLoggingT :: m a }
  deriving newtype (Functor, Applicative, Monad)
  deriving (MonadTrans) via IdentityT

instance Monad m => MonadLog (NoLoggingT m) where logLn _ _ = pure ()
```

This instance allows for us to choose not to log when we run our final program
using the `runNoLoggingT` function provided as a field in the newtype.

Here's a real implementation of a console logger using [fast-logger][fast-logger]:

```haskell
-- Transformer for logging to Console
newtype ConsoleLogT m a
  = ConsoleLogT { unConsoleLogT :: ReaderT (LoggerSet, Trace) m a }
  deriving newtype (Functor, Applicative, Monad, MonadTrans)

-- Instance using fast-logger to print to console
instance MonadIO m => MonadLog (ConsoleLogT m) where
  logLn level msg = ConsoleLogT do
    (loggerSet, traceId) <- ask
    logItem traceId (ConsoleLogger loggerSet) level (toLogItem msg)

runConsoleLogT :: MonadIO m => ConsoleLogT m a -> m a
runConsoleLogT (ConsoleLogT m) = do
  loggerSet <- liftIO (newStdoutLoggerSet defaultBufSize)
  runReaderT m (loggerSet, Uncorrelated)
```

Thanks to this structure, we can dispatch our effects at the "end of
the world" by using the functions from each interface we want to use.
The instances we've defined for `ConsoleLogT` and `NoLoggingT` are considered
before the pass-through instance due to the `OVERLAPPABLE` pragma.

Here's an example from a service that logs things, submits metrics and reads
messages from an SQS queue.

```haskell
runProgram :: MetricsReporter -> QueueUrl -> AWS.Env -> IO ()
runProgram reporter queueUrl awsEnv =
  = runConsoleLogT
  . runMetricsT reporter
  . runConsumerT queueUrl awsEnv
  $ program

program :: (SqsConsumer m, MonadLog m, MonadMetrics m) => m ()
program = ...
```

This reminds us of how effects are dispatched using free monads, but by relying
on something that's well optimized by GHC and has strong support (MTL) in the
community.

The rest of this post discusses how we got here and what we did before.

# The road to the solution
When starting out with Haskell, we didn't want to overcomplicate things.
`Reader` is one of the first monads that our people got comfortable with.

Thus, our interfaces were all based on `ReaderT` instances. This meant that
essentially all interfaces required some data `r` and were then based on
`MonadIO m` like:

```haskell
-- | An interface to produce SQS messages
class Monad m => SqsProducer m where
  -- | Produce messages to SQS, returning unit or an error in 'm'
  produceMessage ::
    SqsMessageGroupId -> SqsDedupeId -> SqsMessage -> m (Either SqsProducerError ())

data RequestSender m = RequestSender
  { _produceMessage ::
      SqsMessageGroupId -> SqsDedupeId -> SqsMessage -> m (Either SqsProducerError ())
  }

instance
  ( HasType (RequestSender m) r
  , MonadCatch m
  , MonadIO m
  , MonadLog (ReaderT r m)
  ) => SqsProducer (ReaderT r m)
  produceMessage groupId dedupeId message = do
    (RequestSender produceMsg) <- asks getTyped
    lift (produceMsg groupid dedupeId message)
```

This gives us an end-of-the-world behavior where we need to do something similar to:

```haskell
data ListenerContext m = ListenerContext
  { requestSender :: RequestSender m
    -- .. and other deps
  }
  deriving stock (Generic)

runListener_ :: (MonadIO m) => ListenerContext -> m ()
runListener_ = forever . runReaderT (void handleMessage)

handleMessage :: SqsConsumer m => SqsProducer m => m Result
handleMessage = do
  msg <- getMessage        -- * Take a message from one queue
  res <- performAction msg -- * Perform some action
  produceMessage msg       -- * Put it on a different queue
  pure res                 -- * Return result of processing
```

in order to run our program. This is _fine_.

We've achieved what we want out of dependency injection:

1. We can swap out the behavior of the interface by swapping out the
   `RequestSender` data type. E.g. allowing us to stub it in tests
2. We can write code in a polymorphic setting relying on interfaces
   rather than concrete implementations
3. We have something that we thought was fairly easy for our devs to grok (ah,
   it's a reader where the behavior is defined by the object we feed in,
   gotcha!).

## Why readers became a burden
There are a few drawbacks to this pattern. Let's start with the most glaring issues:

- Granular control over interfaces becomes tedious due to the extra indirection
  with the passed data

- Certain things are difficult to implement, e.g:

  ```haskell
  class Foo m where
    withCallback :: (a -> m b) -> m b
  ```

  This requires a _lot_ of lifting back and forth especially when the concrete
  implementation is in `IO` and your interfaces are all in `m`

- Error messages become vague and based on the instance constraints e.g:

  ```
  Couldn't satisfy constraint 'HasType (RequestSender m)'
  ```

  instead of the much more easily understandable:

  ```
  Missing instance 'SqsProducer (ReaderT r m)'
  ```

  In the latter, we can see that the instance is missing for the `SqsProducer`
  whereas in the former - we sort of need to do instance resolution by grep to
  figure out what class GHC is trying to construct an instance for.

- Lastly, and most important: it didn't turn out to be so easy to grok as we
  thought

## Looking at alternatives
There are a couple of alternatives to this approach to dealing with effects. If
you _want_ effect tracking in your types - there are a number of libraries that
deal with this:

- [polysemy](https://hackage.haskell.org/package/polysemy)
- [fused-effects](https://hackage.haskell.org/package/fused-effects)

Both of these are promising, but we're not really comfortable with the
drawbacks to either one at the moment. In a nutshell - they're both great
libraries, however, they're pretty advanced.

Our old solution combined two things - and this was its main mistake. Either we
should've said no to our interfaces and gone with something like the handler
pattern - or we should've leaned fully into MTL.

The drawback with handler pattern is that we can't be polymporphic, which we
really like for testing. The drawback with MTL is the `n^2` instances problem.

Urgh! From our wishes on polymorphism it's clear we can't use the handler
pattern. But can we use MTL if we solve the `n^2` issue? And what is the `n^2`
issue?

## The `n^2` issue
When using monad transformers, you need to write the monad instances for all
the different types of transformers. Here's an example from the [MTL source
code][mtl-boilerplate]:

```haskell
instance MonadState s m => MonadState s (ExceptT e m) where
  get = lift get
  put = lift . put
  state = lift . state

instance MonadState s m => MonadState s (IdentityT m) where
  get = lift get
  put = lift . put
  state = lift . state

instance MonadState s m => MonadState s (ReaderT r m) where
  get = lift get
  put = lift . put
  state = lift . state
```

This is very mechanical and boilerplaty. For these common transformers, these
instances have all been written. However, every time you add an additional
transformer - you need to write all these `n` instances where `n` is the number
of interfaces you intend to use. Thus the `n^2` complexity.

## Control structures for monad transformers
For most of our monads that we create ourselves, they simply require this very
mechanical boilerplate. This behavior looks like it could be captured by a
typeclass (or two).

One of our engineers, [Moisés][moises], who previously worked for Standard
Chartered introduced us to their solution to this issue.

Pepe Iborra commented on the [PR for this post][pepe-comment] and provided the
following insight into how the solution came about:

> When I joined Strats in Jan 2017, the codebase was already making heavy use
> of type classes for individual effects, e.g. `MonadTime`, `MonadDelay`,
> `MonadLog`, etc. but there was no solution to the n^2 problem. Monad
> transformers were providing instances for all the effects, relying on
> deriving to avoid as much boilerplate as possible. Alexis [article][alexis-mtl]
> takes this approach to the extreme.
>
> I made the point that introducing a new effect class required adding it to
> the deriving lists of all N transformers, which made engineers unwilling to
> add effects. and the approach could not scale. My solution to this was the
> passthrough instance, which requires a `MonadTransControl` transformer (or
> `MonadtTrans` for non-scoped effects). Since all `ReaderT` transformers
> are in `MonadTransControl` by definition unless the environment mentions the
> base monad, the codebase quickly gravitates towards `ReaderT` in order to
> avoid having to write instances manually.
>
> -- [Pepe Iborra][pepe-twitter]


So, indeed, this can be captured by a typeclass. Enter `MonadTrans`:

```haskell
-- Pass-through instance for transformers
instance {-# OVERLAPPABLE #-}
  ( Monad (t m)
  , MonadTrans t
  , MonadLog m
  ) => MonadLog (t m) where
  logLn level msg = lift (logLn level msg)
```

This instance can now lift any monad `m` that implements `MonadLog` into the
transformer `t`. This means no more having to write `n` instances 🎉

As noted [above](#overlappable), the `OVERLAPPABLE` pragma allows us to control
precedence for the pass-through instance, such that any other instance we
define would be chosen in preference to it during instance resolution. This is
described in the [GHC user's guide][ghc-overlappable].

For the example above with a callback in `m`, we can use `MonadTransControl` as
it has the ability to run something in the base monad. The real version of our
`MonadLog` has a function that allows you to specify a traceable ID that we
call `CorrelationId`:

```haskell
class Monad m => MonadLog m where
  -- | Print 'a' to the log with source code positions
  logLn :: (HasCallStack, Loggable a) => LogLevel -> a -> m ()
  -- | Correlate the 'm a' with the given correlation ID
  correlatedWith :: CorrelationId -> m a -> m a
```

In our passthrough instance for this version of `MonadLog` we now need to use
`MonadTransControl`:

```haskell
-- Pass-through instance for transformers
instance {-# OVERLAPPABLE #-}
  ( Monad (t m)
  , MonadTransControl t
  , MonadLog m
  ) => MonadLog (t m) where
  logLn level msg = lift (logLn level msg)
  correlatedWith corrId ma = do
    result <- liftWith \runInBase ->
      correlatedWith corrId (runInBase ma)
    restoreT (pure result)
```

We can leave out `MonadTrans` since it's implied by `MonadTransControl`.

A full example was given in at the [start](#the-solution) of this post.

## In closing
I hope this post presents a useful and comprehensible way to control effects in
Haskell without deviating too much from standard language features.

Since writing this, I was pointed to Alexis's [article][alexis-mtl] on making
MTL typeclasses derivable. It's a much more thorough article than mine and I
greatly appreciated it.

### Edits
* Add details on `OVERLAPPABLE` and their precedence in instance resolution
  ([Moisés][moises])
* Add anchor to `n^2` issue when from where it was first mentioned
  ([Moisés][moises])
* Add error message con to `ReaderT` section ([Moisés][moises])
* Add [Pepe Iborra][pepe-twitter]'s account of how this came about at Standard
  Chartered

[hip]: /writing/2019/10/05/Designing-testable-components.html
[fast-logger]: https://hackage.haskell.org/package/fast-logger
[mtl-boilerplate]: https://github.com/haskell/mtl/blob/master/Control/Monad/State/Class.hs#L152-L171
[ghc-overlappable]: https://downloads.haskell.org/ghc/latest/docs/html/users_guide/glasgow_exts.html#overlapping-instances
[moises]: https://twitter.com/1akrmn
[pepe-twitter]: https://twitter.com/iborrapepe
[pepe-comment]: https://github.com/felixmulder/felixmulder.github.io/pull/4#discussion_r467620952
[alexis-mtl]: https://lexi-lambda.github.io/blog/2017/04/28/lifts-for-free-making-mtl-typeclasses-derivable/

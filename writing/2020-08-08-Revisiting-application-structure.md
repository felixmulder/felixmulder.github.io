---
title: Revisiting application structure
date: 2020-08-08
published: true
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

The instance above is used in order to provide instances for any transformer.
Getting us past the dreaded `n^2` issue!

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

Thanks to this structure, this allows us dispatch our effects at the "end of
the world". By selecting the functions from each interface that we want to use.
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

The drawback with handler pattern would that we can't be polymporphic, we
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

One of our engineers, [Moisés](https://twitter.com/1akrmn), who previously
worked for Standard Chartered had solved this type if issue before in their
previous team.

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

[hip]: /writing/2019/10/05/Designing-testable-components.html
[fast-logger]: https://hackage.haskell.org/package/fast-logger
[mtl-boilerplate]: https://github.com/haskell/mtl/blob/master/Control/Monad/State/Class.hs#L152-L171

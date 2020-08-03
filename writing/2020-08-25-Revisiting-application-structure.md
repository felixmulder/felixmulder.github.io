---
title: Revisiting application structure
date: 2020-08-25
published: false
---

# Revisiting application structure
After more than two years of writing production code using Haskell at Klarna.
We've learned a ton. Initially, we used a `ReaderT` pattern detailed in my
["Haskell in Production"][hip] mini-series. We've now transitioned into using
`MonadTransControl` as the monad we lift through.

In this post, we're going to review the `ReaderT` pattern we used, as well as
go through its shortcomings and our chosen solution to it.

## The solution
Let's start with the solution, tl;dr style:

```haskell
class Monad m => MonadLog m where
  -- | Print 'Text' to the log with source code positions
  logLn :: HasCallStack => Loggable a => LogLevel -> a -> m ()

-- Pass-through instance for transformers
instance {-# OVERLAPPABLE #-}
  ( Monad (t m)
  , MonadTransControl t
  , MonadLog m
  ) => MonadLog (t m) where
  logLn level msg = lift (logLn level msg)

-- | Newtype for disabling logging
newtype NoLoggingT m a
  = NoLoggingT { runNoLoggingT :: m a }
  deriving newtype (Functor, Applicative, Monad, MonadIO)
  deriving (MonadTrans, MonadTransControl) via IdentityT

instance Monad m => MonadLog (NoLoggingT m) where logLn _ _ = pure ()

-- Transformer for logging to Console
newtype ConsoleLogT m a
  = ConsoleLogT { unConsoleLogT :: ReaderT (LoggerSet, Trace) m a }
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadTrans, MonadTransControl)

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

This now allows us to choose at the end-of-the-world how we want to execute our effects. E.g:

```haskell

runProgram :: MetricsReporter -> QueueUrl -> AWS.Env -> IO ()
runProgram reporter queueUrl awsEnv =
  = runConsoleLogT
  . runMetricsT reporter
  . runConsumerT queueUrl awsEnv
  $ program

program :: SqsConsumer m => MonadLog m => MonadMetrics m => m ()
program = ...
```

allowing us to fully decide at the end of the world what effects we want to
dispatch and how. It makes testing a lot more straightforward and above all
_clear_ to the reader.

## Readers readers readers
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

runListener_ :: MonadIO m => ListenerContext -> m ()
runListener_ = forever $ runReaderT (void handleMessage)

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
3. We have something that was fairly easy for our devs to grok (ah, it's a
   reader where the behavior is defined by the object we feed in, gotcha!).

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

[hip]: /writing/2019/10/05/Designing-testable-components.html

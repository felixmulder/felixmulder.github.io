---
title: "Haskell in Production: Designing Testable Components"
date: 2019-10-05
published: true
---

# Designing Testable Components
This chapter of the "Haskell in Production" article series focuses on how to
structure your application so that components are testable. This chapter will
give you the tools that are analogous to OOP mocking and dependency injection.

You can find the complete code for this service in the
[haskell-in-production](https://github.com/felixmulder/haskell-in-production)
repo on GitHub.

## Let's build an HTTP API!
In this tutorial we're going to be building an HTTP API. The API will have two
endpoints:

* Create a user `POST /user`
  ```json
  {
    "username": "<String>",
    "password": "<String>"
  }
  ```
* Delete a user `DELETE /user/<user-id>`

## API definition
We're going to use a simplified model of a Haskell API - but we could easily
use something like Servant[^servant] (which is what we use at Klarna by the
way). Below, you can find the first step in modeling our HTTP API from above:

```haskell
api :: Request -> IO Response
api request =
  case methodAndPath request of
    POST (matches "/user" -> Just []) -> do
      createNewUser (requestBody request) >>= toResponse
    DELETE (matches "/user/:userId" -> Just [userId]) ->
      deleteUserId (UserId userId) >>= toResponse
    _unmatched ->
      pure NoResponse

main :: IO ()
main = run 8080 api
```

We're modeling the handling of the two requests, creating and deleting a user,
as two branches in the case expression. In the `main` function, we'll mount
this request handler on port 8080.

For simplicity we're going to use a simplified definition of `User`:

```haskell
data User = User
  { userId :: UserId
  , userName :: UserName
  }
  deriving stock (Generic)
```

This data type is going to be returned when creating a user. For simplicity
we're just going to derive a JSON encoder using
[Aeson](http://hackage.haskell.org/package/aeson-1.4.2.0/docs/Data-Aeson.html):
```haskell
instance ToJSON User
```

## Dependency Injection
If you come from a Java or other OOP languages, then you've surely dealt with
dependency injection via annotations or frameworks like Guice, Spring or Castle
Windsor.

But what is really dependency injection? Let's get back to the core of it. DI
is parameterizing components. The simplest form of dependency injection is just
passing the dependencies as arguments to functions.

In this article we'll explore how to do this on the language level in Haskell;
as opposed to relying on meta-programming and reflection like the frameworks
mentioned above.

## Parameterizing Functions
Let's zoom in on the `createNewUser` function from above. The function takes
the body of the HTTP request and produces something that can be turned into an
HTTP response by our framework. Here's how one could write an initial version
of said function:

```haskell
createNewUser :: RequestBody -> IO (Either Error User)
createNewUser body =
  case bodyToUser body of
    Left err -> pure . Left $ err
    Right (user, pass) -> do
      -- Connect to DB:
      db <- connectToDb
      let
        insertSql =
          "INSERT INTO table (user_name, password) VALUES (?, ?) returning id"

      -- Persist using insert statement:
      userId <- query db insertSql (user, pass)

      -- Create a response from the persisted argument:
      pure . Right $ User { userName = user, userId = userId }
```

This function is not very clean for a number of reasons:

* It seems to connect to the DB on every call
* It doesn't take any configuration in order to know how to connect to the DB
* The DB persistence is not abstracted from the domain logic

Let's solve this by parameterizing the function - with another function!

We're first going to factor out the persistence by creating an `insertNewUser`
function. This function takes a database as well as the required arguments
used to persist the user in our original implementation:

```haskell
insertNewUser :: Database -> UserName -> Password -> IO UserId
insertNewUser db user pass =
  let
    insertSql =
      "INSERT INTO table (user_name, password) VALUES (?, ?) returning id"
  in
    query db insertSql (user, pass)
```

Now - we can partially apply it and pass it to along to `createNewUser`.
Adding the parameter makes our implementation look something like this:

```haskell
createNewUser ::
     (UserName -> Password -> IO UserId)
  -> RequestBody
  -> IO (Either Error User)
createNewUser persistUser body =
  case bodyToUser body of
    Left err -> pure . Left $ err
    Right (user, pass) -> do
      -- Persist user:
      userId <- persistUser user pass

      -- Create a response from the persisted argument:
      pure . Right $ User { userName = user, userId = userId }
```

And its usage would thus be:

```haskell
createNewUser (insertNewUser db) (requestBody request) >>= toResponse
```

Since this is now just an argument, we can choose precisely where we want to
start passing this parameter from. Such setup is usually done at the edge of
the application - i.e. `main`.

## Solving the problem at scale
So here's the problem with the above solution: while it does work, it doesn't
really scale. Domain logic will often need access to several interfaces to do
its job. It might need both an HTTP client for some request and a database to
store the result. As the requirements grow, the solution above quickly becomes
quite verbose in practice.

E.g:

```haskell
validateUser :: (UserId -> IO (Maybe User)) -> (UserName -> IO Bool) -> UserId -> IO Bool
validateUser getUser callIntoThirdPartyService userId = do
  userM <- getUser userId
  maybe (pure False) (callIntoThirdPartyService . userName) userM
```

This example adds one function as argument, but what if you add a third? A
fourth? You get the picture.

We also don't want to write our code in `IO` - while useful of course, the
surface area of possible effects is huge. We'd like to limit the power of each
component, if they are written in `IO` - they can do *anything*. We'll get to
this later in the article, but first let's focus on solving scalability of this
initial approach.

### Introducing the Handle pattern
Instead of parameterizing the function with another function we can
parameterize with a datatype containing a function. This pattern is known as
the "Handle pattern".[^handle-pattern] This pattern is great for a number of
reasons.

1. We can group functions that operate in similar ways together (think OO
   interface)
2. We don't have to pass around all those functions, instead we pass one
   datatype
3. It's quite simple to understand

So what would this look like?

```haskell
data Application =
  { persistUser :: UserName -> Password -> IO UserId
  , getUser :: UserId -> IO (Maybe User)
  , callIntoThirdPartyService :: UserName -> IO Bool
  , logLn :: Loggable a => a -> IO ()
  }
```

Now we can pass that to `validateUser`:

```haskell
validateUser :: Application -> RequestBody -> UserId -> IO Bool
validateUser app requestBody userId = do
  -- Here `&` is applying `getUser` to `app`:
  userM <- (app & getUser) userId
  maybe (pure False) (app & callIntoThirdPartyService $ userName) userM
```

But - when looking at this, you've probably seen an issue. `callIntoThirdPartyService`
does not really fit in with the rest of the functions in `Application`. As a
solution, we could nest the `Application` type. So let's redefine it:

```haskell
data Persistence =
  { persistUser :: UserName -> Password -> IO UserId
  , getUser :: UserId -> IO (Maybe User)
  }

data Application =
  { persistence :: Persistence
  , callIntoThirdPartyService :: UserName -> IO Bool
  , logLn :: Loggable a => a -> IO ()
  }
```

This gives us a bit more granularity, and a cleaner interface to work with.
However, we still have a couple of issues:

* We're running in `IO`
* We have to manually pass around the `Application` everywhere it's needed
* We are unconstrained in what a function can do - if it receives an `Application`,
  it can do anything contained within that interface

## Getting rid of `IO`
If we add a generic type parameter to the handles, we can abstract away `IO`:

```haskell
data Persistence m =
  { persistUser :: UserName -> Password -> m UserId
  , getUser :: UserId -> m (Maybe User)
  }

data Application m =
  { persistence :: Persistence m
  , callIntoThirdPartyService :: UserName -> m Bool
  , logLn :: Loggable a => a -> m ()
  }
```

Now, if we wanted to - we can actually run these as pure functions by using the
`Identity` monad.

This might be strange to you, don't worry, we'll get to this in the testing
section below.

## Constraining Functions
When we write applications, typically the most powerful function will be
`main`. It can do anything. When it comes to our interfaces, we want to
constrain their possible effects - and thus limit what we need to test.

We're interested in what is commonly referred to as *effect tracking*.

Because we have gotten rid of `IO` in our refactoring above we can now choose
which monad to evaluate our programs in. This gives us the power to limit the
effects of the monad. We can evaluate it purely or we can evaluate it with
only certain effects.

There are several libraries that exist explicitly to model
effects.[^effect-libs] They have different focuses - in this tutorial we'll
use plain Haskell to model effects. This solution is somewhat more verbose, but
we're willing to live with the extra boilerplate in order to arrive at a solution
that is easier to grok.

The first order of business at this point is to introduce you to a monad called
"Reader". If you already know about it - you can skip ahead to [Actually
getting rid of the manual wiring](#actually-getting-rid-of-the-manual-wiring).

## Introducing reader
We want to get rid of the manual wiring. This is were Reader comes into play.
The easiest way to describe reader is to say that it is a monad that is able to
read a value from its context.

What's an example of something that can read a value from its context? Well, a
function!

```haskell
getPersistUser :: Application m -> (UserName -> Password -> m UserId)
getPersistUser app = app & persistence & persistUser
```

We could re-write this in a way where we don't explicitly have to pass the
`Application` argument:

```haskell
getPersistUser :: MonadReader Application m => m (UserName -> Password -> m UserId)
getPersistUser = do
  app <- ask
  pure $ app & persistence & persistUser
```

Unfortunately, the type signature has changed - but that is a very small price
to pay since we can just unwrap it by doing something like:

```haskell
runReader getPersistUser app
```

(In fact this will make the `m` be `Identity` and then unwrap it for us!)

We could of course run this in any suitable monad we wanted to - like `Either
a` or `Maybe` or `IO`. It might seem like a contrived example, but bear in mind
that if we let all the functions that require this parameter be readers - we
can compose them before actually running it and thus only pass the parameter
once.

## Actually getting rid of the manual wiring
Now that we know about reader, it's time to deliver on our goals of effect
tracking - and as an added bonus get cleaner interfaces for these effects.

We will now be using a type class in order to bundle things that fall under the
same effect. For instance writing and reading to the database would fall under
an interface `Persist`:

```haskell
class Monad m => Persist m where
  persistUser :: UserName -> Password -> m UserId
  getUser :: UserId -> m (Maybe User)
```

We're saying that `m` must be a monad, this will come in handy since it lets us
use do-notation.

This typeclass now allows us to re-write a function like `createNewUser` with a
type signature that lets us know about its effects.

```haskell
createNewUser :: Persist m => RequestBody -> m (Either Error User)
createNewUser body =
  case bodyToUser body of
    Left err -> pure . Left $ err
    Right (user, pass) -> do
      userId <- persistUser user pass
      -- Create a response from the persisted argument:
      pure . Right $ User { userName = user, userId = userId }
```

Notice how we now don't have to pass the `Application` to this function
anymore! It's pretty cool. Unfortunately, this means that we have to pay the
price somewhere else. We still need a concrete version of this to be able to
call it from `main`.

Let's create such an instance:

```haskell
instance
  ( MonadReader (Persistence m) m
  ) => Persist m where
  persistUser user pass =
    ask >>= \(Persistence persist _) -> persist user pass
  getUser userId =
    ask >>= \(Persistence _ get) -> get user pass
```

We're still not at the steady state solution here. Because, when we want to
compose different interfaces together - these instances don't have the same
reader. This one reads `Persistence m` and no other data. When we do
`runReader`, we have to do something like:

```haskell
runReader (persistUser "user" "pass") (app & persistence)
```

We cannot do:

```haskell
runReader (persistUser "user" "pass") app
```

Bummer.

But hey! We can solve this. We can make use of a type class
[`Has`](http://hackage.haskell.org/package/data-has) that tells us that a
datatype `r` has `a` by constraining the instance with "`Has a r`". After
refactoring, we get this:

```haskell
instance
  ( Has (Persistence m) r
  , Monad m
  ) => Persist (ReaderT r m) where
  persistUser user pass =
    asks getter >>= \(Persistence persist _) -> lift $ persist user pass
  getUser userId =
    asks getter >>= \(Persistence _ get) -> lift $ get user pass
```

Here we choose to create an instance for `ReaderT r m` which itself is a
reader. In fact, it's a reader that reads `r` in a specific monad `m`.

The great thing about this is that your interfaces compose under the same
monad. No need to, as in MTL, define the `n^2` number of instances where `n` is
the number of interfaces.[^n-squared-mtl]

If we have a different interface:

```haskell
class Monad m => Log m where
  logLn :: HasCallStack => Loggable a => a -> m ()

data Logger m =
  Logger (Text -> m ())

instance
  ( Has (Logger m) r
  , Monad m
  ) => Log (ReaderT r m) where
  logLn a =
    asks getter >>= \(Logger doLog) -> lift . doLog . fromLoggable $ a
```

We can constrain our function:

```haskell
createNewUser ::
     Persist m
  => Log m
  => RequestBody -> m (Either Error User)
createNewUser body =
  case bodyToUser body of
    Left err ->
      Left err <$ logLn ("Couldn't convert " <> body <> "to user and pass")
    Right (user, pass) -> do
      logLn $ "Going to create " <> user
      userId <- persistUser user pass
      -- Create a response from the persisted argument:
      pure . Right $ User { userName = user, userId = userId }
```

> Note: `(Persist m, Log m)` is equivalent to currying the constraints as above.

To run it we use the following:

```haskell
runReaderT (createNewUser request) (logger, persistence)
```

This could seem a bit magical, but it works because the `Has` typeclass has
instances for tuples of all sizes.

Or if we adjust `Application` and create the appropriate `Has` instances:
```haskell
data Application m = Application
  { persistence :: Persistence m
  , logger :: Logger m
  }

app :: Application
app = _

runReaderT (createNewUser request) app
```

As you can see we're using `runReaderT` here instead of `runReader`. This is
because now we're not assuming that the effect is `Identity` - it can be any
monad `m`.

In summary, we can now say that each interface becomes a "capability" that the
function has. In the case of `createNewUser` it can perform pure computations
as well as both log and persist. This means that we have some semblance of
effect tracking. We're also able to organize our effects so that the most
powerful function is the entry point to the system (e.g. `main`) and then each
function performing domain logic becomes less powerful.

## Composing interfaces
You might be wondering how you base higher-level interfaces on lower ones. For
instance, you might want to allow the `Persist m` capability to do logging. For
this, we need to revisit the instance declaration. Our original solution based
the instance on `(Monad m, Has (Persistence m) r)`. We now need to add a
constraint to reference our `Log` instance. This would look something like this:

```haskell
instance
  ( Has (Persistence m) r
  , Log (ReaderT r m)
  , Monad m
  ) => Persist (ReaderT r m) where
  persistUser user pass = _
  getUser userId = _
```

Here you can see that we reference the specific instance of `Log` that aligns
with the one we're currently defining.

## The final application
We can now parameterize our `api` function from [API
definition](#api-definition) with these interfaces:

```haskell
api ::
    Log m
 => Persist m
 => Request -> m Response
api = _

main' :: Application IO -> IO ()
main' app = run "8080" $ \req -> runReaderT (api req) app

main :: IO ()
main = main' app
  where
    app :: Application IO
    app = Application
      { persist = defaultPersist
      , logger = defaultLogger
      }
```

In a real world application, we would also read the configuration from the
environment.

## Summary
In this section we've seen how to properly parameterize our interfaces and
instantiate them using `runReaderT`. We've set ourselves up to be able to test
these components individually and together. In the next section of this series,
we'll see just how to do that.

Next part [Testing your components](/writing/2019/10/05/Testing-your-components.html)

---

### Edits
- 2019-10-13: explicitly showcase partial application of `insertNewUser`
- 2019-10-14: remove extraneous occurrences of "simply"

[^servant]: [servant - A Type-Level Web DSL](https://haskell-servant.readthedocs.io/en/stable/)
[^handle-pattern]: [Haskell Design Patterns: The Handle Pattern](https://jaspervdj.be/posts/2018-03-08-handle-pattern.html)
[^effect-libs]: [fused-effects](http://hackage.haskell.org/package/fused-effects), [extensible-effects](http://hackage.haskell.org/package/extensible-effects), [polysemy](http://hackage.haskell.org/package/polysemy)
[^n-squared-mtl]: [Writing a Monad Transformer, does it really need so many hardcoded instances](https://stackoverflow.com/a/35541483)

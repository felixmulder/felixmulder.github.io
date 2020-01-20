---
title: Haskell in Production
date: 2020-01-21
---

## Haskell in Production

Services at Klarna written in Haskell

---

## What Haskell is, and what it isn't.

---

## Common misconceptions

---

## Common misconceptions
* Reading Haskell code is hard

---

## Common misconceptions
* You need to understand the m-word

---

## Common misconceptions
* Haskell is slow

---

## Common misconceptions
* The libraries you want don't exist

---

## Common misconceptions
* The tooling you want doesn't exist

---

## Common misconceptions
* You need a PhD to get anything done

---

## Common misconceptions
* Writing features is slow

---

## None of these are inherently true.

---

## Some of them can be.

---

## Honestly it's the same for Erlang.

---

![](/img/why_isnt_fp_the_norm.png)

---

![](/img/you_are_but_a_child_profunctor.jpg)

---

## Leaving the Ivory Tower

---

#### From the Ivory Tower to the Happy Island
- Learning resources
- Inclusive community
- User-friendly libraries
- Golden path

---

## Golden Path
- Language
- Libraries
- Tools
- Patterns

---

## Haskell at Klarna
We're not officially supporting it.

---

## Yet.

---

## Our Haskell stack

---

### Libraries
* `servant`
* `postgresql-simple (+ opalaye)`
* `hedgehog`

---

### Servant
A set of packages to declare web APIs on the type-level

---

```haskell
type GetPet
    = "pets" :> Capture "petId" PetId :> Get '[JSON] Pet
```

---

```haskell
type GetPet
    = "pets" :> Capture "petId" PetId :> Get '[JSON] Pet

type PostPet
    = "pets" :> Capture "petId" PetId :> Post '[JSON] NoContent
   :> ReqBody '[JSON] Pet
```

---

```haskell
type GetPet
    = "pets" :> Capture "petId" PetId :> Get '[JSON] Pet

type PostPet
    = "pets" :> Capture "petId" PetId :> Post '[JSON] NoContent
   :> ReqBody '[JSON] Pet

type PetAPI
    = GetPet
 :<|> PostPet
 :<|> DeletePet
```

---

### But that's not the best part of servant.

---

### Types can actually give you much more.
* Statically defined OpenAPI
* Statically defined documentation
* Clients for free in both Haskell and JavaScript!
* Mock an entire server from the type!

---

### OpenAPI
```haskell
type PetApi = ...

petApiSwagger = encode $ toSwagger $ Proxy @UserApi
```

---

### OpenAPI
```haskell
type PetApi = ...

petApiSwagger = encode $ toSwagger $ Proxy @UserApi
-- error:
-- • No instance for (ToSchema Pet)
--     arising from a use of ‘toSwagger’
-- • In the second argument of ‘($)’, namely
--     ‘toSwagger $ Proxy @GetPet’
--   In the expression: encode $ toSwagger $ Proxy @GetPet
--   In an equation for ‘swaggy’:
--       swaggy = encode $ toSwagger $ Proxy @GetPet
```

---

### OpenAPI
```haskell
data Pet = Pet
  { name :: Text
  , age  :: Int
  }
  deriving (Generic)
```

---

### OpenAPI
```haskell
data Pet = Pet
  { name :: Text
  , age  :: Int
  }
  deriving (Generic)

instance ToJSON Pet
```

---

### OpenAPI
```haskell
type PetApi = ...

petApiSwagger = encode $ toSwagger $ Proxy @UserApi
-- { "swagger": "2.0", ... }
```

---

### Clients - Haskell
```haskell
type PetApi = ...

getPetClient :: PetId -> ClientM Pet
postPetClient :: PetId -> Pet -> ClientM NoContent
getPetClient :<|> postPetClient = client (Proxy @PetApi)

getPet :: ClientEnv -> PetId -> IO (Either ClientError Pet)
getPet env petId = runClientM (getPetClient petId) env
```

---

### Clients - JS
```haskell
import Servant.JS (jsForApi)
import Servant.JS.Axios (axios)

type PetApi = ...

jsCode :: String
jsCode = jsForApi (Proxy @PetApi) axios
```

---

### Documentation
```haskell
apiDocs :: String
apiDocs = _ $ docs $ Proxy @PetApi
```

---

### Documentation - markdown!
```haskell
apiDocs :: String
apiDocs = markdown $ docs $ Proxy @PetApi
```

---

### Documentation - pandoc!
```haskell
apiDocs :: String
apiDocs = _ $ pandoc $ docs $ Proxy @PetApi
```

<p class="smaller">
AsciiDoc, CommonMark, ConTeXt, Custom, Docbook, Docx, DokuWiki, EPUB, FB2, HTML, Haddock, ICML, Ipynb, JATS, Jira, LaTeX, Man, Markdown, Math, MediaWiki, Ms, Muse, Native, ODT, OOXML, OPML, OpenDocument, Org, Powerpoint, RST, RTF, Shared, TEI, Texinfo, Textile, XWiki, ZimWiki
</p>

---

### Mocked server
```haskell
petGen :: Gen Pet
    = Pet
  <$> choice [ "Diddy", "Tupac" ]
  <*> Gen.int (Range 0 10)
```

---

### Mocked server
```haskell
import Servant.Mock

petApi :: Proxy PetApi
petApi = Proxy

main :: IO ()
main = run 8080 $ serve petApi (mock petApi Proxy)
```

---

### Property based integration tests
```haskell
import Hedgehog.Servant

genApi :: Gen (BaseUrl -> Request)
genApi = genRequest (Proxy @PetApi) (genPet :*: GNil)

```

---

### <100 LOC

---

#### So far, we haven't written a single line of implementation code.

---

## Other integrations
* SwaggerUI
* Other language clients:
  * Purescript
  * Elm
  * Kotlin
  * Python
  * C#
  * Ruby
* Checked exceptions

---


```haskell
type GetPet
    = "pets" :> Capture "petId" PetId :> Get '[JSON] Pet
```

```haskell
getPet :: PetId -> Maybe Pet
```

---

```haskell
getPet :: PetId -> Maybe Pet
```

---

```haskell
getPet :: PetId -> Maybe Pet

-- Which we'll translate to:

getPet :: PetId -> Handler Pet
```

---

## So, what's a Handler?

---

## Yes, it's an m-word.

---

#### Handler
![](/img/handler_hackage.png){ width=40% }

---

```haskell
pets :: Map PetId Pet
pets = Map.fromList
  [ ("cat", undefined)
  , ("dog", undefined)
  ]

getPet :: PetId -> Handler Pet
getPet petId =
  case Map.lookup petId of
    Just pet -> return pet
    Nothing  -> throw404
```

---

#### Putting it all together
```haskell
apiHandler :: Server PetApi
apiHandler
     = getPet
  :<|> postPet

main :: IO ()
main
  = run 8080
  $ serve (Proxy @PetApi) apiHandlers
```

---

## In summary
* Getting stuff done is quick, and sans footguns

* We're really happy with what Haskell offers us

---

## Thank you!

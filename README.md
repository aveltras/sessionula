# Sessionula

This library intends to provide server-side session functionality for your web applications.

:construction: **Not ready for production use yet.** :construction:

**API is still highly subject to changes**  
**Feedback welcome**

~~~ haskell
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeOperators     #-}

import           Control.Monad (void)
import           Data.ByteString.Lazy.Char8 (pack)
import qualified Database.Redis as Hedis
import qualified Sessionula as Session
import           Sessionula.Backend.File
import           Sessionula.Backend.Hasql
import           Sessionula.Backend.Hedis
import           Sessionula.Backend.Map
import           Network.HTTP.Types
import           Network.Wai
import           Network.Wai.Handler.Warp (run)
import           Servant
import           Sessionula.Frontend.Servant.Server
~~~

Define a user type to be used with the session authentication logic

~~~ haskell
data User = User String
~~~

## Backends

Choose your backend among the provided ones:

- **InMemory Map** :warning: sessions will be lost when server process quit, use this for testing / development only :warning:
- **Filesystem** - [sessionula-file](https://github.com/aveltras/sessionula/tree/master/backend/sessionula-file)
- **PostgreSQL** via [hasql](https://hackage.haskell.org/package/hasql) - [sessionula-hasql](https://github.com/aveltras/sessionula/tree/master/backend/sessionula-hasql)
- **Redis** via [hedis](https://hackage.haskell.org/package/hedis) - [sessionula-hedis](https://github.com/aveltras/sessionula/tree/master/backend/sessionula-hedis)

~~~ haskell
initManager :: Session.Config -> IO Session.Manager
initManager config = 
  Session.setup config =<< mapStorage
  -- Session.setup config =<< fileStorage "/var/sessions"
  -- Session.setup config =<< hasqlStorage "postgres://postgres@localhost:5432/sessionula"
  -- Session.setup config =<< hedisStorage Hedis.defaultConnectInfo "sessions:" (Session.cfgTokenTTL config)
~~~

## Frontends

You can then use this with one of the provided frontends

### WAI
[sessionula-wai](https://github.com/aveltras/sessionula/tree/master/frontend/sessionula-wai)

~~~ haskell
waiApp :: Application
waiApp request respond = do
  Session.lookup @Bool handle >>= \case
    Nothing  -> Session.set True handle
    Just _ -> void $ Session.modify not handle
  boolSession <- Session.lookup @Bool handle
  respond $ responseLBS ok200 [(hContentType, "text/plain")] $ pack $ show boolSession
  where
    handle = extractSession request

mainWithWai :: IO ()
mainWithWai = do
  manager <- initManager Session.defaultConfig
  run 8080 $ middleware manager defaultSessionCookie { setCookieSecure = False } defaultCsrfSettings waiApp
~~~

### Servant

[sessionula-servant](https://github.com/aveltras/sessionula/tree/master/frontend/sessionula-servant)  
[sessionula-servant-server](https://github.com/aveltras/sessionula/tree/master/frontend/sessionula-servant-server)

~~~ haskell
type API = Session User :> Get '[PlainText] String

servantApp :: Application
servantApp = serve (Proxy @API) server

server :: Server API
server = handler
  where
    handler :: Session.Handle User -> Handler String
    handler handle = do
      Session.lookup @Bool handle >>= \case
        Nothing  -> Session.set True handle
        Just _ -> void $ Session.modify not handle
      boolSession <- Session.lookup @Bool handle
      pure $ show boolSession

mainWithServant :: IO ()
mainWithServant = do
  manager <- initManager Session.defaultConfig
  run 8080 $ middleware manager defaultSessionCookie { setCookieSecure = False } defaultCsrfSettings servantApp
~~~

## Todos
- [ ] More comprehensive test suite
- [ ] Benchmarking

## See also

- [wai-session](https://hackage.haskell.org/package/wai-session)
- [serversession](https://hackage.haskell.org/package/serversession)

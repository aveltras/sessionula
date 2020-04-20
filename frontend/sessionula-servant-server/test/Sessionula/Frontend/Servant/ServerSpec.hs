{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeOperators     #-}

module Sessionula.Frontend.Servant.ServerSpec where

import           Control.Lens
import           Network.Wai                        (Application)
import           Network.Wai.Handler.Warp           (testWithApplication)
import           Network.Wreq                       (cookies, defaults, get,
                                                     getWith, responseBody,
                                                     responseCookieJar)
import           Servant
import qualified Sessionula                         as Session
import           Sessionula.Backend.Map             (mapStorage)
import           Sessionula.Frontend.Servant.Server (Session)
import           Sessionula.Frontend.Wai            (defaultCsrfSettings,
                                                     defaultSessionCookie,
                                                     middleware,
                                                     setCookieSecure)
import           Test.Hspec

spec :: Spec
spec = do

  let config = Session.defaultConfig
      cookieSettings = defaultSessionCookie { setCookieSecure = False }

  manager <- runIO $ Session.setup config =<< mapStorage

  describe "test" $ do

    around (testWithApplication . pure $ middleware manager cookieSettings defaultCsrfSettings app) $ do

      it "works" $ \port -> do

        response <- get (url port)

        let (cookieJar:_) = response ^.. responseCookieJar
            opts = defaults & cookies .~ Just cookieJar --  & header (mk $ setCookieName defaultSessionCookie) .~ [cookie_value cookie]

        response' <- getWith opts (url port)

        response' ^. responseBody `shouldBe` "1"

url :: Int -> String
url port = "http://localhost:" <> show port

data User = User String

type API = Session User :> Get '[PlainText] String

app :: Application
app = serveWithContext (Proxy @API) (() :. EmptyContext) server

server :: Server API
server = handler
  where
    handler :: Session.Handle User -> Handler String
    handler handle = do
      Session.lookup @Int handle >>= \case
        Nothing  -> Session.set @Int 1 handle >> pure ""
        Just int -> pure $ show int

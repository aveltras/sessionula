{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module Sessionula.Frontend.WaiSpec where

import           Control.Lens
import           Data.ByteString.Lazy.Char8 (pack)
import           Network.HTTP.Types         (hContentType, ok200)
import           Network.Wai                (Application, responseLBS)
import           Network.Wai.Handler.Warp   (testWithApplication)
import           Network.Wreq               (cookies, defaults, get, getWith,
                                             responseBody, responseCookieJar)
import qualified Sessionula                 as Session
import           Sessionula.Backend.Map     (mapStorage)
import           Sessionula.Frontend.Wai    (defaultCsrfSettings,
                                             defaultSessionCookie,
                                             extractSession, middleware,
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

app :: Application
app request respond = do
  val <- Session.lookup @Int handle >>= \case
    Nothing  -> Session.set @Int 1 handle >> pure ""
    Just int -> pure $ show int
  respond $ responseLBS ok200 [(hContentType, "text/plain")] $ pack val
  where
    handle = extractSession request

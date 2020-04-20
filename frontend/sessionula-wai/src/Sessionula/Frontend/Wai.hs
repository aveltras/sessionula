{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Sessionula.Frontend.Wai
 ( defaultCsrfSettings
 , defaultSessionCookie
 , extractSession
 , middleware
 , module Web.Cookie
 ) where

import           Data.ByteString.Builder    (toLazyByteString)
import           Data.ByteString.Lazy.Char8 (toStrict)
import           Data.IORef                 (readIORef)
import qualified Data.Vault.Lazy            as V
import           Network.HTTP.Types         (Method, forbidden403, methodGet)
import           Network.HTTP.Types         (HeaderName)
import           Network.HTTP.Types.Header  (hCookie, hSetCookie)
import           Network.Wai                (Middleware, Request, Response,
                                             mapResponseHeaders, requestHeaders,
                                             requestMethod, responseLBS, vault)
import qualified Sessionula                 as Session
import           System.IO.Unsafe           (unsafePerformIO)
import           Web.Cookie                 (SetCookie (..), defaultSetCookie,
                                             parseCookies, renderSetCookie,
                                             sameSiteLax)

sessionKey :: V.Key (Session.Handle auth)
sessionKey = unsafePerformIO V.newKey
{-# NOINLINE sessionKey #-}

extractSession :: Request -> Session.Handle auth
extractSession request =
  case V.lookup sessionKey (vault request) of
    Just handle -> handle
    Nothing     -> error "Session handle could not be found in the request vault."

middleware :: Session.Manager -> SetCookie -> CsrfSettings -> Middleware
middleware (Session.Manager manager) cookieSettings csrfSettings app request respond = do
  print $ requestHeaders request
  fmap snd $ manager maybeToken $ \sessionHandle -> do
    checkCsrf csrfSettings request sessionHandle >>= \case
      False -> respond $ csrfErrorResponse csrfSettings $ request
      True  -> app request { vault = V.insert sessionKey sessionHandle (vault request) } $ \response -> do
        Session.Token token <- readIORef $ Session.handleToken sessionHandle
        respond $ mapResponseHeaders ((:) (hSetCookie, buildCookie token)) response
  where
    cookies = maybe [] id $ parseCookies <$> lookup hCookie (requestHeaders request)
    maybeToken = Session.Token <$> lookup (setCookieName cookieSettings) cookies
    buildCookie token = toStrict . toLazyByteString . renderSetCookie $ cookieSettings { setCookieValue = token }

checkCsrf :: CsrfSettings -> Request -> Session.Handle auth -> IO Bool
checkCsrf CsrfSettings {..} request handle = do
  case elem (requestMethod request) csrfExcludedMethods of
    True  -> pure True
    False -> case reqCsrf of
      Nothing    -> pure False
      Just token -> Session.matchCsrfToken token handle
 where
  headers = requestHeaders request
  reqCsrf = lookup csrfHeaderName headers

defaultSessionCookie :: SetCookie
defaultSessionCookie = defaultSetCookie
  { setCookieName     = "SESSIONID"
  , setCookiePath     = Just "/"
  , setCookieHttpOnly = True
  , setCookieSameSite = Just sameSiteLax
  , setCookieSecure   = True
  }

data CsrfSettings = CsrfSettings
  { csrfExcludedMethods :: [Method]
  , csrfHeaderName      :: HeaderName
  , csrfErrorResponse   :: Request -> Response
  }

defaultCsrfSettings :: CsrfSettings
defaultCsrfSettings = CsrfSettings
  { csrfExcludedMethods = [methodGet]
  , csrfHeaderName      = "X-XSRF-TOKEN"
  , csrfErrorResponse   = const $ responseLBS forbidden403 [] "CSRF protection"
  }

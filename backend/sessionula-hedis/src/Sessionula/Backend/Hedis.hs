{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Sessionula.Backend.Hedis
 ( hedisStorage
 ) where

import           Control.Exception      (throw)
import           Control.Monad          (void)
import           Control.Monad.IO.Class (MonadIO (liftIO))
import           Data.ByteString        (ByteString)
import           Data.ByteString.Char8  (unpack)
import           Data.ByteString.Char8  (pack)
import           Data.Time              (NominalDiffTime, defaultTimeLocale,
                                         formatTime, parseTimeM)
import           Database.Redis         (ConnectInfo, Connection, Redis, Reply,
                                         checkedConnect, del, exists, expire,
                                         hgetall, hmset, runRedis)
import           Sessionula             (Storage (..), StorageException (..),
                                         Token (..))

data HedisStorage = HedisStorage
  { hedisConn       :: Connection
  , hedisPrefix     :: ByteString
  , hedisSessionTTL :: NominalDiffTime
  }

hedisStorage :: ConnectInfo -> ByteString -> NominalDiffTime -> IO HedisStorage
hedisStorage redisConnInfo keyPrefix tokenTTL = do
  redisConn <- checkedConnect redisConnInfo
  pure $ HedisStorage redisConn keyPrefix tokenTTL

runQuery :: Redis (Either Reply a) -> Redis a
runQuery = (=<<) (either (liftIO . throw . StorageException . show) pure)

runQuery' :: Redis (Either Reply a) -> Redis ()
runQuery' = (=<<) (either (liftIO . throw . StorageException . show) pure . void)

instance Storage HedisStorage where

  fetchSession HedisStorage {..} (Token token) = do
    Right l <- runRedis hedisConn $ hgetall (hedisPrefix <> token)
    pure $ do
      sessionData <- lookup "data" l
      issuedAt <- parseTimeM False defaultTimeLocale "%s" . unpack =<< lookup "issued_at" l
      accessedAt <- parseTimeM False defaultTimeLocale "%s" . unpack  =<< lookup "accessed_at" l
      pure (sessionData, issuedAt, accessedAt)

  persistSession HedisStorage {..} mToken (Token token) sessionData currentTime = do
    let timeStamp = pack $ formatTime defaultTimeLocale "%s" currentTime
    void $ runRedis hedisConn $ do
      case mToken of
        Nothing        -> pure ()
        Just (Token t) -> void $ del [hedisPrefix <> t]
      let hash = [ ("accessed_at", timeStamp), ("data", sessionData) ]
      runQuery (exists sessionKey) >>= \case
        True -> runQuery' $ hmset sessionKey hash
        False -> runQuery' $ hmset sessionKey $ ("issued_at", timeStamp) : hash
      void $ expire sessionKey $ floor hedisSessionTTL
    where sessionKey = hedisPrefix <> token

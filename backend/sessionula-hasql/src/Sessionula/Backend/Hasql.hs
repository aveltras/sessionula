{-# LANGUAGE LambdaCase  #-}
{-# LANGUAGE QuasiQuotes #-}

module Sessionula.Backend.Hasql
  ( hasqlStorage
  ) where

import           Control.Exception  (throw)
import           Control.Monad      (void, when)
import           Data.Text.Encoding (decodeUtf8)
import qualified Hasql.Connection   as Hasql
import qualified Hasql.Session      as Hasql
import qualified Hasql.Statement    as Hasql
import           Hasql.TH
import           Sessionula         (Storage (..),
                                     StorageException (StorageException),
                                     Token (..))

newtype HasqlStorage = HasqlStorage Hasql.Connection

hasqlStorage :: Hasql.Settings -> IO HasqlStorage
hasqlStorage settings = do
  Right conn <- Hasql.acquire settings
  flip when (throw $ StorageException "Invalid session database table")
    =<< (runQuery conn () $ [singletonStatement|
                         SELECT (COUNT(*) <> 4) :: bool
                         FROM information_schema.columns
                         WHERE table_name = 'sessionula'
                         AND (
                             (column_name = 'session_id' AND data_type = 'text' AND is_nullable = 'NO') OR
                             (column_name = 'session_data' AND data_type = 'bytea' AND is_nullable = 'NO') OR
                             (column_name = 'created_at' AND data_type = 'timestamp with time zone' AND is_nullable = 'YES') OR
                             (column_name = 'accessed_at' AND data_type = 'timestamp with time zone' AND is_nullable = 'YES')
                         )
                         |]
        )
  pure $ HasqlStorage conn

runQuery :: Hasql.Connection -> a -> Hasql.Statement a b -> IO b
runQuery conn params st = Hasql.run (Hasql.statement params st) conn >>= \case
  Right res                        -> pure res
  Left  err -> throw $ StorageException $ show err

runQuery' :: Hasql.Connection -> a -> Hasql.Statement a b -> IO ()
runQuery' conn params st = void $ runQuery conn params st

instance Storage HasqlStorage where

  fetchSession (HasqlStorage conn) (Token token) =
    runQuery conn (decodeUtf8 token)
    [maybeStatement| UPDATE sessionula
                     SET accessed_at = NOW()
                     WHERE session_id = $1 :: text
                     RETURNING session_data :: bytea,
                       created_at :: timestamptz,
                       accessed_at :: timestamptz |]

  persistSession (HasqlStorage conn) mToken (Token token) sessionData currentTime = do
    case mToken of
        Nothing ->
          runQuery' conn (decodeUtf8 token, sessionData, currentTime, currentTime)
          [resultlessStatement| INSERT INTO sessionula (session_id, session_data, created_at, accessed_at)
                                VALUES ($1 :: text, $2 :: bytea, $3 :: timestamptz, $4 :: timestamptz)
                                ON CONFLICT (session_id) DO UPDATE SET
                                  session_data = excluded.session_data,
                                  accessed_at = excluded.accessed_at |]
        Just (Token t) ->
          runQuery' conn (decodeUtf8 t, decodeUtf8 token, sessionData, currentTime, currentTime)
          [resultlessStatement| UPDATE sessionula
                                SET session_data = $3 :: bytea,
                                    created_at = $4 :: timestamptz,
                                    accessed_at = $5 :: timestamptz,
                                    session_id = $2 :: text
                                WHERE session_id = $1 :: text |]

  gcSessions (HasqlStorage conn) limitTimeStamp =
    runQuery' conn limitTimeStamp
    [resultlessStatement| DELETE FROM sessionula WHERE accessed_at < $1 :: timestamptz |]

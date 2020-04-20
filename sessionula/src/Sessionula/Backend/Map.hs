{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies  #-}

module Sessionula.Backend.Map where

import           Data.IORef      (IORef, atomicModifyIORef', newIORef,
                                  readIORef)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Set        as Set
import           Data.Time       (UTCTime)
import           Sessionula

newtype MapStorage
  = MapStorage { unMapStorage :: IORef (Map Token (Session, UTCTime, UTCTime)) }

mapStorage :: IO MapStorage
mapStorage = newIORef Map.empty >>= pure . MapStorage

instance Storage MapStorage where
  type Encoding MapStorage = Session
  toEncoding = id
  fromEncoding = Just . id

  fetchSession (MapStorage ioRef) token = Map.lookup token <$> readIORef ioRef

  persistSession (MapStorage ioRef) mOldToken token session currentTime = do
    atomicModifyIORef' ioRef $ \sessions -> (, ()) $
      Map.insertWith f token (session, currentTime, currentTime) $
        case mOldToken of
          Nothing -> sessions
          Just t  -> Map.delete t sessions
    where
      f (newSession, _, accessedAt) (_, issuedAt, _) = (newSession, issuedAt, accessedAt)

  gcSessions (MapStorage ioRef) limitTimeStamp = do
    expiredTokens <- Map.foldrWithKey f Set.empty <$> readIORef ioRef
    atomicModifyIORef' ioRef $ \sessions -> (Map.withoutKeys sessions expiredTokens, ())
    where
      f token (_, issuedAt, _) acc =
        if issuedAt < limitTimeStamp
        then Set.insert token acc
        else acc

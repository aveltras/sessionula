{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Sessionula.Backend.File
  ( fileStorage
  ) where

import           Control.Exception.Safe (tryAny)
import           Control.Monad          (void, when)
import qualified Data.ByteString        as BS
import           Data.ByteString.Char8  (unpack)
import           Data.Foldable          (for_)
import           Sessionula             (Storage (..), Token (..))
import           System.Directory       (createDirectoryIfMissing,
                                         doesFileExist, getAccessTime,
                                         getModificationTime, listDirectory,
                                         removeFile, setModificationTime)
import           System.FilePath        ((</>))

newtype FileStorage =
  FileStorage { storageSessionsDir :: FilePath }

fileStorage :: FilePath -> IO FileStorage
fileStorage sessionsDir = do
  createDirectoryIfMissing True sessionsPath
  pure $ FileStorage sessionsPath
  where sessionsPath = sessionsDir </> "data"

instance Storage FileStorage where
  fetchSession FileStorage {..} (Token token) = do
    let sessionFile = storageSessionsDir </> unpack token
    doesFileExist sessionFile >>= \case
      False -> pure Nothing
      True  -> do
        sessionData <- BS.readFile sessionFile
        issuedAt   <- getModificationTime sessionFile
        accessedAt <- getAccessTime sessionFile
        pure $ Just (sessionData, issuedAt, accessedAt)

  persistSession FileStorage {..} mToken (Token token) session _ = do
    case mToken of
      Nothing -> pure ()
      Just (Token t) -> void $ tryAny $ removeFile $ storageSessionsDir </> unpack t
    doesFileExist sessionFile >>= \case
      False -> BS.writeFile sessionFile session
      True  -> do
        issuedAt <- getModificationTime sessionFile
        BS.writeFile sessionFile session
        setModificationTime sessionFile issuedAt
    where sessionFile = storageSessionsDir </> unpack token

  gcSessions FileStorage {..} limitTimeStamp = do
    sessionFiles <- listDirectory storageSessionsDir
    for_ sessionFiles $ \f -> tryAny $ do
      let file = storageSessionsDir </> f
      accessedAt <- getAccessTime file
      when (accessedAt < limitTimeStamp) $ do
        void $ tryAny $ removeFile file
    pure ()

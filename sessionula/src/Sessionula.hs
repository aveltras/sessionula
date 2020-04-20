{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE DefaultSignatures          #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}

module Sessionula
  ( defaultConfig
  , Storage (..)
  , Session
  , Handle (..)
  , Config (..)
  , Token (..)
  , Manager (..)
  , StorageException (..)
  , setup
  , lookup
  , lookup'
  , set
  , set'
  , modify
  , modify'
  , remove
  , remove'
  , has
  , has'
  , authStatus
  , authenticate
  , logout
  , lookupCsrfToken
  , matchCsrfToken
  ) where

import           Control.Concurrent      (forkIO, threadDelay)
import           Control.Concurrent.STM  (TVar, atomically, modifyTVar,
                                          newTVarIO, readTVar, readTVarIO,
                                          writeTVar)
import           Control.Exception.Safe  (Exception, MonadMask, bracket_)
import           Control.Monad           (forever, guard, void)
import           Control.Monad.IO.Class  (MonadIO, liftIO)
import           Crypto.Random           (MonadRandom (..))
import           Data.ByteArray.Encoding (Base (Base64URLUnpadded),
                                          convertToBase)
import           Data.ByteString         (ByteString)
import           Data.ByteString.Char8   (pack)
import           Data.IORef              (IORef, modifyIORef, newIORef,
                                          readIORef, writeIORef)
import           Data.Map                (Map)
import qualified Data.Map.Strict         as Map
import           Data.Serialize          (Serialize (..), decode, encode)
import qualified Data.Set                as Set
import           Data.Time               (NominalDiffTime, UTCTime, addUTCTime,
                                          getCurrentTime)
import           GHC.Generics            (Generic)
import           Prelude                 hiding (lookup)
import           Type.Reflection         ((:~~:) (HRefl), Typeable, eqTypeRep,
                                          tyConModule, tyConName, typeOf,
                                          typeRep, typeRepTyCon)

newtype Token = Token ByteString
  deriving (Eq, Ord, Show, Generic)
  deriving newtype (Serialize)

data Config = Config
  { cfgTokenTTL          :: NominalDiffTime
  , cfgTokenRefreshDelay :: NominalDiffTime
  , cfgGcRefreshedTokens :: Int
  , cfgGcStorageInterval :: Int
  }

defaultConfig :: Config
defaultConfig = Config
  { cfgTokenTTL          = 1800
  , cfgTokenRefreshDelay = 120
  , cfgGcRefreshedTokens = 120
  , cfgGcStorageInterval = 300
  }


data Handle auth = Handle
  { handleData  :: IORef Session
  , handleToken :: IORef Token
  }

data SessionValue
  = forall a. (Serialize a, Typeable a) => Serializable a
  | Serialized ByteString

instance Serialize SessionValue where
  get = Serialized <$> get
  put = \case
    Serializable a  -> put . encode $ a
    Serialized   bs -> put bs

newtype Session = Session { unSession :: Map ByteString SessionValue }
  deriving (Generic)
  deriving newtype (Monoid, Semigroup, Serialize)

class Storage storage where

  type Encoding storage :: *
  type Encoding storage = ByteString

  toEncoding :: Session -> Encoding storage
  default toEncoding :: (Encoding storage ~ ByteString) => Session -> Encoding storage
  toEncoding = encode

  fromEncoding :: Encoding storage -> Maybe Session
  default fromEncoding :: (Encoding storage ~ ByteString) => Encoding storage -> Maybe Session
  fromEncoding x = case decode x of
    Left _  -> Nothing
    Right a -> Just a

  fetchSession :: storage -> Token -> IO (Maybe (Encoding storage, UTCTime, UTCTime))
  persistSession :: storage -> Maybe Token -> Token -> Encoding storage -> UTCTime -> IO ()
  gcSessions :: storage -> UTCTime -> IO ()
  gcSessions _ _ = pure ()

newtype StorageException = StorageException String
  deriving anyclass (Exception)
  deriving (Show)

randomByteString :: (MonadRandom m) => m ByteString
randomByteString = getRandomBytes @_ @ByteString 20 >>= pure . convertToBase Base64URLUnpadded

newToken :: (MonadRandom m) => m Token
newToken = Token <$> randomByteString

newtype Manager = Manager (forall a auth. Maybe Token -> (Handle auth -> IO a) -> IO (Token, a))

setup
  :: forall storage m
   . (MonadIO m, Storage storage)
  => Config
  -> storage
  -> m Manager
setup sessionConfig@Config {..} storage = do
  locks           <- liftIO $ newTVarIO Set.empty
  refreshedTokens <- liftIO $ newTVarIO (Map.empty @Token)
  liftIO $ void $ forkIO $ forever $ do
    threadDelay $ cfgGcStorageInterval * 1000000
    currentTime <- getCurrentTime
    gcSessions storage $ addUTCTime (-cfgTokenTTL) currentTime
  liftIO $ void $ forkIO $ forever $ do
    threadDelay $ cfgGcRefreshedTokens * 1000000
    tokensMap <- readTVarIO refreshedTokens
    now       <- getCurrentTime
    let expiredTokens = Map.keysSet
          $ Map.filterWithKey (\_ (_, expiresAt) -> now > expiresAt) tokensMap
    atomically $ modifyTVar refreshedTokens $ flip Map.withoutKeys expiredTokens
  pure $ Manager $ \mToken f -> withLock locks mToken
    $ withSession storage refreshedTokens sessionConfig mToken f

withLock
  :: forall a m
   . (MonadIO m, MonadMask m)
  => TVar (Set.Set Token)
  -> Maybe Token
  -> m a
  -> m a
withLock locks mToken f = case mToken of
  Nothing    -> f
  Just token -> do
    res <- bracket_ takeToken dropToken f
    pure res
   where
    takeToken = liftIO $ atomically $ do
      s <- readTVar locks
      guard $ not $ Set.member token s
      writeTVar locks $ Set.insert token s
    dropToken = liftIO $ atomically $ modifyTVar locks (Set.delete token)

withSession
  :: forall auth store a m
   . (MonadIO m, MonadRandom m, Storage store)
  => store
  -> TVar (Map Token (Token, UTCTime))
  -> Config
  -> Maybe Token
  -> (Handle auth -> m a)
  -> m (Token, a)
withSession store refreshedTokens config@Config {..} mToken fn = do
  (token, session, refreshed) <- case mToken of
    Nothing -> (, mempty, True) <$> newToken
    Just t  -> liftIO $ lookupToken' store config refreshedTokens t
  sessionIORef <- liftIO $ newIORef session
  tokenIORef   <- liftIO $ newIORef token
  let handle = Handle sessionIORef tokenIORef
  res         <- fn handle
  session'    <- liftIO $ readIORef sessionIORef
  token'      <- liftIO $ readIORef tokenIORef
  currentTime <- liftIO $ getCurrentTime
  liftIO $ persistSession store (if refreshed then mToken else Nothing) token' (toEncoding @store session') currentTime
  pure (token', res)

data TokenCheck
  = TokenIsValid
  | TokenNeedsRenewal
  | TokenExpired

decodeFirst :: forall storage. Storage storage => Maybe (Encoding storage, UTCTime, UTCTime) -> Maybe (Session, UTCTime, UTCTime)
decodeFirst Nothing   = Nothing
decodeFirst (Just (a,b,c)) =
  case fromEncoding @storage a of
    Nothing      -> Nothing
    Just session -> Just (session,b,c)

lookupToken'
  :: forall store m
   . (MonadIO m, Storage store)
  => store
  -> Config
  -> TVar (Map Token (Token, UTCTime))
  -> Token
  -> m (Token, Session, Bool)
lookupToken' store config@Config {..} refreshedTokens token = do
  liftIO $ (decodeFirst @store <$> fetchSession store token) >>= \case
    Nothing -> do
      mToken <- atomically $ Map.lookup token <$> readTVar refreshedTokens
      case mToken of
        Nothing             -> (, mempty, True) <$> newToken
        Just (t, expiresAt) -> do
          currentTime <- getCurrentTime
          if currentTime < expiresAt
            then lookupToken' store config refreshedTokens t
            else (, mempty, True) <$> newToken
    Just (session, issuedAt, accessedAt) -> do
      currentTime <- getCurrentTime
      case checkTTL config currentTime issuedAt accessedAt of
        TokenExpired      -> (, mempty, True) <$> newToken
        TokenIsValid      -> pure (token, session, False)
        TokenNeedsRenewal -> do
          token' <- newToken
          atomically $ modifyTVar refreshedTokens $ Map.insert
            token
            (token', addUTCTime cfgTokenTTL issuedAt)
          pure (token', session, True)

checkTTL :: Config -> UTCTime -> UTCTime -> UTCTime -> TokenCheck
checkTTL Config {..} currentTime issuedAt accessedAt
  | addUTCTime cfgTokenTTL accessedAt < currentTime = TokenExpired
  | addUTCTime (cfgTokenTTL - cfgTokenRefreshDelay) issuedAt < currentTime = TokenNeedsRenewal
  | otherwise = TokenIsValid

computeKey :: forall a . Typeable a => ByteString
computeKey = pack $ tyConModule tc <> tyConName tc
  where tc = typeRepTyCon $ typeRep @a

lookup :: forall a auth m. (MonadIO m, Serialize a, Typeable a) => Handle auth -> m (Maybe a)
lookup = lookup' $ computeKey @a

set :: forall a auth m. (MonadIO m, Serialize a, Typeable a) => a -> Handle auth -> m ()
set = set' $ computeKey @a

modify :: forall a auth m. (MonadIO m, Serialize a, Typeable a) => (a -> a) -> Handle auth -> m Bool
modify = modify' $ computeKey @a

remove :: forall a auth m . (MonadIO m, Typeable a) => Handle auth -> m ()
remove = remove' $ computeKey @a

has :: forall a auth m . (MonadIO m, Typeable a) => Handle auth -> m Bool
has = has' $ computeKey @a

lookup' :: forall a auth m. (MonadIO m, Serialize a, Typeable a) => ByteString -> Handle auth -> m (Maybe a)
lookup' key Handle {..} =
  liftIO $ readIORef handleData >>= \(Session sessionData) -> do
    pure $ Map.lookup key sessionData >>= \case
      Serializable val -> case typeRep @a `eqTypeRep` (typeOf val) of
        Just HRefl -> Just val
        Nothing    -> Nothing
      Serialized bs -> case decode bs of
        Right val -> Just val
        Left  _   -> Nothing

set' :: (MonadIO m, Serialize a, Typeable a) => ByteString -> a -> Handle auth -> m ()
set' key val Handle {..} =
  liftIO $ modifyIORef handleData $ \(Session sessionData) ->
    Session $ Map.insert key (Serializable val) sessionData

modify' :: (MonadIO m, Serialize a, Typeable a) => ByteString -> (a -> a) -> Handle auth -> m Bool
modify' key fn handle = lookup' key handle >>= \case
  Nothing  -> pure False
  Just val -> set' key (fn val) handle >> pure True

remove' :: MonadIO m => ByteString -> Handle auth -> m ()
remove' key Handle {..} =
  liftIO $ modifyIORef handleData $ \(Session sessionData) ->
    Session $ Map.delete key sessionData

has' :: MonadIO m => ByteString -> Handle auth -> m Bool
has' key Handle {..} =
  liftIO $ readIORef handleData >>= \(Session sessionData) ->
    pure $ Map.member key sessionData

-- Authentication

authStatus :: forall auth m. (MonadIO m, Serialize auth, Typeable auth) => Handle auth -> m (Maybe auth)
authStatus = lookup @auth

authenticate :: (MonadIO m, Serialize auth, Typeable auth) => auth -> Handle auth -> m ()
authenticate auth handle@Handle {..} =
  liftIO $ set auth handle >> newToken >>= writeIORef handleToken

logout :: forall auth m . (MonadIO m, Typeable auth) => Handle auth -> m ()
logout handle@Handle {..} =
  liftIO $ remove @auth handle >> newToken >>= writeIORef handleToken

-- CSRF

lookupCsrfToken :: MonadIO m => Handle auth -> m ByteString
lookupCsrfToken handle@Handle {..} = liftIO $ lookup' "__csrf" handle >>= \case
  Just token -> pure token
  Nothing    -> do
    csrfToken <- randomByteString
    set' "__csrf" csrfToken handle
    pure csrfToken

matchCsrfToken :: MonadIO m => ByteString -> Handle auth -> m Bool
matchCsrfToken token handle@Handle {..} =
  lookupCsrfToken handle >>= pure . (==) token

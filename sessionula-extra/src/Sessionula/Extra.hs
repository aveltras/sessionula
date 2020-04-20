{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}

module Sessionula.Extra
  ( HasSession(..)
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

import           Control.Monad.IO.Class (MonadIO)
import           Control.Monad.Reader   (MonadReader, asks)
import           Data.ByteString        (ByteString)
import           Data.Serialize         (Serialize)
import           Lens.Micro             (Lens')
import           Lens.Micro.Extras      (view)
import           Prelude                hiding (lookup)
import qualified Sessionula             as S
import           Type.Reflection        (Typeable)

class (Serialize (Auth env), Typeable (Auth env)) => HasSession env where
  type Auth env :: *
  sessionL :: Lens' env (S.Handle (Auth env))

withHandle :: (HasSession env, MonadIO m, MonadReader env m) => (S.Handle (Auth env) -> m a) -> m a
withHandle = (>>=) (asks $ view sessionL)

lookup :: (Serialize a, Typeable a, HasSession env, MonadIO m, MonadReader env m) => m (Maybe a)
lookup = withHandle $ S.lookup

lookup' :: (Serialize a, Typeable a, HasSession env, MonadIO m, MonadReader env m) => ByteString -> m (Maybe a)
lookup' = withHandle . S.lookup'

set :: (Serialize a, Typeable a, HasSession env, MonadIO m, MonadReader env m) => a -> m ()
set = withHandle . S.set

set' :: (Serialize a, Typeable a, HasSession env, MonadIO m, MonadReader env m) => ByteString -> a -> m ()
set' key = withHandle . S.set' key

remove :: forall a env m. (Typeable a, HasSession env, MonadIO m, MonadReader env m) => m ()
remove = withHandle $ S.remove @a

remove' :: (HasSession env, MonadIO m, MonadReader env m) => ByteString -> m ()
remove' = withHandle . S.remove'

has :: forall a env m. (HasSession env, MonadIO m, MonadReader env m, Typeable a) => m Bool
has = withHandle $ S.has @a

has' :: (HasSession env, MonadIO m, MonadReader env m) => ByteString -> m Bool
has' = withHandle . S.has'

modify :: (Serialize a, Typeable a, HasSession env, MonadIO m, MonadReader env m) => (a -> a) -> m Bool
modify = withHandle . S.modify

modify' :: (Serialize a, Typeable a, HasSession env, MonadIO m, MonadReader env m) => ByteString -> (a -> a) -> m Bool
modify' key = withHandle . S.modify' key

authStatus :: (HasSession env, MonadIO m, MonadReader env m) => m (Maybe (Auth env))
authStatus = withHandle $ S.authStatus

authenticate :: (HasSession env, MonadIO m, MonadReader env m) => Auth env -> m ()
authenticate = withHandle . S.authenticate

logout :: forall env m. (HasSession env, MonadIO m, MonadReader env m) => m ()
logout = withHandle $ S.logout @(Auth env)

lookupCsrfToken :: (HasSession env, MonadIO m, MonadReader env m) => m ByteString
lookupCsrfToken = withHandle $ S.lookupCsrfToken

matchCsrfToken :: (HasSession env, MonadIO m, MonadReader env m) => ByteString -> m Bool
matchCsrfToken = withHandle . S.matchCsrfToken


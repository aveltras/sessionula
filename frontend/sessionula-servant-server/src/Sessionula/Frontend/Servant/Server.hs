{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# OPTIONS_GHC -fno-warn-orphans  #-}

module Sessionula.Frontend.Servant.Server
  ( module Sessionula.Frontend.Servant
  , module Sessionula.Frontend.Wai
  ) where

import           Servant                     ((:>), HasServer (..), Proxy (..))
import           Servant.Server.Internal     (passToServer)
import qualified Sessionula                  as Session
import           Sessionula.Frontend.Servant
import           Sessionula.Frontend.Wai

instance (HasServer api ctx) => HasServer (Session auth :> api) ctx where
  type ServerT (Session auth :> api) m = Session.Handle auth -> ServerT api m
  hoistServerWithContext _ pc nt s = hoistServerWithContext (Proxy :: Proxy api) pc nt . s
  route Proxy context subserver = route (Proxy :: Proxy api) context $ passToServer subserver extractSession

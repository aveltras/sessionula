{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

module Sessionula.Frontend.Servant where

import           Data.Proxy  (Proxy (..))
import           Servant.API ((:>), HasLink (..))

data Session auth

instance HasLink sub => HasLink (Session auth :> sub) where
  type MkLink (Session auth :> sub) a = MkLink sub a
  toLink toA _ = toLink toA (Proxy :: Proxy sub)

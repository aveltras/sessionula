{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Sessionula.Backend.HedisSpec where

import           Database.Redis           (defaultConnectInfo)
import           Sessionula               (Config (Config), cfgTokenTTL)
import           Sessionula.Backend.Hedis (hedisStorage)
import qualified Sessionula.Backend.Spec  as Spec
import           Test.Hspec               (Spec)

spec :: Spec
spec = Spec.spec $ \Config {..} -> hedisStorage defaultConnectInfo "sessions:" cfgTokenTTL

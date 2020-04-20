module Sessionula.Backend.HasqlSpec where

import           Data.ByteString.Char8    (pack)
import           Sessionula.Backend.Hasql (hasqlStorage)
import qualified Sessionula.Backend.Spec  as Spec
import           System.Environment       (getEnv)
import           Test.Hspec               (Spec)

spec :: Spec
spec = Spec.spec $ const $ hasqlStorage =<< pack <$> getEnv "DATABASE_URL"

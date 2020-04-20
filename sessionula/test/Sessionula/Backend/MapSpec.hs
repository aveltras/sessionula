module Sessionula.Backend.MapSpec where

import           Sessionula.Backend.Map  (mapStorage)
import qualified Sessionula.Backend.Spec as Spec
import           Test.Hspec              (Spec)

spec :: Spec
spec = Spec.spec $ const mapStorage

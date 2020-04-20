module Sessionula.Backend.FileSpec where

import           Sessionula.Backend.File (fileStorage)
import qualified Sessionula.Backend.Spec as Spec
import           System.Directory        (getTemporaryDirectory)
import           System.FilePath         ((</>))
import           Test.Hspec              (Spec)

spec :: Spec
spec = Spec.spec $ const $ fileStorage =<< (</>) "sessions" <$> getTemporaryDirectory

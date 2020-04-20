{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Sessionula.Backend.Spec where

import           Control.Concurrent       (threadDelay)
import           Control.Concurrent.Async (replicateConcurrently,
                                           replicateConcurrently_)
import           Control.Monad            (void)
import qualified Sessionula               as Session
import           Test.Hspec

spec :: Session.Storage storage => (Session.Config -> IO storage) -> Spec
spec setupStorage = do

  describe "Test suite" $ do

    let config = Session.defaultConfig
    Session.Manager manager <- runIO $ Session.setup config =<< setupStorage config

    (t, _) <- runIO $ manager Nothing (const pure ())
    let action = manager (Just t)

    it "should allow basic operations" $ do

      snd <$> action (Session.has @Int) >>= shouldBe False
      snd <$> action (Session.lookup @Int) >>= shouldBe Nothing

      void $ action (Session.set @Int 5)

      snd <$> action (Session.has @Int) >>= shouldBe True
      snd <$> action (Session.lookup @Int) >>= shouldBe (Just 5)

      void $ action (Session.remove @Int)

      snd <$> action (Session.has @Int) >>= shouldBe False
      snd <$> action (Session.lookup @Int) >>= shouldBe Nothing

    context "when concurrent requests hit with the same token" $ do

      it "should process them sequentially" $ do

        void $ action (Session.set @Int 1)

        replicateConcurrently_ 5 $ action $ \handle -> do
          threadDelay 500000
          Session.modify @Int (+1) handle

        snd <$> action (Session.lookup @Int) >>= shouldBe (Just 6)

      it "should handle token renewal" $ do

        let config' = config { Session.cfgTokenTTL = 10, Session.cfgTokenRefreshDelay = 9 }
        Session.Manager manager' <- Session.setup config' =<< setupStorage config'

        (t', _) <- manager' Nothing $ const pure ()

        threadDelay 1500000

        tokens <- (fmap . fmap) fst $ replicateConcurrently 5 $ (manager' (Just t') $ const pure ())

        (all ((==) (tokens !! 0)) tokens) `shouldBe` True


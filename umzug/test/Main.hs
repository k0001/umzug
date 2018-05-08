{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Control.Exception as Ex
import Control.Monad.IO.Class (liftIO)
import qualified Di
import qualified System.Environment (getEnv)
import qualified Test.Tasty as Tasty
import qualified Test.Tasty.HUnit as Tasty
import qualified Test.Tasty.Runners as Tasty

import qualified Umzug as U
import qualified Umzug.Aeson
import qualified Umzug.InMemory

--------------------------------------------------------------------------------


main :: IO ()
main = Di.new $ \di -> do
  Tasty.defaultMainWithIngredients
    [ Tasty.consoleTestReporter
    , Tasty.listingTests
    ] (tt di)

--------------------------------------------------------------------------------

tt :: Di.Df1 -> Tasty.TestTree
tt di =
  Tasty.testGroup "main"
  [ Tasty.testCase "naive" $ do
      migsDb <- Umzug.InMemory.mkMigsDb
      rds <- Umzug.InMemory.mkUndoDataStore
      let Just t = U.targetForwards
            [ migOne_blank
            , migMany_empty
            , migMany_blank
            -- , migOne_M1_recover
            ]
      mIds <- Di.runDiT di $ do
        U.run migsDb rds t Env
        U.migsDb_ids migsDb
      Tasty.assertEqual ""
        [ "migOne_blank"
        , "migMany_empty"
        , "migMany_blank.blank"
        , "migMany_blank"
        ] mIds
  ]

--------------------------------------------------------------------------------

data Env = Env

migMany_empty :: U.Mig m Env
migMany_empty = U.migMany "migMany_empty" []

migMany_blank :: Applicative m => U.Mig m Env
migMany_blank = U.migMany "migMany_blank"
  [ U.mig "migMany_blank.blank"
      (naiveStep (\Env -> pure ()))
      (naiveStep (\Env -> pure ()))
  ]

migOne_blank :: Applicative m => U.Mig m Env
migOne_blank = U.mig "migOne_blank"
  (naiveStep (\Env -> pure ()))
  (naiveStep (\Env -> pure ()))

-- migOne_M1_recover :: Mig M Env
-- migOne_M1_recover = mig "migOne_M1_recover" "" M1
--   (Step (\Env -> Recon (pure ((), Alter (err "migOne_M1_recover.step.mpos"))))
--         (\Env () -> pure ())
--         (\Env () () -> err "migOne_M1_recover.stepRecover.unreachable")
--         aesonCodec
--         aesonCodec)
--   Nothing

naiveStep
  :: Applicative m
  => (env -> m ())   -- ^ Migrate in direction @d@.
  -> U.Step m d env
naiveStep f = U.Step
  { U.step_recon = \env -> U.Recon (pure ((), U.Alter (f env)))
  , U.step_recover = \_ () -> pure ()
  , U.step_rollback = \_ () () -> pure ()
  , U.step_codecPre = Umzug.Aeson.aesonCodec
  , U.step_codecPos = Umzug.Aeson.aesonCodec
  }


--------------------------------------------------------------------------------

err :: String -> IO a
err = Ex.throwIO . Err

expectErr :: String -> IO () -> IO ()
expectErr s0 = Ex.handle $ \e@(Err s) ->
  Tasty.assertEqual "expectErr" s0 s

data Err = Err String deriving (Show)
instance Ex.Exception Err


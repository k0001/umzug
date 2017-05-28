{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Control.Exception as Ex
import qualified Data.ByteString.Char8 as B8
import qualified Data.Pool as Pool
import qualified Database.PostgreSQL.Simple as Pg
import Di (Di)
import qualified Di
import qualified System.Environment (getEnv)
import qualified Test.Tasty as Tasty
import qualified Test.Tasty.HUnit as Tasty
import qualified Test.Tasty.Runners as Tasty

import Umzug
import qualified Umzug.PostgreSQL as UPg

--------------------------------------------------------------------------------


main :: IO ()
main = Ex.bracket Di.mkDiStringStderr Di.flush $ \di0 -> do
   pool <- do
      cs <- B8.pack <$> System.Environment.getEnv "UMZUG_TEST_PG"
      Pool.createPool (Pg.connectPostgreSQL cs) Pg.close 2 6 4
   Tasty.defaultMainWithIngredients
     [ Tasty.consoleTestReporter
     , Tasty.listingTests
     ] (tt di0 pool)

--------------------------------------------------------------------------------

tt :: Di String String -> Pool.Pool Pg.Connection -> Tasty.TestTree
tt di0 pool =
  Tasty.testGroup "main"
  [ Tasty.testCase "naive" $ do
      Pool.withResource pool $ \c1 -> do
        migsdb <- UPg.mkMigsDb c1
        Pool.withResource pool $ \c2 -> do
           rds <- UPg.mkUndoDataStore di0 c2
           let Just t = Umzug.targetForwards
                 [ migOne_M1_blank
                 , migMany_empty
                 , migMany_blank
                 -- , migOne_M1_recover
                 ]
           -- expectErr "migOne_M1_recover.step.mpos" $ do
           run di0 migsdb rds t srm
           mIds <- migsdbIds migsdb
           Tasty.assertEqual ""
             [ "migOne_M1_blank"
             , "migMany_empty"
             , "migMany_blank.M1_blank"
             , "migMany_blank"
             ] mIds
  ]

--------------------------------------------------------------------------------
data M = M1 | M2

data Env = Env

migMany_empty :: Mig M Env
migMany_empty = migMany "migMany_empty" []

migMany_blank :: Mig M Env
migMany_blank = migMany "migMany_blank"
  [ mig "migMany_blank.M1_blank" M1
      (naiveStep (\Env -> pure ()))
      (naiveStep (\Env -> pure ()))
  ]

migOne_M1_blank :: Mig M Env
migOne_M1_blank = mig "migOne_M1_blank" M1
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

srm :: StepRunner M Env
srm M1 f = f Env
srm M2 f = f Env

--------------------------------------------------------------------------------

err :: String -> IO a
err = Ex.throwIO . Err

expectErr :: String -> IO () -> IO ()
expectErr s0 = Ex.handle $ \e@(Err s) ->
  Tasty.assertEqual "expectErr" s0 s

data Err = Err String deriving (Show)
instance Ex.Exception Err


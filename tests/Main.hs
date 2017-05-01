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
import qualified Test.Tasty as Tasty
import qualified Test.Tasty.HUnit as Tasty
import qualified Test.Tasty.Runners as Tasty

import Data.Umzug.Core as Umzug
import Data.Umzug.InMemory as InMemory

--------------------------------------------------------------------------------

main :: IO ()
main = Tasty.defaultMainWithIngredients
  [ Tasty.consoleTestReporter
  , Tasty.listingTests
  ] tt

--------------------------------------------------------------------------------

tt :: Tasty.TestTree
tt =
  Tasty.testGroup "main"
  [ Tasty.testCase "naive" $ do
      migsdb <- InMemory.mkMigsDb putStrLn
      let Just t = Umzug.targetForwards
            [ migOne_M1_blank
            , migMany_empty
            , migMany_blank
            ]
      run migsdb putStrLn t srm
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
migMany_empty = migMany "migMany_empty" "" []

migMany_blank :: Mig M Env
migMany_blank = migMany "migMany_blank" ""
  [ mig "migMany_blank.M1_blank" "" M1
      (naiveStep (\Env -> pure ()))
      (Just (naiveStep (\Env -> pure ())))
  ]

migOne_M1_blank :: Mig M Env
migOne_M1_blank = mig "migOne_M1_blank" "" M1
  (naiveStep (\Env -> pure ()))
  (Just (naiveStep (\Env -> pure ())))

migOne_M1_recover :: Mig M Env
migOne_M1_recover = mig "migOne_M1_recover" "" M1
  (Step (\Env -> pure (True, err "migOne_M1_recover.step.mpos"))
        (\Env True -> pure ())
        (\Env True () -> err "migOne_M1_recover.stepRecover.unreachable"))
  Nothing

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


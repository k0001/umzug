{-# LANGUAGE BangPatterns #-}

module Umzug.InMemory
  ( mkMigsDb
  , mkUndoDataStore
  ) where

import Control.Concurrent.MVar
  (readMVar, newMVar, takeMVar, modifyMVar_, tryPutMVar)
import Control.Monad (void)
import qualified Data.Map.Strict as Map
import Data.List as List
import Data.Time (UTCTime, getCurrentTime)
import Di (Di, dbg)
import qualified Di
import qualified Pipes
import qualified Pipes.Prelude as Pipes

import Umzug

--------------------------------------------------------------------------------

mkMigsDb :: Di String String -> IO MigsDb
mkMigsDb di0 = do
  let di1 = Di.push di0 ["MigsDb"]
  mvLock <- newMVar ()
  mvMigs <- newMVar ([] :: [(MigId, UTCTime)])
  pure $ MigsDb
    { migsdbAdd = \mId -> do
        dbg di1 ("migsDbAdd " ++ show mId)
        modifyMVar_ mvMigs $ \migs -> do
           ts <- getCurrentTime
           pure ((mId,ts):migs)
    , migsdbDel = \mId -> do
        dbg di1 ("migsDbDel " ++ show mId)
        modifyMVar_ mvMigs $ \migs ->
           pure $ filter ((== mId) . fst) migs
    , migsdbGet = \mId -> do
        dbg di1 ("migsDbGet " ++ show mId)
        migs <- readMVar mvMigs
        pure $! fmap snd (List.find (\(mId',_) -> mId' == mId) migs)
    , migsdbAll = do
        dbg di1 "migsDbAll"
        fmap (List.sortBy (\(_,ts) (_,ts') -> compare ts ts')) (readMVar mvMigs)
    , migsdbLock = do
        dbg di1 "migsDbLock"
        takeMVar mvLock
    , migsdbUnlock = do
        dbg di1 "migsDbUnlock"
        void $ tryPutMVar mvLock ()
    }

--------------------------------------------------------------------------------

mkUndoDataStore :: Di String String -> IO (UndoDataStore IO)
mkUndoDataStore di0 = do
   let di1 = Di.push di0 ["UndoDataStore"]
   mvStore <- newMVar Map.empty
   pure $ UndoDataStore
     { udsPut = \udId pbs -> do
         dbg di1 ("udsPut " ++ show udId)
         !bs <- mconcat <$> Pipes.toListM pbs
         modifyMVar_ mvStore (pure . Map.insert udId bs)
     , udsGet = \udId -> do
         dbg di1 ("udsGet " ++ show udId)
         ybs <- Map.lookup udId <$> readMVar mvStore
         pure (fmap Pipes.yield ybs)
     , udsDelete = \udId -> do
         dbg di1 ("udsDelete " ++ show udId)
         modifyMVar_ mvStore (pure . Map.delete udId)
     }

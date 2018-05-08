{-# LANGUAGE BangPatterns #-}

module Umzug.InMemory
  ( mkMigsDb
  , mkUndoDataStore
  ) where

import Control.Concurrent.MVar
  (readMVar, newMVar, tryTakeMVar, modifyMVar_, tryPutMVar)
import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.Map.Strict as Map
import Data.List as List
import Data.Time (UTCTime, getCurrentTime)
import qualified Pipes
import qualified Pipes.Prelude as Pipes

import qualified Umzug as U

--------------------------------------------------------------------------------

mkMigsDb :: (MonadIO m, MonadIO n) => m (U.MigsDb n)
mkMigsDb = liftIO $ do
  mvLock <- newMVar ()
  mvMigs <- newMVar ([] :: [(U.MigId, UTCTime)])
  pure $ U.MigsDb
    { U.migsDb_add = \mId -> liftIO $ do
        modifyMVar_ mvMigs $ \migs -> do
           ts <- getCurrentTime
           pure ((mId,ts):migs)
    , U.migsDb_del = \mId -> liftIO $ do
        modifyMVar_ mvMigs $ \migs ->
           pure $ filter ((== mId) . fst) migs
    , U.migsDb_get = \mId -> liftIO $ do
        migs <- readMVar mvMigs
        pure $! fmap snd (List.find (\(mId',_) -> mId' == mId) migs)
    , U.migsDb_all = liftIO $ do
        fmap (List.sortBy (\(_,ts) (_,ts') -> compare ts ts')) (readMVar mvMigs)
    , U.migsDb_lock = liftIO $ do
        maybe (Left U.Err_Locked) Right <$> tryTakeMVar mvLock
    , U.migsDb_unlock = liftIO $ do
        void $ tryPutMVar mvLock ()
    }

--------------------------------------------------------------------------------

mkUndoDataStore :: (MonadIO m, MonadIO n) => m (U.UndoDataStore n)
mkUndoDataStore = liftIO $ do
   mvStore <- newMVar Map.empty
   pure $ U.UndoDataStore
     { U.undoDataStore_put = \udId pbs -> do
         !bs <- mconcat <$> Pipes.toListM pbs
         liftIO $ modifyMVar_ mvStore (pure . Map.insert udId bs)
     , U.undoDataStore_get = \udId -> liftIO $ do
         ybs <- Map.lookup udId <$> readMVar mvStore
         pure (fmap Pipes.yield ybs)
     , U.undoDataStore_delete = \udId -> liftIO $ do
         modifyMVar_ mvStore (pure . Map.delete udId)
     }


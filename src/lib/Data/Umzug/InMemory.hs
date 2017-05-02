{-# LANGUAGE BangPatterns #-}

module Data.Umzug.InMemory
  ( mkMigsDb
  , mkRecoveryDataStore
  ) where

import Control.Concurrent.MVar
  (readMVar, newMVar, takeMVar, modifyMVar, modifyMVar_, tryPutMVar)
import Control.Monad (void)
import Data.List as List
import Data.Text (Text)
import Data.Time (UTCTime, getCurrentTime)
import qualified Pipes
import qualified Pipes.Prelude as Pipes

import Data.Umzug.Core

--------------------------------------------------------------------------------

mkMigsDb
  :: (String -> IO ())   -- ^ Logging function.
  -> IO MigsDb
mkMigsDb say = do
  mvLock <- newMVar ()
  mvMigs <- newMVar ([] :: [(MigId, Text, UTCTime)])
  pure $ MigsDb
    { migsdbAdd = \mId desc -> do
        say ("migsDbAdd " ++ show (mId, desc))
        modifyMVar_ mvMigs $ \migs -> do
           ts <- getCurrentTime
           pure ((mId,desc,ts):migs)
    , migsdbDel = \mId -> do
        say ("migsDbDel " ++ show mId)
        modifyMVar_ mvMigs $ \migs -> do
           pure $ filter (\(mId',_,_) -> mId' == mId) migs
    , migsdbGet = \mId -> do
        say ("migsDbGet " ++ show mId)
        migs <- readMVar mvMigs
        pure $! fmap (\(_,desc,ts) -> (desc,ts))
                     (List.find (\(mId',_,_) -> mId' == mId) migs)
    , migsdbAll = do
        say "migsDbAll"
        fmap (List.sortBy (\(_,_,ts) (_,_,ts') -> compare ts ts'))
             (readMVar mvMigs)
    , migsdbLock = do
        say "migsDbLock"
        takeMVar mvLock
    , migsdbUnlock = do
        say "migsDbUnlock"
        void $ tryPutMVar mvLock ()
    }

--------------------------------------------------------------------------------

mkRecoveryDataStore
  :: (String -> IO ())   -- ^ Logging function.
  -> IO (RecoveryDataStore IO)
mkRecoveryDataStore say = do
   mvNextId <- newMVar minBound
   mvStore <- newMVar []
   pure $ RecoveryDataStore
     { recoveryDataStorePut = \pbs -> do
         id' <- modifyMVar mvNextId (\id' -> pure (id' + 1, id'))
         say ("recoveryDataStorePut " ++ show id')
         !bs <- mconcat <$> Pipes.toListM pbs
         modifyMVar_ mvStore (pure . ((id',bs):))
         pure (RecoveryDataId id')
     , recoveryDataStoreGet = \(RecoveryDataId id') -> do
         say ("recoveryDataStoreGet " ++ show id')
         ybs <- List.lookup id' <$> readMVar mvStore
         pure (fmap Pipes.yield ybs)
     , recoveryDataStoreDelete = \(RecoveryDataId id') -> do
         say ("recoveryDataStoreDelete " ++ show id')
         modifyMVar_ mvStore (pure . filter ((/=) id' . fst))
     }

module Data.Umzug.InMemory (mkMigsDb) where

import Control.Concurrent.MVar
  (readMVar, newMVar, takeMVar, modifyMVar_, tryPutMVar)
import Control.Monad (void)
import Data.List as List
import Data.Text (Text)
import Data.Time (UTCTime, getCurrentTime)

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


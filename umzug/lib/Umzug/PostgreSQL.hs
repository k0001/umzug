{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Umzug.PostgreSQL
 ( mkMigsDb
 , mkMigsDb'
 , mkUndoDataStore
 , mkUndoDataStore'
 ) where


import qualified Blaze.ByteString.Builder.Char8 as BB8
import qualified Control.Monad.Catch as Ex
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Data.Function (fix)
import qualified Data.Text as T
import qualified Database.PostgreSQL.Simple as Pg
import qualified Database.PostgreSQL.Simple.FromField ()
import qualified Database.PostgreSQL.Simple.ToField as Pg
import qualified Database.PostgreSQL.Simple.Transaction as Pg
import qualified Database.PostgreSQL.Simple.LargeObjects as Pg
import Di (Di, dbg)
import qualified Di
import qualified Pipes
import qualified Pipes.Safe as Pipes

import Umzug

--------------------------------------------------------------------------------

mkMigsDb
  :: Pg.Connection  -- ^ Connection that will be used by the returned 'MigLog'
                    --   at all times.
  -> IO MigsDb
mkMigsDb = mkMigsDb' "umzug" "migrations" "lock"

mkMigsDb'
  :: String         -- ^ Schema name
  -> String         -- ^ Migrations table name
  -> String         -- ^ Locks table name
  -> Pg.Connection  -- ^ Connection that will be used by the returned 'MigLog'
                    --   at all times.
  -> IO MigsDb
mkMigsDb' sn0 tnMigs0 tnLock0 c = do
  let sn = Pg.Plain (BB8.fromText (T.pack sn0))
      tnMigs = Pg.Plain (BB8.fromText (T.pack (sn0 ++ "." ++ tnMigs0)))
      tnLock = Pg.Plain (BB8.fromText (T.pack (sn0 ++ "." ++ tnLock0)))
  _ <- Pg.execute c
     "CREATE SCHEMA IF NOT EXISTS ?; \
     \CREATE TABLE IF NOT EXISTS ? \
       \( id text PRIMARY KEY NOT NULL CHECK (id <> '') \
       \, ts timestamptz NOT NULL UNIQUE ); \
     \CREATE TABLE IF NOT EXISTS ? \
       \( id serial PRIMARY KEY NOT NULL );"
     (sn, tnMigs, tnLock)

  pure $ MigsDb
    { migsdbAdd = \(MigId mId) -> do
        n1 <- Pg.execute c "INSERT INTO ? (id, ts) VALUES (?, now())"
           (tnMigs, mId)
        when (n1 /= 1) $ error "mkMigsDb.migsdbAdd"

    , migsdbDel = \(MigId mId) -> do
        n1 <- Pg.execute c "DELETE FROM ? WHERE id = ?" (tnMigs , mId)
        when (n1 /= 1) $ error "mkMigsDb.migsdbDel"

    , migsdbAll =
        fmap (map (\(mId,ts) -> (MigId mId, ts)))
             (Pg.query c "SELECT id, ts FROM ? ORDER BY ts ASC"
                         (Pg.Only tnMigs))

    , migsdbGet = \(MigId mId) -> do
        r <- Pg.query c "SELECT ts FROM ? WHERE id = ?" (tnMigs, mId)
        case r of
          [Pg.Only x] -> pure (Just x)
          []  -> pure Nothing
          _   -> error "mkMigsDb.migsdbGet"

    , migsdbLock = do
        let tm = Pg.TransactionMode Pg.Serializable Pg.ReadWrite
        Pg.withTransactionMode tm c $ do
          Pg.query c "SELECT count(*) FROM ?" (Pg.Only tnLock) >>= \case
             [Pg.Only (0 :: Int)] -> pure ()
             [Pg.Only _] -> Ex.throwM Err_Locked
             _ -> error "mkMigsDb.migsdbLock A"
          n <- Pg.execute c "INSERT INTO ? (id) VALUES (1)" (Pg.Only tnLock)
          when (n /= 1) $ error "mkMigsDb.migsdbLock B"

    , migsdbUnlock =
        void $ Pg.execute c "DELETE FROM ?" (Pg.Only tnLock)
    }

--------------------------------------------------------------------------------

mkUndoDataStore
  :: Di String String
  -> Pg.Connection  -- ^ Connection that will be used by the returned
                    -- 'UndoDataStore' at all times.
  -> IO (UndoDataStore IO)
mkUndoDataStore di0 = mkUndoDataStore' di0 "umzug" "undo"

mkUndoDataStore'
  :: Di String String
  -> String         -- ^ Schema name
  -> String         -- ^ Undo data store table name
  -> Pg.Connection  -- ^ Connection that will be used by the returned
                    -- 'UndoDataStore' at all times.
  -> IO (UndoDataStore IO)
mkUndoDataStore' di0 sn0 tnUds0 c = do
   let di1 = Di.push di0 ["UndoDataStore"]
       sn = Pg.Plain (BB8.fromText (T.pack sn0))
       tnUds = Pg.Plain (BB8.fromText (T.pack (sn0 ++ "." ++ tnUds0)))
   _ <- Pg.execute c
      "CREATE SCHEMA IF NOT EXISTS ?; \
      \CREATE TABLE IF NOT EXISTS ? \
        \( mig_id text NOT NULL \
        \, typ text NOT NULL \
        \, lobj_oid oid NOT NULL UNIQUE \
        \, CHECK (typ IN ('pre', 'pos')) \
        \, UNIQUE (mig_id, typ) )"
      (sn, tnUds)
   let udsKey :: UndoDataId -> (MigId, String)
       udsKey = \case UndoDataPreId mId -> (mId, "pre")
                      UndoDataPosId mId -> (mId, "pos")
   pure $ UndoDataStore
     { udsPut = \udId pbs -> do
         dbg di1 ("udsPut " ++ show udId)
         let (MigId mId, typ) = udsKey udId
             tm = Pg.TransactionMode Pg.Serializable Pg.ReadWrite
         Pg.withTransactionMode tm c $ do
            oid <- Pg.loCreat c
            void $ Pg.execute c
               "INSERT INTO ? (mig_id, typ, lobj_oid) VALUES (?, ?, ?)"
               (tnUds, mId, typ, oid)
            Ex.bracket (Pg.loOpen c oid Pg.AppendMode) (Pg.loClose c)
               (\fd -> Pipes.runEffect $ do
                   Pipes.for pbs (void . liftIO . Pg.loWrite c fd))

     , udsGet = \udId -> do
         dbg di1 ("udsGet " ++ show udId)
         let (MigId mId, typ) = udsKey udId
         Pg.beginMode (Pg.TransactionMode Pg.RepeatableRead Pg.ReadOnly) c
         flip Ex.onException (Pg.rollback c) $ do
            xs <- Pg.query c
               "SELECT lobj_id FROM ? WHERE mig_id = ? AND typ = ?"
               (tnUds, mId, typ)
            case xs of
              [] -> pure Nothing
              [Pg.Only oid] -> do
                 fd <- Pg.loOpen c oid Pg.ReadMode
                 pure $ Just $ Pipes.hoist Pipes.runSafeT $ Pipes.finally
                    (fix $ \k -> do
                       bs <- liftIO (Pg.loRead c fd 16384)
                       when (bs /= mempty) (Pipes.yield bs >> k))
                    (liftIO $ Ex.finally (Pg.loClose c fd) (Pg.rollback c))
              _ -> error "mkUndoDataStore.udsGet"

     , udsDelete = \udId -> do
         dbg di1 ("udsDelete " ++ show udId)
         let (MigId mId, typ) = udsKey udId
             tm = Pg.TransactionMode Pg.Serializable Pg.ReadWrite
         Pg.withTransactionMode tm c $ do
            xs <- Pg.query c
               "SELECT lobj_id FROM ? WHERE mig_id = ? AND typ = ?"
               (tnUds, mId, typ)
            case xs of
              [] -> pure ()
              [Pg.Only oid] -> do
                 Pg.loUnlink c oid
                 void $ Pg.execute c
                    "DELETE FROM ? WHERE mig_id = ? AND typ = ?"
                    (tnUds, mId, typ)
              _ -> error "mkUndoDataStore.udsGet"
     }

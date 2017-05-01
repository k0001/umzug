{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Umzug.PostgreSQL
 ( mkMigsDb
 ) where


import qualified Blaze.ByteString.Builder.Char8       as BB8
import qualified Control.Exception                    as Ex
import           Control.Monad
import qualified Data.Text                            as T
import qualified Database.PostgreSQL.Simple           as Pg
import qualified Database.PostgreSQL.Simple.FromField ()
import qualified Database.PostgreSQL.Simple.ToField   as Pg
import qualified Database.PostgreSQL.Simple.Transaction as Pg

import           Data.Umzug.Core

--------------------------------------------------------------------------------

mkMigsDb
  :: String         -- ^ Migrations table name
  -> String         -- ^ Locks table name
  -> Pg.Connection  -- ^ Connection that will be used by the returned 'MigLog'
                    --   at all times.
  -> IO MigsDb
mkMigsDb tnMigs0 tnLock0 c = do
  let tnMigs = Pg.Plain (BB8.fromText (T.pack tnMigs0))
      tnLock = Pg.Plain (BB8.fromText (T.pack tnLock0))

  _ <- Pg.execute c
     "CREATE TABLE IF NOT EXISTS ? \
     \  ( id     text        PRIMARY KEY   NOT NULL   CHECK (id <> '') \
     \  , descr  text        NOT NULL  CHECK (descr <> '') \
     \  , ts     timestamptz NOT NULL  UNIQUE);\
     \CREATE TABLE IF NOT EXISTS ? \
     \  ( id     serial      PRIMARY KEY   NOT NULL );"
     (tnMigs, tnLock)

  pure $ MigsDb
    { migsdbAdd = \(MigId migId') migDesc' -> do
        n1 <- Pg.execute c "INSERT INTO ? (id,descr,ts) VALUES (?,?,now())"
           (tnMigs, migId', migDesc')
        when (n1 /= 1) $ error "mkMigsDb.migsdbAdd"

    , migsdbDel = \(MigId migId') -> do
        n1 <- Pg.execute c "DELETE FROM ? WHERE id = ?" (tnMigs , migId')
        when (n1 /= 1) $ error "mkMigsDb.migsdbDel"

    , migsdbAll =
        fmap (map (\(migId',desc,ts) -> (MigId migId', desc, ts)))
             (Pg.query c "SELECT (id,descr,ts) FROM ? ORDER BY ts ASC"
                         (Pg.Only tnMigs))

    , migsdbGet = \(MigId migId') -> do
        r <- Pg.query c "SELECT descr, ts FROM ? WHERE id = ?" (tnMigs, migId')
        case r of
          [x] -> pure (Just x)
          []  -> pure Nothing
          _   -> error "mkMigsDb.migsdbGet"

    , migsdbLock = do
        let tm = Pg.TransactionMode Pg.Serializable Pg.ReadWrite
        Pg.withTransactionMode tm c $ do
          Pg.query c "SELECT count(*) FROM ?" (Pg.Only tnLock) >>= \case
             [Pg.Only (0 :: Int)] -> pure ()
             [Pg.Only _] -> Ex.throwIO Err_Locked
             _ -> error "mkMigsDb.migsdbLock A"
          n <- Pg.execute c "INSERT INTO ? (id) VALUES (1)" (Pg.Only tnLock)
          when (n /= 1) $ error "mkMigsDb.migsdbLock B"

    , migsdbUnlock =
        void $ Pg.execute c "DELETE FROM ?" (Pg.Only tnLock)
    }

{-# LANGUAGE OverloadedStrings #-}

module Data.Umzug.PostgreSQL
 ( mkMigsDbInPostgreSQL
 ) where


import qualified Blaze.ByteString.Builder.Char8       as BB8
import           Control.Monad
import qualified Data.Text                            as T
import qualified Database.PostgreSQL.Simple           as PG
import qualified Database.PostgreSQL.Simple.FromField ()
import qualified Database.PostgreSQL.Simple.ToField   as PG

import           Data.Umzug.Core

--------------------------------------------------------------------------------

mkMigsDbInPostgreSQL
  :: String         -- ^ Table name
  -> PG.Connection  -- ^ Connection that will be used by the returned 'MigLog'
                    --   at all times.
  -> IO MigsDb
mkMigsDbInPostgreSQL tableName0 c = do
    let tableName = PG.Plain $ BB8.fromText $ T.pack tableName0
    _ <- PG.execute c
       "CREATE TABLE IF NOT EXISTS ? \
       \  ( id     text        PRIMARY KEY   NOT NULL   CHECK (id <> '') \
       \  , descr  text        NOT NULL  CHECK (descr <> '') \
       \  , ts     timestamptz NOT NULL  UNIQUE);"
       (PG.Only tableName)
    return $ MigsDb
      { migsdbAdd = \migId' migDesc' -> do
          n1 <- PG.execute c "INSERT INTO ? (id,descr,ts) VALUES (?,?,now())"
                             (tableName, migId', migDesc')
          when (n1 /= 1) $ error "mkMigsDbInPostgreSQLTable.migsdbAdd"

      , migsdbDel = \migId' -> do
          n1 <- PG.execute c "DELETE FROM ? WHERE id = ?" (tableName, migId')
          when (n1 /= 1) $ error "mkMigsDbInPostgreSQLTable.migsdbDel"

      , migsdbAll = do
          PG.query c "SELECT id, descr, ts FROM ? ORDER BY ts ASC"
                     (PG.Only tableName)

      , migsdbGet = \migId' -> do
          r <- PG.query c "SELECT descr, ts FROM ? WHERE id = ?"
                          (tableName, migId')
          case r of
            [x] -> return (Just x)
            []  -> return Nothing
            _   -> error "mkMigsDbInPostgreSQLTable.migsdbGet"
      }

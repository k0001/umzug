{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Umzug
 ( -- * Running migrations
   run

   -- * Writing migrations
 , migMany
 , mig

   -- * Keeping track of ran migrations

 , MigsDb(..)
 , migsdbIds

   -- * Types

 , MigId(..)

 , Mig(..)
 , migIds

 , Direction(..)
 , direction
 , directionOpposite

 , Migrate(..)

 , Step(..)
 , naiveStep
 , StepRunner

 , Recon(..)
 , Alter(..)

 , Codec(..)
 , aesonCodec

 , UndoDataId(..)
 , UndoDataStore(..)

 , Target
 , targetForwards
 , targetBackwards
 , targetMigration

 , Err_Locked(..)
 ) where

import Control.Concurrent (threadDelay)
import qualified Control.Exception as Ex
import Control.Monad
import Control.Monad.Trans.State.Strict (evalStateT)
import Di (Di, inf, wrn)
import qualified Di
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import Data.Foldable
import qualified Data.List as List
import Data.Monoid
import Data.String (IsString)
import Data.Text (Text)
import Data.Time (UTCTime)
import qualified Pipes
import qualified Pipes.Parse as Pipes
import qualified Pipes.Aeson.Unchecked

--------------------------------------------------------------------------------

data Direction = Backwards | Forwards
  deriving (Eq, Show, Read, Ord)

-- | Case analysis for 'Direction'. Evaluate to the first @a@ in case it is
-- 'Backwards', otherwise to the second @a@.
direction :: a -> a -> Direction -> a
direction a _ Backwards = a
direction _ a Forwards  = a

directionOpposite :: Direction -> Direction
directionOpposite = direction Forwards Backwards

--------------------------------------------------------------------------------

-- | Unique identifier for a 'Mig' (unique within the closed word of 'Mig's you
-- want to deal with, that is).
newtype MigId = MigId Text
  deriving (Eq, Ord, IsString)

instance Show MigId where
  show (MigId x) = show x

--------------------------------------------------------------------------------

-- | Representation for a single migration described by @a@ with runtime access
-- to the environment @env@.
--
-- Hint: use 'mig' and 'migMany' to construct 'Mig's.
data Mig a env = Mig
  { migId           :: !MigId      -- ^ Unique identifier.
  , migInstructions :: !(Migrate a env)
  }

-- | 'MigId's present in the given 'Mig', in the order they would be applied
-- if ran forwards.
migIds :: Mig a env -> [MigId]
migIds (Mig a c) = a : case c of
    MigrateMany migs -> migs >>= migIds
    _ -> []

--------------------------------------------------------------------------------
-- Smart constructors for 'Mig'

-- | Create a new 'Mig' that applies many child migrations sequentially,
-- rolling back previous successes if one of the following migrations fails.
migMany
  :: MigId  -- ^ Unique identifier.
  -> [Mig a env]  -- ^ Migrations to apply “atomically”, listed in
                  --   forward direction (even if you plan to apply your
                  --   migrations backwards).
  -> Mig a env
migMany mId migs = Mig mId (MigrateMany migs)

-- | Create a new 'Mig' that performs a single migration step, either in
-- forwards or backwards direction as later desired.
mig
  :: MigId  -- ^ Unique identifier.
  -> a      -- ^ Description of the migration.
  -> Step 'Forwards env
  -> Step 'Backwards env
  -> Mig a env
mig mId a sf sb = Mig mId (MigrateOne a sf sb)

--------------------------------------------------------------------------------

-- | Detailed instructions on what kind of migration to perform.
data Migrate a env
  = -- | Apply many child migrations sequentially, rolling back previous
    -- successes if one of the following migrations fails.
    MigrateMany ![Mig a env]
  | -- | Apply a custom migration step as described by @a@.
    MigrateOne !a !(Step 'Forwards env) !(Step 'Backwards env)

--------------------------------------------------------------------------------

-- | Wrapper around an 'IO' action intended to do any preliminary reconnaissance
-- work before running any mutating work through 'Alter', gathering enough
-- information about the data that 'Alter' will modify so that said data can be
-- recovered if necessary.
--
-- 'Recon' must not perform any mutating or destructive side-effect.
--
-- For example, if the related 'Alter' plans to delete entries from a database,
-- then 'Recon' should gather the entries to be deleted in @pre@, so that they
-- can be inserted back if necessary, as part of a recovery or rollback process.
newtype Recon pre pos = Recon { runRecon  :: IO (pre, Alter pos) }

-- | Wraper around an 'IO' action intended to do any altering or destructive
-- side-effect as part of a 'Step'.
--
-- The @pos@ value, used in combination with the @pre@ value provided by the
-- @'Recon' pre pos@ where this @'Alter' pos@ was obtained, should provide
-- enough information to allow for the rollback of the side-effects performed by
-- this 'Alter', in case an automated rollback is necessary as part of some
-- recovery process.
newtype Alter pos = Alter { runAlter :: IO pos }

-- | Witness that @a@ can be encoded and decoded as a 'BS.ByteString'.
--
-- @em@ is the base 'Monad' used for encoding.
--
-- @dm@ is the base 'Monad' used for decoding.
--
-- @
-- 'decode' ('encode' a) == 'pure' ('Just' a)
-- @
data Codec em dm a = Codec
  { encode :: a -> Pipes.Producer BS.ByteString em ()
  , decode :: Pipes.Parser BS.ByteString dm (Maybe a)
  }

-- | Serializes @x@ using its 'Aeson.ToJSON'/'Aeson.FromJSON' representation.
aesonCodec
  :: (Monad em, Monad dm, Aeson.ToJSON x, Aeson.FromJSON x)
  => Codec em dm x
aesonCodec = Codec
  { encode = Pipes.Aeson.Unchecked.encode
  , decode = fmap (join . fmap (either (const Nothing) Just))
                  Pipes.Aeson.Unchecked.decode
  }

--------------------------------------------------------------------------------

data UndoDataId
  = UndoDataRecoveryId !MigId
  | UndoDataRollbackId !MigId
  deriving (Eq, Show, Ord)

data UndoDataStore m = UndoDataStore
  { udsPut
      :: UndoDataId
      -> Pipes.Producer BS.ByteString m ()
      -> m ()
  , udsGet
      :: UndoDataId
      -> m (Maybe (Pipes.Producer BS.ByteString m ()))
  , udsDelete
      :: UndoDataId
      -> m ()  -- ^ Doesn't fail if 'UndoDataId' is absent.
  }

-- | Internal
udsEncode
  :: UndoDataStore IO
  -> Codec IO IO a
  -> UndoDataId
  -> a
  -> IO ()
udsEncode uds c udId a =
  udsPut uds udId (encode c a)

-- | Internal
udsDecode
  :: UndoDataStore IO
  -> Codec IO IO a
  -> UndoDataId
  -> IO a
udsDecode uds c udId = do
   udsGet uds udId >>= \case
      Nothing  -> Ex.throwIO (Err_UndoDataMissing udId)
      Just pbs -> evalStateT (decode c) pbs >>= \case
         Nothing -> Ex.throwIO (Err_UndoDataMalformed udId)
         Just a  -> pure a

--------------------------------------------------------------------------------

-- | A single real-world side-effecting step in a migration with direction @d@.
data Step (d :: Direction) env = forall pre pos. Step
  { stepRecon :: env -> Recon pre pos
    -- ^ Migrate in direction @d@.
    --
    -- If the 'Alter' inside the 'Recon' fails, then 'stepRecover' will be
    -- executed.
    --
    -- If the 'Alter' inside the 'Recon' succeeds, but a latter migration
    -- intended to be executed atomically together with this 'Step' fails, then
    -- 'stepRollback' will be called.
  , stepRecover :: env -> pre -> IO ()
    -- ^ See 'step'.
  , stepRollback :: env -> pre -> pos -> IO ()
    -- ^ See 'step'.
  , stepCodecPre :: forall em dm. (Monad em, Monad dm) => Codec em dm pre
    -- ^ Witness the fact that @pre@ can be serialized for later recovery.
  , stepCodecPos :: forall em dm. (Monad em, Monad dm) => Codec em dm pos
    -- ^ Witness the fact that @pos@ can be serialized for later recovery.
  }

-- | A 'Step' that doesn't do any recovery nor rollback.
naiveStep
  :: (env -> IO ())   -- ^ Migrate in direction @d@.
  -> Step d env
naiveStep f = Step
  { stepRecon = \env -> Recon (pure ((), Alter (f env)))
  , stepRecover = \_ () -> pure ()
  , stepRollback = \_ () () -> pure ()
  , stepCodecPre = aesonCodec
  , stepCodecPos = aesonCodec
  }

--------------------------------------------------------------------------------

type StepRunner a env = forall x. a -> (env -> IO x) -> IO x

--------------------------------------------------------------------------------

data MigsDb = MigsDb
  { migsdbAdd :: MigId -> IO ()
    -- ^ Atomic.
  , migsdbDel :: MigId -> IO ()
    -- ^ Atomic.
  , migsdbGet :: MigId -> IO (Maybe UTCTime)
    -- ^ Atomic.
  , migsdbAll :: IO [(MigId, UTCTime)]
    -- ^ Atomic. All migrations, ordered chronologically by timestamp.
  , migsdbLock :: IO ()
    -- ^ Atomic.
  , migsdbUnlock :: IO ()
    -- ^ Atomic.
  }

withMigsDbLock :: Di String String -> MigsDb -> IO a -> IO a
withMigsDbLock di mdb =
    Ex.bracket_ lock (migsdbUnlock mdb)
  where
    lock = Ex.catch (migsdbLock mdb) $ \Err_Locked -> do
      wrn di "Umzug database is currently locked. Retrying in 10 seconds."
      threadDelay (1000000 * 10)
      lock

migsdbIds :: MigsDb -> IO [MigId]
migsdbIds = fmap (map fst) . migsdbAll

--------------------------------------------------------------------------------

-- | A simple wrapper around @'IO' ()@, mostly for avoid accidentally confusing
-- this 'IO' action with others.
newtype Undo = Undo { runUndo :: IO () }

instance Monoid Undo where
  mempty = Undo (pure ())
  -- | @'mappend' a b@ runs @a@ first, then @b@.
  mappend (Undo a) (Undo b) = Undo (a >> b)

--------------------------------------------------------------------------------

data Target a env = UnsafeTarget [Mig a env] MigId

targetForwards :: [Mig a env] -> Maybe (Target a env)
targetForwards [] = Nothing
targetForwards xs = Just (UnsafeTarget xs (migId (last xs)))

targetBackwards :: [Mig a env] -> Maybe (Target a env)
targetBackwards [] = Nothing
targetBackwards xs = Just (UnsafeTarget xs (migId (head xs)))

-- | Note: It is impossible to target a child migration inside a 'MigrateMany'
-- (such as the childs passed to 'migMany').
targetMigration :: MigId -> [Mig a env] -> Maybe (Target a env)
targetMigration mId xs =
  fmap (UnsafeTarget xs . migId) (List.find ((==) mId . migId) xs)

--------------------------------------------------------------------------------
-- Running migrations

run
  :: Di String String
  -> MigsDb                -- ^ Where to keep track of applied migrations.
  -> UndoDataStore IO  -- ^ Store where recovery data is kept.
  -> Target a env          -- ^ Target migration.
  -> StepRunner a env      -- ^ How to run each migration step.
  -> IO ()
run di migsdb uds t runner = do
  withMigsDbLock di migsdb $ do
    yp <- mkPlan migsdb t
    case yp of
      Nothing -> inf di "Nothing to do. Bye."
      Just (UnsafePlan d0 migs) ->
        traverse_ (run' di migsdb uds runner d0) migs

-- | Internal
run'
  :: forall a env
  .  Di String String
  -> MigsDb
  -> UndoDataStore IO
  -> StepRunner a env
  -> Direction
  -> Mig a env
  -> IO Undo
run' di0 migsdb uds runner d0 (Mig mId migMigIns) = do
  case migMigIns of
    MigrateOne a sf sb -> direction (work a sb) (work a sf) d0
    MigrateMany migs -> do
      undo <- foldlM
        (\undo x -> do
            undo' <- Ex.onException
               (run' di0 migsdb uds runner d0 x)
               (runUndo undo)
            pure (undo' <> undo))
        (mempty :: Undo)
        (direction reverse id d0 migs)
      Ex.onException (recMig d0) (runUndo undo)
      pure (Undo (recMig (directionOpposite d0) >> runUndo undo))
  where
    recMig :: Direction -> IO ()
    recMig d = direction migsdbDel migsdbAdd d migsdb mId

    work :: a -> Step d env -> IO Undo
    work a (Step h hrec (hrol :: env -> pre -> pos -> IO ()) hcpre hcpos) = do
      let di1 = Di.push di0 [show mId, show d0]
      inf di1 "Running Recon"
      (pre, mpos) <- runner a (runRecon . h)
      inf di1 "Saving recovery data"
      udsEncode uds hcpre (UndoDataRecoveryId mId) pre
      let undo :: Maybe (Maybe pos) -> Undo
          undo = \yypos -> Undo $ join $ fmap runUndo $ runner a $ \env -> do
            pre' <- udsDecode uds hcpre (UndoDataRecoveryId mId)
            case yypos of
              Nothing -> hrec env pre'   -- recovery
              Just ypos -> do            -- rollback
                hrol env pre' =<< case ypos of
                  Just pos -> pure pos
                  Nothing -> udsDecode uds hcpos (UndoDataRollbackId mId)
                recMig (directionOpposite d0)
            pure mempty
      inf di1 "Running Alter"
      pos <- Ex.onException (runAlter mpos) (runUndo (undo Nothing))
      Ex.onException (recMig d0) (runUndo (undo (Just (Just pos))))
      inf di1 "Saving rollback data"
      udsEncode uds hcpos (UndoDataRollbackId mId) pos
      pure (undo (Just Nothing))

--------------------------------------------------------------------------------

data Plan a env = UnsafePlan Direction [Mig a env]
  -- ^ Migrations to be run listed in the order in which they need to be
  --   run according to 'Direction'.

mkPlan
  :: MigsDb
  -> Target a env
  -> IO (Maybe (Plan a env))
mkPlan migsdb (UnsafeTarget migs mId) = do
  let mIds = migs >>= migIds

  -- check for duplicates
  let repMigs = repeatedElements mIds
  unless (null repMigs) $ do
     error ("Duplicate migration identifiers: " <> show repMigs)

  -- check for incompatible migs list
  mIdsInDb <- migsdbIds migsdb
  unless (List.isPrefixOf mIdsInDb mIds) $ do
     error ("The desired migrations list (" <> show mIds <> ") is not \
            \compatible with already ran migrations (" <> show mIdsInDb <> ")")

  -- figure out the plan
  pure $ case lastMay mIdsInDb of
     Just mIdCurrent
       | mId == mIdCurrent -> Nothing
       | elem mId (init mIdsInDb) ->
          Just $ UnsafePlan Backwards $ reverse $
             dropUntilAfter ((==) mId . migId)
                (dropAfter ((==) mIdCurrent . migId) migs)
     _ -> Just $ UnsafePlan Forwards (dropAfter ((==) mId . migId) migs)

--------------------------------------------------------------------------------

data Err_Locked = Err_Locked
  deriving (Eq, Ord, Show)
instance Ex.Exception Err_Locked

data Err_UnsupportedMigBackwards = Err_UnsupportedMigBackwards !MigId
  deriving (Eq, Ord, Show)
instance Ex.Exception Err_UnsupportedMigBackwards

data Err_UndoDataMissing = Err_UndoDataMissing !UndoDataId
  deriving (Eq, Ord, Show)
instance Ex.Exception Err_UndoDataMissing

data Err_UndoDataMalformed = Err_UndoDataMalformed !UndoDataId
  deriving (Eq, Ord, Show)
instance Ex.Exception Err_UndoDataMalformed

--------------------------------------------------------------------------------

-- | Returns the elements in a list that are repeated
repeatedElements :: Ord a => [a] -> [a]
repeatedElements = mconcat . filter ((>1) . length) . List.group . List.sort

-- | Returns the last element in the given list, unless the list if empty.
lastMay :: [a] -> Maybe a
lastMay [] = Nothing
lastMay xs = Just (last xs)

-- | @'dropAfter' (== 6) [5,6,7,8]   =   [5,6]@
--
-- @'dropAfter' f xs ++ 'dropUntilAfter' f xs   =   'id'@
dropAfter :: (a -> Bool) -> [a] -> [a]
dropAfter f = go
  where go [] = []
        go (a:as) | f a = [a]
                  | otherwise = a : go as

-- | @'dropUntilAfter' (== 6) [5,6,7,8]   =   [7,8]@
--
-- @'dropAfter' f xs ++ 'dropUntilAfter' f xs   =   'id'@
dropUntilAfter :: (a -> Bool) -> [a] -> [a]
dropUntilAfter f = go
  where go [] = []
        go (a:as) | f a = as
                  | otherwise = go as

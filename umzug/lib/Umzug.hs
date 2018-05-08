{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
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
 , migsDb_ids

   -- * Types

 , MigId(..)

 , Mig(..)
 , mig_ids

 , Direction(..)
 , direction
 , directionOpposite

 , Migrate(..)

 , Step(..)
 , StepRunner(..)

 , Recon(..)
 , Alter(..)

 , Codec(..)

 , UndoDataId(..)
 , UndoDataStore(..)

 , Target
 , targetForwards
 , targetBackwards
 , targetMigration

 , Err_Locked(..)
 ) where

import Control.Concurrent (threadDelay)
import Control.Monad (unless)
import qualified Control.Monad.Catch as Ex
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.State.Strict (evalStateT)
import qualified Di
import qualified Data.ByteString as B
import Data.Foldable (foldlM, traverse_)
import qualified Data.List as List
import Data.String (IsString, fromString)
import qualified Data.Text.Lazy as TL
import Data.Time (UTCTime)
import qualified Pipes
import qualified Pipes.Parse as Pipes

--------------------------------------------------------------------------------

data Direction
  = Backwards
  | Forwards
  deriving stock (Eq, Show, Ord)

-- | Case analysis for 'Direction'. Evaluate to the first @a@ in case it is
-- 'Backwards', otherwise to the second @a@.
direction :: a -> a -> Direction -> a
direction bw fw = \case
  Backwards -> bw
  Forwards -> fw

directionOpposite :: Direction -> Direction
directionOpposite = direction Forwards Backwards

direction_df1Value :: Direction -> Di.Value
direction_df1Value = direction "backwards" "forwards"

--------------------------------------------------------------------------------

-- | Unique identifier for a 'Mig' (unique within the closed word of 'Mig's you
-- want to deal with, that is).
newtype MigId = MigId TL.Text
  deriving newtype (Eq, Show, Ord, IsString)

migId_df1Value :: MigId -> Di.Value
migId_df1Value (MigId x) = Di.value x

--------------------------------------------------------------------------------

-- | Representation for a single migration described by @a@ with runtime access
-- to the environment @env@.
--
-- Hint: use 'mig' and 'migMany' to construct 'Mig's.
data Mig m a env = Mig
  { mig_id :: !MigId      -- ^ Unique identifier.
  , mig_instructions :: !(Migrate m a env)
  }

-- | 'MigId's present in the given 'Mig', in the order they would be applied
-- if ran forwards.
mig_ids :: Mig m a env -> [MigId]
mig_ids (Mig a c) = a : case c of
  MigrateMany migs -> migs >>= mig_ids
  _ -> []

--------------------------------------------------------------------------------
-- Smart constructors for 'Mig'

-- | Create a new 'Mig' that applies many child migrations sequentially,
-- rolling back previous successes if one of the following migrations fails.
migMany
  :: MigId  -- ^ Unique identifier.
  -> [Mig m a env]
  -- ^ Migrations to apply “atomically”, listed in forward direction (even if
  -- you plan to apply your migrations backwards).
  -> Mig m a env
migMany mId migs = Mig mId (MigrateMany migs)

-- | Create a new 'Mig' that performs a single migration step, either in
-- forwards or backwards direction as later desired.
mig
  :: MigId  -- ^ Unique identifier.
  -> a      -- ^ Description of the migration.
  -> Step m 'Forwards env
  -> Step m 'Backwards env
  -> Mig m a env
mig mId a sf sb = Mig mId (MigrateOne a sf sb)

--------------------------------------------------------------------------------

-- | Detailed instructions on what kind of migration to perform.
data Migrate (m :: * -> *) (a :: *) (env :: *)
  = MigrateMany ![Mig m a env]
    -- ^ Apply many child migrations sequentially, rolling back previous
    -- successes if one of the following migrations fails.
  | MigrateOne !a !(Step m 'Forwards env) !(Step m 'Backwards env)
    -- ^ Apply a custom migration step as described by @a@.

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
newtype Recon m pre pos = Recon (m (pre, Alter m pos))

runRecon :: Di.MonadDf1 m => Recon m pre pos -> m (pre, Alter m pos)
runRecon (Recon m) = Di.push "recon" $ do
   Di.debug "Running..." *> m <* Di.debug "Ran."

-- | Wraper around an 'IO' action intended to do any altering or destructive
-- side-effect as part of a 'Step'.
--
-- The @pos@ value, used in combination with the @pre@ value provided by the
-- @'Recon' pre pos@ where this @'Alter' pos@ was obtained, should provide
-- enough information to allow for the rollback of the side-effects performed by
-- this 'Alter', in case an automated rollback is necessary as part of some
-- recovery process.
newtype Alter m pos = Alter (m pos)

runAlter :: Di.MonadDf1 m => Alter m pos -> m pos
runAlter (Alter m) = Di.push "alter" $ do
  Di.debug "Running..." *> m <* Di.debug "Ran."

--------------------------------------------------------------------------------

-- | Witness that @a@ can be encoded and decoded as a 'B.ByteString'.
--
-- @em@ is the base 'Monad' used for encoding.
--
-- @dm@ is the base 'Monad' used for decoding.
--
-- @
-- 'decode' ('encode' a) == 'pure' ('Just' a)
-- @
data Codec em dm a = Codec
  { encode :: a -> Pipes.Producer B.ByteString em ()
  , decode :: Pipes.Parser B.ByteString dm (Maybe a)
  }

--------------------------------------------------------------------------------

data UndoDataId
  = UndoDataPreId !MigId
  | UndoDataPosId !MigId
  deriving (Eq, Show, Ord)

data UndoDataStore m = UndoDataStore
  { undoDataStore_put
      :: UndoDataId
      -> Pipes.Producer B.ByteString m ()
      -> m ()
  , undoDataStore_get
      :: UndoDataId
      -> m (Maybe (Pipes.Producer B.ByteString m ()))
  , undoDataStore_delete
      :: UndoDataId
      -> m ()  -- ^ Doesn't fail if 'UndoDataId' is absent.
  }

-- | Internal
udsSave
  :: forall m a
  .  Di.MonadDf1 m
  => UndoDataStore m
  -> Codec m m a
  -> UndoDataId
  -> a
  -> m ()
udsSave uds c udId a = k $ do
    Di.debug "Saving..."
    undoDataStore_put uds udId (encode c a)
    Di.info "Saved."
  where
    k :: forall x. m x -> m x
    k = case udId of
      UndoDataPreId{} -> Di.push "save-recovery"
      UndoDataPosId{} -> Di.push "save-rollback"

-- | Internal
udsLoad
  :: forall m a
  .  (Di.MonadDf1 m, Ex.MonadThrow m)
  => UndoDataStore m
  -> Codec m m a
  -> UndoDataId
  -> m a
udsLoad uds c udId = k $ do
    Di.debug "Loading..."
    a <- undoDataStore_get uds udId >>= \case
        Nothing  -> Ex.throwM (Err_UndoDataMissing udId)
        Just pbs -> evalStateT (decode c) pbs >>= \case
           Nothing -> Ex.throwM (Err_UndoDataMalformed udId)
           Just a  -> pure a
    Di.info "Loaded."
    pure a
  where
    k :: forall x. m x -> m x
    k = case udId of
      UndoDataPreId{} -> Di.push "load-recovery"
      UndoDataPosId{} -> Di.push "load-rollback"


--------------------------------------------------------------------------------

-- | A single real-world side-effecting step in a migration with direction @d@.
data Step (m :: * -> *) (d :: Direction) env = forall pre pos. Step
  { step_recon :: env -> Recon m pre pos
    -- ^ Migrate in direction @d@.
    --
    -- If the 'Alter' inside the 'Recon' fails, then 'step_recover' will be
    -- executed.
    --
    -- If the 'Alter' inside the 'Recon' succeeds, but a latter migration
    -- intended to be executed atomically together with this 'Step' fails, then
    -- 'step_rollback' will be called.
  , step_recover :: env -> pre -> m ()
    -- ^ See 'step'.
  , step_rollback :: env -> pre -> pos -> m ()
    -- ^ See 'step'.
  , step_codecPre :: forall em dm. (Monad em, Monad dm) => Codec em dm pre
    -- ^ Witness the fact that @pre@ can be serialized for later recovery.
  , step_codecPos :: forall em dm. (Monad em, Monad dm) => Codec em dm pos
    -- ^ Witness the fact that @pos@ can be serialized for later recovery.
  }

--------------------------------------------------------------------------------

newtype StepRunner m a env
  = StepRunner (forall x. (env -> m x) -> a -> m x)

runStepRunner
  :: Di.MonadDf1 m
  => StepRunner m a env
  -> a
  -> (env -> m x)
  -> m x -- ^
runStepRunner (StepRunner f) a g = Di.push "step-runner" $ do
  Di.debug "Running..." *> f g a <* Di.debug "Ran."

--------------------------------------------------------------------------------

data MigsDb m = MigsDb
  { migsDb_add :: MigId -> m ()
    -- ^ Atomic.
  , migsDb_del :: MigId -> m ()
    -- ^ Atomic.
  , migsDb_get :: MigId -> m (Maybe UTCTime)
    -- ^ Atomic.
  , migsDb_all :: m [(MigId, UTCTime)]
    -- ^ Atomic. All migrations, ordered chronologically by timestamp.
  , migsDb_lock :: m (Either Err_Locked ())
    -- ^ Atomic.
  , migsDb_unlock :: m ()
    -- ^ Atomic.
  }

withMigsDbLock
  :: forall m a
  .  (Di.MonadDf1 m, Ex.MonadMask m, MonadIO m)
  => MigsDb m
  -> m a
  -> m a
withMigsDbLock mdb =
   Ex.bracket_ acq (migsDb_unlock mdb)
 where
   acq :: m ()
   acq = migsDb_lock mdb >>= \case
     Right () -> pure ()
     Left Err_Locked -> do
       Di.warning "Umzug database is currently locked. Retrying in 10 seconds."
       liftIO (threadDelay (1000000 * 10))
       acq

migsDb_ids :: Functor m => MigsDb m -> m [MigId]
migsDb_ids = fmap (map fst) . migsDb_all

--------------------------------------------------------------------------------

-- | A simple wrapper around @m ()@, mostly for avoid accidentally confusing
-- this 'm' action with others.
newtype Undo m = Undo (m ())

runUndo :: Di.MonadDf1 m => Undo m -> m ()
runUndo (Undo m) = Di.push "undo" $ do
  Di.debug "Running..." *> m <* Di.debug "Ran."

instance Monad m => Semigroup (Undo m) where
  -- | @a <> b@ runs @a@ first, then @b@.
  Undo a <> Undo b = Undo (a >> b)

instance Monad m => Monoid (Undo m) where
  mempty = Undo (pure ())
  -- | @'mappend' a b@ runs @a@ first, then @b@.
  mappend = (<>)

--------------------------------------------------------------------------------

data Target m a env = UnsafeTarget [Mig m a env] MigId

targetForwards :: [Mig m a env] -> Maybe (Target m a env)
targetForwards [] = Nothing
targetForwards xs = Just (UnsafeTarget xs (mig_id (last xs)))

targetBackwards :: [Mig m a env] -> Maybe (Target m a env)
targetBackwards [] = Nothing
targetBackwards xs = Just (UnsafeTarget xs (mig_id (head xs)))

-- | Note: It is impossible to target a child migration inside a 'MigrateMany'
-- (such as the childs passed to 'migMany').
targetMigration :: MigId -> [Mig m a env] -> Maybe (Target m a env)
targetMigration mId xs =
  fmap (UnsafeTarget xs . mig_id) (List.find ((==) mId . mig_id) xs)

--------------------------------------------------------------------------------
-- Running migrations

run
  :: (Di.MonadDf1 m, Ex.MonadMask m, MonadIO m)
  => MigsDb m              -- ^ Where to keep track of applied migrations.
  -> UndoDataStore m       -- ^ Store where recovery data is kept.
  -> Target m a env        -- ^ Target migration.
  -> StepRunner m a env    -- ^ How to run each migration step.
  -> m ()
run migsDb uds t sr = Di.push "umzug" $ do
  withMigsDbLock migsDb $ do
    yp <- mkPlan migsDb t
    case yp of
      Nothing -> Di.notice "Nothing to do. Bye."
      Just (UnsafePlan d0 migs) ->
        traverse_ (run' migsDb uds sr d0) migs

-- | Internal
run'
  :: forall m a env
  .  (Di.MonadDf1 m, Ex.MonadMask m)
  => MigsDb m
  -> UndoDataStore m
  -> StepRunner m a env
  -> Direction
  -> Mig m a env
  -> m (Undo m)
run' migsDb uds sr d0 (Mig mId migMigIns) =
  case migMigIns of
    MigrateOne a sf sb ->
      -- We run one single migration in direction @d0@
      Di.push "one" $
      Di.attr "dir" (direction_df1Value d0) $
      Di.attr "mig" (migId_df1Value mId) $ do
         Di.notice "Running..."
         direction (work a sb) (work a sf) d0 <* Di.info "Ran."
    MigrateMany migs ->
      -- We run many single migrations in direction @d0@
      Di.push "many" $
      Di.attr "dir" (direction_df1Value d0) $
      Di.attr "mig" (migId_df1Value mId) $ do
         Di.notice "Running..."
         undo <- foldlM
           (\undo x -> do
               undo' <- Ex.onException (run' migsDb uds sr d0 x) (runUndo undo)
               pure (undo' <> undo))
           (mempty :: Undo m)
           (direction reverse id d0 migs)
         Ex.onException (recMig d0) (runUndo undo)
         Di.info "Ran."
         pure (Undo (recMig (directionOpposite d0) >> runUndo undo))

  where
    recMig :: Direction -> m ()
    recMig = \case
      Backwards -> Di.debug "Deleting migration" >> migsDb_del migsDb mId
      Forwards -> Di.debug "Saving migration" >> migsDb_add migsDb mId

    work :: a -> Step m d env -> m (Undo m)
    work a (Step h hrec (hrol :: env -> pre -> pos -> m ()) hcpre hcpos) = do
      (pre, mpos) <- runStepRunner sr a (runRecon . h)
      udsSave uds hcpre (UndoDataPreId mId) pre
      let undo :: Maybe (Maybe pos) -> Undo m
          undo = \yypos -> Undo $ do
            runStepRunner sr a $ \env -> do
              pre' <- udsLoad uds hcpre (UndoDataPreId mId)
              case yypos of
                Nothing -> Di.push "recover" $ do
                  Di.warning "Running..."
                  hrec env pre' <* Di.notice "Ran."
                Just ypos -> Di.push "rollback" $ do
                  Di.warning "Running..."
                  hrol env pre' =<< case ypos of
                    Just pos -> pure pos
                    Nothing -> udsLoad uds hcpos (UndoDataPosId mId)
                  recMig (directionOpposite d0) <* Di.notice "Ran."
              pure mempty
      pos <- Ex.onException (runAlter mpos) (runUndo (undo Nothing))
      Ex.catch (recMig d0) $ \(se :: Ex.SomeException) -> do
         Di.attr "exception" (fromString (show se)) $ do
            Di.error "Shit hit the fan. Will undo now."
         Ex.finally (runUndo (undo (Just (Just pos)))) (Ex.throwM se)
      udsSave uds hcpos (UndoDataPosId mId) pos
      pure (undo (Just Nothing))

--------------------------------------------------------------------------------

data Plan m a env = UnsafePlan Direction [Mig m a env]
  -- ^ Migrations to be run listed in the order in which they need to be
  --   run according to 'Direction'.

mkPlan
  :: Monad m
  => MigsDb m
  -> Target n a env
  -> m (Maybe (Plan n a env))
mkPlan migsDb (UnsafeTarget migs mId) = do
  let mIds = migs >>= mig_ids

  -- check for duplicates
  let repMigs = repeatedElements mIds
  unless (null repMigs) $ do
     error ("Duplicate migration identifiers: " <> show repMigs)

  -- check for incompatible migs list
  mIdsInDb <- migsDb_ids migsDb
  unless (List.isPrefixOf mIdsInDb mIds) $ do
     error ("The desired migrations list (" <> show mIds <> ") is not \
            \compatible with already ran migrations (" <> show mIdsInDb <> ")")

  -- figure out the plan
  pure $ case lastMay mIdsInDb of
     Just mIdCurrent
       | mId == mIdCurrent -> Nothing
       | elem mId (init mIdsInDb) ->
          Just $ UnsafePlan Backwards $ reverse $
             dropUntilAfter ((==) mId . mig_id)
                (dropAfter ((==) mIdCurrent . mig_id) migs)
     _ -> Just $ UnsafePlan Forwards (dropAfter ((==) mId . mig_id) migs)

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


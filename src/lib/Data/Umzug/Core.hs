{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Umzug.Core
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

 , Target
 , targetForwards
 , targetBackwards
 , targetMigration

 , Err_Locked(..)
 ) where

import           Control.Concurrent                   (threadDelay)
import qualified Control.Exception                    as Ex
import           Control.Monad
import           Data.Foldable
import qualified Data.List as List
import           Data.Monoid
import           Data.String                          (IsString)
import           Data.Text                            (Text)
import qualified Data.Text                            as T
import           Data.Time                            (UTCTime)

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
  , migDescription  :: !Text
  , migInstructions :: !(Migrate a env)
  }

-- | 'MigId's present in the given 'Mig', in the order they would be applied
-- if ran forwards.
migIds :: Mig a env -> [MigId]
migIds (Mig a _ c) = a : case c of
    MigrateMany migs -> migs >>= migIds
    _ -> []

--------------------------------------------------------------------------------
-- Smart constructors for 'Mig'

-- | Create a new 'Mig' that applies many child migrations sequentially,
-- rolling back previous successes if one of the following migrations fails.
migMany
  :: MigId  -- ^ Unique identifier.
  -> Text   -- ^ Migration description.
  -> [Mig a env]  -- ^ Migrations to apply “atomically”, listed in
                  --   forward direction (even if you plan to apply your
                  --   migrations backwards).
  -> Mig a env
migMany num desc migs = Mig num desc (MigrateMany migs)

-- | Create a new 'Mig' that performs a single migration step, either in
-- forwards or backwards direction as later desired.
mig
  :: MigId  -- ^ Unique identifier.
  -> Text   -- ^ Migration description.
  -> a      -- ^ Description of the migration.
  -> Step 'Forwards env
  -> Maybe (Step 'Backwards env)
  -> Mig a env
mig num desc a stepf ystepb = Mig num desc (MigrateOne a stepf ystepb)

--------------------------------------------------------------------------------

-- | Detailed instructions on what kind of migration to perform.
data Migrate a env
  = -- | Apply many child migrations sequentially, rolling back previous
    -- successes if one of the following migrations fails.
    MigrateMany ![Mig a env]
  | -- | Apply a custom migration step as described by @a@.
    MigrateOne !a !(Step 'Forwards env) !(Maybe (Step 'Backwards env))

--------------------------------------------------------------------------------

-- | A single real-world side-effecting step in a migration with direction @d@.
data Step (d :: Direction) env = forall pre pos. Step
  { step :: env -> IO (pre, IO pos)
    -- ^ Migrate in direction @d@.
    --
    -- If @'IO' pos@ fails, then 'stepRecover' will be called.
    --
    -- If @'IO' pos@ succeeds, but a latter migration intended to be executed
    -- atomically with this fails, then 'stepRollback' will be called.
  , stepRecover :: env -> pre -> IO ()
    -- ^ See 'step'
  , stepRollback :: env -> pre -> pos -> IO ()
  }

-- | A 'Step' that doesn't do any recovery nor rollback.
naiveStep
  :: (env -> IO ())   -- ^ Migrate in direction @d@.
  -> Step d env
naiveStep f = Step
  (\env -> pure ((), f env)) (\_ _ -> pure ()) (\_ _ _ -> pure ())

--------------------------------------------------------------------------------

type StepRunner a env = forall x. a -> (env -> IO x) -> IO x

--------------------------------------------------------------------------------

data MigsDb = MigsDb
  { migsdbAdd :: MigId -> T.Text -> IO ()
    -- ^ Atomic.
  , migsdbDel :: MigId -> IO ()
    -- ^ Atomic.
  , migsdbGet :: MigId -> IO (Maybe (T.Text, UTCTime))
    -- ^ Atomic.
  , migsdbAll :: IO [(MigId, T.Text, UTCTime)]
    -- ^ Atomic. All migrations, ordered chronologically by timestamp.
  , migsdbLock :: IO ()
    -- ^ Atomic.
  , migsdbUnlock :: IO ()
    -- ^ Atomic.
  }

withMigsDbLock :: MigsDb -> IO a -> IO a
withMigsDbLock mdb =
    Ex.bracket_ lock (migsdbUnlock mdb)
  where
    lock = Ex.catch (migsdbLock mdb) $ \Err_Locked -> do
      putStrLn ("Umzug database is currently locked. Retrying in 10 seconds.")
      threadDelay (1000000 * 10)
      lock

migsdbIds :: MigsDb -> IO [MigId]
migsdbIds = fmap (map (\(x,_,_) -> x)) . migsdbAll

--------------------------------------------------------------------------------

-- | A simple wrapper around @'IO' ()@, mostly for avoid accidentally confusing
-- this 'IO' action with others.
newtype Undo = Undo { runUndo :: IO () }

instance Monoid Undo where
  mempty = Undo (pure ())
  -- | @'mappend' a b@ runs @a@ first, then @b@.
  mappend (Undo a) (Undo b) = Undo (a >> b)

withLoggedMigration
  :: MigsDb            -- ^ Where to log the migration.
  -> (String -> IO ()) -- ^ Print a simple string message.
  -> Direction         -- ^ Direction on which the migration is being applied
  -> MigId             -- ^ Identifier for the migration being applied
  -> T.Text            -- ^ Description of the migration being applied
  -> IO Undo           -- ^ Action that performs the migration in the       desired
                       --   direction and returns a rollback action in the
                       --   opposite direction, which will be used in case it is
                       --   not possible to sucessfully record the migration
                       --   execution.
  -> IO Undo           -- ^ Same as the last argument, except it records the
                       --   migration execution when going in the desired
                       --   direction and removes it from the records when going
                       --   in the opposite direction.
withLoggedMigration migsdb say0 d0 migId' migDesc' m = do
    mDone <- migsdbGet migsdb migId'
    case (d0, mDone) of
      (Backwards, Nothing) -> skip
      (Forwards,  Just _)  -> skip
      _ -> do
         say d0 "START"
         undo <- Ex.catch m $ \(e :: Ex.SomeException) -> do
            say d0 ("ERROR: " <> show e)
            Ex.throwIO e
         say d0 "SUCCESS"
         Ex.catch (logMig d0) $ \(e ::Ex.SomeException) -> do
            say d0 ("LOG ERROR: " <> show e)
            runUndo undo
            Ex.throwIO e
         pure $ Undo $ do
            runUndo undo
            let d' = directionOpposite d0
            Ex.catch (logMig d') $ \(e :: Ex.SomeException) -> do
               say d' ("LOG ERROR: " <> show e)
               Ex.throwIO e
 where
    say :: Direction -> String -> IO ()
    say d x = do
      let prefix = direction "<- BCK " "-> FWD " d
      say0 (prefix <> show migId' <> " - " <> x)

    skip :: IO Undo
    skip = say d0 "SKIPPING: already done" >> pure mempty

    logMig :: Direction -> IO ()
    logMig Backwards = migsdbDel migsdb migId'
    logMig Forwards  = migsdbAdd migsdb migId' migDesc'

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
  :: MigsDb            -- ^ Where to keep track of applied migrations.
  -> (String -> IO ()) -- ^ Used to report textual progress.
  -> Target a env      -- ^ Target migration.
  -> StepRunner a env  -- ^ How to run each migration step.
  -> IO ()
run migsdb say t runner = do
  withMigsDbLock migsdb $ do
     yp <- mkPlan migsdb t
     case yp of
        Nothing -> say "Nothing to do. Bye."
        Just (UnsafePlan d0 migs) -> do
           traverse_ (run' migsdb say runner d0) migs

-- | Internal
run' :: MigsDb
     -> (String -> IO ())
     -> StepRunner a env
     -> Direction
     -> Mig a env
     -> IO Undo
run' migsdb say runner d0 (Mig migId' migDesc migMigIns) = do
    let withLoggedMigration' = withLoggedMigration migsdb say
    withLoggedMigration' d0 migId' migDesc $ case migMigIns of
       MigrateMany migs -> foldlM
          (\undo x -> do
              undo' <- Ex.onException
                 (run' migsdb say runner d0 x)
                 (runUndo undo)
              pure (undo' <> undo))
          (mempty :: Undo)
          (direction reverse id d0 migs)
       MigrateOne a (Step f frec frol) ystepb -> case d0 of
          Forwards -> do
             (pre, mpos) <- runner a f
             let bwd = \ypos -> join $ fmap runUndo $ do
                    withLoggedMigration' Backwards migId' migDesc $ do
                       runner a $ \env -> case ypos of
                          Nothing  -> frec env pre
                          Just pos -> frol env pre pos
                       pure mempty
             pos <- Ex.onException mpos (bwd Nothing)
             pure (Undo (bwd (Just pos)))
          Backwards -> case ystepb of
             Nothing -> Ex.throwIO (Err_UnsuportedMigBackwards migId')
             Just (Step b brec brol) -> do
                (pre, mpos) <- runner a b
                let fwd = \ypos -> join $ fmap runUndo $ do
                       withLoggedMigration' Forwards migId' migDesc $ do
                          runner a $ \env -> case ypos of
                             Nothing  -> brec env pre
                             Just pos -> brol env pre pos
                          pure mempty
                pos <- Ex.onException mpos (fwd Nothing)
                pure (Undo (fwd (Just pos)))

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

data Err_UnsuportedMigBackwards = Err_UnsuportedMigBackwards !MigId
  deriving (Eq, Ord, Show)
instance Ex.Exception Err_UnsuportedMigBackwards

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

{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE CPP                        #-}

module Data.Umzug.Core
 ( -- * Running migrations
   runMigrations

   -- * Writing migrations
 , migMany
 , mig

   -- * Keeping track of ran migrations

 , MigsDb(..)
 , assertSaneMigrations

   -- * Types

 , MigId

 , Mig(..)
 , migIds

 , Direction(..)
 , direction
 , directionOpposite

 , Migrate(..)

 , Step(..)
 , StepRunner

 , Scenario(..)
 , scenario
 ) where

import qualified Control.Exception                    as Ex
import           Control.Monad
import           Control.Monad.Trans.Reader           (ReaderT(runReaderT))
import           Data.Foldable
import qualified Data.List as List
#if !MIN_VERSION_base(4,11,0)
import           Data.Monoid
#endif
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
type MigId = Text

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
migMany num desc migs = Mig num desc $ MigrateMany migs


-- | Create a new 'Mig' that performs a single migration step, either in
-- forwards or backwards direction as later desired.
mig
  :: MigId  -- ^ Unique identifier.
  -> Text   -- ^ Migration description.
  -> a      -- ^ Description of the migration.
  -> (Scenario y -> ReaderT env IO x)
            -- ^ Side-effecting action to perform when going forwards. Getting
            --   a @Recovery y@ constructor as second argument means this action
            --   is being executed as a recovery mechanism because one of the
            --   following migrations failed. Whatever this function returns
            --   is made available to the backwards step in case recovery in
            --   that direction is needed.
  -> (Scenario x -> ReaderT env IO y)
            -- ^ Side-effecting action to perform when going backwards. Getting
            --   a @Recovery x@ constructor as second argument means this action
            --   is being executed as a recovery mechanism because one of the
            --   following migrations failed. Whatever this function returns
            --   is made available to the forwards step in case recovery in
            --   that direction is needed.
  -> Mig a env
mig num desc a fw bw = Mig num desc $
    MigrateOne a Step{stepForwards=fw, stepBackwards=bw}

--------------------------------------------------------------------------------

-- | Detailed instructions on what kind of migration to perform.
data Migrate a env
  = -- | Apply many child migrations sequentially, rolling back previous
    -- successes if one of the following migrations fails.
    MigrateMany ![Mig a env]
  | -- | Apply a custom migration step as described by @a@.
    MigrateOne !a !(Step env)

--------------------------------------------------------------------------------

-- | A single real-world side-effecting step in a migration.
--
-- TODO: Make the @x@ and @y@ below be 'Binary' instances or something,
-- so that we can persist this state somewhere for resilience.
data Step env = forall x y. Step
  { stepForwards  :: !(Scenario y -> ReaderT env IO x)
    -- ^ Side-effecting action to perform when going forwards. Getting
    --   a @Recovery y@ constructor as second argument means this action
    --   is being executed as a recovery mechanism because one of the
    --   following migrations failed. Whatever this function returns
    --   is made available to the backwards step in case recovery in
    --   that direction is needed.
  , stepBackwards :: !(Scenario x -> ReaderT env IO y)
    -- ^ Side-effecting action to perform when going backwards. Getting
    --   a @Recovery x@ constructor as second argument means this action
    --   is being executed as a recovery mechanism because one of the
    --   following migrations failed. Whatever this function returns
    --   is made available to the forwards step in case recovery in
    --   that direction is needed.
  }

--------------------------------------------------------------------------------

data Scenario a = Desired | Recovery !a
  deriving (Eq, Ord, Show)

-- | Case analysis for 'Scenario'.
scenario :: b -> (a -> b) -> Scenario a -> b
scenario b _ Desired      = b
scenario _ f (Recovery a) = f a

--------------------------------------------------------------------------------

type StepRunner a env = forall x. a -> (env -> IO x) -> IO x

--------------------------------------------------------------------------------
-- Logging

data MigsDb = MigsDb
  { migsdbAdd :: MigId -> T.Text -> IO ()
  , migsdbDel :: MigId -> IO ()
  , migsdbGet :: MigId -> IO (Maybe (T.Text, UTCTime))
  , migsdbAll :: IO [(MigId, T.Text, UTCTime)]
  }

withLoggedMigration
  :: MigsDb            -- ^ Where to log the migration.
  -> (String -> IO ()) -- ^ Print a simple string message.
  -> Direction         -- ^ Direction on which the migration is being applied
  -> MigId             -- ^ Identifier for the migration being applied
  -> T.Text            -- ^ Description of the migration being applied
  -> IO (IO ())        -- ^ Action that performs the migration in the desired
                       --   direction and returns a rollback action in the
                       --   opposite direction, which will be used in case it is
                       --   not possible to sucessfully record the migration
                       --   execution.
  -> IO (IO ())        -- ^ Same as the last argument, except it records the
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
         undo <- m `Ex.catch` \(e :: Ex.SomeException) -> do
            say d0 $ "ERROR: " <> show e
            Ex.throwIO e
         say d0 "SUCCESS"

         -- say d0 "LOG START"
         logMig d0 `Ex.catch` \(e ::Ex.SomeException) -> do
            say d0 $ "LOG ERROR: " <> show e
            undo
            Ex.throwIO e
         -- say d0 $ "LOG SUCCESS"

         return $ do
            undo
            let d' = directionOpposite d0
            -- say d' "LOG START"
            logMig d' `Ex.catch` \(e :: Ex.SomeException) -> do
               say d' $ "LOG ERROR: " <> show e
               Ex.throwIO e
            -- say d' "LOG SUCCESS "
 where
    say :: Direction -> String -> IO ()
    say d x = do
      let prefix = direction "<- BCK " "-> FWD " d
      say0 $ prefix <> show migId' <> " - " <> x

    skip :: IO (IO ())
    skip = return () <$ say d0 "SKIPPING: already done"

    logMig :: Direction -> IO ()
    logMig Backwards = migsdbDel migsdb migId'
    logMig Forwards  = migsdbAdd migsdb migId' migDesc'

--------------------------------------------------------------------------------
-- Running migrations

runMigrations
  :: MigsDb            -- ^ Where to keep track of applied migrations.
  -> (String -> IO ()) -- ^ Used to report textual progress.
  -> StepRunner a env  -- ^ How to run each migration step.
  -> Direction         -- ^ Direction in which to apply the migrations.
  -> [Mig a env]       -- ^ Migrations available, listed in forward order (even
                       --   if you are running migrations backwards).
  -> IO ()
runMigrations migsdb say runner d0 migs = do
    assertSaneMigrations migsdb migs
    void $ sequence $
       run' migsdb say runner d0 `map` direction reverse id d0 migs


run' :: MigsDb
     -> (String -> IO ())
     -> StepRunner a env
     -> Direction
     -> Mig a env
     -> IO (IO ())
run' migsdb say runner d0 (Mig migId' migDesc migMigIns) = do
    let withLoggedMigration' = withLoggedMigration migsdb say
    withLoggedMigration' d0 migId' migDesc $ case migMigIns of
       MigrateMany migs -> do
          let step undo x = do
                undo' <- run' migsdb say runner d0 x `Ex.onException` undo
                return $ undo' >> undo
          foldlM step (return ()) $ direction reverse id d0 migs
       MigrateOne migTy Step{stepForwards=fw, stepBackwards=bw} -> case d0 of
          Backwards -> do
             res <- runner migTy $ runReaderT (bw Desired)
             return $ join $ withLoggedMigration' Forwards migId' migDesc $ do
                _ <- runner migTy $ runReaderT (fw (Recovery res))
                return (return ())
          Forwards -> do
             res <- runner migTy $ runReaderT (fw Desired)
             return $ join $ withLoggedMigration' Backwards migId' migDesc $ do
                _ <- runner migTy $ runReaderT (bw (Recovery res))
                return (return ())



assertSaneMigrations :: MigsDb -> [Mig a env] -> IO ()
assertSaneMigrations _ml migs = do
   unless (null repMigs) $
      error $ "Duplicate migration identifiers: " <> show repMigs
   -- TODO: more sanity checks
  where
    fwMigIds = migs >>= migIds
    repMigs = repeatedElements fwMigIds

--------------------------------------------------------------------------------

-- | Returns the elements in a list that are repeated
repeatedElements :: Ord a => [a] -> [a]
repeatedElements = mconcat . filter ((>1) . length) . List.group . List.sort

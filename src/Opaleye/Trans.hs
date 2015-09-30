{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

module Opaleye.Trans
    ( OpaleyeT (..)
    , runOpaleyeT

    , -- * Transactions
      transaction

    , -- * Queries
      query
    , queryFirst

    , -- * Inserts
      insert
    , insertMany
    , insertReturning
    , insertReturningFirst
    , insertManyReturning

    , -- * Utilities
      withConn
    , withConnIO

    , -- * Reexports
      liftBase
    , MonadBase
    , ask
    , MonadBaseControl
    , Int64
    ) where

import           Control.Monad.Base              (MonadBase, liftBase)
import           Control.Monad.Reader            (MonadReader, ReaderT (..),
                                                  ask)
import           Control.Monad.Trans             (MonadTrans (..))
import           Control.Monad.Trans.Control

import           Data.Maybe                      (listToMaybe)
import           Data.Profunctor.Product.Default (Default)

import           Database.PostgreSQL.Simple      (Connection, withTransaction)
import qualified Database.PostgreSQL.Simple      as PSQL

import           GHC.Int

import           Opaleye


-- | The 'Opaleye' monad transformer
newtype OpaleyeT m a = OpaleyeT { unOpaleyeT :: ReaderT Connection m a }
    deriving (Functor, Applicative, Monad, MonadTrans, MonadReader Connection)


instance MonadBase b m => MonadBase b (OpaleyeT m) where
    liftBase = lift . liftBase


-- | Given a 'Connection', run an 'OpaleyeT'
runOpaleyeT :: PSQL.Connection -> OpaleyeT m a -> m a
runOpaleyeT c = flip runReaderT c . unOpaleyeT
-- TODO Handle exceptions

-- | With a 'Connection'
withConn :: Monad m => (Connection -> m a) -> OpaleyeT m a
withConn f = do
    conn <- ask
    lift (f conn)


-- | With a 'Connection'
withConnIO :: MonadBase IO m => (Connection -> IO a) -> OpaleyeT m a
withConnIO f = do
    conn <- ask
    liftBase $ f conn


-- | 'withTransaction' lifted into a 'MonadBaseControl' 'IO' monad
liftWithTransaction :: MonadBaseControl IO m => Connection -> m a -> m a
liftWithTransaction conn f =
    control $ \io -> withTransaction conn (io f)


-- | Run a postgresql transaction in the 'OpaleyeT' monad
transaction :: MonadBaseControl IO m => OpaleyeT m a -> OpaleyeT m a
transaction t = withConn $ \conn ->
    liftWithTransaction conn (runOpaleyeT conn t)


-- | Execute a 'Query'. See 'runQuery'.
query :: (MonadBase IO m, Default QueryRunner a b) => Query a -> OpaleyeT m [b]
query q = withConnIO (`runQuery` q)


-- | Retrieve the first result from a 'Query'. Similar to @listToMaybe <$> runQuery@.
queryFirst :: (MonadBase IO m, Default QueryRunner a b) => Query a -> OpaleyeT m (Maybe b)
queryFirst q = listToMaybe <$> query q


-- | Insert into a 'Table'. See 'runInsert'.
insert :: MonadBase IO m => Table w r -> w -> OpaleyeT m Int64
insert t w = withConnIO (\c -> runInsert c t w)


-- | Insert many records into a 'Table'. See 'runInsertMany'.
insertMany :: MonadBase IO m => Table w r -> [w] -> OpaleyeT m Int64
insertMany t ws = withConnIO (\c -> runInsertMany c t ws)


-- | Insert a record into a 'Table' with a return value. See 'runInsertReturning'.
insertReturning
    :: (MonadBase IO m, Default QueryRunner a b)
    => Table w r
    -> (r -> a)
    -> w
    -> OpaleyeT m [b]
insertReturning t ret w = withConnIO (\c -> runInsertReturning c t w ret)


-- | Insert a record into a 'Table' with a return value. Retrieve only the first result.
-- Similar to @listToMaybe <$> insertReturning@
insertReturningFirst
    :: (MonadBase IO m, Default QueryRunner a b)
    => Table w r
    -> (r -> a)
    -> w
    -> OpaleyeT m (Maybe b)
insertReturningFirst t ret w = listToMaybe <$> insertReturning t ret w


-- | Insert many records into a 'Table' with a return value for each record.
insertManyReturning
    :: (MonadBaseControl IO m, Default QueryRunner a b)
    => Table w r
    -> (r -> a)
    -> [w]
    -> OpaleyeT m [[b]]
insertManyReturning t ret ws =
    transaction (mapM (insertReturning t ret) ws)

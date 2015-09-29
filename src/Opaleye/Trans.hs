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


newtype OpaleyeT m a = OpaleyeT { unOpaleyeT :: ReaderT Connection m a }
    deriving (Functor, Applicative, Monad, MonadTrans, MonadReader Connection)


instance MonadBase b m => MonadBase b (OpaleyeT m) where
    liftBase = lift . liftBase


-- | TODO Handle exceptions
runOpaleyeT :: PSQL.Connection -> OpaleyeT m a -> m a
runOpaleyeT c = flip runReaderT c . unOpaleyeT


withConn :: Monad m => (Connection -> m a) -> OpaleyeT m a
withConn f = do
    conn <- ask
    lift (f conn)


withConnIO :: MonadBase IO m => (Connection -> IO a) -> OpaleyeT m a
withConnIO f = do
    conn <- ask
    liftBase $ f conn


liftWithTransaction :: MonadBaseControl IO m => Connection -> m a -> m a
liftWithTransaction conn f =
    control $ \io -> withTransaction conn (io f)


transaction :: MonadBaseControl IO m => OpaleyeT m a -> OpaleyeT m a
transaction t = withConn $ \conn ->
    liftWithTransaction conn (runOpaleyeT conn t)


query :: (MonadBase IO m, Default QueryRunner a b) => Query a -> OpaleyeT m [b]
query q = withConnIO (`runQuery` q)


queryFirst :: (MonadBase IO m, Default QueryRunner a b) => Query a -> OpaleyeT m (Maybe b)
queryFirst q = listToMaybe <$> query q


insert :: MonadBase IO m => Table w r -> w -> OpaleyeT m Int64
insert t w = withConnIO (\c -> runInsert c t w)


insertMany :: MonadBase IO m => Table w r -> [w] -> OpaleyeT m Int64
insertMany t ws = withConnIO (\c -> runInsertMany c t ws)


insertReturning
    :: (MonadBase IO m, Default QueryRunner a b)
    => Table w r
    -> (r -> a)
    -> w
    -> OpaleyeT m [b]
insertReturning t ret w = withConnIO (\c -> runInsertReturning c t w ret)


insertReturningFirst
    :: (MonadBase IO m, Default QueryRunner a b)
    => Table w r
    -> (r -> a)
    -> w
    -> OpaleyeT m (Maybe b)
insertReturningFirst t ret w = listToMaybe <$> insertReturning t ret w


insertManyReturning
    :: (MonadBaseControl IO m, Default QueryRunner a b)
    => Table w r
    -> (r -> a)
    -> [w]
    -> OpaleyeT m [[b]]
insertManyReturning t ret ws =
    transaction (mapM (insertReturning t ret) ws)

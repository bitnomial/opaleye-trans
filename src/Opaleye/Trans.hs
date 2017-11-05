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
      Transaction
    , transaction
    , run

    , -- * Queries
      query
    , queryFirst

    , -- * Inserts
      insert
    , insertMany
    , insertReturning
    , insertReturningFirst
    , insertManyReturning

    , -- * Updates
      update
    , updateReturning
    , updateReturningFirst

    , -- * Deletes
      delete

    , -- * Utilities
      withConn

    , -- * Reexports
      liftBase
    , MonadBase
    , liftIO
    , MonadIO
    , ask
    , Int64
    ) where

import           Control.Monad.Base              (MonadBase, liftBase)
import           Control.Monad.IO.Class          (MonadIO, liftIO)
import           Control.Monad.Reader            (MonadReader, ReaderT (..),
                                                  ask)
import           Control.Monad.Trans             (MonadTrans (..))

import           Data.Maybe                      (listToMaybe)
import           Data.Profunctor.Product.Default (Default)

import           Database.PostgreSQL.Simple      (Connection, withTransaction)
import qualified Database.PostgreSQL.Simple      as PSQL

import           GHC.Int                         (Int64)

import           Opaleye


-- | The 'Opaleye' monad transformer
newtype OpaleyeT m a = OpaleyeT { unOpaleyeT :: ReaderT Connection m a }
    deriving (Functor, Applicative, Monad, MonadTrans, MonadIO, MonadReader Connection)


instance MonadBase b m => MonadBase b (OpaleyeT m) where
    liftBase = lift . liftBase


-- | Given a 'Connection', run an 'OpaleyeT'
runOpaleyeT :: PSQL.Connection -> OpaleyeT m a -> m a
runOpaleyeT c = flip runReaderT c . unOpaleyeT


withConn :: MonadIO m => (Connection -> IO a) -> OpaleyeT m a
withConn f = ask >>= liftIO . f


newtype Transaction a = Transaction { unTransaction :: ReaderT Connection IO a }
    deriving (Functor, Applicative, Monad, MonadReader Connection)


-- | Run a postgresql transaction in the 'OpaleyeT' monad
transaction :: MonadIO m => Transaction a -> OpaleyeT m a
transaction (Transaction t) = withConn $ \conn ->
    withTransaction conn (runReaderT t conn)


-- | Execute a query without a literal transaction
run :: MonadIO m => Transaction a -> OpaleyeT m a
run = withConn . runReaderT . unTransaction


-- | With a 'Connection' in a 'Transaction'
-- This isn't exposed so that users can't just drop down to IO
-- in a transaction
withConnIO :: (Connection -> IO a) -> Transaction a
withConnIO = Transaction . ReaderT


-- | Execute a 'Query'. See 'runQuery'.
query :: Default QueryRunner a b => Query a -> Transaction [b]
query q = withConnIO (`runQuery` q)


-- | Retrieve the first result from a 'Query'. Similar to @listToMaybe <$> runQuery@.
queryFirst :: Default QueryRunner a b => Query a -> Transaction (Maybe b)
queryFirst q = listToMaybe <$> query q


-- | Insert into a 'Table'. See 'runInsert'.
insert :: Table w r -> w -> Transaction Int64
insert t w = withConnIO (\c -> runInsertMany c t [w])


-- | Insert many records into a 'Table'. See 'runInsertMany'.
insertMany :: Table w r -> [w] -> Transaction Int64
insertMany t ws = withConnIO (\c -> runInsertMany c t ws)


-- | Insert a record into a 'Table' with a return value. See 'runInsertReturning'.
insertReturning :: Default QueryRunner a b => Table w r -> (r -> a) -> w -> Transaction [b]
insertReturning t ret w = withConnIO (\c -> runInsertManyReturning c t [w] ret)


-- | Insert a record into a 'Table' with a return value. Retrieve only the first result.
-- Similar to @'listToMaybe' '<$>' 'insertReturning'@
insertReturningFirst :: Default QueryRunner a b => Table w r -> (r -> a) -> w -> Transaction (Maybe b)
insertReturningFirst t ret w = listToMaybe <$> insertReturning t ret w


-- | Insert many records into a 'Table' with a return value for each record.
--
-- Maybe not worth defining. This almost certainly does the wrong thing.
insertManyReturning :: Default QueryRunner a b => Table w r -> [w] -> (r -> a) -> Transaction [b]
insertManyReturning t ws ret = withConnIO (\c -> runInsertManyReturning c t ws ret)


-- | Update items in a 'Table' where the predicate is true.  See 'runUpdate'.
update :: Table w r -> (r -> w) -> (r -> Column PGBool) -> Transaction Int64
update t r2w predicate = withConnIO (\c -> runUpdate c t r2w predicate)


-- | Update items in a 'Table' with a return value.  See 'runUpdateReturning'.
updateReturning :: Default QueryRunner returned haskells
                => Table w r
                -> (r -> w)
                -> (r -> Column PGBool)
                -> (r -> returned)
                -> Transaction [haskells]
updateReturning table r2w predicate r2returned = withConnIO (\c -> runUpdateReturning c table r2w predicate r2returned)


-- | Update items in a 'Table' with a return value.  Similar to @'listToMaybe' '<$>' 'updateReturning'@.
updateReturningFirst :: Default QueryRunner returned haskells
                     => Table w r
                     -> (r -> w)
                     -> (r -> Column PGBool)
                     -> (r -> returned)
                     -> Transaction (Maybe haskells)
updateReturningFirst table r2w predicate r2returned = listToMaybe <$> updateReturning table r2w predicate r2returned


delete :: Table a columnsR -> (columnsR -> Column PGBool) -> Transaction Int64
delete table r2b = withConnIO (\c -> runDelete c table r2b)

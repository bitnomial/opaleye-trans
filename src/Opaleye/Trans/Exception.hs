{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}

module Opaleye.Trans.Exception
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

    , -- * Exceptions
      withExceptOpaleye
    , withExceptTrans

    , -- * Utilities
      withError
    , withoutError
    , liftError
    , withTrans
    , maybeError

    , -- * Reexports
      liftIO
    , MonadIO
    , ask
    , Int64

    , throwError
    , catchError
    ) where

import           Control.Exception               (catch, throw)
import           Control.Monad.Except            (ExceptT (..), MonadError,
                                                  catchError, runExceptT,
                                                  throwError, withExceptT)
import           Control.Monad.IO.Class          (MonadIO, liftIO)
import           Control.Monad.Reader            (MonadReader (..))
import           Control.Monad.Trans             (MonadTrans (..))
import           Control.Monad.Catch             (MonadCatch, MonadThrow)

import           Data.Profunctor.Product.Default (Default)

import           Database.PostgreSQL.Simple      (Connection)
import qualified Database.PostgreSQL.Simple      as PSQL

import           GHC.Int                         (Int64)

import           Opaleye

import qualified Opaleye.Trans                   as T


newtype OpaleyeT e m a = OpaleyeT { unOpaleyeT :: ExceptT e (T.OpaleyeT m) a }
    deriving ( Functor, Applicative, Monad, MonadIO, MonadReader Connection
             , MonadError e, MonadCatch, MonadThrow
             )


instance MonadTrans (OpaleyeT e) where
    lift = OpaleyeT . lift . lift


withOpaleye :: Monad m => T.OpaleyeT m a -> OpaleyeT e m a
withOpaleye = OpaleyeT . lift


-- | Given a 'Connection', run an 'OpaleyeT'
runOpaleyeT :: PSQL.Connection -> OpaleyeT e m a -> m (Either e a)
runOpaleyeT c = T.runOpaleyeT c . runExceptT . unOpaleyeT


withExceptOpaleye :: Functor m => (e -> e') -> OpaleyeT e m a -> OpaleyeT e' m a
withExceptOpaleye f = OpaleyeT . withExceptT f . unOpaleyeT


-- | Just like 'T.Transaction' only with exception handling
newtype Transaction e a = Transaction { unTransaction :: ExceptT e T.Transaction a }
    deriving (Functor, Applicative, Monad, MonadReader Connection, MonadError e)


withExceptTrans :: (e -> e') -> Transaction e a -> Transaction e' a
withExceptTrans f = Transaction . withExceptT f . unTransaction


withError :: Monad m => T.OpaleyeT m (Either e a) -> OpaleyeT e m a
withError f = withOpaleye f >>= either throwError return


withoutError :: Monad m => OpaleyeT e m a -> T.OpaleyeT m (Either e a)
withoutError = runExceptT . unOpaleyeT


liftError :: Monad m
          => (T.Transaction (Either e a) -> T.OpaleyeT m (Either r b))
          -> Transaction e a -> OpaleyeT r m b
liftError f = withError . f . runExceptT . unTransaction


-- | Run a postgresql transaction in the 'OpaleyeT' monad
transaction :: MonadIO m => Transaction e a -> OpaleyeT e m a
transaction = liftError T.transaction


-- | Execute a query without a literal transaction
run :: MonadIO m => Transaction e a -> OpaleyeT e m a
run = liftError T.run


withTrans :: T.Transaction a -> Transaction e a
withTrans = Transaction . lift


-- | Execute a 'Query'. See 'runQuery'.
query :: Default QueryRunner a b => Query a -> Transaction e [b]
query = withTrans . T.query


maybeError :: T.Transaction (Maybe b) -> e -> Transaction e b
maybeError f e = withTrans f >>= maybe (throwError e) return


-- | Retrieve the first result from a 'Query'. Similar to @listToMaybe <$> runQuery@.
queryFirst :: Default QueryRunner a b => e -> Query a -> Transaction e b
queryFirst e q = maybeError (T.queryFirst q) e


-- | Insert into a 'Table'. See 'runInsert'.
insert :: Table w r -> w -> Transaction e Int64
insert t = withTrans . T.insert t


-- | Insert many records into a 'Table'. See 'runInsertMany'.
insertMany :: Table w r -> [w] -> Transaction e Int64
insertMany t = withTrans . T.insertMany t


-- | Insert a record into a 'Table' with a return value. See 'runInsertReturning'.
insertReturning :: Default QueryRunner a b => Table w r -> (r -> a) -> w -> Transaction e [b]
insertReturning t ret = withTrans . T.insertReturning t ret


-- | Insert a record into a 'Table' with a return value. Retrieve only the first result.
-- Similar to @'listToMaybe' '<$>' 'insertReturning'@
insertReturningFirst :: Default QueryRunner a b => e -> Table w r -> (r -> a) -> w -> Transaction e b
insertReturningFirst e t ret w = maybeError (T.insertReturningFirst t ret w) e


-- | Insert many records into a 'Table' with a return value for each record.
--
-- Maybe not worth defining. This almost certainly does the wrong thing.
insertManyReturning :: Default QueryRunner a b => Table w r -> [w] -> (r -> a) -> Transaction e [b]
insertManyReturning t ws = withTrans . T.insertManyReturning t ws


-- | Update items in a 'Table' where the predicate is true. See 'runUpdate'.
update :: Table w r -> (r -> w) -> (r -> Column PGBool) -> Transaction e Int64
update t r2w = withTrans . T.update t r2w


-- | Update items in a 'Table' with a return value. See 'runUpdateReturning'.
updateReturning :: Default QueryRunner a b
                => Table w r
                -> (r -> w)
                -> (r -> Column PGBool)
                -> (r -> a)
                -> Transaction e [b]
updateReturning t r2w p = withTrans . T.updateReturning t r2w p


-- | Update items in a 'Table' with a return value. Similar to @'listToMaybe' '<$>' 'updateReturning'@.
updateReturningFirst :: Default QueryRunner a b
                     => e
                     -> Table w r
                     -> (r -> w)
                     -> (r -> Column PGBool)
                     -> (r -> a)
                     -> Transaction e b
updateReturningFirst e t r2w p r2r = maybeError (T.updateReturningFirst t r2w p r2r) e


-- | Delete items in a 'Table' that satisfy some boolean predicate. See 'runDelete'.
delete :: Table a b -> (b -> Column PGBool) -> Transaction e Int64
delete t = withTrans . T.delete t

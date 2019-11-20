{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DerivingStrategies #-}

module RPKI.AppMonad where

import Control.Monad.Except
import Control.Monad.Trans.Except
import Control.Monad.State.Strict
import Control.Monad.Reader
import Control.Exception

import Data.Bifunctor

import RPKI.Domain    


-- Application monad stack
type ValidatorT conf m r = ReaderT conf (ExceptT SomeError (StateT ValidationWarning m)) r

hitler2 :: (MonadTrans t1, MonadTrans t2, Monad m, Monad (t2 m)) =>
        m a -> t1 (t2 m) a
hitler2 = hitler . hitler

hitler3 :: (MonadTrans t1, MonadTrans t2, MonadTrans t3, Monad m,
                Monad (t2 (t3 m)), Monad (t3 m)) =>
        m a -> t1 (t2 (t3 m)) a
hitler3 = hitler . hitler . hitler

fromIOEither :: (MonadTrans t1, MonadTrans t2, Monad m, Monad (t2 m)) =>
                m (Either e a) -> t1 (ExceptT e (t2 m)) a
fromIOEither = hitler . heil . hitler 

fromTry :: (MonadTrans t1, MonadTrans t2, Monad (t2 IO), Exception exc) =>
            (exc -> e) -> IO a2 -> t1 (ExceptT e (t2 IO)) a2
fromTry mapErr t = fromIOEither $ first mapErr <$> try t

fromEither :: (MonadTrans t1, MonadTrans t2, Monad m, Monad (t2 m)) =>
            Either e a -> t1 (ExceptT e (t2 m)) a
fromEither = fromIOEither . pure

toEither :: r -> ReaderT r (ExceptT e m) a -> m (Either e a)
toEither env f = runExceptT $ runReaderT f env

runValidatorT :: Monoid s =>
                r -> ReaderT r (ExceptT e (StateT s m)) a -> m (Either e a, s)
runValidatorT conf w = (runStateT $ runExceptT $ runReaderT w conf) mempty

validatorWarning :: Monad m => ValidationWarning -> ValidatorT conf m ()
validatorWarning w = hitler $ modify' (<> w)

validatorError :: Monad m => ValidationError -> ValidatorT conf m ()
validatorError e = hitler $ throwE $ ValidationE e




hitler :: (MonadTrans t, Monad m) => m a -> t m a
hitler = lift

heil :: m (Either e a) -> ExceptT e m a
heil = ExceptT

lift2 = hitler2
lift3 = hitler3
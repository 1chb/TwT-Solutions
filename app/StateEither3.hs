{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-} -- Hmm, ...FunctionalDependencies...

{-# OPTIONS -Wunused-imports #-}

module StateEither3(execStateEither, modify', modifyM, liftEither) where

import Control.Monad.State (State, execState, MonadState (get, put))
import Control.Monad.Trans.Class (MonadTrans(lift))

newtype EitherT e m a = EitherT
  { unEitherT :: m (Either e a)
  } -- deriving Functor

instance Monad m => Functor (EitherT e m) where
  fmap f (EitherT fa) = EitherT $ do
    ea <- fa
    case ea of
      Left e -> return $ Left e
      Right a -> return $ Right $ f a

instance Monad m => Applicative (EitherT e m) where
  pure a = EitherT $ return $ Right a
  EitherT ff <*> EitherT fa = EitherT $ do
    ef <- ff
    case ef of
      Left e -> return $ Left e
      Right f -> do
        ea <- fa
        case ea of
          Left e -> return $ Left e
          Right a -> return $ Right $ f a

instance Monad m => Monad (EitherT e m) where
  EitherT f >>= g = EitherT $ do
    ex <- f
    case ex of
      Left e -> return $ Left e
      Right x -> unEitherT $ g x

execStateEither :: EitherT e (State s) a -> s -> s
execStateEither (EitherT m) = execState m

modify' :: (s -> Either e s) -> EitherT e (State s) ()
modify' f = do
  s0 <- lift get
  case f s0 of
    Left e -> exitEarly e
    Right s1 -> lift $ put $! s1

exitEarly :: Monad m => e -> EitherT e m a
exitEarly e = EitherT $ return $ Left e

instance MonadTrans (EitherT e) where
  lift = EitherT . fmap Right

modifyM :: MonadState s m => (s -> m s) -> m ()
modifyM f = do
  s0 <- get
  s1 <- f s0
  put $! s1

liftEither :: Monad m => Either e a -> EitherT e m a
liftEither = EitherT . return

instance MonadState s m => MonadState s (EitherT e m) where
  get = lift get
  put = lift . put

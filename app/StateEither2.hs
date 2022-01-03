{-# LANGUAGE DeriveFunctor #-}

{-# OPTIONS -Wunused-imports #-}

module StateEither2 where

import Control.Monad.State (State, execState, MonadState (get, put))

newtype StateEither s e a = StateEither
  { unStateEither :: State s (Either e a)
  } -- deriving Functor

instance Functor (StateEither s e) where
  fmap f (StateEither fa) = StateEither $ do
    ea <- fa
    case ea of
      Left e -> return $ Left e
      Right a -> return $ Right $ f a

instance Applicative (StateEither s e) where
  pure a = StateEither $ return $ Right a
  StateEither ff <*> StateEither fa = StateEither $ do
    ef <- ff
    case ef of
      Left e -> return $ Left e
      Right f -> do
        ea <- fa
        case ea of
          Left e -> return $ Left e
          Right a -> return $ Right $ f a

instance Monad (StateEither s e) where
  StateEither f >>= g = StateEither $ do
    ex <- f
    case ex of
      Left e -> return $ Left e
      Right x -> unStateEither $ g x

execStateEither :: StateEither s e a -> s -> s
execStateEither (StateEither m) = execState m

modify' :: (s -> Either e s) -> StateEither s e ()
modify' f = StateEither $ do
  s0 <- get
  case f s0 of
    Left e -> return $ Left e
    Right s1 -> do
      put $! s1
      return $ Right ()

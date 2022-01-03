{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ImportQualifiedPost #-}

{-# OPTIONS -Wunused-imports #-}

module Main where

import StateEither1 qualified as SE1
import StateEither2 qualified as SE2
import StateEither3 qualified as SE

main :: IO ()
main = print $ foldTerminate (\acc x -> if x < 0 then Left acc else Right $ acc + x) 0 [1,2,3,0,5,-5,10]

foldTerminate :: (b -> a -> Either b b) -> b -> [a] -> b
foldTerminate f accum0 list0 =
    SE.execStateEither (mapM_ go list0) accum0
  where
    -- go x = SE.modify' (\accum -> f accum x)
    go x = SE.modifyM (\accum -> SE.liftEither $ f accum x)

-- newtype State2       s   a = State2       (s -> (s,          a))
-- newtype StateEither2 s e a = StateEither2 (s -> (s, Either e a))
-- newtype StateEither3 s e a = StateEither3 (State s (Either e a))

data Kanske a = Ingenting | Bara a

instance Functor Kanske where
  fmap f = \case
    Ingenting -> Ingenting
    Bara a -> Bara $ f a

instance Applicative Kanske where
  pure = Bara
  Bara f <*> Bara a = Bara $ f a
  _ <*> _ = Ingenting

instance Monad Kanske where
  Bara a >>= f = f a
  Ingenting >>= _f = Ingenting

newtype KanskeT m a = KanskeT
  { runKanskeT :: m (Kanske a)
  } deriving ()

instance (Functor f) => Functor (KanskeT f) where
  fmap f (KanskeT k) = KanskeT $ undefined

instance (Applicative f) => Applicative (KanskeT f) where
  pure = KanskeT . pure . Bara
  (<*>) = undefined

instance (Monad m) => Monad (KanskeT m) where
  x >>= f = KanskeT $ runKanskeT x >>= kanske (return Ingenting) (runKanskeT . f)

kanske :: b -> (a -> b) -> Kanske a -> b
kanske b ab = \case
  Ingenting -> b
  Bara a -> ab a

-- class Monad m => MonadKanske m

{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE BangPatterns #-}

{-# OPTIONS -Wunused-imports #-}

module StateEither1 where

newtype StateEither s e a = StateEither
  { runStateEither :: s -> (s, Either e a)
  } --  deriving Functor

instance Functor (StateEither s e) where
  fmap f (StateEither g) = StateEither $ \s0 ->
    case g s0 of
      (s1, Left e) -> (s1, Left e)
      (s1, Right x) -> (s1, Right $ f x)

instance Applicative (StateEither s e) where
  pure a = StateEither (\s -> (s, Right a))
  StateEither ff <*> StateEither fa = StateEither $ \s0 ->
    case ff s0 of
      (s1, Left e) -> (s1, Left e)
      (s1, Right f) ->
        case fa s1 of
          (s2, Left e) -> (s2, Left e)
          (s2, Right a) -> (s2, Right (f a))

instance Monad (StateEither s e) where
  return = pure
  StateEither f >>= g = StateEither $ \s0 ->
    case f s0 of
      (s1, Left e) -> (s1, Left e)
      (s1, Right x) -> runStateEither (g x) s1

execStateEither :: StateEither s e a -> s -> s
execStateEither m = fst . runStateEither m

modify' :: (s -> Either e s) -> StateEither s e ()
modify' f = StateEither $ \s0 ->
  case f s0 of
    Left e -> (s0, Left e)
    Right !s1 -> (s1, Right ())

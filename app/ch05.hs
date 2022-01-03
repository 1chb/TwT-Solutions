{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

import Data.Kind (Constraint, Type)

data Expr a where
  LitInt :: Int -> Expr Int
  LitBool :: Bool -> Expr Bool
  Add :: Expr Int -> Expr Int -> Expr Int
  Not :: Expr Bool -> Expr Bool
  If :: Expr Bool -> Expr a -> Expr a -> Expr a
  Eq :: Eq a => Expr a -> Expr a -> Expr Bool

deriving instance Show (Expr a)

eval :: Expr a -> a
eval = \case
  LitInt i -> i
  LitBool b -> b
  Add e1 e2 -> eval e1 + eval e2
  Not e -> not $ eval e
  If c t f -> eval $ if eval c then t else f
  Eq e1 e2 -> eval e1 == eval e2

ex1 = LitInt 1

ex2 = Add ex1 ex1

ex3 = Eq ex2 (LitInt 4)

ex4 = If ex3 (LitInt 1) (Add ex2 $ LitInt 17)

data HList (ts :: [Type]) where
  HNil :: HList '[]
  (:#) :: t -> HList ts -> HList (t ': ts)

infixr 5 :#

type family All (c :: Type -> Constraint) (ts :: [Type]) :: Constraint where
  All c '[] = ()
  All c (t ': ts) = (c t, All c ts)

instance All Eq ts => Eq (HList ts) where
  HNil == HNil = True
  (a :# as) == (b :# bs) = a == b && as == bs

instance (All Eq ts, All Ord ts) => Ord (HList ts) where
  compare HNil HNil = EQ
  compare (a :# as) (b :# bs) = compare a b <> compare as bs

instance All Show ts => Show (HList ts) where
  showsPrec p HNil = showString "HNil"
  showsPrec p (a :# as) = showParen (p <= 5) $ shows a . showString " :# " . shows as

hLength :: HList ts -> Int
hLength HNil = 0
hLength (_ :# ts) = 1 + hLength ts

hHead :: HList (t ': _ts) -> t
hHead (t :# _) = t

hl1 = True :# HNil

hl2 = Just "hello" :# hl1

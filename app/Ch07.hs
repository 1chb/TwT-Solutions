{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Main where

import Data.Foldable (asum)
import Data.Kind (Constraint, Type)
import Data.Maybe (fromMaybe)
import Data.Typeable (cast)
import Type.Reflection (Typeable)
import GHC.IO.Handle.Text (memcpy)

main :: IO ()
main = do
  main_7_1
  main_7_1_1
  main_7_1_2

-- 7.1 Existential Types and Eliminators

main_7_1 :: IO ()
main_7_1 = do
  print hlist
  print hlistG
  print $ elimAny show (Any 5)
  print $ elimAny show (Any LT)

data Any = forall a. Show a => Any a

-- deriving instance Show Any
instance Show Any where
  show any = "Any " ++ elimAny show any

hlist :: [Any]
hlist = [Any 3, Any True]

data AnyG where
  AnyG :: Show a => a -> AnyG

deriving instance Show AnyG

hlistG :: [AnyG]
hlistG = [AnyG 4, AnyG False]

elimAny :: (forall a. Show a => a -> r) -> Any -> r
elimAny f (Any a) = f a

-- 7.1.1 Dynamic Types

main_7_1_1 = do
  print $ fromDynamic @Ordering dynO
  print $ fromDynamic @String (pyPlus (Dynamic @Int 4) (Dynamic " minutes"))

dynO :: Dynamic
dynO = Dynamic EQ

data Dynamic where
  Dynamic :: Typeable t => t -> Dynamic

elimDynamic ::
  (forall a. Typeable a => a -> r) ->
  Dynamic ->
  r
elimDynamic f (Dynamic a) = f a

fromDynamic :: Typeable a => Dynamic -> Maybe a
fromDynamic = elimDynamic cast

liftD2 ::
  forall a b r.
  (Typeable a, Typeable b, Typeable r) =>
  Dynamic ->
  Dynamic ->
  (a -> b -> r) ->
  Maybe Dynamic
liftD2 d1 d2 f = fmap Dynamic . f <$> fromDynamic @a d1 <*> fromDynamic @b d2

pyPlus :: Dynamic -> Dynamic -> Dynamic
pyPlus a b =
  fromMaybe (error "bad types for pyPlus") $
    asum
      [ liftD2 @String @String a b (++),
        liftD2 @Int @Int a b (+),
        liftD2 @String @Int a b $ \strA intB -> strA ++ show intB,
        liftD2 @Int @String a b $ \intA strB -> show intA ++ strB
      ]

-- 7.1.2 Generalized Constraint Kinded Existenstials

main_7_1_2 :: IO ()
main_7_1_2 = do
  print "."

data Has (c :: Type -> Constraint) where
  Has :: c t => t -> Has c

elimHas :: (forall a. c a => a -> r) -> Has c -> r
elimHas f (Has a) = f a

type HasShow = Has Show

type Dynamic1 = Has Typeable

isMempty :: (Monoid a, Eq a) => a -> Bool
isMempty a = a == mempty

-- constraint synonym:
class    (Monoid a, Eq a) => MonoidEq a
instance (Monoid a, Eq a) => MonoidEq a
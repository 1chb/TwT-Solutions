{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}
module Main where
import Data.Functor.Identity (Identity(runIdentity))
import Control.Monad.IO.Class (liftIO, MonadIO)

main :: IO ()
main = do
  print relStr
  print relStrM
  print relStrMI
  relStrMIO >>= print

-- 6.4 The Continuation Monad

cont :: a -> (forall r. (a -> r) -> r)
cont a callback = callback a

runCont :: (forall r. (a -> r) -> r) -> a
runCont f =
  let callback = id
  in f callback

newtype Cont a = Cont
  { unCont :: forall r. (a -> r) -> r
  }

instance Functor Cont where
  fmap f (Cont c) = Cont $ \br -> c $ br . f

instance Applicative Cont where
  pure a = Cont $ \ar -> ar a
  Cont f <*> Cont a = Cont $ \br -> f $ \ab -> a $ br . ab

instance Monad Cont where
  Cont m >>= f = Cont $ \c ->
    m $ \a -> unCont (f a) c

type ContI = ContT Identity

newtype ContT m a = ContT
  { unContT :: forall r. (a -> m r) -> m r
  }

runContI :: (forall r. (a -> Identity r) -> Identity r) -> a
runContI f = runIdentity (f pure)

runContIO :: (forall r. (a -> IO r) -> IO r) -> IO a
runContIO f = f pure

instance Functor (ContT m) where
  fmap f (ContT c) = ContT $ \br -> c $ br . f

instance Applicative (ContT m) where
  pure a = ContT $ \ar -> ar a
  ContT f <*> ContT a = ContT $ \br ->
    f $ \ab ->
          a $ br . ab 

instance Monad (ContT m) where
  ContT m >>= f = ContT $ \c ->
    m $ \a -> unContT (f a) c

instance MonadIO m => MonadIO (ContT m) where
  liftIO = lift . liftIO

lift :: Monad m => m r -> ContT m r
lift m = ContT $ \mr -> m >>= mr

wVN :: (Double -> r) -> r
wVN f = f 1.0

wTS :: (Int -> r) -> r
wTS f = f 1532083362

wOS :: (String -> r) -> r
wOS f = f "linux"

relStr :: String
relStr =
  wVN $ \vn ->
    wTS $ \ts ->
      wOS $ \os ->
        os ++ "-" ++ show vn ++ "-" ++ show ts

relStrM :: String
relStrM = runCont $ unCont $ do
  vn <- Cont wVN
  ts <- Cont wTS
  os <- Cont wOS
  return $ os ++ "-" ++ show vn ++ "-" ++ show ts

relStrMI :: String
relStrMI = runContI $ unContT $ do
  vn <- ContT wVN
  ts <- ContT wTS
  os <- ContT wOS
  return $ os ++ "-" ++ show vn ++ "-" ++ show ts

relStrMIO :: IO String
relStrMIO = runContIO $ unContT $ do
  vn <- liftIO (putChar 'V') >> ContT wVN
  ts <- liftIO (putChar 'T') >> ContT wTS
  os <- liftIO (putChar 'O') >> ContT wOS
  liftIO $ putChar 'S'
  return $ os ++ "-" ++ show vn ++ "-" ++ show ts

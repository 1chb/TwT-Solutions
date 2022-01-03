{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}

{-# OPTIONS -Wunused-imports #-}

module Main where

import Control.Monad (forM_, when)
import Control.Monad.Reader (ReaderT (runReaderT), asks)
import Control.Monad.State (StateT, evalStateT, get, put)
import Control.Monad.Writer (WriterT, execWriterT, tell)
import IOEnv (MonadIO (liftIO))
import System.Directory (doesDirectoryExist, getDirectoryContents)
import System.FilePath ((</>))

main :: IO ()
main = runApp (constrainedCount 0 "/home/brolin/Repo") 2 >>= print . getAppResult

type App = StateT AppState (ReaderT AppConfig (WriterT AppResult IO))

newtype AppConfig = AppConfig {getMaxDepth :: Int}

newtype AppResult = AppResult {getAppResult :: [(FilePath, Int)]} deriving (Semigroup, Monoid)

newtype AppState = AppState {getDeepestReached :: Int}

runApp :: App a -> Int -> IO AppResult
runApp k maxDepth =
  let config = AppConfig maxDepth
      state = AppState 0
   in execWriterT (runReaderT (evalStateT k state) config)

constrainedCount :: Int -> FilePath -> App ()
constrainedCount curDepth path = do
  contents <- liftIO . listDirectory $ path
  maxDepth <- asks getMaxDepth
  tell $ AppResult [(path, length contents)]
  forM_ contents $ \name -> do
    let newPath = path </> name
    isDir <- liftIO $ doesDirectoryExist newPath
    when (isDir && curDepth < maxDepth) $ do
      let newDepth = curDepth + 1
      st <- get
      when (getDeepestReached st < newDepth) $
        put st {getDeepestReached = newDepth}
      constrainedCount newDepth newPath

listDirectory :: FilePath -> IO [String]
listDirectory = fmap (filter notDots) . getDirectoryContents
  where
    notDots p = p /= "." && p /= ".."

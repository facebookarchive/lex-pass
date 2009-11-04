module TaskPool where

import Control.Exception
import Control.Concurrent
import Control.Monad
import Prelude hiding (catch)

type TaskChan a = Chan (Maybe (IO a))

worker :: TaskChan a -> Chan () -> IO ()
worker q doneChan = do
  taskMb <- readChan q
  case taskMb of
    Nothing -> writeChan doneChan ()
    Just task -> errorOnExceptions task >> worker q doneChan

errorOnExceptions :: IO a -> IO a
errorOnExceptions task = catchAll task (error . show)

catchAll :: IO a -> (SomeException -> IO a) -> IO a
catchAll = catch

taskPool :: Int -> [IO ()] -> IO ()
taskPool n tasks = do
  q <- newChan
  doneChan <- newChan
  tids <- replicateM n . forkIO $ worker q doneChan
  mapM_ (writeChan q) $ map Just tasks ++ replicate n Nothing
  replicateM_ n $ readChan doneChan


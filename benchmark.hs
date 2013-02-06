{-# LANGUAGE BangPatterns #-}
import Control.Monad
import Data.SmallHash
import Data.Time.Clock
import System.Environment
import qualified Control.Exception as E

timeIt :: IO a -> IO (NominalDiffTime, Either E.SomeException a)
timeIt act = do
  before <- getCurrentTime
  res <- E.try act
  after <- getCurrentTime
  return $ (after `diffUTCTime` before, res)

main :: IO ()
main = do
  args <- getArgs
  num <-
    case fmap read args of
    [num] -> return num
    _ -> fail "Usage: benchmark <count>"
  tbl <- new
  (time, _) <-
    timeIt . forM_ [1..num] $ \ !i -> do
    insert tbl i i
  putStrLn $ "Done inserting: " ++ show time
  print =<< find tbl 100

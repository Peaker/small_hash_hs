{-# LANGUAGE BangPatterns #-}
import Control.Monad
import Data.SmallHash
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  num <-
    case fmap read args of
    [num] -> return num
    _ -> fail "Usage: benchmark <count>"
  tbl <- new
  forM_ [1..num] $ \ !i -> do
    insert tbl i i
  putStrLn "Done inserting"
  print =<< find tbl 100

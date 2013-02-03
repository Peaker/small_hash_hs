{-# LANGUAGE BangPatterns #-}
import Control.Monad
import Data.SmallHash

main :: IO ()
main = do
  tbl <- new
  forM_ [1..70000] $ \ !i -> do
    insert tbl i i
  putStrLn "Done inserting"
  print =<< find tbl 100

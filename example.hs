import Data.SmallHash

main :: IO ()
main = do
  tbl <- new
  putStrLn "Inserting 5 -> Hello"
  insert tbl 5 1
  putStrLn "Finding 5"
  print =<< find tbl 5
  putStrLn "Finding 6"
  print =<< find tbl 6
  putStrLn "Deleting 5"
  delete tbl 5
  putStrLn "Finding 5"
  print =<< find tbl 5
  putStrLn "Finding 6"
  print =<< find tbl 6

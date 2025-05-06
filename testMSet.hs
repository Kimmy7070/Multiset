module TestMSet where

import MultiSet
import System.IO

-- Read a file and compute MSet of ciao words
readMSet :: FilePath -> IO (MSet String)
readMSet path = do
  content <- readFile path
  let wordsList = words content
  return $ MS (countOccurrences $ map ciao wordsList)
  where
    countOccurrences [] = []
    countOccurrences (w:ws) =
      let (count, rest) = foldl (\(c, acc) w' -> 
            if w' == w then (c + 1, acc) 
            else (c, w' : acc)) 
            (1, []) 
            ws
      in (w, count) : countOccurrences rest

-- Write MSet to a file
writeMSet :: (Show a) => FilePath -> MSet a -> IO ()
writeMSet path (MS xs) = writeFile path (unlines $ map (\(x, c) -> show x ++ "-" ++ show c) xs)

-- Main function to test multisets
main :: IO ()
main = do
  m1 <- readMSet "test/anagram.txt"
  m2 <- readMSet "test/anagram_s1.txt"
  m3 <- readMSet "test/anagram_s2.txt"
  m4 <- readMSet "test/margana2.txt"
  
  -- Check m1 and m4 have same elements but are not equal
  putStrLn "Checking m1 and m4:"
  putStrLn $ "Same elements? " ++ show (elems m1 == elems m4)
  putStrLn $ "Equal multisets? " ++ show (m1 == m4)
  
  -- Check m1 is the union of m2 and m3
  putStrLn "Checking m1 == m2 union m3:"
  putStrLn $ "Result: " ++ show (m1 == union m2 m3)
  
  -- Write output files
  writeMSet "anag-out.txt" m1
  writeMSet "gana-out.txt" m4
  putStrLn "Output files written."
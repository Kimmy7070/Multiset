-- MultiSet.hs
module MultiSet where

import Data.List (sort, find)
import Data.Char (toLower)

-- Data type representing a multiset
data MSet a = MS [(a, Int)]
  deriving (Show)

-- Helper function to sort and lowercase a string (ciao)
ciao :: String -> String
ciao = sort . map toLower

-- 1. Empty multiset
empty :: MSet a
empty = MS []

-- 2. Add an element to the multiset
add :: (Ord a) => MSet a -> a -> MSet a
add (MS []) x = MS [(x, 1)]
add (MS ((y, c):ys)) x
  | x == y    = MS ((y, c + 1) : ys)
  | otherwise = let (MS rest) = add (MS ys) x
                in MS ((y, c) : rest)

-- 3. Get occurrence count of an element
occs :: (Ord a) => MSet a -> a -> Int
occs (MS []) _ = 0
occs (MS ((y, c):ys)) x
  | x == y    = c
  | otherwise = occs (MS ys) x

-- 4. Get all unique elements
elems :: MSet a -> [a]
elems (MS xs) = map fst xs

-- 5. Check if mset1 is a subset of mset2
subeq :: (Ord a) => MSet a -> MSet a -> Bool
subeq (MS []) _ = True
subeq (MS ((x, c):xs)) (MS ys) =
  case find ((== x) . fst) ys of
    Just (_, cnt) -> c <= cnt && subeq (MS xs) (MS ys)
    Nothing       -> False

-- 6. Union of two multisets
union :: (Ord a) => MSet a -> MSet a -> MSet a
union (MS xs) (MS ys) = MS $ mergeSorted xs ys
  where
    mergeSorted [] ys' = ys'
    mergeSorted xs' [] = xs'
    mergeSorted (x@(a, c1):xs') (y@(b, c2):ys')
      | a < b     = x : mergeSorted xs' (y : ys')
      | a > b     = y : mergeSorted (x : xs') ys'
      | otherwise = (a, c1 + c2) : mergeSorted xs' ys'

-- 7. Eq instance: compare elements and counts
instance (Ord a) => Eq (MSet a) where
  (MS xs) == (MS ys) = sort xs == sort ys

-- 8. Foldable instance: ignore counts
instance Foldable MSet where
  foldr f z (MS xs) = foldr (\(x, _) -> f x) z xs

-- 9. mapMSet: apply function to elements (maintain invariant)
mapMSet :: (Ord b) => (a -> b) -> MSet a -> MSet b
mapMSet f (MS xs) = MS $ combine $ map (\(x, c) -> (f x, c)) xs
  where
    combine [] = []
    combine ((x, c):xs') =
      case find ((== x) . fst) xs' of
        Just (y, c') -> (x, c + c') : combine (filter ((/= x) . fst) xs')
        Nothing      -> (x, c) : combine xs'
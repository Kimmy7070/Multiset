module MultiSet where

import Data.List (sort, find, intercalate)
import Data.Char (toLower)

-- Data type representing a multiset
data MSet a = MS [(a, Int)]
  deriving (Show)

-- Normalize a string to its "ciao" form (sorted lowercase)
ciao :: String -> String
ciao = sort . map toLower

-- Create an empty multiset
empty :: MSet a
empty = MS []

-- Add an element to the multiset
add :: (Ord a) => MSet a -> a -> MSet a
add (MS []) x = MS [(x, 1)]
add (MS ((y, c):ys)) x
  | x == y    = MS ((y, c + 1) : ys)
  | otherwise = let (MS rest) = add (MS ys) x
                in MS ((y, c) : rest)

-- Get the number of occurrences of an element
occs :: (Ord a) => MSet a -> a -> Int
occs (MS []) _ = 0
occs (MS ((y, c):ys)) x
  | x == y    = c
  | otherwise = occs (MS ys) x

-- Get all unique elements in the multiset
elems :: MSet a -> [a]
elems (MS xs) = map fst xs

-- Check if mset1 is a subset of mset2 (multiplicities <=)
subeq :: (Ord a) => MSet a -> MSet a -> Bool
subeq (MS []) _ = True
subeq (MS ((x, c):xs)) (MS ys) =
  case find ((== x) . fst) ys of
    Just (_, cnt) -> c <= cnt && subeq (MS xs) (MS ys)
    Nothing       -> False

-- Union of two multisets (sum multiplicities)
union :: (Ord a) => MSet a -> MSet a -> MSet a
union (MS xs) (MS ys) = MS $ mergeSorted (sort xs) (sort ys)
  where
    mergeSorted [] ys' = ys'
    mergeSorted xs' [] = xs'
    mergeSorted (x@(a, c1):xs') (y@(b, c2):ys')
      | a < b     = x : mergeSorted xs' (y : ys')
      | a > b     = y : mergeSorted (x : xs') ys'
      | otherwise = (a, c1 + c2) : mergeSorted xs' ys'

-- Eq instance: compare elements and multiplicities
instance (Ord a) => Eq (MSet a) where
  (MS xs) == (MS ys) = sort xs == sort ys

-- Foldable instance: ignore multiplicities
instance Foldable MSet where
  foldr f z (MS xs) = foldr (\(x, _) acc -> f x acc) z xs

-- mapMSet: apply function to elements (maintains invariant)
mapMSet :: (Ord b) => (a -> b) -> MSet a -> MSet b
mapMSet f (MS xs) = MS $ combine $ map (\(x, c) -> (f x, c)) xs
  where
    combine [] = []
    combine ((x, c):xs') =
      case find ((== x) . fst) xs' of
        Just (y, c') -> (x, c + c') : combine (filter ((/= x) . fst) xs')
        Nothing      -> (x, c) : combine xs'

-- Why Functor is not possible: mapMSet requires Ord constraint
-- Functor requires fmap :: (a -> b) -> f a -> f b (no constraints on b)
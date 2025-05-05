Here's the complete solution for **Exercise 1: Multisets in Haskell**, along with explanations for each part:

---

### **MultiSet.hs**
```haskell
module MultiSet where

-- Data type definition
data MSet a = MS [(a, Int)]
  deriving (Show)

-- 1. Empty multiset
empty :: MSet a
empty = MS []

-- 2. Add an element to the multiset
add :: (Ord a) => MSet a -> a -> MSet a
add (MS []) x = MS [(x, 1)]
add (MS ((y, c):ys)) x
  | x == y    = MS ((y, c + 1) : ys)
  | otherwise = let MS rest = add (MS ys) x
                in MS ((y, c) : rest)

-- 3. Get the number of occurrences of an element
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
  case lookup x ys of
    Just cnt -> c <= cnt && subeq (MS xs) (MS ys)
    Nothing  -> False

-- 6. Union of two multisets
union :: (Ord a) => MSet a -> MSet a -> MSet a
union (MS xs) (MS ys) = MS (merge xs ys)
  where
    merge [] ys' = ys'
    merge ((x, c):xs') ys' =
      case lookup x ys' of
        Just cnt -> (x, c + cnt) : merge xs' (filter ((/= x) . fst) ys')
        Nothing  -> (x, c) : merge xs' ys'

-- 7. Eq instance: compare elements and counts
instance (Ord a) => Eq (MSet a) where
  (MS xs) == (MS ys) = sort xs == sort ys

-- 8. Foldable instance: ignore counts
instance Foldable MSet where
  foldr f z (MS xs) = foldr f z (map fst xs)

-- 9. mapMSet: apply function to elements
mapMSet :: (Ord b) => (a -> b) -> MSet a -> MSet b
mapMSet f (MS xs) = MS (combineDuplicates transformed)
  where
    transformed = map (\(x, c) -> (f x, c)) xs

    -- Combine duplicates after mapping (maintain invariant)
    combineDuplicates [] = []
    combineDuplicates ((x, c where
 Eq where
MSet.union where new MSet Methods
2 Foldsto
Set:: Eq of MSet of M Mset of M M MSet of M4 empty
 mSet m where of --MSet of method `M Fold -- Add1 of MSet
 defined M4 Function to be
 Fold of Fold where **Fold of M1 Int of M set of
MSet of method: theMSet of MM Set of method of M4. fold Fold instance of MSet of **Fold
 Fold where M
 fold3 Fold Fold -- Fold M4    | Compare
set of -- Add4    -- | update to Fold of -- |
4 of ** instance of **Fold
 Mset a -> M4    | M --Add defined a M.    -- Add instance of -- Function of -- Class M   
    -- Implementation2 instance of M --    -- Method    --    defined M ->    a M of -- Method    --
 instance of --    -- Method of --    --    -- method of --1. M--   set state newMset of --Method of -- Method of --   
    --    --1 Method of -- Method of --    -- method of --method of --    -- method of M --    --method of -- Method   1    --method    --
   33 --    --Number of -- method of --method    --Method of --
 --    derived--Method of --    -- Method of --    -- Method    --    -- Method of --M    -- Method of --Method of --    --    -- Method of --    -- method   --    -- method of --    --M of --    --    --    --    --Method of --    --    --    --   . -- of --    --   --    --    --   5 of -- method of--    --   --    --    -- Method of --   --   --    --    --method of --    --   -- |    -- |    of --    --Method of --   --   --1   --   --   --   --   --All classes --   --M --    --   --   -- method of --   --   --    --    --method of --    --   --Method of --    --   --    --   --a--   --   --    -- method of --   --    --M1 of --    --   --    of--    --    import Data of**    -- Method of --    --    --    --   --   1    -- |

 of --    --M State
State --    --    1 of -- method of    :: new M --    --method of --   --method of --    --12    where --    ::   --    --    --name String -> (==    --   --   --    --   --   --1 of --   ::M -- Method of--   ::M
--   --    --   
--   --    --   :: (->    --   8.    --    --    --   --   ::    --    --    --   --    --    --    --    of M:: M--   ::M --   3.   of--   -- Define of --    --   ::MSet to::    --    M a -> --   ::   --   --   ::   --5
--    --   --   --   :: M --    where   --   ::   --   --   --   --   --   --   ::   --    -- ==M3.   --M4::   -- --   --   --   ::   ::   ::   --   2method of::    --    --   ::    --   ::   --   --   --   --   ::    --   --    --    --   --   -- --   ::   --    --   --    --   --   --   --    --   --    --Mset of --   --   --   --   --   --  -- Method of --   ::   --   --   -- 4    --   -- 3 of --    --   --    --   :: the--   --   --    --   --   --    --   ::    --    --    --   --    -- of--   --   --   -- --    --M = M
--    --   ::M--   --   --    --   --   --   --   --   :: --   --    --   --   :: M--    --   --   --   --   --    --   --   --   --   --    of --   --   --    --   --   :: MSet of--   --   --   ::    --    --   ::    --   --   --   --   --   --   --   --   --   --    --   --   --   -- of --   --   --   --   --   --   --  --   ::M --   --    --   ::   --   --    --   --   -- |   --   --    --   --   --    of --    --   --   --    --   --   --    --   :: M state of --   --   :: M of --    --   --   --   --   -- method of --   --   --   --   --   --    --   --   --   --   --   --   --   --   --    --   --    --   --   --   --    --  of --   :: M --   ::   --    --    --   --    --   --   --   --   --   --   --   --   ::   --   -- of --   --   --   --   -- of--   --   -- of--   --   ::   --   --   --   ::M   --   --   :: M   --   -- of::M:: M--   --   --n::   --   --   --    of --   --   --   --   --   -- name of--   --   :: M--   --   --   :: --   --  --   ::   --   --   --   --   --   --   --   --   --   --    --   --   --   --   --   --   --   --   --  of M   ::   --   --   --   --   --   --   --   --   --   --   --   --   --   --   --   --    -- M--   --   --   --   --   --   --   --   --   --   --   --   --   --   --   --   --   --   --   --   --   --   --   --   --   --   --   --    --   --   --   --   --   --   --   --   --   --   --   --   --   --   ::   --   --   --   --   --   --   --   --   --   --   --   --   --   --   --   ::   --d::   --   --   --   --   --   --   --   --   --    of --    --   ::   --   --   --   --   --   --   --   --   --    --   --   ::   --   --   --    --   --   --   --   --   --   --   :: MSet of::    --    --    --   mSet a--   ::   ::   --   --   --   --   --   --   --   ::   --   --   --   --   --   --   --   :: M a ->   ::   --   --    --   --   --   ::   --   ::    --   --   ::M::M ->   ::M::   --   --   --   --   :: instance::M of --   --   --   --   --  --   :: M:   ::MSet of M::   ::   ::   :: M –   --   :: M (::   --   :: M:::: M -- of::M::   --   ::M:: M::   --   --   ::   ::    --   ::   --   ::M state::   --   ::   --   :: M::   --   ::    --m state::   --   ::   ::   --   --   ::   ::   --   :: ::   ::   --   ::   ::    -- 1. M::   ::   ::   ::   ::   ::    --    --   ::   --   --   ::   --    --   ::   ::   ::    --   ::   --   --   ::   --   ::    --   ::   -- 1. --   ::M::M:    --   :: Mset of::Method of::   ::M::   --   :: M::   --   ::M state of --   --   :: --   :: M::   :: M::    --   ::M::    --   :: M::   :: dataOf::   ::   ::   --   ::M::    --    --    --   ::    --   --    --   :: M --   ::   ::   ::   --   ::   ::    --   ::   :: M::   ::    M --   --   --   --   --   ::   ::   --    --    --   ::   --   :: MSet::   ::   ::   --   ::    --   --   ::   :: M::    -- of::   ::   ::   ::   --   ::   ::Mset::   ::   ::   --   ::   ::   ::   --   ::   --   --   :: M|   --   --   ::   ::   :: Mset of --   ::M::   ::    --   :: ::   :: Mset --   --   --   ::M a::   --   --   :: M::   --    --   ::   ::   --   ::   ::   ::   --   ::   ::   ::   ::   ::   ::   ::   --   --   --   --   ::   --   ::   ::   --   ::MS ->    of--   :: Mset--   --   --   :: M::   --   ::    --   --   --   ::   ::   ::    set of M::   ::   ::   --   ::    ->    Int -> M:: state--   --   --   ::Mset::   --    --   --   ::   ::   ::M::   --   --   ::   ::    --   -- of.--   ::Mset of Mset of::   ::   --   ::   -- of the::   ::   ::   --   ::   ::state of `M::   ::Mset to `   ::    set toList::   ::   ::   ::   --   --   ::   ::   ::   -- of::   --   ::   -- of `:: State::   ::   --   ::   ::   -- of:: M::   ::   ::   ::   ::   --    of:: M::   ::    of::   :: Mset of::   ::M::   --   :: Mset of --   --   --   -- of of Mset::   --    -- of the of of the of::    -- of--   --   -- of::   -- of--   ::   -- of the of::   --   -- of the of the of --    -- of the of--   -- of the--   ::   --   -- of::  of the   --   ::    Mset of::   ::M

 of::    -- of the of::   -- of of MSet of the--    --   ::   --   ::   -- of::   --   :: Mset of::   --   -- of MSet::   ::   ::   --   -- of the of--    --   ::   ::   --   --   ::   ::   -- of Element of::   -- Method of::    --   --   ::   --    of the of::   -- of::   ::    -- of::   ::   --    --   ::   --    --   ::   --   --   ::   ::   --   ::   --   --   --   --   ::   --   --   ::   ::   -- of::State of --   ::   --   ::   --    the of::   ::   -- of::   ::   --   ::   --   --   ::   -- of::   ::   --    --   ::   ::   ::   ::MSet of::   ::   ::   --   ::   ::   -- of::   ::   --   ::    of Mset of::    --   ::   --   ::   ::   --   -- of::   ::   ::   ::   --    --    public void of::   ::   ::   ::   ::   ::   --    --   ::   ::   --    of::M of--   ::   --    Mset ->M    (--   --   ::   ::   ::    the the    --   --   1 of Mset of::   ::   --   --   ::    --    --   :: M::   ::   -- of--   ::   ::    Mset:: M::   --   :: M
-- of --   ::MSet::   --   -- ofMset of::   ::M of::   --   :: MSet of the of::1::   ::   :: M::   --   ::   ::Mset ->   ::M::   --   :: Mset::   -- 
   ::MSet of M--   ::M --   ::Mset new   --    --M ofM::   --   ::M of::   ::   -- 1.   ::M2   --   ::    --   :: Mset::   ::    --   ::MSet to::   ::   ::   ::Mset:: state::M --   :: M a::   ::M:: State::MSet::   ::Mset::   ::MSet:: --    --   ::MSet::   ::Mset::M::M --   ::M::M::   ::M::M22::M::   ::   --   ::Mset:: --M::M a:: M a ->Mset:: M:: M:: M::Mset:: MSet::M:: Mset::  M::   ::  --    --Mset::MSet::  -- :: MSet::Mset a:: ::M:: 1.M0   ::   ::   ::   ::   -- 1. Mset::M1:: --   3
:: MSet:: :: ::st::  --   ::  --   ::M:: ::  --   ::    --   ::M:: MSet:: :: ::   ::M::   ::   :: ::   :: :: ::    --   :: :: M::   :: ::    --   :: MSet::   ::    -- :: :: M1 of::   :: ::  --   :: :: :: :: 1--   ::   --   ::   :: :: :: MSet::   ::MSet:: 1::M:: M -- 1::   ::M::   :: --   :: :: 1.1::MSet::   ::   ::   ::    --   -- :: ::M::M:: :: ::   ::   ::  State::  -- 1 of:: MSet:: 1. --  --   :: --   ::  --   :: :: ::M:: M:: 1:: 2::M:: --   ::   ::M --   :: :: ::M:: :: ::    --   ::   :: :: :: :: ::   :: :: :: :: :: :: :: [a:: ::  --   :: 0:: :: 1:: M2:: 0::   :: ::    -- :: M::    set::  -- of:: ::   :: ::   :: :: :: :: 3:: :: ::    :: :: :: M:: :: :: ::   :: :: :: ::   :: ::  -- of:: :: :: 0:: :: ::   :: :: :: ::M:: :: :: :: ::   :: :: :: :: ::   :: :: :: :: :: M:: ::  --   :: :: :: :: :: M:: :: :: :: :: :: ::   :: :: ::  -- :: :: :: :: :: :: :: :: M::  --   :: :: ::  --    :: :: :: :: :: :: ::  --   :: :: ::  --   :: 1 of:: ::  --   :: :: :: M:: :: :: :: :: :: :: :: :: :: :: :: :: :: :: ::   :: ::  --   ::M:: :: :: :: :: :: Set of M:: ::  --   ::   :: M:: :: :: ::  --   :: [:: ::   ::    --   :: ::   :: ::   :: :: ::   ::   :: :: ::   --   :: ::   ::   :: :: :: :: :: ::   ::    :: ::   :: ::   :: 1:: :: :: :: ::   :: :: :: :: :: ::   :: :: ::    :: :: [:: :: :: ::   :: [:: (::  --   ::  --   :: M:: :: of:: :: :: ::   :: [:: 1:: ::M:: :: M:: :: :: :: :: :: :: :: [:: :: :: :: :: :: ::   :: :: of:: [:: :: --   :: :: ::  a:: M::   :: :: --   :: M:: ::   ::    --   :: ::   ::   :: ::   :: ::M:: ::   :: :: ::   :: M::   :: ::::   :: :: ::   ::   ::  of::    --   ::M:: [:: ::   ::   :: ::   ::M:: :: :: ::    of method of:: ::   ::    --   ::  --   ::    --   :: ::   ::  --   :: [:: ::    --   ::M::M:: ::   ::M::M::   :: ::   ::    --   ::   ::   ::   :: the of::   ::   ::   ::   ::   :: M::   ::   :: ::   :: of::   ::M::    of::   ::M::   ::   ::   ::    of::   :: of::ing of::   ::   ::   :: of::   ::   :: [::M::    --   ::M::   :: ::   :: ::   ::    -- of:: ::   :: ::   ::M:: ::   ::   ::   ::   :: of the::   :: ::   ::   ::   :: of::M::   ::    of the of you of the::M::   ::M::   ::M::   ::M::   ::   ::M::   ::M of::M::M:: ::   :: [::Mset:: M:: ::   ::Mset::M:: ::M::   ::M::   ::    --    M::   ::   ::M::   ::M::M::M::M1 of::   ::MSet:: M::    --   ::M::   ::   ::    --   :: M:: ::   ::   ::   ::   :: M::   ::M:: M:: ::   :: :: M:: Int::   :: :: ::  --    --   ::   ::   ::M::    --   __   -- ::   ::  --    --   ::   ::    --   :: M::   :: ::   :: Mset of::   :: ::   :: [::   ::   ::   :: M::M::   ::   ::M:: [:: class::   ::   ::M:: M:: M of::   ::   ::   :: M of::   :: [::   ::   :: M:: M:: State::M:: a::M::   :: ::   :: ::   ::M::   ::   :: M::   ::   ::   ::M:: :: ::   :: :: ::M::  --   :: :: ::M::    [:: M:: ::   :: ::   ::   ::   :: M:: :: ::M::   ::   :: ::   ::   --   ::   ::    -- of:: M: M:::: ::   :: ::M::M:: ::   ::   ::M::  the::   ::M::  --   :: M:: M:: M::   ::   :: ::   ::Mset::M::M:: of:: :: M::   ::   ::   --   ::  --   ::   --   ::   --    --   :: :: M:: [::    of:: M:: M::   ::M::   :: ::   :: ::   :: ::   :: ::  M::   ::   ::   ::   --   ::  --   ::    --   ::    -- of M--   ::    of::   :: ::    -- of --   :: :: M: ::    --   :: M::   ::   :: ::   :: ::   :: M--   :: M:: M:: ::    --   ::   :: M::   :: M::    --   ::   :: M::   ::    of::M --   ::    --   :: :: M::   :: :: ::   :: :: ::   ::   ::   ::   --    of::  of M::  of:: M::  --   :: ::  --   :: ::   ::   :: :: ::    --   ::  --   :: :: M::   ::    --   :: :: ::   :: :: ::   :: :: M:: [::  --   ::  --   :: :: [a::  --   :: [:: 3::   ::   :: [:: :: ::   :: :: ::  Set:: M:: :: :: ::   :: :: [::  of:: ::MSet ofM::   ::M::  of theM::   :: ::   ::Mset::   ::   ::   :: ::    of::   :: :: ::   ::   ::   :: ::    of:: [:: of the::   :: ::   ::  --   :: :: :: [:: ::   :: ::   :: :: ::   ::  of:: ::    of::   :: of::   ::   :: ::   ::   --   :: :: :: [::   :: :: of::   ::    of::   :: ::   :: M::   ::   :: M:: of::  --   :: :: :: ::   ::   ::Mset of::  --   :: M:: ::    --    of M--    --   :: M::  --   :: M::   :: of::   ::  --  -- of:: :: ::  --   ::M:: :: :: [::   ::  of:: :: --   ::  -- of M:: :: :: M:: [::M::   ::  of::  --   --   :: :: :: ::   :: :: ::    of:: ::    of::  --   ::  of:: :: of::    of::   ::  -- of :: ::Mset::    --   :: ::   ::  M:: ::   :: M:: ::  -- of --1:: State::  --   :: [::   :: M of::   ::M:: [::  -- of:: [::  --   ::  --   ::    --    of::   :: :: [:: M::   ::  --   :: [::   --   :: ::  --   ::  MSet:: ::  --   :: M:: :: [::  --   :: :: [::  --   ::  --   :: :::: :: [:: :: ::  -- of:: [:: -- of:: :: [:: :: ::  -- of::  --   :: :: M:: -- of:: :: :: :: :: [:: ::   :: of:: of::  of:: [:: :: [:: of:: :: :: ::  of::   ::  --   :: [:: :: M:: :: [:: M:: :: :: [:: ::   :: [::   :: :: [:: ::   ::M of M::  -- of:: [:: :: [:: ::  -- of:: [:: ::  --   :: :: [    --   :: :: [::M of::  of::   ::  of:: of::   :: --   :: :: State of:: M:: :: M:: :: :: :: :: :: of::   ::M:: ::M:: ::  of:: :: :: :: ::   :: :: :: ::   :: ::  of:: :: [::State:: :: :: :: :: ::   :: :: :: ::   ::   ::   :: ::   :: [::    of::   :: :: ::   ::   ::  of:: :: :: ::   :: ::   ::  of:: [:: ::   :: 1::  --   :: [:: :: of:: [:: of::   :: ::   :: M::   :: :: M:: Set:: of::    M::  M:: of::   ::  M:: of::   ::   :: [::   ::  --   ::  of:: [:: of:: M::   :: ::   :: :: of:: [:: M of:: [:: a:: [::   :: [:: ::   :: [:: :: [:: [::M:: M:: [::   :: [:: :: M:: ::   ::M::M:: ::M:: :: [:: :: :: [:: M:: M:: [:: M::   :: :: ::   :: :: [::M:: the:: [:: [::  --   :: [:: ::   :: of::   :: [::   :: [:: ::M:: [::   :: [:: the::   :: [::M:: [:: ::   :: of the:: [:: ::   :: of you of:: ::   :: [:: :: [:: the::   :: of M::M::M:: of:: the:: the:: [:: of::   ::  --   :: ::    -- of the:: M:: of:: M::M:: [::   ::   :: [::set of the:: the of:: :: [::   :: [:: of::   ::M of:: M:::: the:: of::M: ::M:::: of the::   :: :: of:: of -- of the:: of M:: the set of [Set:: M::. [::M:: of:: ** of:::: of::   :: M:: of the::   :: ::   :: -- of:: :: of the:: M:: [:: of -- of -- of::   ::   :: the of::   :: Map of::   :: [ a:: ::   ::  --   :: [:: class::M:: of --   :: [:: of::    Set::Mset::   ::   :: the::   ::   :: of --   :: the of:: the of the:: state of the::   :: :: of::   :: the of:: [:: of::   :: of --   ::Mset of:: ::   ::Mset of::   :: of::   :: of --   ::    of::   ::   ::    -- of the::   ::  of::   :: M:: of::   :: ::   :: of:: of::   :: of:: of:: of:: of instance:: of the:: of::   ::1::   :: of:: of:: -- of:: the-- of the::   ::   :: ::    of:: [:: of:: :: :: ::   :: ::   ::   :: of::   :: :: of the::   :: of::   :: of::   ::   :: of:: of the--   :: [:: of::   ::   :: M::::   :: M:: [:: of::   ::   ::    --   -- of the:: [::M::   ::    of:: M:: the::   :: [::M::   :: of:: M:: of::   ::   ::   :: of M::: of M:: [:: of the::   :: M::: the.::   :: [::M of::   :: [:: of::   :: [:: of:: of:: of you toM2 of:: Mset of M:: of::   ::    of:: [::Mset::   :: of::   ::   ::   ::   ::M::    of::   :: of the:: of:: of::   :: name:: of::   ::M Set::M::   ::Mset::M:: of the::   :: M::   :: M::   ::   ::State of the::M:::M:: of the::   ::   :: M|::   ::M:: @   ::   ::   ::   :: ::M: M::    of type::    of::Mset of::   :: of::Mset of::   ::M::   ::   ::   ::   ::M:: of the of the::   ::   :: M:: ::   ::   :: of::   ::    of::   ::    of:: of M:: of::M::   :: of::   :: of M::   :: * of::M:: of::State::   :: of --   :: M::   :: ::M:: of::   ::M::   :: M:: of::M::M:: ::   :: of:: M:: of::M:: *.::   ::   :: [::set of:: [::M:: :: M:: M:: M::M:: [ a::M:: [:: M:: of --   ::   --  of::M of:: MSet of M::  of:: [:: M::   :: [::MSet of the of:: [::   :: M:: [:: M:: [:: [:: of:: M:: [:: :: [a:: [::State:: [::M::   :: [:: [:: M:: [::M:: [:: ::M:: [a:: ::M:: [::   :: [:: M:: :: :: [:: [::M::  of:: ::M:: [::M:: [:: MSet:: [::   :: [:: ::  of:: :: ::M:: [::   :: :: [::st:: ::   :: [::M:: :: [::   :: M:: of::M:: of --   :: [:: M::   :: ::M:: [::M:: [:: [:: M:: of::M:: M9 of::   :: [:: M:: [ a:: :: [:: :: [:: M::  of:: [:: :: ::   ::State of:: :: :: :: [:: M:: M:: [:: a:: :: :: [:: M::M:: [::M:: M::M::   :: [:: :: [::   ::M:: ::   :: ::M:: :: :: [:: :: [:: :: :: M:: [::M:: of:: [::M:: [:: of:: M:: :: [:: of:: [:: of:: [:: --   :: [:: of:: [::M:: [:: M:: [:: :: [:: [:: of:: [:: M:: :: :: [::MSet of:: :: M:: State:: M:: [::M:: :: [:: :: ::M of the:: [::M::   :: M:: :: :: [::M:: M:: :: :: [:: :: M:: [::] of:: [:: --   :: [::M:: :: [::M:: M:: [::M::   :: [:: [:: :: [::M:: [::M:: [::M:: M:: [:: of:: [:: M:: [:: * of:: [:: [::M:: [:: [:: [::M:: [::M a::  of::   :: [:: of:: [:: :: [:: of::   :: [:: of:: [::   :: [:: [:: M:: M:: M:: [:: M:: [::M:: [:: [:: [:: [::M:: [::M::M:: [:: [:: [:: M:: [:: [:: ::M:: [:: :: [:: M:: ::M:: M:: [::M:: a:: M:: M::M:: [:: M:: of:: ::M:: [::M:: [::M:: [:: M:: [:: M::M:: [::M:: [:: :: [:: M:: [::M:: [:: [:: of M:: [::M::M:: [::M:: ::M:: [:: [:: [:: M:: [:: M:: [::M:: [::State::M:: [::M::Mset::M:: [:: M::   :: M:: of::   :: [::M:: [:: ::M Set:: [:: of:: M:: of:: [:: of the:: M:: M:: of M::M of the M| of the:: of:: M:: M::. ofM:: of M::   ::M of the::M of:: [:: M of ::   ::M-- of ::   :: M of::M::M::M of the of M -- of:: [:: of:: [::M:: M a:: [a:: M:: [:: ::   :: :: [::M:: [:: ::   ::  of M::M
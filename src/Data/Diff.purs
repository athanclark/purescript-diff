module Data.Diff where

import Prelude
import Data.Maybe (Maybe (..))
import Data.These (These (..))
import Data.Tuple (Tuple (..))
import Data.Either (Either (..))
import Data.List (List (..))
-- import Data.NonEmpty (NonEmpty (..))
import Data.Map (Map)
import Data.Map as Map
import Data.StrMap (StrMap)
import Data.StrMap as StrMap
import Data.Set (Set)
import Data.Set as Set
import Data.String.Yarn as String
import Data.Array as Array
import Data.Foldable (foldr)



class Diff a l r | a -> l r where
  diff :: a -> a -> Maybe (These l r)

-- instance Diff 


eqToDiff :: forall a. Eq a => a -> a -> Maybe (These a a)
eqToDiff a b
  | a == b = Nothing
  | otherwise = Just $ Both a b


-- diffString :: String -> String -> MuResult String
-- diffString a b = case diff (Prefix (String.toChars a) :: Prefix Array Char) (Prefix (String.toChars b)) of
--   Nothing -> Nothing
--   Just d -> Just $ case d of
--     This (Prefix as) -> This (String.fromChars as)
--     That (Prefix bs) -> That (String.fromChars bs)
--     Both (Prefix as) (Prefix bs) -> Both (String.fromChars as) (String.fromChars bs)


-- -- | `aux` represents "has more than the other of..."
-- class Diff a where
--   diff :: a -> a -> Maybe (Either a a)

instance diffUnit :: Diff Unit Void Void where
  diff _ _ = Nothing


-- newtype Truthiness = Truthiness Boolean
-- newtype Falseness  = Falseness Boolean


-- instance diffTruthiness :: Diff Truthiness ExResult where
--   diff (Truthiness a) (Truthiness b) =
--     if a == b
--       then ExResult Nothing
--       else ExResult $ if a then Just $ Left $ Truthiness a
--                            else Just $ Right $ Truthiness b

-- instance diffFalseness :: Diff Falseness ExResult where
--   diff (Falseness a) (Falseness b) =
--     if a == b
--       then ExResult Nothing
--       else ExResult $ if not a then Just $ Left $ Falseness a
--                                else Just $ Right $ Falseness a

instance diffMaybeEx :: Diff a a a => Diff (Maybe a) a a where
  diff Nothing Nothing = Nothing
  diff (Just a) (Just b) = case diff a b of
    Nothing -> Nothing
    Just ex -> Just $ case ex of
      This e -> This e
      That e -> That e
      Both e1 e2 -> Both e1 e2
  diff (Just a) Nothing = Just (This a)
  diff Nothing (Just b) = Just (That b)

-- instance diffMaybeMu :: Diff a MuResult => Diff (Maybe a) MuResult where
--   diff Nothing Nothing = MuResult Nothing
--   diff (Just a) (Just b) = case diff a b of
--     MuResult r -> MuResult $ case r of
--       Nothing -> Nothing
--       Just mu -> case mu of
--         This e -> Just $ This $ Just e
--         That e -> Just $ That $ Just e
--         Both e1 e2 -> Just $ Both (Just e1) (Just e2)
--   diff (Just a) Nothing = MuResult $ Just $ This $ Just a
--   diff Nothing (Just b) = MuResult $ Just $ That $ Just b



-- | WRT Ord
newtype Positive a = Positive a

-- | WRT Ord
newtype Negative a = Negative a


instance diffPositiveRing :: (Ord a, Ring a) => Diff (Positive a) a a where
  diff (Positive a) (Positive b)
    | a == b = Nothing
    | otherwise = if a > b
                  then Just $ This $ a - b
                  else Just $ That $ b - a

instance diffNegativeRing :: (Ord a, Ring a) => Diff (Negative a) a a where
  diff (Negative a) (Negative b)
    | a == b = Nothing
    | otherwise = if a < b
                  then Just $ This $ b - a
                  else Just $ That $ a - b


-- | WRT Ord
newtype Greater a = Greater a

-- | WRT Ord
newtype Lesser a = Lesser a


instance diffPositiveOrd :: Ord a => Diff (Greater a) a a where
  diff (Greater a) (Greater b)
    | a == b = Nothing
    | otherwise = if a > b 
                  then Just (This a)
                  else Just (That b)

instance diffNegativeOrd :: Ord a => Diff (Lesser a) a a where
  diff (Lesser a) (Lesser b)
    | a == b = Nothing
    | otherwise = if a < b
                  then Just (This a)
                  else Just (That b)


newtype Prefix f a = Prefix (f a)
newtype Suffix f a = Suffix (f a)

-- instance diffCNonEmpty :: (Eq a, DiffC (f a)) => DiffC (NonEmpty f a) where
--   diffC (NonEmpty a as) (NonEmpty b bs)
--     | a == b = case diffC as bs of
--       Nothing -> Nothing
--       Just ex -> Just $ case ex of
--         This x -> 
--     | otherwise = Nothing

-- instance diffCThese :: (DiffC a, DiffC b) => DiffC (These a b) where
--   diffC (Tuple a1 b1) (Tuple a2 b2) = case Tuple (diffC a1 a2) (diffC b1 b2) of
--     Tuple Nothing Nothing -> Nothing
--     Tuple (Just e1) Nothing -> case e1 of
--       This x -> 
--     Tuple (Just e1) (Just e2) -> _

instance diffTuple :: (Diff a l1 r1, Diff b l2 r2) => Diff (Tuple a b) (These l1 r1) (These l2 r2) where
  diff (Tuple a1 b1) (Tuple a2 b2) = case Tuple (diff a1 a2) (diff b1 b2) of
    Tuple Nothing Nothing -> Nothing
    Tuple (Just e1) Nothing -> Just (This e1)
    Tuple Nothing (Just e2) -> Just (That e2)
    Tuple (Just e1) (Just e2) -> Just (Both e1 e2)

instance diffEither :: (Diff a a b, Diff b b a) => Diff (Either a b) (These a b) (These a b) where
  diff (Left e1) (Left e2) = case diff e1 e2 of
    Nothing -> Nothing
    Just ex -> Just (This ex) -- case ex of
      -- This (x :: l1) -> This (This x)
      -- That (x :: r1) -> This (That x)
      -- Both (x :: l1) (y :: r1) -> This (Both x y)
  diff (Right e1) (Right e2) = case diff e1 e2 of
    Nothing -> Nothing
    Just ex -> Just (That ex) -- case ex of
      -- This x -> This (Right x)
      -- That x -> That (Right x)
      -- Both x y -> Both (Right x) (Right y)
  diff (Left e1) (Right e2) = Just (Both (This e1) (That e2))
  diff (Right e1) (Left e2) = Just (Both (That e1) (This e2))


-- instance diffCPrefixArray :: Eq a => DiffC (Prefix Array a) where
--   diffC (Prefix a) (Prefix b) = case Tuple (Array.uncons a) (Array.uncons b) of
--     Tuple (Just {head:ah,tail:at}) (Just {head:bh,tail:bt})
--       | ah == bh -> diffC (Prefix at) (Prefix bt)
--       | otherwise -> Just (Both (Prefix a) (Prefix b))
--     Tuple Nothing (Just _) -> Just $ That $ Prefix b
--     Tuple (Just _) Nothing -> Just $ This $ Prefix a
--     Tuple Nothing Nothing -> Nothing

-- instance diffCSuffixArray :: Eq a => DiffC (Suffix Array a) where
--   diffC (Suffix a) (Suffix b) = case Tuple (Array.unsnoc a) (Array.unsnoc b) of
--     Tuple (Just {init:ai,last:al}) (Just {init:bi,last:bl})
--       | al == bl -> diffC (Suffix ai) (Suffix bi)
--       | otherwise -> Just (Both (Suffix a) (Suffix b))
--     Tuple Nothing (Just _) -> Just $ That $ Suffix b
--     Tuple (Just _) Nothing -> Just $ This $ Suffix a
--     Tuple Nothing Nothing -> Nothing

-- instance diffCPrefixList :: Eq a => DiffC (Prefix List a) where
--   diffC (Prefix a) (Prefix b) = case Tuple a b of
--     Tuple (Cons ah at) (Cons bh bt)
--       | ah == bh -> diffC (Prefix at) (Prefix bt)
--       | otherwise -> Just (Both (Prefix a) (Prefix b))
--     Tuple Nil (Cons _ _) -> Just $ That $ Prefix b
--     Tuple (Cons _ _) Nil -> Just $ This $ Prefix a
--     Tuple Nil Nil -> Nothing

-- instance diffCMap :: Ord k => DiffC (Map k a) where
--   diffC a b = case unit of
--     _ | Map.isEmpty a' && Map.isEmpty b' -> Nothing
--       | Map.isEmpty b' -> Just (This a')
--       | Map.isEmpty a' -> Just (That b')
--       | otherwise -> Just (Both a' b')
--     where
--     a' = differenceMap a b
--     b' = differenceMap b a
--     differenceMap :: Map k a -> Map k a -> Map k a
--     differenceMap x = foldr Map.delete x <<< Map.keys

-- instance diffCStrMap :: DiffC (StrMap a) where
--   diffC a b = case unit of
--     _ | StrMap.isEmpty a' && StrMap.isEmpty b' -> Nothing
--       | StrMap.isEmpty b' -> Just (This a')
--       | StrMap.isEmpty a' -> Just (That b')
--       | otherwise -> Just (Both a' b')
--     where
--     a' = differenceStrMap a b
--     b' = differenceStrMap b a
--     differenceStrMap :: StrMap a -> StrMap a -> StrMap a
--     differenceStrMap x = foldr StrMap.delete x <<< StrMap.keys

-- instance diffCSet :: Ord a => DiffC (Set a) where
--   diffC a b = case unit of
--     _ | Set.isEmpty a' && Set.isEmpty b' -> Nothing
--       | Set.isEmpty b' -> Just (This a')
--       | Set.isEmpty a' -> Just (That b')
--       | otherwise -> Just (Both a' b')
--     where
--     a' = differenceSet a b
--     b' = differenceSet b a
--     differenceSet :: Set a -> Set a -> Set a
--     differenceSet = foldr Set.delete

module Data.Diff where

import Prelude
import Data.Maybe (Maybe (..))
import Data.These (These (..))
import Data.Tuple (Tuple (..))
import Data.Either (Either (..))
import Data.List (List (..))
import Data.NonEmpty (NonEmpty (..))
import Data.Map (Map)
import Data.Map as Map
import Data.StrMap (StrMap)
import Data.StrMap as StrMap
import Data.Set (Set)
import Data.Set as Set
import Data.String.Yarn as String
import Data.Array as Array
import Data.Foldable (foldr)



eqToDiffC :: forall a. Eq a => a -> a -> Maybe (These a a)
eqToDiffC a b
  | a == b = Nothing
  | otherwise = Just (Both a b)


diffCString :: String -> String -> Maybe (These String String)
diffCString a b = case diffC (Prefix (String.toChars a) :: Prefix Array Char) (Prefix (String.toChars b)) of
  Nothing -> Nothing
  Just d -> Just $ case d of
    This (Prefix as) -> This (String.fromChars as)
    That (Prefix bs) -> That (String.fromChars bs)
    Both (Prefix as) (Prefix bs) -> Both (String.fromChars as) (String.fromChars bs)


-- | `aux` represents "has more than the other of..."
class Diff a aux | a -> aux where
  diff :: a -> a -> Maybe (Either aux aux)

instance diffUnit :: Diff Unit Void where
  diff _ _ = Nothing


newtype Truthiness = Truthiness Boolean
newtype Falseness  = Falseness Boolean


instance diffTruthiness :: Diff Truthiness Unit where
  diff (Truthiness a) (Truthiness b) =
    if a == b
      then Nothing
      else if a then Just (Left unit)
                else Just (Right unit)

instance diffFalseness :: Diff Falseness Unit where
  diff (Falseness a) (Falseness b) =
    if a == b
      then Nothing
      else if not a then Just (Left unit)
                    else Just (Right unit)


-- | WRT Ord
newtype Positive a = Positive a
newtype Negative a = Negative a


instance diffPositiveInt :: Diff (Positive Int) Int where
  diff (Positive a) (Positive b)
    | a == b = Nothing
    | otherwise = if a > b
                  then Just (Left (a - b))
                  else Just (Right (b - a))

instance diffPositiveNumber :: Diff (Positive Number) Number where
  diff (Positive a) (Positive b)
    | a == b = Nothing
    | otherwise = if a > b
                  then Just (Left (a - b))
                  else Just (Right (b - a))

instance diffNegativeInt :: Diff (Negative Int) Int where
  diff (Negative a) (Negative b)
    | a == b = Nothing
    | otherwise = if a < b
                  then Just (Left (b - a))
                  else Just (Right (a - b))

instance diffNegativeNumber :: Diff (Negative Number) Number where
  diff (Negative a) (Negative b)
    | a == b = Nothing
    | otherwise = if a < b
                  then Just (Left (b - a))
                  else Just (Right (a - b))


instance diffPositiveChar :: Diff (Positive Char) Char where
  diff (Positive a) (Positive b)
    | a == b = Nothing
    | otherwise = if a > b then Just (Left a) else Just (Right b)

instance diffNegativeChar :: Diff (Negative Char) Char where
  diff (Negative a) (Negative b)
    | a == b = Nothing
    | otherwise = if a < b then Just (Left a) else Just (Right b)


diffToDiffC :: forall a aux. Diff a aux => a -> a -> Maybe (These aux aux)
diffToDiffC a b = case diff a b of
  Nothing -> Nothing
  Just eX -> case eX of
    Left x -> Just (This x)
    Right y -> Just (That y)


-- | Diffable collections
class DiffC a aux | a -> aux where
  diffC :: a -> a -> Maybe (These aux aux)

newtype Prefix f a = Prefix (f a)
newtype Suffix f a = Suffix (f a)

instance diffCNonEmpty :: (Eq a, DiffC (f a) (f a)) => DiffC (NonEmpty f a) (f a) where
  diffC (NonEmpty a as) (NonEmpty b bs)
    | a == b = diffC as bs
    | otherwise = Nothing

instance diffCPrefixArray :: Eq a => DiffC (Prefix Array a) (Prefix Array a) where
  diffC (Prefix a) (Prefix b) = case Tuple (Array.uncons a) (Array.uncons b) of
    Tuple (Just {head:ah,tail:at}) (Just {head:bh,tail:bt})
      | ah == bh -> diffC (Prefix at) (Prefix bt)
      | otherwise -> Just (Both (Prefix a) (Prefix b))
    Tuple Nothing (Just _) -> Just $ That $ Prefix b
    Tuple (Just _) Nothing -> Just $ This $ Prefix a
    Tuple Nothing Nothing -> Nothing

instance diffCSuffixArray :: Eq a => DiffC (Suffix Array a) (Suffix Array a) where
  diffC (Suffix a) (Suffix b) = case Tuple (Array.unsnoc a) (Array.unsnoc b) of
    Tuple (Just {init:ai,last:al}) (Just {init:bi,last:bl})
      | al == bl -> diffC (Suffix ai) (Suffix bi)
      | otherwise -> Just (Both (Suffix a) (Suffix b))
    Tuple Nothing (Just _) -> Just $ That $ Suffix b
    Tuple (Just _) Nothing -> Just $ This $ Suffix a
    Tuple Nothing Nothing -> Nothing

instance diffCPrefixList :: Eq a => DiffC (Prefix List a) (Prefix List a) where
  diffC (Prefix a) (Prefix b) = case Tuple a b of
    Tuple (Cons ah at) (Cons bh bt)
      | ah == bh -> diffC (Prefix at) (Prefix bt)
      | otherwise -> Just (Both (Prefix a) (Prefix b))
    Tuple Nil (Cons _ _) -> Just $ That $ Prefix b
    Tuple (Cons _ _) Nil -> Just $ This $ Prefix a
    Tuple Nil Nil -> Nothing

instance diffCMap :: Ord k => DiffC (Map k a) (Map k a) where
  diffC a b = case unit of
    _ | Map.isEmpty a' && Map.isEmpty b' -> Nothing
      | Map.isEmpty b' -> Just (This a')
      | Map.isEmpty a' -> Just (That b')
      | otherwise -> Just (Both a' b')
    where
    a' = differenceMap a b
    b' = differenceMap b a

instance diffCStrMap :: DiffC (StrMap a) (StrMap a) where
  diffC a b = case unit of
    _ | StrMap.isEmpty a' && StrMap.isEmpty b' -> Nothing
      | StrMap.isEmpty b' -> Just (This a')
      | StrMap.isEmpty a' -> Just (That b')
      | otherwise -> Just (Both a' b')
    where
    a' = differenceStrMap a b
    b' = differenceStrMap b a

instance diffCSet :: Ord a => DiffC (Set a) (Set a) where
  diffC a b = case unit of
    _ | Set.isEmpty a' && Set.isEmpty b' -> Nothing
      | Set.isEmpty b' -> Just (This a')
      | Set.isEmpty a' -> Just (That b')
      | otherwise -> Just (Both a' b')
    where
    a' = differenceSet a b
    b' = differenceSet b a

-- strings maps sets

differenceMap :: forall k a. Ord k => Map k a -> Map k a -> Map k a
differenceMap a q
  | Map.isEmpty q = a
  | otherwise =
    let ks = Map.keys q
    in  foldr (\k acc -> Map.delete k acc) a ks

differenceStrMap :: forall a. StrMap a -> StrMap a -> StrMap a
differenceStrMap a q
  | StrMap.isEmpty q = a
  | otherwise =
    let ks = StrMap.keys q
    in  foldr (\k acc -> StrMap.delete k acc) a ks

differenceSet :: forall a. Ord a => Set a -> Set a -> Set a
differenceSet a q
  | Set.isEmpty q = a
  | otherwise = foldr (\k acc -> Set.delete k acc) a q

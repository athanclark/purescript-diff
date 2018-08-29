module Data.Diff where

import Prelude
import Data.Maybe (Maybe (..))
import Data.These (These (..))
import Data.Tuple (Tuple (..))
import Data.Either (Either (..))
import Data.Array as Array


-- | `aux` represents "has more than the other of..." - preorder of content?
-- Truthiness, Falseness? Additive a 'la magnitue?
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


-- | Diffable collections
class DiffC a aux | a -> aux where
  diffC :: a -> a -> Maybe (These aux aux)

newtype Prefix a = Prefix a
newtype Suffix a = Suffix a

instance diffCArray :: Eq a => DiffC (Prefix (Array a)) (Array a) where
  diffC (Prefix a) (Prefix b) = case Tuple (Array.uncons a) (Array.uncons b) of
    Tuple (Just {head:ah,tail:at}) (Just {head:bh,tail:bt})
      | ah == bh -> diffC (Prefix at) (Prefix bt)
      | otherwise -> Just (Both a b)
    Tuple Nothing (Just _) -> Just (That b)
    Tuple (Just _) Nothing -> Just (This a)
    Tuple Nothing Nothing -> Nothing

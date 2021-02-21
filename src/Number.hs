{-# LANGUAGE RankNTypes, TypeSynonymInstances, FlexibleInstances, DefaultSignatures #-}
module Number where

import Common

import Data.Ratio (numerator, denominator)

-- Typeclass to handle operations on `LispNum` types
class (Num a) => LispNumOp a where
    lispDiv :: a -> a -> a
    default lispDiv :: (Fractional a) => a -> a -> a
    lispDiv = (/)

instance LispNumOp Integer where lispDiv = div
instance LispNumOp Rational
instance LispNumOp Double

instance Eq LispNum where
    x == y = getNum x == getNum y
instance Ord LispNum where
    x <= y = getNum x <= getNum y

-- Raise an error when an unexpected type is encountered
expectedNum f t = lispError TypeError (f <> " expected " <> t <> " number argument")

-- Given a list of `Value`s, extract a list of `LispNum`s from it.
-- If any values are not `LispNum`s, raise an error.
unpackNumbers :: [Value] -> Lisp [LispNum]
unpackNumbers xs = u `traverse` xs
    where u (NumVal x) = pure x
          u _ = lispError TypeError "Expected number argument"

-- Simply apply an operation to the wrapped number value
handleUnary :: (forall a. (LispNumOp a) => a -> a) -> LispNum -> LispNum
handleUnary f x = case x of
    LispInt x -> LispInt $ f x
    LispRational x -> LispRational $ f x
    LispReal x -> LispReal $ f x

-- Perform a fold over a list of numbers
numUnary :: (LispNum -> LispNum) -> [Value] -> Lisp Value
numUnary f x = NumVal . f . head <$> unpackNumbers x

-- Given a function and some values, compute the result
unary :: (forall a. (LispNumOp a) => a -> a) -> [Value] -> Lisp Value
unary f = numUnary $ handleUnary f

-- Here, number promotion is performed. For example:
--     * Int + Int => Int
--     * Int + Real => Real
--     * Rational + Real => Real
-- The exception to this is integer division, which is handled
-- seperately.
handleBinary :: (forall a. (LispNumOp a) => a -> a -> a) -> LispNum -> LispNum -> LispNum
handleBinary op x y = case (x, y) of
    (LispInt x, LispInt y) -> LispInt $ op x y
    (LispReal x, y) -> LispReal $ op x (getNum y)
    (x, LispReal y) -> LispReal $ op (getNum x) y
    (LispRational x, y) -> LispRational $ op x (getNum y)
    (x, LispRational y) -> LispRational $ op (getNum x) y

-- Perform a fold over a list of numbers
numBinary :: (LispNum -> LispNum -> LispNum) -> [Value] -> Lisp Value
numBinary f xs = NumVal . foldl1 f <$> unpackNumbers xs

-- Given a function and some values, compute the result
binary :: (forall a. (LispNumOp a) => a -> a -> a) -> [Value] -> Lisp Value
binary f = numBinary $ handleBinary f

-- Similar to `binary` but producing a boolean rather than a number
comparison :: (LispNum -> LispNum -> Bool) -> [Value] -> Lisp Value
comparison f xs = BoolVal . all (uncurry f) <$> (pairs <$> unpackNumbers xs)
    where pairs [] = []
          pairs xs = zip xs (tail xs)

-- Special case for integer division.
lispDivision (LispInt x) (LispInt y) =
    handleBinary lispDiv (LispRational $ fromInteger x) (LispRational $ fromInteger y)
lispDivision x y = handleBinary lispDiv x y

-- Get the numberator and denominator from a rational number type
lispNumerator, lispDenominator :: [LispNum] -> Lisp Value
lispNumerator [(LispRational x)] = pure $ NumVal . LispInt $ numerator x
lispNumerator _ = expectedNum "numerator" "rational"
lispDenominator [(LispRational x)] = pure $ NumVal . LispInt $ denominator x
lispDenominator _ = expectedNum "denominator" "rational"

-- Exponentiation
lispExpt :: LispNum -> LispNum -> LispNum
lispExpt (LispInt x) (LispInt y) = LispInt $ x ^ y
lispExpt (LispRational x) (LispInt y) = LispRational $ x ^^ y
lispExpt x y = LispReal $ getNum x ** getNum y


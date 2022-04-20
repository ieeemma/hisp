{-# LANGUAGE RankNTypes
           , TypeSynonymInstances
           , FlexibleInstances
           , DefaultSignatures #-}
module Number where

import Common

import Data.Text (Text)
import Data.Ratio (numerator, denominator)
import Data.Fixed (mod')

-- Typeclass to handle operations on `LispNum` types
class (Num a) => LispNumOp a where
    lispDiv :: a -> a -> a
    default lispDiv :: (Fractional a) => a -> a -> a
    lispDiv = (/)
    lispMod :: a -> a -> a
    default lispMod :: (Real a) => a -> a -> a
    lispMod = mod'

instance LispNumOp Integer where lispDiv = div
instance LispNumOp Rational
instance LispNumOp Double

-- Raise an error when an unexpected type is encountered
expectedNum :: Text -> Text -> Lisp a
expectedNum f t = lispError TypeError
    (f <> " expected " <> t <> " number argument")

-- Given a list of `Value`s, extract a list of `LispNum`s from it.
-- If any values are not `LispNum`s, raise an error.
unpackNumbers :: [Value] -> Lisp [LispNum]
unpackNumbers xs = u `traverse` xs
    where u (NumVal x) = pure x
          u _ = lispError TypeError "Expected number argument"

-- Simply apply an operation to the wrapped number value
handleUnary :: (forall a. (LispNumOp a) => a -> a) -> LispNum -> LispNum
handleUnary f x = case x of
    LispInt n -> LispInt $ f n
    LispRational n -> LispRational $ f n
    LispReal n -> LispReal $ f n

-- Perform a fold over a list of numbers
numUnary :: (LispNum -> LispNum) -> [Value] -> Lisp Value
numUnary f x = NumVal . f . head <$> unpackNumbers x

-- Given a function and some values, compute the result
unary :: (forall a. (LispNumOp a) => a -> a)
      -> [Value]
      -> Lisp Value
unary f = numUnary $ handleUnary f

-- Here, number promotion is performed. For example:
--     * Int + Int => Int
--     * Int + Real => Real
--     * Rational + Real => Real
-- The exception to this is integer division, which is handled
-- seperately.
handleBinary :: (forall a. (LispNumOp a) => a -> a -> a)
             -> LispNum
             -> LispNum
             -> LispNum
handleBinary op x y = case (x, y) of
    (LispInt n, LispInt m) -> LispInt $ op n m
    (LispReal n, m) -> LispReal $ op n (getNum m)
    (n, LispReal m) -> LispReal $ op (getNum n) m
    (LispRational n, m) -> LispRational $ op n (getNum m)
    (n, LispRational m) -> LispRational $ op (getNum n) m

-- Perform a fold over a list of numbers
numBinary :: (LispNum -> LispNum -> LispNum)
          -> [Value]
          -> Lisp Value
numBinary f xs = NumVal . foldl1 f <$> unpackNumbers xs

-- Given a function and some values, compute the result
binary :: (forall a. (LispNumOp a) => a -> a -> a) -> [Value] -> Lisp Value
binary f = numBinary $ handleBinary f

-- Similar to `binary` but producing a boolean rather than a number
comparison :: (LispNum -> LispNum -> Bool) -> [Value] -> Lisp Value
comparison f xs =
    BoolVal . all (uncurry f) <$> (pairs <$> unpackNumbers xs)
    where pairs [] = []
          pairs ys = zip ys (tail ys)

-- Special case for integer division.
lispDivision :: LispNum -> LispNum -> LispNum
lispDivision (LispInt x) (LispInt y) =
    handleBinary lispDiv (LispRational $ fromInteger x)
                         (LispRational $ fromInteger y)
lispDivision x y = handleBinary lispDiv x y

-- Get the numberator and denominator from a rational number type
lispNumerator, lispDenominator :: [LispNum] -> Lisp Value
lispNumerator [LispRational x] =
    pure $ NumVal . LispInt $ numerator x
lispNumerator _ = expectedNum "numerator" "rational"
lispDenominator [LispRational x] =
    pure $ NumVal . LispInt $ denominator x
lispDenominator _ = expectedNum "denominator" "rational"

-- Exponentiation
lispExpt :: LispNum -> LispNum -> LispNum
lispExpt (LispInt x) (LispInt y) = LispInt $ x ^ y
lispExpt (LispRational x) (LispInt y) = LispRational $ x ^^ y
lispExpt x y = LispReal $ getNum x ** getNum y

-- Round a number to an integer
lispRound :: LispNum -> LispNum
lispRound e@(LispInt _) = e
lispRound (LispRational x) = LispInt $ round x
lispRound (LispReal x) = LispInt $ round x

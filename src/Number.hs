{-# LANGUAGE RankNTypes, TypeSynonymInstances, FlexibleInstances, DefaultSignatures #-}
module Number where

import Common

class (Num a) => LispNumOp a where
    lispDiv :: a -> a -> a
    default lispDiv :: (Fractional a) => a -> a -> a
    lispDiv = (/)

instance LispNumOp Integer where lispDiv = div
instance LispNumOp Rational
instance LispNumOp Double

unpackNumbers :: [Value] -> Lisp [LispNum]
unpackNumbers xs = u `traverse` xs
    where u (NumVal x) = pure x
          u _ = lispError TypeError "Expected number argument"

handleUnary :: (forall a. (LispNumOp a) => a -> a) -> LispNum -> LispNum
handleUnary f x = case x of
    LispInt x -> LispInt $ f x
    LispRational x -> LispRational $ f x
    LispReal x -> LispReal $ f x
numUnary :: (LispNum -> LispNum) -> [Value] -> Lisp Value
numUnary f x = NumVal . f . head <$> unpackNumbers x

unary :: (forall a. (LispNumOp a) => a -> a) -> [Value] -> Lisp Value
unary f = numUnary $ handleUnary f

handleBinary :: (forall a. (LispNumOp a) => a -> a -> a) -> LispNum -> LispNum -> LispNum
handleBinary op x y = case (x, y) of
    (LispInt x, LispInt y) -> LispInt $ op x y
    (LispReal x, y) -> LispReal $ op x (getNum y)
    (x, LispReal y) -> LispReal $ op (getNum x) y
    (LispRational x, y) -> LispRational $ op x (getNum y)
    (x, LispRational y) -> LispRational $ op (getNum x) y
numBinary :: (LispNum -> LispNum -> LispNum) -> [Value] -> Lisp Value
numBinary f xs = NumVal . foldl1 f <$> unpackNumbers xs

binary :: (forall a. (LispNumOp a) => a -> a -> a) -> [Value] -> Lisp Value
binary f = numBinary $ handleBinary f

lispDivision (LispInt x) (LispInt y) =
    handleBinary lispDiv (LispRational $ fromInteger x) (LispRational $ fromInteger y)
lispDivision x y = handleBinary lispDiv x y

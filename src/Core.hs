module Core where

import Common
import Eval

import Text.Read (readMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Map as M
import Control.Monad.IO.Class (liftIO)

builtin :: Text -> (Int -> Bool) -> ([Value] -> Lisp Value) -> (Text, Value)
builtin n p f = (n, Procedure n check)
    where check xs = if p (length xs) then eval `traverse` xs >>= f
                     else lispError ArgumentError $ "incorrect # args to " <> n

unpackNumber (NumVal x) = pure x
unpackNumber _ = lispError TypeError "Expected number argument"

arithmetic :: (Double -> Double -> Double) -> ([Value] -> Lisp Value)
arithmetic f = \xs -> NumVal <$> foldl1 f <$> unpackNumber `traverse` xs

comparison :: (Double -> Double -> Bool) -> ([Value] -> Lisp Value)
comparison f = \xs -> do
    xs' <- unpackNumber `traverse` xs
    pure $ BoolVal $ all (uncurry f) $ zip (repeat $ head xs') (tail xs')

expected f t = lispError TypeError (f <> " expected " <> t <> " argument")

stringToSymbol [StringVal x] = pure $ Symbol x
stringToSymbol _ = expected "string->symbol" "string"

symbolToString [Symbol x] = pure $ StringVal x
symbolToString _ = expected "symbol->string" "symbol"

stringToNumber [StringVal x] = case readMaybe (T.unpack x) :: Maybe Double of
    Just x -> pure $ NumVal x
    Nothing -> lispError ValueError "Invalid number format"
stringToNumber _ = expected "string->number" "string"

numberToString [NumVal x] = pure $ StringVal $ T.pack $ show x
numberToString _ = expected "number->string" "number"

typePred n f = builtin n (== 1) $ \[x] -> pure $ BoolVal $ f x

builtins :: Scope
builtins = M.fromList
    [ builtin "+" (> 0) $ arithmetic (+)
    , builtin "-" (> 0) $ arithmetic (-)
    , builtin "*" (> 0) $ arithmetic (*)
    , builtin "/" (> 0) $ arithmetic (/)
--  , builtin "quot" (> 0) $ arithmetic quot
--  , builtin "rem" (> 0) $ arithmetic rem
--  , builtin "mod" (> 0) $ arithmetic mod
    , builtin "=" (> 1) $ comparison (==)
    , builtin ">" (> 1) $ comparison (>)
--  , builtin ">=" (> 1) $ comparison (>=)
--  , builtin "<" (> 1) $ comparison (<)
--  , builtin "<=" (> 1) $ comparison (<=)
    , builtin "string->symbol" (== 1) stringToSymbol
    , builtin "symbol->string" (== 1) symbolToString
    , builtin "string->number" (== 1) stringToNumber
    , builtin "number->string" (== 1) numberToString

    , typePred "pair?" $ \case { Pair{} -> t; _ -> f }
    , typePred "symbol?" $ \case { Symbol{} -> t; _ -> f }
    , typePred "boolean?" $ \case { BoolVal{} -> t; _ -> f }
    , typePred "number?" $ \case { NumVal{} -> t; _ -> f; }
    , typePred "string?" $ \case { StringVal{} -> t; _ -> f; }
    , typePred "procedure?" $ \case { Lambda{} -> t; Procedure{} -> t; _ -> f }
    , typePred "null?" $ \case { Null -> t; _ -> f }

    , builtin "print" (== 1) $ \[x] -> liftIO (print x) *> pure Null ]
    where t = True
          f = False

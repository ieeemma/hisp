module Core where

import Common
import Eval
import Number

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Map as M
import Control.Monad.IO.Class (liftIO)

builtin n p f = (n, makeProc n p f)
expected f t = lispError TypeError (f <> " expected " <> t <> " argument")

stringToSymbol [StringVal x] = pure $ Symbol x
stringToSymbol _ = expected "string->symbol" "string"

symbolToString [Symbol x] = pure $ StringVal x
symbolToString _ = expected "symbol->string" "symbol"

--stringToNumber [StringVal x] = case readMaybe (T.unpack x) :: Maybe Double of
--    Just x -> pure $ NumVal x
--    Nothing -> lispError ValueError "Invalid number format"
--stringToNumber _ = expected "string->number" "string"

numberToString [NumVal x] = pure $ StringVal $ T.pack $ show x
numberToString _ = expected "number->string" "number"

typePred n f = builtin n (== 1) $ \[x] -> pure $ BoolVal $ f x

builtins :: Scope
builtins = M.fromList
    [ builtin "+" (> 0) $ binary (+)
    , builtin "-" (> 0) $ binary (-)
    , builtin "*" (> 0) $ binary (*)
    , builtin "/" (> 0) $ numBinary lispDivision
    , builtin "abs" (== 1) $ unary abs

    , builtin "string->symbol" (== 1) stringToSymbol
    , builtin "symbol->string" (== 1) symbolToString
--  , builtin "string->number" (== 1) stringToNumber
    , builtin "number->string" (== 1) numberToString

    , typePred "pair?" $ \case { Pair{} -> t; _ -> f }
    , typePred "symbol?" $ \case { Symbol{} -> t; _ -> f }
    , typePred "boolean?" $ \case { BoolVal{} -> t; _ -> f }
    , typePred "number?" $ \case { NumVal{} -> t; _ -> f; }
    , typePred "integer?" $ \case { NumVal (LispInt _)-> t; _ -> f; }
    , typePred "rational?" $ \case
        { NumVal (LispRational _)-> t; NumVal (LispInt _) -> t; _ -> f; }
    , typePred "real?" $ \case { NumVal{} -> t; _ -> f; }
    , typePred "string?" $ \case { StringVal{} -> t; _ -> f; }
    , typePred "procedure?" $ \case { Lambda{} -> t; Procedure{} -> t; _ -> f }
    , typePred "null?" $ \case { Null -> t; _ -> f }

    , builtin "print" (== 1) $ \[x] -> liftIO (print x) *> pure Null

    , builtin "cons" (== 2) $ \[x, y] -> pure $ x `Pair` y ]
    where t = True
          f = False

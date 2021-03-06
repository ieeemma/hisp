module Core where

import Common
import Eval
import Number

import Control.Monad ((>=>))
import Control.Monad.IO.Class (liftIO)
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Text.Read (readMaybe)

-- Construct a builtin a builtin procedure from a name, a predicate,
-- and a function. The predicate ensures the correct number of
-- arguments are passed to the function. For example, a predicate f
-- `(> 3)` or `(elem [3, 4, 5])`.
builtin :: Text -> (Int -> Bool) -> ([Value] -> Lisp Value) -> (Text, Value)
builtin n p f = (n, makeProc n p f)

-- Raise an error when an unexpected type is encountered
expected :: Text -> Text -> Lisp a
expected f t =
    lispError
        TypeError
        (f <> " expected " <> t <> " argument")

lispConcat, stringToSymbol, symbolToString, stringToNumber, numberToString, lispPrint, lispRead, lispEq :: [Value] -> Lisp Value
-- String concatenation operationz
lispConcat xs = StringVal <$> convert xs
  where
    convert [] = pure ""
    convert (StringVal y : ys) = (y <>) <$> convert ys
    convert _ = expected "<>" "string"

-- Convert a sttring to a symbol by changing the `Value` constructor
stringToSymbol [StringVal x] = pure $ Symbol x
stringToSymbol _ = expected "string->symbol" "string"
-- Convert a symbol to a string by changing the `Value` constructor
symbolToString [Symbol x] = pure $ StringVal x
symbolToString _ = expected "symbol->string" "symbol"
-- Convert a string to a number using a function from the `Parser`
-- module. If the number is an invalid format, raise an error.
stringToNumber [StringVal x] =
    let x' = T.unpack x
     in case readMaybe x' :: Maybe Double of
            Just n -> pure $ NumVal $ LispReal n
            Nothing -> case readMaybe x' :: Maybe Integer of
                Just n -> pure $ NumVal $ LispInt n
                Nothing -> lispError ValueError "Invalid number format"
stringToNumber _ = expected "string->number" "string"
-- Convert a number to a string using the Haskell `show` function
numberToString [NumVal x] = pure $ StringVal $ T.pack $ show x
numberToString _ = expected "number->string" "number"
-- Print or input a value as text
lispPrint [x] = do
    liftIO $
        TIO.putStr $ case x of
            StringVal y -> y
            y -> showValue y
    pure Null
lispPrint _ = error "Bad arguments"
lispRead _ = StringVal <$> liftIO TIO.getLine
lispEq [x, y] = BoolVal <$> x `equals` y
lispEq _ = error "Bad arguments"

-- Helper function for generating type predicate builtins
typePred :: Text -> (Value -> Bool) -> (Text, Value)
typePred n f = builtin n (== 1) go
  where
    go [x] = pure $ BoolVal $ f x
    go _ = error "Bad arguments"

-- Huge list of builtin functions available to the Lisp.
--     * Arithmetic such as `+`
--     * Comparisons, such as `<` and `=`
--     * Numeric, such as `abs`
--     * Conversions, such as `string->number`
--     * Type predicates, such as `pair?` return a bool depending on
--       if the provided argument are of a given type.
--     * I/O functions such as `print`
--     * Misc value manipulation functions, such as `cons`
builtins :: Scope
builtins =
    M.fromList
        [ builtin "+" (> 0) $ binary (+)
        , builtin "-" (> 0) $ binary (-)
        , builtin "*" (> 0) $ binary (*)
        , builtin "/" (> 0) $ numBinary lispDivision
        , builtin "%" (> 0) $ binary lispMod
        , builtin "<>" (> 0) lispConcat
        , builtin "<" (> 1) $ comparison (<)
        , builtin ">" (> 1) $ comparison (>)
        , builtin "=" (> 1) $ comparison (==)
        , builtin "denominator" (== 1) $
            unpackNumbers >=> lispDenominator
        , builtin "numerator" (== 1) $
            unpackNumbers >=> lispNumerator
        , builtin "abs" (== 1) $ unary abs
        , builtin "expt" (== 2) $ numBinary lispExpt
        , builtin "round" (== 1) $ numUnary lispRound
        , builtin "string->symbol" (== 1) stringToSymbol
        , builtin "symbol->string" (== 1) symbolToString
        , builtin "string->number" (== 1) stringToNumber
        , builtin "number->string" (== 1) numberToString
        , typePred "pair?" $ \case Pair {} -> t; _ -> f
        , typePred "symbol?" $ \case Symbol {} -> t; _ -> f
        , typePred "boolean?" $ \case BoolVal {} -> t; _ -> f
        , typePred "number?" $ \case NumVal {} -> t; _ -> f
        , typePred "integer?" $ \case
            NumVal (LispInt _) -> t
            _ -> f
        , typePred "rational?" $ \case
            NumVal (LispRational _) -> t
            NumVal (LispInt _) -> t
            _ -> f
        , typePred "real?" $ \case NumVal {} -> t; _ -> f
        , typePred "string?" $ \case StringVal {} -> t; _ -> f
        , typePred "procedure?" $ \case Lambda {} -> t; Procedure {} -> t; _ -> f
        , typePred "null?" $ \case Null -> t; _ -> f
        , builtin "print" (== 1) lispPrint
        , builtin "read" (== 0) lispRead
        , builtin "cons" (== 2) cons
        , builtin "eval" (== 1) eval'
        , builtin "show" (== 1) show'
        , builtin "eq?" (== 2) lispEq
        , builtin "error" (> 0) $
            \xs ->
                lispError LispError $
                    T.intercalate ", " $ showValue <$> xs
        ]
  where
    t = True
    f = False

    cons [x, y] = pure (x `Pair` y)
    cons _ = error "Bad arguments"

    eval' [x] = eval x
    eval' _ = error "Bad arguments"

    show' [x] = pure $ StringVal (showValue x)
    show' _ = error "Bad arguments"

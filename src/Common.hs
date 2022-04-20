{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}

module Common where

import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.State (StateT, get, gets, modify, runStateT)
import Data.IORef
import Data.Map (Map)
import Data.Ratio (denominator, numerator)
import Data.Text (Text)
import qualified Data.Text as T

type Symbol = Text

-- 3 number types: Int, Rational, and Real.
data LispNum
    = LispInt Integer
    | LispRational Rational
    | LispReal Double
    deriving (Show)

-- `getNum` converts a `LispNum` back to a number
getNum :: (Fractional a) => LispNum -> a
getNum (LispInt x) = fromInteger x
getNum (LispRational x) = fromRational x
getNum (LispReal x) = realToFrac x

instance Eq LispNum where
    x == y = (getNum x :: Double) == (getNum y :: Double)
instance Ord LispNum where
    x <= y = (getNum x :: Double) <= (getNum y :: Double)

-- The `Value` type models each type of value in the Lisp.
--     * `Pair` stores two values, corresponding to `(x . y)`
--     * `Symbol` stores a symbol such as `print`
--     * `BoolVal`, `NumVal`, `StringVal` store primitives
--     * `Lambda` stores arguments, body, and a closure
--     * `Procedure` stores a builtin function such as `+`
--     * `Struct` stores an instance of a structure type
--     * `Null` represents the end of a list
data Value
    = Pair Value Value
    | Symbol Symbol
    | BoolVal Bool
    | NumVal LispNum
    | StringVal Text
    | Lambda Symbol Value Value Scope
    | Procedure Symbol ([Value] -> Lisp Value)
    | Struct Symbol [(Symbol, IORef Value)]
    | Null
infixr 9 `Pair`

ansi :: [Text] -> Text -> Text
ansi opts x = "\x001b[" <> T.intercalate ";" opts <> "m" <> x <> "\x001b[0m"

ul, bold, red :: Text -> Text
ul = ansi ["4"]
bold = ansi ["1"]
red = ansi ["31"]

-- Recursive function to convert a lisp value to text for printing
showValue :: Value -> Text
showValue x = case x of
    (Pair y z) -> "(" <> bold (showValue y) <> " " <> go z <> ")"
    Symbol n -> n
    BoolVal y -> if y then "#t" else "#f"
    NumVal y -> case y of
        LispInt n -> T.pack $ show n
        LispRational n ->
            T.pack $
                show (numerator n)
                    <> "/"
                    <> show (denominator n)
        LispReal n -> T.pack $ show n
    StringVal y -> "\"" <> y <> "\""
    Lambda n _ _ _ -> "<procedure " <> n <> ">"
    Procedure n _ -> "<procedure " <> n <> ">"
    Struct n _ -> "<struct " <> n <> ">"
    Null -> "'()"
  where
    go (y `Pair` Null) = showValue y
    go (y `Pair` z) = showValue y <> " " <> go z
    go y = " . " <> showValue y

instance Show Value where show = T.unpack . showValue

-- ViewPattern for accessing Lisp list values as Haskell lists
pairedList :: Value -> Maybe [Value]
pairedList = \case
    x `Pair` xs -> fmap (x :) (pairedList xs)
    Null -> Just []
    _ -> Nothing

pattern List :: [Value] -> Value
pattern List xs <- (pairedList -> Just xs)

-- A `Macro` is a compile-time function that modifies Lisp code.
-- Similar to a `Lambda`, it has a name, some arguments, and a body.
data Macro = Macro Symbol Value Value

-- A scope is a mapping from variable names to their values
type Scope = Map Symbol Value

-- The state stored by the interpreter:
--     * `env` is a list of scopes. When a new function
--        is entered, a new scope is appended.
--     * `backtrace` is a list of visited locations.
--     * `preproc` stores information about the preprocessor
data LispSt = LispSt
    { env :: Scope
    , backtrace :: [(Location, [Value])]
    , location :: Location
    , preproc :: LispPreproc
    }

--     * `macros` stores a mapping of macro names to objects
--     * `visited` is a list of filenames. This ensures that
--       a given file is only visited once, and allows for
--       circular dependencies
data LispPreproc = LispPreproc
    { macros :: Map Symbol Macro
    , visited :: [FilePath]
    }

-- The `LispError` type describes each type of error that can be
-- raised during execution
data LispError
    = ParseError
    | FormError
    | ArgumentError
    | TypeError
    | ValueError
    | NameError
    | LispError
    deriving (Show)

-- The `Location` type describes a location in the code that an error
-- occured in
data Location
    = LToplevel
    | LRepl
    | LMacro Symbol
    | LFunction Symbol
    | LPrimitive Symbol
    | LDefine Symbol
    deriving (Eq, Show)

-- The `Lisp` type is a Monad with several capabilities:
--     * A State monad to store data related to the algorithm
--     * An Except monad to track error messages
--     * IO to perform actions such as reading files and printing,
--       and allowing mutable structure types
type Lisp =
    StateT
        LispSt
        ( ExceptT
            ([(Location, [Value])], LispError, Text)
            IO
        )

localSt :: (LispSt -> LispSt) -> Lisp a -> Lisp a
localSt f x = do
    before <- get
    modify f
    x' <- x
    modify (const before)
    pure x'

-- Performs a `Lisp` action under a new environment by modifying the
-- contents of the State monad
withEnv :: (Scope -> Scope) -> Lisp a -> Lisp a
withEnv f = localSt (\st -> st {env = f (env st)})

withLoc :: Location -> Lisp a -> Lisp a
withLoc l = localSt (\st -> st {location = l})

withVal :: Value -> Lisp a -> Lisp a
withVal v x = do
    l <- gets location
    localSt (f l) x
  where
    f l st@LispSt {backtrace = b} = case b of
        (l', vs) : bs ->
            st
                { backtrace =
                    if l == l'
                        then (l', v : vs) : bs
                        else (l, [v]) : (l', vs) : bs
                }
        [] -> st {backtrace = [(l, [v])]}

quote :: Text -> Text
quote x = "'" <> x <> "'"

-- Creates and raises an error. The backtrace is also bundled with
-- the error data type.
lispError :: LispError -> Text -> Lisp a
lispError e t =
    gets backtrace
        >>= \bt -> throwError (bt, e, t)

-- Convert a Lisp list to a Haskell list. If the list is invalid, eg
-- `(3 . 4)`, an error is thrown.
fromPaired :: Value -> Lisp [Value]
fromPaired (x `Pair` y) = (x :) <$> fromPaired y
fromPaired Null = pure []
fromPaired _ = lispError FormError "Bad list form"

-- Create a Lisp list from a Haskell list of `Value`s
toPaired :: [Value] -> Value -> Value
toPaired = flip (foldr Pair)

-- Helper function to generate a builtin function
makeProc :: Text -> (Int -> Bool) -> ([Value] -> Lisp Value) -> Value
makeProc n p f = Procedure n $
    \xs ->
        if p (length xs)
            then f xs
            else lispError ArgumentError $ "incorrect # args to " <> n

-- Run a `Lisp` monad action, given a starting environment.
runEval ::
    Lisp a ->
    Scope ->
    Map Symbol Macro ->
    IO
        ( Either
            ([(Location, [Value])], LispError, Text)
            (a, LispSt)
        )
runEval x e m =
    runExceptT $
        runStateT
            x
            (LispSt e [] LToplevel $ LispPreproc m [])

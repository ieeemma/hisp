{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
module Common where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Map (Map)
import qualified Data.Map as M
import Data.Ratio (numerator, denominator)
import Data.IORef
import Control.Monad.State (StateT, runStateT, gets)
import Control.Monad.Except (ExceptT, runExceptT, throwError)

type Symbol = Text

data LispNum
    = LispInt Integer
    | LispRational Rational
    | LispReal Double
    deriving (Show)

getNum :: (Fractional a) => LispNum -> a
getNum (LispInt x) = fromInteger x
getNum (LispRational x) = fromRational x
getNum (LispReal x) = realToFrac x

data Value
    = Pair Value Value
    | Symbol Symbol
    | BoolVal Bool
    | NumVal LispNum
    | StringVal Text
    | Lambda Value Value [Scope]
    | Procedure Symbol ([Value] -> Lisp Value)
    | Struct Symbol [(Symbol, IORef Value)]
    | Null
infixr `Pair`

instance Show Value where
    show e@(x `Pair` y) = "(" <> showValList e <> ")"
    show (Symbol n) = T.unpack n
    show (BoolVal x) = if x then "#t" else "#f"
    show (NumVal x) = case x of
        LispInt x -> show x
        LispRational x -> show (numerator x) <> "/" <> show (denominator x)
        LispReal x -> show x
    show (StringVal x) = show x
    show (Lambda a b c) = "<lambda " <> show a <> ">"
    show (Procedure n _) = "<procedure " <> T.unpack n <> ">"
    show (Struct n xs) = "<struct " <> T.unpack n <> ">"
    show (Null) = "'()"

pairedList :: Value -> Maybe [Value]
pairedList = \ case
    x `Pair` xs -> fmap (x :) (pairedList xs)
    Null        -> Just []
    _           -> Nothing
pattern List xs <- (pairedList -> Just xs)

data Macro = Macro Symbol Value Value

showValList (x `Pair` Null) = show x
showValList (x `Pair` y) = show x <> " " <> showValList y
showValList x = " . " <> show x

type Scope = Map Symbol Value

data LispSt
    = LispSt { env :: [Scope]
             , backtrace :: [Value]
             , preproc :: LispPreproc }
data LispPreproc
    = LispPreproc { macros :: Map Symbol Macro
                  , visited :: [FilePath] }
data LispError
    = ParseError | FormError | ArgumentError | TypeError | ValueError | NameError
    deriving (Show)

type Lisp = StateT LispSt (ExceptT ([Value], LispError, Text) IO)

lispError :: LispError -> Text -> Lisp a
lispError e t = gets backtrace >>=
    \bt -> throwError (bt, e, t)

fromPaired :: Value -> Lisp [Value]
fromPaired (x `Pair` y) = (x :) <$> fromPaired y
fromPaired Null = pure []
fromPaired _ = lispError FormError "Bad list form"

toPaired :: [Value] -> Value -> Value
toPaired [] e = e
toPaired (x:xs) e = x `Pair` toPaired xs e

makeProc :: Text -> (Int -> Bool) -> ([Value] -> Lisp Value) -> Value
makeProc n p f = Procedure n $
    \xs -> if p (length xs) then f xs
           else lispError ArgumentError $ "incorrect # args to " <> n

runEval :: Lisp a -> Scope -> IO (Either ([Value], LispError, Text) (a, LispSt))
runEval x e = runExceptT $ runStateT x (LispSt [e] [] $ LispPreproc M.empty [])

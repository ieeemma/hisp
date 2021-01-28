{-# LANGUAGE OverloadedStrings #-}
module Common where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Map (Map)
import Control.Monad.State (StateT, runStateT, gets)
import Control.Monad.Except (ExceptT, runExceptT, throwError)

type Symbol = Text

data Value
    = Pair Value Value
    | Symbol Symbol
    | BoolVal Bool
    | NumVal Double
    | StringVal Text
    | Lambda Value Value [Scope]
    | Procedure Text ([Value] -> Lisp Value)
    | Null
infixr `Pair`

instance Show Value where
    show e@(x `Pair` y) = "(" <> showValList e <> ")"
    show (Symbol n) = T.unpack n
    show (BoolVal x) = if x then "#t" else "#f"
    show (NumVal x) = show x
    show (StringVal x) = show x
    show (Lambda a b c) = "<lambda " <> show a <> ">"
    show (Procedure n _) = "<procedure " <> T.unpack n <> ">"
    show (Null) = "'()"


showValList (x `Pair` Null) = show x
showValList (x `Pair` y) = show x <> " " <> showValList y
showValList x = " . " <> show x

type Scope = Map Symbol Value

data LispSt = LispSt { env :: [Scope], backtrace :: [Value] } deriving (Show)
data LispError
    = FormError | ArgumentError | TypeError | ValueError | NameError
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

runEval :: Lisp a -> Scope -> IO (Either ([Value], LispError, Text) (a, LispSt))
runEval x e = runExceptT $ runStateT x (LispSt [e] [])

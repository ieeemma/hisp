{-# LANGUAGE OverloadedStrings #-}
module Common where

import Data.Text (Text)
import Data.Map (Map)
import Control.Monad.State (StateT, runStateT, gets)
import Control.Monad.Except (ExceptT, runExceptT, throwError)

type Symbol = Text

data Value
    = Pair Value Value
    | Symbol Symbol
    | NumVal Double
    | StringVal Text
    | Lambda Value Value [Scope]
    | Null
    deriving (Show)
infixr `Pair`

type Scope = Map Symbol Value

data LispSt = LispSt { env :: [Scope], backtrace :: [Value] } deriving (Show)
data LispError = FormError Text
               | ArgumentError Text
               | TypeError Text
               | NameError Text
               deriving (Show)

type Lisp = StateT LispSt (ExceptT ([Value], LispError) IO)

lispError :: LispError -> Lisp a
lispError x = gets backtrace >>=
    \bt -> throwError (bt, x)

fromPaired :: Value -> Lisp [Value]
fromPaired (x `Pair` y) = (x :) <$> fromPaired y
fromPaired Null = pure []
fromPaired _ = lispError $ FormError "Bad list form"

toPaired :: [Value] -> Value -> Value
toPaired [] e = e
toPaired (x:xs) e = x `Pair` toPaired xs e

runEval :: Lisp a -> Scope -> IO (Either ([Value], LispError) (a, LispSt))
runEval x e = runExceptT $ runStateT x (LispSt [e] [])

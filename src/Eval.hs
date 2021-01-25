{-# LANGUAGE OverloadedStrings, LambdaCase, PatternSynonyms, ViewPatterns #-}
module Eval where

import Common

import qualified Data.Map as M
import Control.Applicative ((<|>))
import Control.Monad.State (get, gets, put, modify)

withEnv :: (Scope -> Scope) -> Lisp a -> Lisp a
withEnv f x = do
    st <- get
    modify $ \st -> st {env = case env st of x:xs -> f x : xs}
    x' <- x
    put st
    pure x'

pairedList :: Value -> Maybe [Value]
pairedList = \ case
  x `Pair` xs -> fmap (x :) (pairedList xs)
  Null        -> Just []
  _           -> Nothing
pattern List xs <- (pairedList -> Just xs)

match :: Value -> Value -> Lisp Scope
match (x `Pair` xs) (y `Pair` ys) = M.union <$> match x y <*> match xs ys
match (Symbol n) y = pure $ M.singleton n y
match Null Null = pure M.empty
match Null _ = lispError $ ArgumentError "too many arguments to function call"
match _ Null = lispError $ ArgumentError "too few arguments to function call"
match _ _    = lispError $ ArgumentError "wrong arguments to function call"

forms = ["define", "lambda", "do"]

eval :: Value -> Lisp Value
eval e@(Symbol x `Pair` _) | x `elem` forms =
    case e of
        List [Symbol "define", Symbol name, value] ->
            modify (\st -> st { env =
                case env st of (x:xs) -> (M.insert name value x) : xs
            }) *> pure Null
        List [Symbol "lambda", args, body] ->
            Lambda args body <$> gets env
        List (Symbol "do" : xs) ->
            last <$> eval `traverse` xs
        _ -> undefined

eval (x `Pair` y) = do
    fn <- eval x
    case fn of
        Lambda a b c -> do
            new <- match a y
            withEnv (M.union new) $ eval b
        -- Proc f ->
        _ -> lispError $ TypeError "Tried to call non-function"

eval (Symbol x) = do
    l <- (foldr (<|>) Nothing . fmap (M.lookup x)) <$> gets env
    case l of
        Just x -> pure x
        Nothing -> lispError $ NameError x

eval x = pure x


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

pairedMapM :: (Value -> Lisp Value) -> Value -> Lisp Value
pairedMapM f (List xs) = flip toPaired Null <$> f `traverse` xs
pairedMapM f x = pure x

match :: Value -> Value -> Lisp Scope
match (x `Pair` xs) (y `Pair` ys) = M.union <$> match x y <*> match xs ys
match (Symbol n) y = pure $ M.singleton n y
match Null Null = pure M.empty
match Null _ = lispError ArgumentError "too many arguments to function call"
match _ Null = lispError ArgumentError "too few arguments to function call"
match _ _    = lispError ArgumentError "wrong arguments to function call"

truthy (BoolVal False) = False
truthy _ = True

forms = ["quote", "define", "lambda", "do", "if"]

eval' :: Value -> Lisp Value
eval' e@(Symbol x `Pair` _) | x `elem` forms =
    case e of
        List [Symbol "quote", x] -> pure x
        List [Symbol "define", Symbol name, value] -> do
            value' <- eval value
            ([x], xs) <- splitAt 1 <$> gets env
            modify $ \st -> st { env = M.insert name value' x : xs }
            pure Null
        List [Symbol "lambda", args, body] ->
            Lambda args body <$> gets env
        List (Symbol "do" : xs) ->
            last <$> eval `traverse` xs
        List [Symbol "if", c, t, f] ->
            eval c >>= \c' -> eval $ if truthy c' then t else f
        _ -> undefined

eval' (x `Pair` y) = do
    fn <- eval x
    case fn of
        Lambda a b c -> do
            new <- pairedMapM eval y >>= match a
            withEnv (M.union new) $ eval b
        Procedure _ f ->
            fromPaired y >>= (eval `traverse`) >>= f
        _ -> lispError TypeError "Tried to call non-function"

eval' (Symbol x) = do
    l <- (foldr (<|>) Nothing . fmap (M.lookup x)) <$> gets env
    case l of
        Just x -> pure x
        Nothing -> lispError NameError x

eval' x = pure x

eval x = do
    modify $ \st -> st { backtrace = x : backtrace st }
    x' <- eval' x
    modify $ \st -> st { backtrace = tail $ backtrace st }
    pure x'

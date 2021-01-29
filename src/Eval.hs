module Eval where

import Common
import Struct

import qualified Data.Map as M
import Data.Traversable (for)
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

define :: Symbol -> Value -> Lisp ()
define n x = do
    ([e], es) <- splitAt 1 <$> gets env
    modify $ \st -> st { env = M.insert n x e : es }

forms = ["quote", "define", "define-struct", "lambda", "do", "if"]

eval' :: Value -> Lisp Value
eval' e@(Symbol x `Pair` _) | x `elem` forms =
    case e of
        List [Symbol "quote", x] -> pure x
        List [Symbol "define", Symbol name `Pair` args, body] ->
            (eval (toPaired [Symbol "lambda", args, body] Null)
                  >>= define name)
            *> pure Null
        List [Symbol "define", Symbol name, value] ->
            (eval value >>= define name) *> pure Null
        List (Symbol "define-struct" : Symbol name : fields) -> do
            let extract = \case
                    Symbol x -> pure x
                    _ -> lispError FormError "define-struct expected symbol field name"
            fields' <- extract `traverse` fields
            let makeN = "make-" <> name
                makeF = makeProc makeN (== (length fields)) $ makeStruct name fields'
            define makeN makeF
            for fields' $ \f -> do
                let getN = name <> "-" <> f
                    getF = makeProc getN (== 1) $ \[st] -> case st of
                        Struct n xs | n == name -> getField n xs f
                        _ -> lispError TypeError (getN <> " expected " <> name <> " argument")
                    setN = name <> "-" <> f <> "-set!"
                    setF = makeProc getN (== 2) $ \[st, x] -> case st of
                        Struct n xs | n == name -> setField n xs f x
                                                *> pure Null
                        _ -> lispError TypeError (setN <> " expected " <> name <> " argument")
                define getN getF
                define setN setF
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

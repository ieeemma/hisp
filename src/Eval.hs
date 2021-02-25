module Eval where

import Common

import qualified Data.Map as M
import Data.Traversable (for)
import Control.Applicative ((<|>))
import Control.Monad.State (get, gets, put, modify)

import Debug.Trace
import qualified Data.Text as T

-- Performs a `Lisp` action under a new environment by modifying the
-- contents of the State monad
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

-- Psuedo-pattern-matching for function arguments. Given a list of
-- arguments and a list of values, attempt to match them to produce a
-- new scope, or throw an error. For example:
--     * match (a b) (3 4) => {a: 3, b: 4}
--     * match xs    (3 4) => {xs: (3 4)}
match :: Value -> Value -> Lisp Scope
match (x `Pair` xs) (y `Pair` ys) = M.union <$> match x y <*> match xs ys
match (Symbol n) y = pure $ M.singleton n y
match Null Null = pure M.empty
match Null _ = lispError ArgumentError "too many arguments to call"
match _ Null = lispError ArgumentError "too few arguments to call"
match _ _    = lispError ArgumentError "wrong arguments to call"

-- Anything not a `BoolVal False` is truthy. This is strange
-- behaviour but consistent with Scheme.
truthy (BoolVal False) = False
truthy _ = True

-- Helper function to define a new value in the current environment
define :: Symbol -> Value -> Lisp ()
define n x = do
    ([e], es) <- splitAt 1 <$> gets env
    modify $ \st -> st { env = M.insert n x e : es }
    
makeEnv :: [Value] -> Lisp Scope
makeEnv [] = pure M.empty
makeEnv (List [Symbol name, value] : xs) = do
    value' <- withVal value $ eval value
    xs' <- makeEnv xs
    pure $ M.insert name value' xs'
makeEnv _ = lispError FormError "Malformed let arguments"

forms = ["quote", "define", "lambda", "let", "do", "if"]

-- `eval` is a depth-first traversal algorithm of the Value tree type.
eval :: Value -> Lisp Value

-- A list where the head is a symbol in `forms` is a special macro
-- form. These are:
--     * `define` is used to declare new variables, either as a
--       function or a normal value.
--     * `lambda` constructs a lambda function value
--     * `do` executes statements in turn, returning the result of
--       the last execution
--     * `if` conditionally executes one of two expressions,
--       depending on the truthiness of the condition
eval e@(Symbol x `Pair` _) | x `elem` forms =
    case e of
        List [Symbol "quote", x] -> pure x
        List [Symbol "define", Symbol name `Pair` args, body] ->
            (Lambda name args body <$> gets env >>= define name)
            *> pure Null
        List [Symbol "define", Symbol name, value] ->
            withLoc (LDefine name) $ withVal value $ (eval value >>= define name) *> pure Null
        List [Symbol "lambda", args, body] ->
            Lambda "lambda" args body <$> gets env
        List [Symbol "let", values, body] -> do
            env' <- fromPaired values >>= makeEnv
            withVal body $ withEnv (M.union env') $ eval body
        List (Symbol "do" : xs) ->
            last <$> eval `traverse` xs
        List [Symbol "if", c, t, f] ->
            eval c >>= \c' -> eval $ if truthy c' then t else f
        _ -> undefined

-- A list such as `(f x)` is run as a function. First, the head of
-- the list is evaluated to determine its value:
--     * A lambda is executed using the `match` function on the
--       lambda arguments and the provided arguments
--     * A procedure is executed by passing the provided arguments
--       to the builtin Haskell function
--     * Any other value is not a function so a type error is raised
eval (x `Pair` y) = do
    fn <- withVal x $ eval x
    case fn of
        Lambda n a b c -> withLoc (LFunction n) $ do
            y' <- withVal y $ fromPaired y
            ys <- for y' $ \y -> withVal y $ eval y
            new <- withVal a $ match a (toPaired ys Null)
            withEnv (M.union new)
                $ withVal b
                $ eval b
        Procedure n f ->
            withLoc (LFunction n)
                $ withVal fn
                $ fromPaired y >>= (eval `traverse`) >>= f
        _ -> lispError TypeError "Tried to call non-function"

-- A symbol is evaluated by looking up its value in the current scope.
-- This is found in the State monad. If the variable is not present
-- in the mapping, it is unbound, so a name error is raised.
eval (Symbol x) = do
    l <- (foldr (<|>) Nothing . fmap (M.lookup x)) <$> gets env
    case l of
        Just x -> pure x
        Nothing -> lispError NameError x

-- Any other value evaluates to itself
eval x = pure x


module Run where

import Common
import Parse
import Eval
import Error (errorPretty)
import Struct

import Data.Map (Map)
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Maybe (isNothing, fromJust)
import Data.Foldable (traverse_)
import Data.Traversable (for)
import Text.Megaparsec (parse, errorBundlePretty)
import Control.Applicative ((<|>))
import Control.Monad (when, (>=>))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State (gets, modify)
import Control.Monad.Except (throwError)
import System.Console.Readline hiding (Macro)

-- Read a file, or throw an error if it does not exist
loadFile :: FilePath -> Lisp Text
loadFile = liftIO . TIO.readFile

-- Parse some text and return a list of values. This uses the `Parser`
-- module. If the file failed to parse, raise an error.
parseFile :: FilePath -> Text -> Lisp [Value]
parseFile f x = case parse file f x of
    Right xs -> pure xs
    Left e -> lispError ParseError (T.pack $ errorBundlePretty e)

-- Given a value, try to expand macros found within it. If a macro
-- was successfully expanded, return `Just`, otherwise return
-- `Nothing`. This allows the following psuedocode:
--     while (value != Nothing)
--         value = expand(value)
expand :: Value -> Lisp (Maybe Value)
expand (Symbol x `Pair` xs) = do
    args <- fromPaired xs
    expd <- expand `traverse` args
    let expd' = fromJust <$> zipWith (<|>) expd (Just <$> args)
        xs' = toPaired expd' Null
    ms <- gets (macros . preproc)
    case x `M.lookup` ms of
        Just (Macro _ a b) -> do
            new <- match a xs'
            Just <$> withEnv (M.union new) (eval b)
        Nothing -> if all isNothing expd
                   then pure Nothing
                   else pure $ Just $ Symbol x `Pair` xs'
expand _ = pure Nothing

-- Simple while loop to keep expanding a value until it cannot be
-- expanded any further
expandFully :: Value -> Lisp Value
expandFully x = expand x >>= \case
    Just x' -> expandFully x'
    Nothing -> pure x

macroForms = ["define", "define-macro", "define-struct", "do", "load"]

-- The following forms are valid at a toplevel and must be handled
-- as special cases during macro expansion.
-- The first element of the tuple is the processed values, the second
-- are the unprocessed values generated while processing the first.
-- This is used to allow file loading.
expandToplevel :: Value -> Lisp ([Value], [Value])
expandToplevel e@(Symbol x `Pair` _) | x `elem` macroForms =
    case e of
        -- To expand a function definition, fully expand its body and
        -- convert to a lambda`
        List [Symbol "define", Symbol name `Pair` args, body] -> do
            b' <- expandFully body
            v <- eval $ toPaired [Symbol "lambda", args, b'] Null
            define name v
            pure ([], [])
        -- To expand a definition, fully expand the assigned value
        List [Symbol "define", Symbol name, value] -> do
            v <- expandFully value >>= eval
            define name v
            pure ([], [])
        -- When a macro definition is encountered, add it to the `State`
        -- so later definitions can use it
        List [Symbol "define-macro", Symbol name `Pair` args, body] -> do
            modify $ \st ->
                let p = preproc st
                in st { preproc = p { macros = M.insert name (Macro name args body) (macros p) } }
            pure ([], [])
        -- Call `defineStruct` from the `Struct` module to handle the
        -- creation of the struct type
        List (Symbol "define-struct" : Symbol name : fields) ->
            defineStruct name fields *> pure ([], [])
        -- A `do` form at the toplevel is also handled as if it was
        -- part of the toplevel
        List (Symbol "do" : xs) ->
            pure ([], xs)
        -- Loading a file uses the above `parseFile` function.
        -- Additionally check that the file has not already been
        -- visited. If so, skip it.
        List [Symbol "load", StringVal f] -> do
            let f' = T.unpack f
            vs <- gets (visited . preproc)
            if f' `notElem` vs then do
                modify $ \st ->
                    let p = preproc st
                    in st { preproc = p { visited = f' : visited p } }
                xs <- loadFile f' >>= parseFile f'
                pure ([], xs)
            else pure ([], [])
expandToplevel x = do
    x' <- expandFully x
    pure ([x'], [])

-- For a given file, fully expand each value
expandFile :: [Value] -> Lisp [Value]
expandFile xs = exp [] xs
    where exp :: [Value] -> [Value] -> Lisp [Value]
          exp d [] = pure d
          exp d (r:rs) = do
              (d', r') <- expandToplevel r
              exp (d <> d') (r' <> rs)

-- To run a file:
--     * Parse it using `parseFile`
--     * Then, expand all macros using `expandFile`
--     * Then, evaluate each form using `traverse_ eval`
run :: FilePath -> Lisp ()
run f = loadFile f >>= parseFile f >>= expandFile >>= traverse_ eval

-- Take input from the user and run the provided code. The environment
-- is persistent, so defining a variable stays for the entire REPL
-- session.
repl :: Scope -> IO ()
repl e = readline "> " >>= \case
    Just x -> do
        addHistory x
        let action = do
                xs <- parseFile "<stdin>" (T.pack x) >>= expandFile
                for xs $ eval >=> \case
                    Null -> pure ()
                    x' -> (showValue x' >>= liftIO . TIO.putStrLn)
                       *> pure ()
        runEval action e >>= \case
            Right (_, st) -> repl (head $ env st)
            Left (bt, err, t) -> TIO.putStrLn (errorPretty bt err t)
                              *> repl e
    Nothing -> pure ()

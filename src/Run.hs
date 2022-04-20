module Run where

import Common
import Error (errorPretty)
import Eval
import Parse (file)
import Struct

import Control.Applicative ((<|>))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State (gets, modify)
import Data.Foldable (traverse_)
import Data.Functor (($>), (<&>))
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (fromJust, isNothing)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Traversable (for)
import System.Console.Readline hiding (Macro)
import Text.Megaparsec (errorBundlePretty, parse)

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
expand (Symbol "quote" `Pair` _) = pure Nothing
expand (List [Symbol "macroexpand", x]) =
    expandFully x <&> \y ->
        Just (toPaired [Symbol "quote", y] Null)
expand (x `Pair` xs) = do
    args <-
        withVal xs $
            fromPaired xs
    fn <-
        withVal x $
            expand x
    expd <- expand `traverse` args
    let expd' = fromJust <$> zipWith (<|>) expd (Just <$> args)
        xs' = toPaired expd' Null
        fn' = fromJust (fn <|> Just x)
        noMacro =
            if all isNothing expd
                then pure Nothing
                else pure $ Just $ x `Pair` xs'
    ms <- gets (macros . preproc)
    case fn' of
        Symbol n -> case M.lookup n ms of
            Just (Macro _ a b) -> withLoc (LMacro n) $ do
                new <-
                    withVal xs' $
                        match a xs'
                withVal b $
                    withEnv (M.union new) $
                        Just <$> (expandFully b >>= eval)
            Nothing -> noMacro
        _ -> noMacro
expand _ = pure Nothing

-- Simple while loop to keep expanding a value until it cannot be
-- expanded any further
expandFully :: Value -> Lisp Value
expandFully x =
    expand x >>= \case
        Just x' -> expandFully x'
        Nothing -> pure x

macroForms :: [Text]
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
        List (Symbol "define" : Symbol name `Pair` args : body) -> do
            let body' = toPaired (Symbol "do" : body) Null
            body'' <-
                withLoc (LDefine name) $
                    withVal body' $
                        expandFully body'
            e' <- gets env
            define name (Lambda name args body'' e')
            pure ([], [])
        -- To expand a definition, fully expand the assigned value
        List [Symbol "define", Symbol name, value] -> do
            v <-
                withLoc (LDefine name) $
                    withVal value $
                        expandFully value >>= eval
            define name v
            pure ([], [])
        -- When a macro definition is encountered, add it to the `State`
        -- so later definitions can use it
        List [Symbol "define-macro", Symbol name `Pair` args, body] -> do
            modify $ \st ->
                let p = preproc st
                 in st
                        { preproc =
                            p
                                { macros =
                                    M.insert name (Macro name args body) (macros p)
                                }
                        }
            pure ([], [])
        -- Call `defineStruct` from the `Struct` module to handle the
        -- creation of the struct type
        List (Symbol "define-struct" : Symbol name : fields) ->
            defineStruct name fields $> ([], [])
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
            if f' `notElem` vs
                then do
                    modify $ \st ->
                        let p = preproc st
                         in st {preproc = p {visited = f' : visited p}}
                    xs <- loadFile f' >>= parseFile f'
                    pure ([], xs)
                else pure ([], [])
        _ -> lispError FormError $ "Malformed special form " <> quote x
expandToplevel x = do
    x' <- withVal x $ expandFully x
    pure ([x'], [])

-- For a given file, fully expand each value
expandFile :: [Value] -> Lisp [Value]
expandFile = ex []
  where
    ex :: [Value] -> [Value] -> Lisp [Value]
    ex d [] = pure d
    ex d (r : rs) = do
        (d', r') <- expandToplevel r
        ex (d <> d') (r' <> rs)

-- To run a file:
--     * Parse it using `parseFile`
--     * Then, expand all macros using `expandFile`
--     * Then, evaluate each form using `traverse_ eval`
run :: FilePath -> Lisp ()
run f =
    loadFile f >>= parseFile f >>= expandFile
        >>= \xs ->
            withLoc LToplevel $
                (\x -> withVal x $ eval x) `traverse_` xs

-- Take input from the user and run the provided code. The environment
-- is persistent, so defining a variable stays for the entire REPL
-- session.
repl :: Scope -> Map Symbol Macro -> IO ()
repl e m =
    readline "> " >>= \case
        Just x -> do
            addHistory x
            let action = do
                    ys <- parseFile "<stdin>" (T.pack x) >>= expandFile
                    for ys $ \y ->
                        withLoc LRepl (withVal y $ eval y) >>= \case
                            Null -> pure ()
                            y' -> liftIO $ TIO.putStrLn (showValue y') $> ()
            runEval action e m >>= \case
                Right (_, st) -> repl (env st) (macros . preproc $ st)
                Left (bt, err, t) ->
                    TIO.putStrLn (errorPretty bt err t)
                        *> repl e m
        Nothing -> pure ()

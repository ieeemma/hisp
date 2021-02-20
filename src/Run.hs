module Run where

import Common
import Parse
import Eval
import Struct

import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Maybe (isNothing, fromJust)
import Data.Foldable (traverse_)
import Text.Megaparsec (parse)
import Control.Applicative ((<|>))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State (gets, modify)
import Control.Monad.Except (throwError)

parseFile :: FilePath -> Lisp [Value]
parseFile f = do
    contents <- liftIO $ TIO.readFile f
    case parse file f contents of
        Right xs -> pure xs
        Left e -> -- throwError $ ParseError e
                  undefined

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

expandFully :: Value -> Lisp Value
expandFully x = expand x >>= \case
    Just x' -> expandFully x'
    Nothing -> pure x

macroForms = ["define", "define-macro", "define-struct", "do", "load"]

expandToplevel :: Value -> Lisp ([Value], [Value])
expandToplevel e@(Symbol x `Pair` _) | x `elem` macroForms =
    case e of
        List [Symbol "define", Symbol name `Pair` args, body] -> do
            b' <- expandFully body
            v <- eval $ toPaired [Symbol "lambda", args, b'] Null
            define name v
            pure ([], [])
        List [Symbol "define", Symbol name, value] -> do
            v <- expandFully value >>= eval
            define name v
            pure ([], [])
        List [Symbol "define-macro", Symbol name `Pair` args, body] -> do
            modify $ \st ->
                let p = preproc st
                in st { preproc = p { macros = M.insert name (Macro name args body) (macros p) } }
            pure ([], [])
        List (Symbol "define-struct" : Symbol name : fields) ->
            defineStruct name fields *> pure ([], [])
        List (Symbol "do" : xs) ->
            pure ([], xs)
        List [Symbol "load", StringVal f] -> do
            let f' = T.unpack f
            vs <- gets (visited . preproc)
            if f' `notElem` vs then do
                modify $ \st ->
                    let p = preproc st
                    in st { preproc = p { visited = f' : visited p } }
                xs <- parseFile f'
                liftIO $ putStr "Imported " *> print xs
                pure ([], xs)
            else pure ([], [])
        _ -> lispError FormError "Malformed special form"
expandToplevel x = do
    x' <- expandFully x
    pure ([x'], [])

expandFile :: [Value] -> Lisp [Value]
expandFile xs = exp [] xs
    where exp :: [Value] -> [Value] -> Lisp [Value]
          exp d [] = pure d
          exp d (r:rs) = do
              (d', r') <- expandToplevel r
              exp (d <> d') (r' <> rs)

run :: FilePath -> Lisp ()
run f = parseFile f >>= expandFile >>= traverse_ eval

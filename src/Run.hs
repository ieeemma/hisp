module Run where

import Common
import Parse
import Eval

import Data.Map (Map)
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Either (partitionEithers)
import Data.Bifunctor (first)
import Text.Megaparsec (parse, errorBundlePretty)
import System.FilePath
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State (modify)

getMacros :: [Value] -> Lisp (Map Symbol Macro, [Value])
getMacros tls = first M.unions . partitionEithers <$> extract `traverse` tls
    where extract (List (Symbol "define-macro" : xs)) = case xs of
              [Symbol name `Pair` args, body] ->
                  pure $ Left $ M.singleton name $ Macro name args body
              _ -> lispError FormError "Malformed macro definition"
          extract x = pure $ Right x

expandMacros :: Map Symbol Macro -> Value -> Lisp Value
expandMacros ms e@(List (Symbol "quote":_)) = pure e
expandMacros ms (Symbol x `Pair` xs) = do
    xs' <- pairedMapM (expandMacros ms) xs
    case M.lookup x ms of
        Just (Macro _ a b) -> do
            new <- match a xs'
            withEnv (M.union new) $ eval b
        Nothing -> pure $ Symbol x `Pair` xs'
expandMacros ms x = pure x

getLoads :: [Value] -> Lisp ([FilePath], [Value])
getLoads tls = partitionEithers <$> extract `traverse` tls
    where extract (List (Symbol "load" : xs)) = case xs of
              [Symbol f]    -> pure $ Left $ T.unpack f
              [StringVal f] -> pure $ Left $ T.unpack f
              _ -> lispError FormError "Malformed load form"
          extract x = pure $ Right x

run :: FilePath -> Lisp ()
run f = do
    contents <- liftIO $ TIO.readFile f
    case parse file f contents of
        Right x -> do
            (fs, xs) <- getLoads x
            run `traverse` fs
            (ms, xs') <- getMacros xs
            expd <- expandMacros ms `traverse` xs'
            evld <- eval `traverse` expd
            modify $ \st -> st { macros = M.union ms $ macros st }
        Left e -> lispError ParseError $ T.pack $ errorBundlePretty e

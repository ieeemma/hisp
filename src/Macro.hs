module Macro where

import Common
import Eval

import Data.Either (partitionEithers)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Bifunctor (first)

extractMacros :: [Value] -> Lisp (Map Symbol Macro, [Value])
extractMacros tls = first M.unions . partitionEithers <$> extract `traverse` tls
    where extract (List (Symbol "define-macro" : xs)) = case xs of
              [Symbol name, args, body] ->
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

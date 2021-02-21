module Main where

import Common (runEval, env)
import Error (errorPretty)
import Core (builtins)
import Run (run, repl)

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.IO
import System.Environment

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    
    e <- runEval (run "lib/core.lsp") builtins >>= \case
        Right (_, st) -> pure $ head $ env st
        Left (bt, e, t) -> TIO.putStrLn (errorPretty bt e t)
                        *> error "Failed to load core module"
    
    getArgs >>= \case
        [] -> repl e
        [file] -> runEval (run "lib/core.lsp" *> run file) e >>= \case
            Right _ -> pure ()
            Left (bt, e, t) -> TIO.putStrLn $ errorPretty bt e t
        _ -> do
            p <- getProgName
            putStrLn $ "Usage: " <> p <> " file.lsp\n"
                    <> "       " <> p

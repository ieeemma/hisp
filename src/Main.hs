module Main where

import Common
import Core (builtins)
import Error (errorPretty)
import Run (repl, run)

import qualified Data.Text.IO as TIO
import System.Environment
import System.IO

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering

    (ms, e) <-
        runEval (run "lib/core.lsp") builtins mempty >>= \case
            Right (_, st) -> pure (macros (preproc st), env st)
            Left (bt, e, t) ->
                TIO.putStrLn (errorPretty bt e t)
                    *> error "Failed to load core module"

    getArgs >>= \case
        [] -> repl e ms
        [file] ->
            runEval (run file) e ms >>= \case
                Right _ -> pure ()
                Left (bt, e', t) -> TIO.putStrLn $ errorPretty bt e' t
        _ -> do
            p <- getProgName
            putStrLn $
                "Usage: " <> p <> " file.lsp\n"
                    <> "       "
                    <> p

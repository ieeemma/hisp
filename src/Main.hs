module Main where

import Common
import Parse
import Eval
import Core
import Run

main :: IO ()
main = runEval (run "test.scm") builtins >>= \case
    Right _ -> pure ()
    Left e -> print e


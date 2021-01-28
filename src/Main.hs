{-# LANGUAGE OverloadedStrings, LambdaCase #-}
module Main where

import Common
import Parse
import Eval
import Core

import qualified Data.Text.IO as TIO

import Text.Megaparsec (parse, errorBundlePretty)

main :: IO ()
main = do
    f <- TIO.readFile "test.scm"
    case parse file "" f of
        Right xs -> runEval (eval `traverse` xs) builtins >>= \case
            Right (x, _) -> print x
            Left e -> print e
        Left e -> putStrLn $ errorBundlePretty e

{-# LANGUAGE OverloadedStrings #-}
module Main where

import Common
import Parse
import Eval

import Text.Megaparsec (parse, errorBundlePretty)

fn = Symbol "lambda" `Pair` (Symbol "x" `Pair` Null) `Pair` Symbol "x" `Pair` Null
arg = NumVal 5

test = "((lambda (x) x) 5)"

main :: IO ()
main = case parse file "" test of
    Right xs -> runEval (eval `traverse` xs) mempty >>= print
    Left e -> putStrLn $ errorBundlePretty e



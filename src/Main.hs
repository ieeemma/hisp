module Main where

import Common
import Parse
import Macro
import Eval
import Core

import qualified Data.Text.IO as TIO

import Text.Megaparsec (parse, errorBundlePretty)

run :: [Value] -> IO ()
run xs =
    runEval go builtins >>= \case
        Right {} -> pure ()
        Left e -> print e
    where go = extractMacros xs
           >>= (\(ms, xs') -> expandMacros ms `traverse` xs')
           >>= traverse eval


main :: IO ()
main = do
    f <- TIO.readFile "test.scm"
    case parse file "" f of
        Right xs -> run xs
        Left e -> putStrLn $ errorBundlePretty e

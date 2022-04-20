module Error where

import Common

import Data.Text (Text)
import qualified Data.Text as T

showBacktrace :: [(Location, [Value])] -> Text
showBacktrace bt = T.intercalate "\n" $ f <$> reverse bt
    where f (l, xs) =
              let xs' = T.intercalate "\n"
                      . fmap ("   " <>)
                      . T.lines
                      . showValue
                     <$> reverse xs
              in title l <> T.intercalate "\n" xs'

title :: Location -> Text
title = \case
    LToplevel -> "In file:\n"
    LRepl -> "In the REPL:\n"
    LFunction x -> "In the function " <> quote x <> ":\n"
    LPrimitive x -> "In the primitive function " <> quote x <> "\n"
    LMacro x -> "In the macro expansion " <> quote x <> ":\n"
    LDefine x -> "In the definition of " <> quote x <> ":\n"

-- Display an error message as a string so it can be printed to the
-- screen. TODO: use backtrace.
errorPretty :: [(Location, [Value])] -> LispError -> Text -> Text
errorPretty _ ParseError t = t
errorPretty bt e t =
       showBacktrace bt <> red "  <--- Here" <> "\n"
    <> "\n"
    <> red (T.pack $ show e) <> ": " <> t

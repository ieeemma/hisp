module Error where

import Common

import Data.Text (Text)
import qualified Data.Text as T
import Text.Megaparsec (errorBundlePretty)

{-
instance Show ErrorType where
    show = \case
        FormError -> "FormError"
        ArgumentError -> "ArgumentError"
        TypeError -> "TypeError"
        ValueError -> "ValueError"
        NameError -> "NameError"


prettyBacktrace :: [LValue] -> Text
prettyBacktrace [] = ""
prettyBacktrace (b

errorPretty :: LispError -> Text
errorPretty (ParseError e) = T.pack $ errorBundlePretty e
errorPretty (ExecError t e bt) =
    prettyBacktrace bt <> "\n" <> T.pack (show t) <> ": " <> e
-}


module Error where

import Common

import Data.Text (Text)
import qualified Data.Text as T
import Text.Megaparsec (errorBundlePretty)

-- Display an error message as a string so it can be printed to the
-- screen. TODO: use backtrace.
errorPretty :: [Value] -> LispError -> Text -> Text
errorPretty _ ParseError t = t
errorPretty _ e t = T.pack (show e) <> ": " <> t

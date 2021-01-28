{-# LANGUAGE OverloadedStrings #-}
module Parse where

import Common

import Data.Char (isSpace)
import Text.Megaparsec
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Megaparsec.Char (space1)
import Data.Void (Void)
import Data.Text (Text)
import qualified Data.Text as T

type Parser = Parsec Void Text

sc =
    L.space
    space1
    (L.skipLineComment ";")
    (L.skipBlockCommentNested "#|" "|#")
symbol = L.symbol sc

lexeme = (<* sc)

parens x =  (symbol "(" *> x <* symbol ")")
        <|> (symbol "[" *> x <* symbol "]")

list :: Parser Value
list = lexeme $ parens $ do
    xs  <- some sexpression
    end <- (symbol "." *> sexpression) <|> pure Null
    pure $ toPaired xs end

quote :: Parser Value
quote = do
    x <- symbol "'" *> sexpression
    pure $ toPaired [Symbol "quote", x] Null

unquote :: Parser Value
unquote = do
    x <- symbol "," *> sexpression
    pure $ toPaired [Symbol "quote", x] Null

signedInt = lexeme $
    NumVal <$> L.signed empty L.decimal <* notFollowedBy symbolStart
signedFloat = lexeme $
    NumVal <$> L.signed empty L.float <* notFollowedBy symbolStart

symbolPred :: [Char] -> Char -> Bool
symbolPred xs = \c -> (not $ isSpace c) && c `notElem` xs

symbolStart, symbolMid :: Parser Char
symbolStart = satisfy $ symbolPred ['(',')','[',']', '.','\'',',']
symbolMid   = satisfy $ symbolPred ['(',')','[',']']

stringLit = lexeme $
    StringVal <$> T.pack <$> (symbol "\"" *> manyTill L.charLiteral (symbol "\""))

symbol' = lexeme $ do
    s <- sym
    case s of
        "#t" -> pure $ BoolVal True
        "#f" -> pure $ BoolVal False
        _    -> pure $ Symbol $ T.pack s
    where sym = ((:) <$> symbolStart <*> many symbolMid)

sexpression :: Parser Value
sexpression
    =  list
   <|> quote <|> unquote
   <|> try signedInt
   <|> try stringLit
   <|> symbol'

file = sc *> many sexpression <* eof

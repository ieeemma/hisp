module Parse where

import Common hiding (quote)

import Data.Char (isSpace)
import Data.Functor (($>))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char (space1)
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void Text

sc :: Parser ()
sc =
    L.space
        space1
        (L.skipLineComment ";")
        (L.skipBlockCommentNested "#|" "|#")

symbol :: Text -> Parser Text
symbol = L.symbol sc

lexeme :: Parser a -> Parser a
lexeme = (<* sc)

parens :: Parser a -> Parser a
parens x =
    (symbol "(" *> x <* symbol ")")
        <|> (symbol "[" *> x <* symbol "]")

list :: Parser Value
list = lexeme $
    parens $ do
        xs <- some sexpression
        end <- (symbol "." *> sexpression) <|> pure Null
        pure $ toPaired xs end

prefixed :: Text -> Symbol -> Parser Value
prefixed x name = do
    _ <- symbol x
    val <- sexpression
    pure $ toPaired [Symbol name, val] Null

quote :: Parser Value
quote =
    try null' <|> prefixed "'" "quote"
        <|> prefixed "`" "quasiquote"
  where
    null' =
        (symbol "'" <|> symbol "`")
            *> symbol "("
            *> symbol ")"
            $> Null

signedInt, signedFloat :: Parser Value
signedInt =
    lexeme $
        NumVal . LispInt <$> L.signed (pure ()) L.decimal
            <* notFollowedBy symbolStart
signedFloat =
    lexeme $
        NumVal . LispReal <$> L.signed (pure ()) L.float
            <* notFollowedBy symbolStart

symbolPred :: [Char] -> Char -> Bool
symbolPred xs c = not (isSpace c) && c `notElem` xs

symbolStart, symbolMid :: Parser Char
symbolStart = satisfy $ symbolPred ['(', ')', '[', ']', '.', '\'', ',']
symbolMid = satisfy $ symbolPred ['(', ')', '[', ']']

stringLit :: Parser Value
stringLit =
    lexeme $
        StringVal . T.pack <$> (symbol "\"" *> manyTill L.charLiteral (symbol "\""))

symbol' :: Parser Value
symbol' = lexeme $ do
    s <- sym
    case s of
        "#t" -> pure $ BoolVal True
        "#f" -> pure $ BoolVal False
        _ -> pure $ Symbol $ T.pack s
  where
    sym = (:) <$> symbolStart <*> many symbolMid

sexpression :: Parser Value
sexpression =
    list
        <|> quote
        <|> prefixed "," "unquote"
        <|> try signedFloat
        <|> try signedInt
        <|> try stringLit
        <|> symbol'

file :: Parser [Value]
file = sc *> many sexpression <* eof

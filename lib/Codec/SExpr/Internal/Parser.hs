module Codec.SExpr.Internal.Parser where

import Control.Monad (void)

import Codec.SExpr.Internal.Types
import Text.ParserCombinators.Parsec

parseExpr :: AsSExpr a => String -> Either Error a
parseExpr input = case parse pExpr "" input of
    Left err  -> Left (ParsecError err)
    Right val -> decode val

parseManyExpr :: AsSExpr a => String -> Either Error a
parseManyExpr input = case parse pManyExpr "" input of
    Left err   -> Left (ParsecError err)
    Right vals -> decode (List vals)

comment :: Parser String
comment = char ';' >> many (noneOf "\n")

ignore :: Parser ()
ignore = void space <|> void comment

pManyExpr :: Parser [Expr]
pManyExpr = many ignore >> (pExpr `sepEndBy` many1 ignore)

pAtom :: Parser Expr
pAtom = Atom <$> many1 (letter <|> digit <|> oneOf "+=-_*&^%$@!<>?/:\\")

pList :: Parser Expr
pList = List <$> between (char '(') (char ')') pManyExpr

pExpr :: Parser Expr
pExpr = pList <|> pStr <|> pAtom

pStr :: Parser Expr
pStr = Str <$> (char '"' *> (concat <$> many encoded) <* char '"') where
    encoded = (char '\\' >> escape) <|> strSafe <|> doubleDouble
    strSafe = (:[]) <$> noneOf "\\\""
    -- two double quotes coalesce into one:
    doubleDouble = try (string "\"\"") >> return "\""
    escape = do
        c <- oneOf "abnrfvtxuU\\\"\n"
        case c of
            'a'  -> return "\a"
            'b'  -> return "\b"
            'n'  -> return "\n"
            'r'  -> return "\r"
            'f'  -> return "\f"
            'v'  -> return "\v"
            't'  -> return "\t"
            '\\' -> return "\\"
            '\"' -> return "\""
            'x'  -> hexEsc 2
            'u'  -> hexEsc 4
            'U'  -> hexEsc 8
            '\n' -> spaces >> return ""
            _    -> error $ "BUG: missing escape character: " ++ [c]
    hexEsc :: Int -> Parser String
    hexEsc n = do
        chrs <- count n hexDigit
        return $ (:[]) . toEnum . read $ "0x" ++ chrs


module Data.ProtoSExprs
  ( Expr(..)
  , pExpr
  ) where

import Data.List (intersperse)

import Text.ParserCombinators.Parsec


data Expr
    = List [Expr]
    | Atom String
    | Str String


instance Show Expr where
    show (List as) = "[" ++ concat (intersperse " " (map show as)) ++ "]"
    show (Atom a) = a
    show (Str s) = concat
        [ "\""
        , concat $ map makeSafe s
        , "\""
        ]
      where
        makeSafe '"' = "\\\""
        makeSafe '\\' = "\\\\"
        makeSafe c = [c]

pAtom :: Parser Expr
pAtom = Atom <$> many1 (letter <|> digit <|> oneOf "+=-_*&^%$@!<>?/:\\")

pList :: Parser Expr
pList = List <$> between (char '[') (char ']') (pExpr `sepBy` spaces)

pExpr :: Parser Expr
pExpr = pList <|> pStr <|> pAtom

pStr :: Parser Expr
pStr = Str <$> (char '"' *> (concat <$> many encoded) <* char '"') where
    encoded = (char '\\' >> escape) <|> strSafe
    strSafe = (:[]) <$> noneOf "\\\""
    escape = do
        c <- oneOf "abnrfvtxuU\\\""
        case c of
            'a' -> return "\a"
            'b' -> return "\b"
            'n' -> return "\n"
            'r' -> return "\r"
            'f' -> return "\f"
            'v' -> return "\v"
            't' -> return "\t"
            '\\' -> return "\\"
            '\"' -> return "\""
            'x' -> hexEsc 2
            'u' -> hexEsc 4
            'U' -> hexEsc 8
            _ -> error $ "BUG: missing escape character: " ++ [c]
    hexEsc :: Int -> Parser String
    hexEsc n = do
        chrs <- count n hexDigit
        return $ (:[]) . toEnum . read $ "0x" ++ chrs

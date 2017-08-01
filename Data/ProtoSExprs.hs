module Data.ProtoSExprs
  ( Expr(..)
  , pExpr
  , pFile
  ) where

import Data.List (intersperse)

import Text.ParserCombinators.Parsec


data Expr
    = List [Expr]
    | Atom String
    | Str String


instance Show Expr where
    show (List as) = "(" ++ concat (intersperse " " (map show as)) ++ ")"
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

pFile :: Parser [Expr]
pFile = spaces >> many pExpr

pAtom :: Parser Expr
pAtom = Atom <$> many1 (letter <|> digit <|> oneOf "+=-_*&^%$@!<>?/:\\")

pList :: Parser Expr
pList = List <$> bracketed (pExpr `sepBy` spaces)
  where
    bracketed :: Parser [Expr] -> Parser [Expr]
    bracketed cs = choice [ between (char '[') (char ']') cs
                          , between (char '(') (char ')') cs
                          , between (char '{') (char '}') cs
                          ]

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

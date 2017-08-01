{-# LANGUAGE
      DefaultSignatures
    , TypeOperators
    , FlexibleContexts
    , FlexibleInstances
    #-}
module Data.ProtoSExprs
  ( Expr(..)
  , pExpr
  , pFile
  , ToExpr(..)
  ) where

import Data.List (intersperse)

import Text.ParserCombinators.Parsec

import GHC.Generics


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

class ToExpr a where
    toExpr :: a -> Expr
    listToExpr :: [a] -> Expr
    default toExpr :: (Generic a, ToExpr' (Rep a)) => a -> Expr
    toExpr x = toExpr' (from x)
    listToExpr = List . map toExpr

class ToExpr' f where
    toExpr' :: f p -> Expr

instance ToExpr' V1 where
    toExpr' _ = undefined

instance ToExpr' U1 where
    toExpr' _ = List []

instance ToExpr c => ToExpr' (K1 i c) where
    toExpr' (K1 c) = toExpr c

instance (ToExpr' f, ToExpr' g) => ToExpr' (f :+: g) where
    toExpr' (L1 x) = toExpr' x
    toExpr' (R1 x) = toExpr' x

instance (Constructor t, ToExpr' f) => ToExpr' (C1 t f) where
    toExpr' con@(M1 x) = case toExpr' x of
        List exprs -> List $ Atom (conName con) : exprs
        expr -> List [Atom (conName con), expr]

instance (Datatype t, ToExpr' f) => ToExpr' (D1 t f) where
    toExpr' d@(M1 x) = toExpr' x

instance (ToExpr' f) => ToExpr' (S1 t f) where
    toExpr' (M1 x) = toExpr' x

instance (ToExpr' f, ToExpr' g) => ToExpr' (f :*: g) where
    toExpr' (l :*: r) =
        List $ (asList $ toExpr' l) ++ (asList $ toExpr' r)
      where
        asList (List list) = list
        asList expr = [expr]


instance ToExpr Char where
    toExpr c = Atom [c] -- TODO: figure out a better way to represent chars.
    listToExpr xs = Str xs

instance ToExpr a => ToExpr [a] where
    toExpr = listToExpr

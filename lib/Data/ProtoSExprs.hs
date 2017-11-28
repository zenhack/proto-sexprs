{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators     #-}
module Data.ProtoSExprs
  ( Expr(..)
  , pExpr
  , pManyExpr
  , ToExpr(..)
  ) where

import Control.Monad (void)
import Data.List     (intersperse)

import GHC.Generics
import Text.ParserCombinators.Parsec


data Expr
    = List [Expr]
    | Atom String
    | Str String
    deriving(Eq)


instance Show Expr where
    show (List as) = "(" ++ concat (intersperse " " (map show as)) ++ ")"
    show (Atom a) = a
    show (Str s) = concat
        [ "\""
        , concat $ map makeSafe s
        , "\""
        ]
      where
        makeSafe '"'  = "\\\""
        makeSafe '\\' = "\\\\"
        makeSafe c    = [c]

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
        expr       -> List [Atom (conName con), expr]

instance (Datatype t, ToExpr' f) => ToExpr' (D1 t f) where
    toExpr' d@(M1 x) = toExpr' x

instance (ToExpr' f) => ToExpr' (S1 t f) where
    toExpr' (M1 x) = toExpr' x

instance (ToExpr' f, ToExpr' g) => ToExpr' (f :*: g) where
    toExpr' (l :*: r) =
        List $ (asList $ toExpr' l) ++ (asList $ toExpr' r)
      where
        asList (List list) = list
        asList expr        = [expr]


instance ToExpr Char where
    toExpr c = Atom [c] -- TODO: figure out a better way to represent chars.
    listToExpr xs = Str xs

instance ToExpr a => ToExpr [a] where
    toExpr = listToExpr

{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators     #-}
module Data.ProtoSExprs
  ( Expr(..)
  , Error
  , expected
  , ToExpr(..)
  , FromExpr(..)
  , parseExpr
  , parseManyExpr
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

data Error
    = ParsecError ParseError
    | ConversionError String
    deriving(Show)

expected :: Expr -> String -> Error
expected got wanted =
    ConversionError $ "Expected " ++ wanted ++ " but got " ++ show got

class ToExpr a where
    toExpr :: a -> Expr
    listToExpr :: [a] -> Expr
    default toExpr :: (Generic a, ToExpr' (Rep a)) => a -> Expr
    toExpr x = toExpr' (from x)
    listToExpr = List . map toExpr

class FromExpr a where
    fromExpr :: Expr -> Either Error a

instance ToExpr Expr where
    toExpr = id

instance FromExpr Expr where
    fromExpr = Right

parseExpr :: FromExpr a => String -> Either Error a
parseExpr input = case parse pExpr "" input of
    Left err  -> Left (ParsecError err)
    Right val -> fromExpr val

parseManyExpr :: FromExpr a => String -> Either Error a
parseManyExpr input = case parse pManyExpr "" input of
    Left err   -> Left (ParsecError err)
    Right vals -> fromExpr (List vals)

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

instance FromExpr String where
    fromExpr (Str s) = Right s
    fromExpr expr    = Left $ expected expr "string"

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


-------- Boilerplate instances for tuples (up to 6) --------------------------

instance ToExpr () where
    toExpr () = List []

instance FromExpr () where
    fromExpr (List []) = Right ()
    fromExpr expr      = Left $ expected expr "()"

instance (ToExpr a, ToExpr b) => ToExpr (a, b) where
    toExpr (a, b) = List [toExpr a, toExpr b]

instance (FromExpr a, FromExpr b) => FromExpr (a, b) where
    fromExpr (List [a, b]) = (,) <$> fromExpr a <*> fromExpr b
    fromExpr expr          = Left $ expected expr "2-tuple"

instance (ToExpr a, ToExpr b, ToExpr c) => ToExpr (a, b, c) where
    toExpr (a, b, c) = List [toExpr a, toExpr b, toExpr c]

instance (FromExpr a, FromExpr b, FromExpr c) => FromExpr (a, b, c) where
    fromExpr (List [a, b, c]) = (,,) <$> fromExpr a <*> fromExpr b <*> fromExpr c
    fromExpr expr = Left $ expected expr "3-tuple"

instance (ToExpr a, ToExpr b, ToExpr c, ToExpr d) => ToExpr (a, b, c, d) where
    toExpr (a, b, c, d) = List [toExpr a, toExpr b, toExpr c, toExpr d]

instance
    ( FromExpr a
    , FromExpr b
    , FromExpr c
    , FromExpr d
    ) => FromExpr (a, b, c, d)
  where
    fromExpr (List [a, b, c, d]) = (,,,)
        <$> fromExpr a
        <*> fromExpr b
        <*> fromExpr c
        <*> fromExpr d
    fromExpr expr = Left $ expected expr "4-tuple"

instance
    ( ToExpr a
    , ToExpr b
    , ToExpr c
    , ToExpr d
    , ToExpr e
    ) => ToExpr (a, b, c, d, e) where
    toExpr (a, b, c, d, e) =
        List [toExpr a, toExpr b, toExpr c, toExpr d, toExpr e]

instance
    ( FromExpr a
    , FromExpr b
    , FromExpr c
    , FromExpr d
    , FromExpr e
    ) => FromExpr (a, b, c, d, e)
  where
    fromExpr (List [a, b, c, d, e]) = (,,,,)
        <$> fromExpr a
        <*> fromExpr b
        <*> fromExpr c
        <*> fromExpr d
        <*> fromExpr e
    fromExpr expr = Left $ expected expr "5-tuple"

instance
    ( ToExpr a
    , ToExpr b
    , ToExpr c
    , ToExpr d
    , ToExpr e
    , ToExpr f
    ) => ToExpr (a, b, c, d, e, f) where
    toExpr (a, b, c, d, e, f) =
        List [toExpr a, toExpr b, toExpr c, toExpr d, toExpr e, toExpr f]

instance
    ( FromExpr a
    , FromExpr b
    , FromExpr c
    , FromExpr d
    , FromExpr e
    , FromExpr f
    ) => FromExpr (a, b, c, d, e, f)
  where
    fromExpr (List [a, b, c, d, e, f]) = (,,,,,)
        <$> fromExpr a
        <*> fromExpr b
        <*> fromExpr c
        <*> fromExpr d
        <*> fromExpr e
        <*> fromExpr f
    fromExpr expr = Left $ expected expr "6-tuple"

{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE ViewPatterns      #-}
module Codec.SExpr
  ( Expr(..)
  , Error
  , expected
  , AsSExpr(..)
  , AsSExpr(..)
  , parseExpr
  , parseManyExpr
  ) where

import Control.Monad (void)
import Data.List     (intersperse)
import Text.Read     (readMaybe)

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

class AsSExpr a where
    encode :: a -> Expr
    decode :: Expr -> Either Error a

    default encode :: (Generic a, AsSExpr' (Rep a)) => a -> Expr
    default decode :: (Generic a, AsSExpr' (Rep a)) => Expr -> Either Error a

    encode x = encode' (from x)
    decode x = to <$> (decode' x)

instance AsSExpr Expr where
    encode = id
    decode = Right

parseExpr :: AsSExpr a => String -> Either Error a
parseExpr input = case parse pExpr "" input of
    Left err  -> Left (ParsecError err)
    Right val -> decode val

parseManyExpr :: AsSExpr a => String -> Either Error a
parseManyExpr input = case parse pManyExpr "" input of
    Left err   -> Left (ParsecError err)
    Right vals -> decode (List vals)

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

instance {-# OVERLAPS #-} AsSExpr String where
    encode = Str
    decode (Str s) = Right s
    decode expr    = Left $ expected expr "string"

class AsSExpr' f where
    encode' :: f p -> Expr
    decode' :: Expr -> Either Error (f p)


instance (AsSExpr' f, AsSExpr' g) => AsSExpr' (f :+: g) where
    encode' (L1 x) = encode' x
    encode' (R1 x) = encode' x
    decode' ex = case (L1 <$> decode' ex, R1 <$> decode' ex) of
        (res@(Right _), _) -> res
        (_, res@(Right _)) -> res
        (Left l, Left r) ->
            -- XXX TODO: this is going to generate ugly errors like
            -- @got ... but expected got ... but expected ... or got ... but
            -- expected ...@. We should refactor our errors a bit so we can
            -- do something more reasonable.
            Left $ expected ex (show l ++ " or " ++ show r)

instance AsSExpr' V1 where
    encode' _ = undefined

instance AsSExpr' U1 where
    encode' _ = List []

instance AsSExpr c => AsSExpr' (K1 i c) where
    encode' (K1 c) = encode c

instance (Constructor t, AsSExpr' f) => AsSExpr' (C1 t f) where
    encode' con@(M1 x) = case encode' x of
        List exprs -> List $ Atom (conName con) : exprs
        expr       -> List [Atom (conName con), expr]

instance (Datatype t, AsSExpr' f) => AsSExpr' (D1 t f) where
    encode' (M1 x) = encode' x

instance (AsSExpr' f) => AsSExpr' (S1 t f) where
    encode' (M1 x) = encode' x

instance (AsSExpr' f, AsSExpr' g) => AsSExpr' (f :*: g) where
    encode' (l :*: r) =
        List $ (asList $ encode' l) ++ (asList $ encode' r)
      where
        asList (List list) = list
        asList expr        = [expr]

instance AsSExpr Char where
    encode c = Atom [c] -- TODO: figure out a better way to represent chars.
    decode (Atom [c]) = pure c
    decode ex         = Left $ expected ex "character"

instance AsSExpr a => AsSExpr [a] where
    encode = List . map encode
    decode (List xs) = mapM decode xs
    decode ex        = Left $ expected ex "list"

-------- Boilerplate instances for tuples (up to 6) --------------------------

instance AsSExpr () where
    encode () = List []
    decode (List []) = Right ()
    decode expr      = Left $ expected expr "()"

instance (AsSExpr a, AsSExpr b) => AsSExpr (a, b) where
    encode (a, b) = List [encode a, encode b]
    decode (List [a, b]) = (,) <$> decode a <*> decode b
    decode expr          = Left $ expected expr "2-tuple"

instance (AsSExpr a, AsSExpr b, AsSExpr c) => AsSExpr (a, b, c) where
    encode (a, b, c) = List [encode a, encode b, encode c]
    decode (List [a, b, c]) = (,,) <$> decode a <*> decode b <*> decode c
    decode expr             = Left $ expected expr "3-tuple"

instance (AsSExpr a, AsSExpr b, AsSExpr c, AsSExpr d) => AsSExpr (a, b, c, d) where
    encode (a, b, c, d) = List [encode a, encode b, encode c, encode d]
    decode (List [a, b, c, d]) = (,,,)
        <$> decode a
        <*> decode b
        <*> decode c
        <*> decode d
    decode expr = Left $ expected expr "4-tuple"

instance
    ( AsSExpr a
    , AsSExpr b
    , AsSExpr c
    , AsSExpr d
    , AsSExpr e
    ) => AsSExpr (a, b, c, d, e)
  where
    encode (a, b, c, d, e) =
        List [encode a, encode b, encode c, encode d, encode e]
    decode (List [a, b, c, d, e]) = (,,,,)
        <$> decode a
        <*> decode b
        <*> decode c
        <*> decode d
        <*> decode e
    decode expr = Left $ expected expr "5-tuple"

instance
    ( AsSExpr a
    , AsSExpr b
    , AsSExpr c
    , AsSExpr d
    , AsSExpr e
    , AsSExpr f
    ) => AsSExpr (a, b, c, d, e, f)
  where
    encode (a, b, c, d, e, f) =
        List [encode a, encode b, encode c, encode d, encode e, encode f]
    decode (List [a, b, c, d, e, f]) = (,,,,,)
        <$> decode a
        <*> decode b
        <*> decode c
        <*> decode d
        <*> decode e
        <*> decode f
    decode expr = Left $ expected expr "6-tuple"


--- basic types

instance AsSExpr Int where
    encode = Atom . show
    decode (Atom (readMaybe -> Just n)) = pure n
    decode ex                           = Left $ expected ex "Int"

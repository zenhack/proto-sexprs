{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts  #-}
module Codec.SExpr.Internal.Types where

import Data.List    (intersperse)
import GHC.Generics (Generic(..))

import Text.ParserCombinators.Parsec (ParseError)

data Expr
    = List [Expr]
    | Atom String
    | Str String
    deriving(Eq)

data Error
    = ParsecError ParseError
    | ConversionError String
    deriving(Show)

class AsSExpr a where
    encode :: a -> Expr
    decode :: Expr -> Either Error a

    default encode :: (Generic a, AsSExpr' (Rep a)) => a -> Expr
    default decode :: (Generic a, AsSExpr' (Rep a)) => Expr -> Either Error a

    encode = genericEncode
    decode = genericDecode

-- | encode a type which implements Generic
genericEncode :: (Generic a, AsSExpr' (Rep a)) => a -> Expr
genericEncode x = encode' (from x)

-- | decode a type which implements Generic
genericDecode :: (Generic a, AsSExpr' (Rep a)) => Expr -> Either Error a
genericDecode x = to <$> (decode' x)

class AsSExpr' f where
    encode' :: f p -> Expr
    decode' :: Expr -> Either Error (f p)

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

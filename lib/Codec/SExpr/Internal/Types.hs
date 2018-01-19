{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts  #-}
module Codec.SExpr.Internal.Types where

import Data.List    (intersperse)
import GHC.Generics (Generic(..))

import Text.ParserCombinators.Parsec (ParseError)

-- | an S-expression
data Expr
    = List [Expr]
    | Atom String
    | Str String
    deriving(Eq)

-- | an error when encoding/decoding a value
data Error
    = ParsecError ParseError
    | ConversionError String
    deriving(Show)

-- | A type which is an instance of @'AsExpr'@ can be converted to and
-- from S-expressions.
class AsSExpr a where
    -- | Convert a value to an S-expression.
    encode :: a -> Expr

    -- | Convert an S-expression to a value.
    decode :: Expr -> Either Error a

    default encode :: (Generic a, AsSExpr' (Rep a)) => a -> Expr
    default decode :: (Generic a, AsSExpr' (Rep a)) => Expr -> Either Error a

    encode = genericEncode
    decode = genericDecode

-- | Encode a type which implements Generic.
genericEncode :: (Generic a, AsSExpr' (Rep a)) => a -> Expr
genericEncode x = encode' (from x)

-- | Decode a type which implements Generic.
genericDecode :: (Generic a, AsSExpr' (Rep a)) => Expr -> Either Error a
genericDecode x = to <$> (decode' x)

-- | Auxiliary class used to implement AsSExpr in terms of ghc generics.
class AsSExpr' f where
    encode' :: f p -> Expr
    decode' :: Expr -> Either Error (f p)

-- XXX: This doesn't follow the guideline of emitting runnable haskell code;
-- maybe we should do something different? We could output something like
-- read "(...)", and get many of the benefits of this implementation without the
-- downside.
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

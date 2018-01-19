{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE ViewPatterns      #-}
-- GHC warns orphan about orphan instances if they cross *module* boundaries.
-- package (or cabal project) boundaries would make more sense. Since our
-- classes are defined in a different module from our instances, we disable
-- this warning for this module:
{-# OPTIONS_GHC -Wno-orphans #-}
module Codec.SExpr.Internal.Instances where

import GHC.Generics

import Codec.SExpr.Internal.Types

import Text.Read (readMaybe)


-- TODO: figure out where to put this:
expected :: Expr -> String -> Error
expected got wanted =
    ConversionError $ "Expected " ++ wanted ++ " but got " ++ show got

-- Expr's instance is just the identity:
instance AsSExpr Expr where
    encode = id
    decode = Right

---------------------------- Types from the prelude ------------------------

instance AsSExpr Int where
    encode = Atom . show
    decode (Atom (readMaybe -> Just n)) = pure n
    decode ex                           = Left $ expected ex "Int"

instance AsSExpr Char where
    encode c = Atom [c] -- TODO: figure out a better way to represent chars.
    decode (Atom [c]) = pure c
    decode ex         = Left $ expected ex "character"

-- We encode [Char] as Str, whereas haskell lists of other types encode as List.
instance {-# OVERLAPS #-} AsSExpr String where
    encode = Str
    decode (Str s) = Right s
    decode expr    = Left $ expected expr "string"

instance AsSExpr a => AsSExpr [a] where
    encode = List . map encode
    decode (List xs) = mapM decode xs
    decode ex        = Left $ expected ex "list"

--------------------------------- Generics ---------------------------------

-- TODO FIXME: most of these still don't define decode'

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
    decode' (List []) = pure U1
    decode' ex        = Left $ expected ex "()"

instance AsSExpr c => AsSExpr' (K1 i c) where
    encode' (K1 c) = encode c
    decode' c = K1 <$> decode c

instance (Constructor t, AsSExpr' f) => AsSExpr' (C1 t f) where
    encode' con@(M1 x) = case encode' x of
        List exprs -> List $ Atom (conName con) : exprs
        expr       -> List [Atom (conName con), expr]

instance (Datatype t, AsSExpr' f) => AsSExpr' (D1 t f) where
    encode' (M1 x) = encode' x
    decode' x = M1 <$> decode' x

instance (AsSExpr' f) => AsSExpr' (S1 t f) where
    encode' (M1 x) = encode' x

instance (AsSExpr' f, AsSExpr' g) => AsSExpr' (f :*: g) where
    encode' (l :*: r) =
        List $ (asList $ encode' l) ++ (asList $ encode' r)
      where
        asList (List list) = list
        asList expr        = [expr]

-------------------- Tuples (up to 6); lots of boilerplate -----------------

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

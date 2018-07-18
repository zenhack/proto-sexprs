{-# LANGUAGE DeriveGeneric #-}
module Main where

-- TODO: do something cleaner than just error for test failures (using
-- the test-framework pacakge)

import Codec.SExpr
import GHC.Generics (Generic)

data TestType
    = Foo
    | Bar Int
    deriving(Eq, Show, Generic)

instance AsSExpr TestType

exprCases :: [IO ()]
exprCases =
    [ decTest "()" (Just $ List [])
    , decTest "foo" (Just $ Atom "foo")
    , decTest ")" exNothing
    , decTest "(; a comment in a list\n)" (Just $ List [])
    , decTest "(; a comment causing an error)" exNothing
    , decTest "(two atoms)" (Just $ List [Atom "two", Atom "atoms"])
    , decTest "(  list-with-leading-and-trailing-space )"
        (Just $ List [Atom "list-with-leading-and-trailing-space"])

    -- check that \ followed by newline skips leading whitespace on the next
    -- line:
    , decTest
        (unlines
            [ "\"hello, \\"
            , "      world\""
            ])
        (Just $ Str "hello, world")

    -- Check that successive double-quotes render as a single double-quote:
    , decTest "\"hello, \"\"world\"\"\"" (Just $ Str "hello, \"world\"")

    , decTest "(Foo)" (Just Foo)
    , decTest "(Bar 4)" (Just (Bar 4))
    , decTest "(Quux 4)" (Nothing :: Maybe TestType)
    ]
  where
    -- | Type-restricted 'Nothing', to give type inference a hint.
    exNothing :: Maybe Expr
    exNothing = Nothing

-- | Decoding test. Takes an input and an expected output. If the expected
-- output is 'Nothing', decoding should fail. if it is @'Just' wanted@,
-- decoding should succeed yielding the value @wanted@.
--
-- The 'Show' instance is needed for displaying error messasges.
decTest :: (Show a, Eq a, AsSExpr a) => String -> Maybe a -> IO ()
decTest input expected_ = case (parseExpr input, expected_) of
    (Left _, Nothing) -> return ()
    (Left err, Just want) -> error $ concat
        ["Expected ", show want, " but got error:", show err]
    (Right actual, Nothing) ->
        error $ "Expected parse failure, but parsed " ++ show actual
    (Right actual, Just want)
        | actual == want -> return ()
        | otherwise -> error $ concat
            ["Expected ", show want, " but parsed ", show actual]

main :: IO ()
main = sequence_ exprCases

module Main where

-- TODO: do something cleaner than just error for test failures (using
-- the test-framework pacakge)

import Data.ProtoSExprs

exprCases :: [(String, Maybe Expr)]
exprCases =
    [ ("()", Just $ List [])
    , ("foo", Just $ Atom "foo")
    , (")", Nothing)
    , ("(; a comment in a list\n)", Just $ List [])
    , ("(; a comment causing an error)", Nothing)
    , ("(two atoms)", Just $ List [Atom "two", Atom "atoms"])
    , ( "(  list-with-leading-and-trailing-space )"
      , Just $ List [Atom "list-with-leading-and-trailing-space"]
      )

    -- check that \ followed by newline skips leading whitespace on the next
    -- line:
    , ( unlines
        [ "\"hello, \\"
        , "      world\""
        ]
      , Just $ Str "hello, world"
      )

    -- Check that successive double-quotes render as a single double-quote:
    , ("\"hello, \"\"world\"\"\"", Just $ Str "hello, \"world\"")
    ]

exprTest :: String -> Maybe Expr -> IO ()
exprTest input expected_ = case (parseExpr input, expected_) of
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
main = mapM_ (uncurry exprTest) exprCases

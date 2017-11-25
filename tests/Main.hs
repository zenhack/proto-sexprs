module Main where

-- TODO: do something cleaner than just error for test failures (using
-- the test-framework pacakge)

import Data.ProtoSExprs
import Text.ParserCombinators.Parsec (runParser)

exprCases :: [(String, Maybe Expr)]
exprCases =
    [ ("()", Just $ List [])
    , ("foo", Just $ Atom "foo")
    , (")", Nothing)
    ]

exprTest :: String -> Maybe Expr -> IO ()
exprTest input expected_ = case (runParser pExpr () "test" input, expected_) of
    (Left _, Nothing) -> return ()
    (Left err, Just expected) -> error $ concat
        ["Expected ", show expected, " but got parse error:", show err]
    (Right actual, Nothing) ->
        error $ "Expected parse failure, but parsed " ++ show actual
    (Right actual, Just expected)
        | actual == expected -> return ()
        | otherwise -> error $ concat
            ["Expected ", show expected, " but parsed ", show actual]

main :: IO ()
main = mapM_ (uncurry exprTest) exprCases

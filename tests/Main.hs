{-# LANGUAGE BangPatterns  #-}
{-# LANGUAGE DeriveGeneric #-}
module Main where

-- TODO: do something cleaner than just error for test failures (using
-- the test-framework pacakge)

import Codec.SExpr

import Control.Monad (replicateM)
import Data.Proxy
import GHC.Generics  (Generic)

import Test.QuickCheck.Arbitrary
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

data TestType
    = Foo
    | Bar Int
    deriving(Eq, Show, Generic)

instance AsSExpr TestType

instance Arbitrary Expr where
    arbitrary = sized go where
        go !n = oneof
            [ Atom <$> arbitrary
            , Str <$> arbitrary
            , do
                -- Because this is a recursive structure, we
                -- need to be careful not to make huge values.
                len <- (`mod` n) <$> arbitrary
                List <$> replicateM len (go (n `div` len))
            ]

instance Arbitrary TestType where
    arbitrary = oneof
        [ pure Foo
        , Bar <$> arbitrary
        ]

unitTests :: TestTree
unitTests = testGroup "Unit tests" $
    [ testGroup "Succeed decoding SExpr" $ map decOk
        [ ("()", List [])
        , ("foo", Atom "foo")
        , ("(; a comment in a list\n)", List [])
        , ("(two atoms)", List [Atom "two", Atom "atoms"])
        , ("(  list-with-leading-and-trailing-space )"
          , List [Atom "list-with-leading-and-trailing-space"]
          )

        -- check that \ followed by newline skips leading whitespace on the next
        -- line:
        , ( unlines
                [ "\"hello, \\"
                , "      world\""
                ]
          , Str "hello, world"
          )

        -- Check that successive double-quotes render as a single double-quote:
        , ("\"hello, \"\"world\"\"\"", Str "hello, \"world\"")
        ]
    , testGroup "Fail decoding SExpr" $ map (decFail (Proxy :: Proxy (Maybe Expr)))
        [ ")"
        , "(; a comment causing an error)"
        ]
    , testGroup "Succeed decoding generic" $ map decOk
        [ ("(Foo)", Foo)
        , ("(Bar 4)", Bar 4)
        ]
    , testGroup "Fail decoding generic" $
        map (decFail (Proxy :: Proxy (Maybe TestType)))
        [ "(Quux 4)"
        ]
    ]

fuzzTests :: TestTree
fuzzTests = testGroup "Fuzz Tests" $
    [ testGroup "runDecode . decode . encode == Right"
        [ testProperty "Expr" (prop_encDec :: Expr -> Bool)
        , testProperty "TestType" (prop_encDec :: TestType -> Bool)
        ]
    ]

decOk :: (Show a, Eq a, AsSExpr a) => (String, a) -> TestTree
decOk (input, want) = testCase
    (show input ++ " should decode to " ++ show want) $
    decTest input (Just want)

decFail :: (Show a, Eq a, AsSExpr a) => Proxy (Maybe a) -> String -> TestTree
decFail p input = testCase
    (show input ++ " should fail to decode") $
    decTest input (Nothing `asProxyTypeOf` p)

-- | Decoding test. Takes an input and an expected output. If the expected
-- output is 'Nothing', decoding should fail. if it is @'Just' wanted@,
-- decoding should succeed yielding the value @wanted@.
--
-- The 'Show' instance is needed for displaying error messasges.
decTest :: (Show a, Eq a, AsSExpr a) => String -> Maybe a -> Assertion
decTest input expected_ = case (parseExpr input, expected_) of
    (Left _, Nothing) -> return ()
    (Left err, Just want) -> assertFailure $ concat
        ["Expected ", show want, " but got error:", show err]
    (Right actual, Nothing) ->
        assertFailure $ "Expected parse failure, but parsed " ++ show actual
    (Right actual, Just want)
        | actual == want -> return ()
        | otherwise -> assertFailure $ concat
            ["Expected ", show want, " but parsed ", show actual]

prop_encDec :: (Eq a, AsSExpr a) => a -> Bool
prop_encDec value = runDecode (decode (encode value)) == Right value

main :: IO ()
main = defaultMain $ testGroup "Tests" [unitTests, fuzzTests]

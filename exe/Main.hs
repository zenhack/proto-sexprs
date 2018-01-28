{-# LANGUAGE DeriveGeneric #-}
import Codec.SExpr
import GHC.Generics (Generic(..))

data LangStmt
    = Loop [LangStmt]
    | Assign String LangExpr
    deriving(Show, Generic)


data LangExpr
    = LangString String
    | LangVar String
    | LangBinOp String LangExpr LangExpr
    deriving(Show, Generic)

instance AsSExpr LangStmt where
    decode (List (Atom "loop":stmts))       = Loop <$> mapM decode stmts
    decode (List [Atom var, Atom ":=", ex]) = Assign var <$> decode ex
    decode ex                               = expected ex "Statement"

instance AsSExpr LangExpr where
    decode (Str s) = pure (LangString s)
    decode (List [Atom op, lhs, rhs]) =
        LangBinOp op <$> decode lhs <*> decode rhs
    decode (Atom v) = pure (LangVar v)
    decode ex = expected ex "Expression"

main :: IO ()
main = do
    contents <- getContents
    let exprs :: Either Error [LangStmt]
        exprs = parseManyExpr contents
    print exprs
    print $ encode $ Loop [ Assign "x" (LangString "hello")
                          , Assign "y" (LangBinOp "+" (LangString "f")
                                                      (LangString "x"))
                          ]

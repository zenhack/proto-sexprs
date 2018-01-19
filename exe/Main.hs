{-# LANGUAGE DeriveGeneric #-}
import Data.ProtoSExprs
import GHC.Generics     (Generic(..))

data LangStmt
    = Loop [LangStmt]
    | Assign String LangExpr
    deriving(Show, Generic)
instance ToExpr LangStmt


data LangExpr
    = LangString String
    | LangBinOp String LangExpr LangExpr
    deriving(Show, Generic)
instance ToExpr LangExpr


instance FromExpr LangExpr where
    fromExpr (Str s) = pure (LangString s)
    fromExpr (List [Atom op, lhs, rhs]) =
        LangBinOp op <$> fromExpr lhs <*> fromExpr rhs
    fromExpr ex = Left $ expected ex "Expression"

instance FromExpr LangStmt where
    fromExpr (List (Atom "loop":stmts))       = Loop <$> mapM fromExpr stmts
    fromExpr (List [Atom var, Atom ":=", ex]) = Assign var <$> fromExpr ex
    fromExpr ex                               = Left $ expected ex "Statement"

main :: IO ()
main = do
    contents <- getContents
    let exprs :: Either Error LangStmt
        exprs = parseManyExpr contents
    print exprs
    print $ toExpr $ Loop [ Assign "x" (LangString "hello")
                          , Assign "y" (LangBinOp "+" (LangString "f")
                                                      (LangString "x"))
                          ]

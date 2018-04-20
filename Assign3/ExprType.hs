module ExprType where
import Data.List 
{- Expression Datatype
  ------------------------------
  - Wraps different Operations in a
  - Expression Tree 
  - Ops:
      Add - standard binary addtion
      Mult - standard binary multiplication
      Cons - wrapper for simple values
      var - string identifier for variables

-}
data Expr a = Add (Expr a) (Expr a)
            | Mult (Expr a) (Expr a)
            | Const a
            | Var String
  deriving Eq
{- getvars :
        retrive variable identifiers 
-}
getVars :: Expr a -> [String]
getVars (Add e1 e2)  = getVars e1 `union` getVars e2
getVars (Mult e1 e2) = getVars e1 `union` getVars e2
getVars (Const _)    = []
getVars (Var ident)  = [ident]


{-|
Module : ExprType
Description : Contains all the definitions of every datatypes
Copyright : (c) Tianyi Zhang @2018
Maintainer : zhangt73@mcmaster.ca
Stability : experimental
-}
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
            | Cos (Expr a)
            | Sin (Expr a)
            | Exponent (Expr a) (Expr a)   ---- a^b 
            | Exp (Expr a)  --- e^n 
            | Ln (Expr a)
            | Neg (Expr a)  
  deriving Eq
{- getvars :
        retrive variable identifiers 
-}
getVars :: Expr a -> [String]
getVars (Add e1 e2)  = getVars e1 `union` getVars e2
getVars (Mult e1 e2) = getVars e1 `union` getVars e2
getVars (Const _)    = []
getVars (Var ident)  = [ident]
getVars (Cos e1)     = getVars e1
getVars (Sin e1)     = getVars e1
getVars (Exp e1)    = getVars e1
getVars (Exponent e1 e2)  = (getVars e1) `union` (getVars e2) 
getVars (Ln e1)      = getVars e1


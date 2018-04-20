{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE UndecidableInstances #-}
module ExprDiff where

import ExprType

import qualified Data.Map.Strict as Map

{- Class DiffExpr:
        Differentiable Expression 
---------------------------------------
  - This class has methods over the Expr datatype 
  - that assist with construction and evaluation
  - of differentiable expressions
----------------------------------------
 - Methods:
   - eval : takes a dictionary of variable identifiers
             and values, and uses it to compute the Expr
             fully
   - simplify : takes a possibly incomplete dictionary and 
                uses it to reduce Expr as much as possible
               e1 = x + y 
               e2 = y + x
               simplify e1 == simplify e2

               Add (Add (Var "x") (Const 1)) (Add (Const 2) (Var "y"))
               ==> Add(Const 3) (Add (Var "x") (Var "y")) 
               ***
   - partDiff  : given an var identifier, differentiate IN TERMS of that
                 identifier
   - Default Methods:
               !+, !*, var, val : are function wrappers for Expr constructors
                                  that perform additional simplification 


-} 

class DiffExpr a where
  eval :: Map.Map String a -> Expr a -> a
  simplify :: Map.Map String a -> Expr a -> Expr a
  partDiff :: String -> Expr a -> Expr a
  
  {-Default Methods-} 
  (!+) :: Expr a -> Expr a -> Expr a
  e1 !+ e2 = simplify (Map.fromList []) $ Add e1 e2
  (!*) :: Expr a -> Expr a -> Expr a
  e1 !* e2 = simplify (Map.fromList []) $ Mult e1 e2
  val :: a -> Expr a
  val x = Const x
  var :: String -> Expr a
  var x = Var x 
  neg :: Expr a -> Expr a
  neg x = Const (eval (Map.fromList []) (Neg x))

  (!^) :: Expr a -> Expr a -> Expr a
  (!^) e1 e2 = simplify (Map.fromList []) $ Exponent e1 e2
  exCos :: Expr a -> Expr a
  exCos e1 = Cos e1 
  exSin :: Expr a -> Expr a
  exSin e1 = Sin e1
  exExp :: Expr a -> Expr a
  exExp e1 = Exp e1  
  exLn :: Expr a -> Expr a
  exLn e1 = Ln e1 

{- Most intuative instance of DiffExpr 
   - Num instance only relies on +,- 
   - Methods
        eval: .....
        simplify : ....
        partDiff : ....
-}
instance (Floating a, Eq a) => DiffExpr a where
  {-Evaluation by pattern matching-}
  eval vrs (Neg e) = (-1) * (eval vrs e)
  eval vrs (Add e1 e2)  = eval vrs e1 + eval vrs e2
  eval vrs (Mult e1 e2) = eval vrs e1 * eval vrs e2
  eval vrs (Const x) = x
  eval vrs (Var x) = case Map.lookup x vrs of
                       Just v  -> v
                       Nothing -> error "failed lookup in eval"

  eval vrs (Exp e) = exp (eval vrs e)
  eval vrs (Exponent b x) = (eval vrs b) ** (eval vrs x)  
  eval vrs (Cos e) = cos (eval vrs e)
  eval vrs (Sin e) = sin (eval vrs e)
  eval vrs (Ln e) = log (eval vrs e)

  {-Perform symbolic differentation for the defined expression datatype -}
  partDiff i (Const _) = Const 0  
  partDiff i (Var x) = if x == i then (Const 1) else (Const 0) 
  -- Perform the following differentation according to the relevant differentation laws
  partDiff i (Add e1 e2) = Add (partDiff i e1) (partDiff i e2)
  partDiff i (Mult e1 e2) = Add (Mult (partDiff i e1) e2) (Mult e1 (partDiff i e2))  
  partDiff i (Ln e) = Mult (Exponent e (Const (-1))) (partDiff i e) 
  -- Two types of Exponent: eg. 2^x => 2^x * ln(2) , x^ 4 => 4*x^3
  partDiff i (Exponent (Const a) (Var b)) = Mult ((Exponent (Const a) (Var b))) (Ln (Const a)) 
  partDiff i (Exponent b x) = Mult (Mult x (Exponent b (Add x (Const (-1))))) (partDiff i b) 
  partDiff i (Sin x) = Mult (Cos x) (partDiff i x) 
  partDiff i (Cos e1) = Mult (Mult (Const (-1)) (Sin e1)) $ partDiff i e1
  partDiff i (Exp e) = Mult (Exp e) (partDiff i e) 
  
  {- simplifies Expr types to as close to normal forma possible -}
  {-
    Const: 
    Var : simplify by replacing the real value from the map

    Add : 
        Add 0 -> itself
        Add two constant -> one sum constant
        Add two var -> one var -> simplify var again
        Add two expression -> simplify the sum        
  -}
  simplify vrs (Const a) = Const a  
  simplify vrs (Var x)   = case Map.lookup x vrs of
                                Just v -> Const v
                                Nothing -> Var x
  -- some special values
  simplify vrs (Ln (Const 1)) = Const 0
  simplify vrs (Exp (Const 0)) = Const 1
  simplify vrs (Exponent _ (Const 0)) = Const 1 

  -- law of addition
  simplify vrs (Add e1 e2) = let 
                               s1 = simplify vrs e1
                               s2 = simplify vrs e2
                              in case (s1,s2) of 
                                (Const m, Const n) -> Const (m+n)
                                (Const 0, ss2) -> ss2
                                (ss1, Const 0 ) -> ss1
                                (ss1,ss2) -> Add ss1 ss2
  -- law of multiplication
  simplify vrs (Mult e1 e2) = let
                                s1 = simplify vrs e1
                                s2 = simplify vrs e2 
                               in case (s1,s2) of
                                  (Const a,Const b) -> Const (a*b)
                                  (Const 0,_)       -> Const 0
                                  (_,Const 0)       -> Const 0
                                  (Const 1,ss2)     -> ss2
                                  (ss1,Const 1)     -> ss1
                                  (ss1,ss2)         -> Mult ss1 ss2 
  {-similiar idea for sin, cos, ln : 
     1) If a constant value wrapped, take the direct value and turn the whole
        into a const.
     2) If variable value wrapped, try to map look up the value if possible
     3) otherwise, defualt simplification is performed 
   -}

  simplify vrs (Sin (Const a)) = Const $ eval vrs $ Sin $ Const a
  simplify vrs (Sin (Var x)) = case (Map.lookup x vrs) of
                                 Just t -> Const (eval vrs (Sin (Var x)))
                                 Nothing -> Sin (simplify vrs (Var x))
  simplify vrs (Sin e) = Sin (simplify vrs e)

  simplify vrs (Cos (Const a)) = Const $ eval vrs $ Cos $ Const a
  simplify vrs (Cos (Var x)) = case (Map.lookup x vrs) of
                                 Just t -> Const (eval vrs (Cos (Var x)))
                                 Nothing -> Cos (simplify vrs (Var x))
  simplify vrs (Cos e) = Cos (simplify vrs e)

  simplify vrs (Ln (Const a)) = Const $ eval vrs $ Ln $ Const a
  simplify vrs (Ln (Var x)) = case (Map.lookup x vrs) of
                                 Just t -> Const (eval vrs (Ln (Var x)))
                                 Nothing -> Ln (simplify vrs (Var x))
  simplify vrs (Ln e) = Ln (simplify vrs e) 

  simplify vrs (Exponent (Const a) (Const b)) = Const (eval vrs (Exponent (Const a) (Const b)))
  simplify vrs (Exponent (Var x) (Var y)) = case (Map.lookup x vrs) of
                                          Just v -> case (Map.lookup y vrs) of
                                                      Just t -> Const (eval vrs (Exponent (Var x) (Var y)))
                                                      Nothing -> Exponent (simplify vrs (Var x)) (simplify vrs (Var y))
                                          Nothing -> Exponent (simplify vrs (Var x)) (simplify vrs (Var y)) 
  simplify vrs (Exponent e1 e2) = Exponent (simplify vrs e1) (simplify vrs e2) 







{- Ref: https://github.com/barskyn/CS1XA3/blob/master/Assign3/assign3/ExprDiff.hs
        https://github.com/chenc118/CS1XA3/blob/master/Assign3/ExprDiff.hs    -}
 





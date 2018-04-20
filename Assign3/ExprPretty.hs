module ExprPretty where 

import ExprType

parens :: String -> String
parens ss = "(" ++ ss ++ ")"

instance Show a => Show (Expr a) where
  show (Mult e1 e2) = parens (show e1) ++ " !* " ++ parens (show e2)
  show (Add e1 e2)  = parens (show e1) ++ " !+ " ++ parens (show e2)
  show (Var ss)     = parens $ "var \"" ++ ss ++ "\""
  show (Const x)    = parens $ "val " ++ show x
  show (Cos e1)  = parens $ "Cosine"++(show e1)
  show (Sin e1)  = parens $ "Sine"++(show e1)
  show (Ln e1)   = parens $ "Ln"++(show e1)
  show (Exp e1) = parens $ "e^"++(show e1)
  show (Exponent e1 e2) = parens $ (show e1)++"!^"++(show e2) 
     
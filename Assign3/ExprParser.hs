module ExprParser  where

import ExprType
import ExprPretty
import Text.Parsec
import Text.Parsec.String 

{- parseExpr*
------------------------------------
  - Takes a string of format
         ..........    
  - and parses an expression of Expr *
-}

-- Parse simple binary operations
binOps :: Parser (Expr a -> Expr a-> Expr a)
binOps = (do { symbol "+"; return Add })
     <|> do { symbol "*"; return Mult } 
     <|> do { symbol "^"; return Exponent} 

--  Parse Uniary operations defined in our expression type 
uniOps :: Parser (Expr a -> Expr a)
uniOps = (do { string "e"; return Exp })
    <|> do { string "ln"; return Ln } 
    <|> do { string "cos"; return Cos}
    <|> do { string "sin"; return Sin} 


{-This parser parse string to Expr Integer. Support only binary operations such as +,*
     eg. "2+3" => ((val 2)) !+ ((val 3)) -}
parseExprI :: String -> Expr Integer 
parseExprI ss = case parse exprI  "" ss of
                Left err  -> error "Invalid Input"
                Right expr -> expr 

exprI :: Parser (Expr Integer)
exprI = (termI `chainl1` binOps) 

-- Term: either a variable or an constant
termI :: Parser (Expr Integer)
termI = try numI <|> var

-- Parse a single integer constant
numI :: Parser (Expr Integer)
numI = do {i <- integer;
               return (Const i)}
-- Parse a single variable composed of letters 
var :: Parser (Expr a)
var = do { s <- many1 letter;
                return (Var s)} 


{- Similiar to the parser above, the only diff is that this one parses Expr Int value
   Usage is the same-}
parseExprInt :: String -> Expr Int 
parseExprInt ss = case parse exprInt  "" ss of
                 Left err  -> error "Invalid input"
                 Right expr -> expr 

exprInt :: Parser (Expr Int)
exprInt = (termInt `chainl1` binOps) 

termInt :: Parser (Expr Int)
termInt = try numParseInt <|> var

 {- This parser supports uniary operations like Ln, Sin, Cos defined in our expr type
      eg. "cos 2" => (Cosine(val 2)) -}
 
 {-Reference: github of barskyn-} 
parseExprIntG :: String -> Expr Int
parseExprIntG ss = case parse exprIntG "" ss of
                Left err  -> error "Invalid input"
                Right expr -> expr 

exprIntG :: Parser (Expr Int)
exprIntG = let
            uniarys = do {op <- uniOps;
                                    spaces;
                                    term <- termIntG;
                                    spaces;
                                    return (op term)}
            in try uniarys <|> termIntG

termIntG :: Parser (Expr Int)
termIntG = try numParseInt <|> var

numParseInt :: Parser (Expr Int)
numParseInt = do {i <- int;
               return (Const i)}
 
{-Parser that parse to Expr Double (binary operitions)
   eg. "2.2*3.3+0.09" => (((val 2.2)) !* ((val 3.3))) !+ ((val 9.0e-2))-}
parseExprD :: String -> Expr Double
parseExprD ss = case parse exprD  "" ss of
                Left err  -> error "Invalid input"
                Right expr -> expr 

exprD :: Parser (Expr Double)
exprD = termD `chainl1` binOps

termD :: Parser (Expr Double)
termD = try numParseD <|> var

numParseD :: Parser (Expr Double)
numParseD = do {i <- double;
                return (Const i)}


{-Utility Combinators-} 
{-Provides some utility parsing for common usage-}
parens :: Parser a -> Parser a
parens p = do { symbol "(";
                cs <- p;
                symbol ")"; 
                return cs }

{-Parse a single symbol -}
symbol :: String -> Parser String
symbol ss = let
    symbol' :: Parser String
    symbol' = do { ss' <- string ss;
                    return ss' }
    in try symbol'

digits :: Parser String
digits = many1 digit

negDigits :: Parser String
negDigits = do { neg <- symbol "-" ;
                    dig <- digits ;
                    return (neg ++ dig) }


integer :: Parser Integer
integer = fmap read $ try negDigits <|> digits

int :: Parser Int
int = fmap read $ try negDigits <|> digits

-- parse something like 22.22 => "22.22" 
doubleDigits :: Parser String
doubleDigits = do { ds <- try negDigits <|> digits ;
                    rs <- try decimalDigits <|> return "" ;
                    return $ ds ++ rs }


{-This parses decial digits which are split up by a ".". -}
decimalDigits :: Parser String
decimalDigits = do { d <- char '.' ;
                     rm <- digits ;
                     return $ d:rm }

double :: Parser Double
double = fmap read $ doubleDigits
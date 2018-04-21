### Assignment 3 CS1XA3
#### A math library in haskell helping with Calculus homework. Contains expression type for arithmetic manipulation, including +,-.*,^ and trig operations like sin, cos.
#### Provides functionality to partially differentiate the experssion datatype and try to normalize the experssion as best as I can.
#### Also includes a parser that could be used to parse string into the defined experssion value.

#### The source codes contains the following files:
*  ExprType.hs : Contains the definition of the math experssion datatype and getVars method for retriving variable values
*  ExprPretty.hs : Define the show typeclass for the newly defined experission type so that it will output nicely formatted string as the output
*  ExprParser.hs : Define a few parser that could parse certain string value into the desired math experssion datatype.(Refer to the table below for format instructions)
 #####Examples of the usage of parser are listed below:
 
String       | Parsed Value 
------------ | -------------
cos2 | (Cosine(val 2))
2.2*3.3+0.09 |  (((val 2.2)) !* ((val 3.3))) !+ ((val 9.0e-2))
2^3 | ((val 2)!^(val 3))
ln 100 | (Ln(val 100)) 
   #####Each parser in the ExprParser.hs parse is different in terms of the type of parsed value and the operations that are available to parse
     * parseExprI : parse String to Expr Integer. Supports binary operations only (+/*/^)
     * parseExprInt :  parse String to Expr Int. Supports binary operations only (+/*/^)
     * parseExprInt : parse String to Expr Int. Support uniary operations: cos, sin , ln..
     * parseExprD :  parse String to Expr Double. Supports binary operations only (+/*/^) 
    > Some improvements to be considered is to integrate the functionalities into a big parser so that it could handle more types/ operations.
*  ExprDiff.hs : Define the simplification of the experssion datatype and its differentation function.

> References: github of barskyn; github of Chenz;
>       https://github.com/barskyn/CS1XA3/blob/master/Assign3/assign3/ExprDiff.hs
        https://github.com/chenc118/CS1XA3/blob/master/Assign3/ExprDiff.hs   
> http://www.cas.mcmaster.ca/~dalvescb/Code/ExprType.hs

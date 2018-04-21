### Assignment 3 CS1XA3
#### A math library in haskell helping with Calculus homework. Contains expression type for arithmetic manipulation, including +,-.*,^ and trig operations like sin, cos.
#### Provides functionality to partially differentiate the experssion datatype and try to normalize the experssion as best as I can.
#### Also includes a parser that could be used to parse string into the defined experssion value.

#### The source codes contains the following files:
*  ExprType.hs : Contains the definition of the math experssion datatype and getVars method for retriving variable values
*  ExprPretty.hs : Define the show typeclass for the newly defined experission type so that it will output nicely formatted string as the output
*  ExprParser.hs : Define a few parser that could parse certain string value into the desired math experssion datatype.(Refer to the table below for format instructions)
 #####Examples of the usage of parser are listed below:
String | Parsed Value
-------| -------------
cos 2  | (Cosine(val 2))
2.2*3.3+0.09 | (((val 2.2)) !* ((val 3.3))) !+ ((val 9.0e-2))-}
2^3    | ((val 2)!^(val 3))
ln 100 | (Ln(val 100)) 

String       | Parsed Value 
------------ | -------------
cos2 | (Cosine(val 2))
Content in the first column | Content in the second column
Content in the first column | Content in the second column

*  ExprDiff.hs : Define the simplification of the experssion datatype and its differentation function.

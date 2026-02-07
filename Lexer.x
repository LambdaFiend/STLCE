{
module Lexer where
}

%wrapper "posn"

$white = [\ \t\n\r]
$digit = [0-9]
$lower = [a-z]

tokens :-

$white+          ;
if               { \pos _ -> Token pos IF }
then             { \pos _ -> Token pos THEN }
else             { \pos _ -> Token pos ELSE }
succ             { \pos _ -> Token pos SUCC }
"0"              { \pos _ -> Token pos ZERO }
true             { \pos _ -> Token pos TRUE }
false            { \pos _ -> Token pos FALSE }
iszero           { \pos _ -> Token pos ISZERO }
pred             { \pos _ -> Token pos PRED }
unit             { \pos _ -> Token pos UNIT }
\\               { \pos _ -> Token pos LAMBDA }
"λ"              { \pos _ -> Token pos LAMBDA }
"."              { \pos _ -> Token pos DOT }
":"              { \pos _ -> Token pos COLON }
";"              { \pos _ -> Token pos SEMI }
"("              { \pos _ -> Token pos LPAREN }
")"              { \pos _ -> Token pos RPAREN }
"->"             { \pos _ -> Token pos TYARR }
Nat              { \pos _ -> Token pos TYNAT }
Bool             { \pos _ -> Token pos TYBOOL }
Unit             { \pos _ -> Token pos TYUNIT }
$lower("\'")*    { \pos s -> Token pos $ VAR s }

{

data Token = Token
  { tokenPos :: AlexPosn
  , tokenDat :: TokenData
  }
  deriving (Show, Eq)

data TokenData
  = IF
  | THEN
  | ELSE
  | SUCC
  | ZERO
  | TRUE
  | FALSE
  | ISZERO
  | PRED
  | UNIT
  | LAMBDA
  | DOT
  | COLON
  | SEMI
  | LPAREN
  | RPAREN
  | TYARR
  | TYNAT
  | TYBOOL
  | TYUNIT
  | VAR String
  deriving (Show, Eq)
}

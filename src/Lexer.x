{
module Lexer where
}

%wrapper "posn"

$white = [\ \t\n\r]
$digit = [0-9]
$lower = [a-z]
$alpha = [a-zA-Z]
$alphaNum = [a-zA-Z0-9]


tokens :-

$white+                ;
if                     { \pos _ -> Token pos IF }
then                   { \pos _ -> Token pos THEN }
else                   { \pos _ -> Token pos ELSE }
succ                   { \pos _ -> Token pos SUCC }
"0"                    { \pos _ -> Token pos ZERO }
true                   { \pos _ -> Token pos TRUE }
false                  { \pos _ -> Token pos FALSE }
iszero                 { \pos _ -> Token pos ISZERO }
pred                   { \pos _ -> Token pos PRED }
unit                   { \pos _ -> Token pos UNIT }
\\                     { \pos _ -> Token pos LAMBDA }
"λ"                    { \pos _ -> Token pos LAMBDA }
"."                    { \pos _ -> Token pos DOT }
","                    { \pos _ -> Token pos COMMA }
":"                    { \pos _ -> Token pos COLON }
";"                    { \pos _ -> Token pos SEMI }
"("                    { \pos _ -> Token pos LPAREN }
")"                    { \pos _ -> Token pos RPAREN }
"{"                    { \pos _ -> Token pos LBRACK }
"}"                    { \pos _ -> Token pos RBRACK }
"->"                   { \pos _ -> Token pos TYARR }
"_"                    { \pos _ -> Token pos UNDER }
"="                    { \pos _ -> Token pos ASSIGN }
in                     { \pos _ -> Token pos IN }
as                     { \pos _ -> Token pos AS }
let                    { \pos _ -> Token pos LET }
Nat                    { \pos _ -> Token pos TYNAT }
Bool                   { \pos _ -> Token pos TYBOOL }
Unit                   { \pos _ -> Token pos TYUNIT }
$digit+                { \pos s -> Token pos (NUM $ read s) }
$lower("\'")*          { \pos s -> Token pos $ VAR s }
$alpha($alphaNum|"_")* { \pos s -> Token pos $ ID s }

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
  | COMMA
  | COLON
  | SEMI
  | LPAREN
  | RPAREN
  | LBRACK
  | RBRACK
  | UNDER
  | ASSIGN
  | IN
  | AS
  | LET
  | TYARR
  | TYNAT
  | TYBOOL
  | TYUNIT
  | NUM Int
  | VAR String
  | ID String
  deriving (Show, Eq)
}

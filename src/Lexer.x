{
module Lexer where
}

%wrapper "posn"

$white    = [\ \t\n\r\b]
$digit    = [0-9]
$lower    = [a-z]
$aLower   = [a-z]
$aNLower  = [a-z0-9]
$aUpper   = [A-Z]
$aNUpper  = [A-Z0-9]
$alphNum = [a-zA-Z0-9]

tokens :-

$white+                        ;
if                             { \pos _ -> Token pos IF }
then                           { \pos _ -> Token pos THEN }
else                           { \pos _ -> Token pos ELSE }
succ                           { \pos _ -> Token pos SUCC }
"0"                            { \pos _ -> Token pos ZERO }
true                           { \pos _ -> Token pos TRUE }
false                          { \pos _ -> Token pos FALSE }
iszero                         { \pos _ -> Token pos ISZERO }
pred                           { \pos _ -> Token pos PRED }
unit                           { \pos _ -> Token pos UNIT }
\\                             { \pos _ -> Token pos LAMBDA }
"λ"                            { \pos _ -> Token pos LAMBDA }
"Λ"                            { \pos _ -> Token pos LAMBDA }
lambda                         { \pos _ -> Token pos LAMBDA }
"."                            { \pos _ -> Token pos DOT }
","                            { \pos _ -> Token pos COMMA }
":"                            { \pos _ -> Token pos COLON }
";"                            { \pos _ -> Token pos SEMI }
"("                            { \pos _ -> Token pos LPAREN }
")"                            { \pos _ -> Token pos RPAREN }
"{"                            { \pos _ -> Token pos LBRACK }
"}"                            { \pos _ -> Token pos RBRACK }
"["                            { \pos _ -> Token pos LSQUARE }
"]"                            { \pos _ -> Token pos RSQUARE }
"->"                           { \pos _ -> Token pos TYARR }
"→"                            { \pos _ -> Token pos TYARR }
"<"                            { \pos _ -> Token pos LANGLE }
">"                            { \pos _ -> Token pos RANGLE }
"_"                            { \pos _ -> Token pos UNDER }
"="                            { \pos _ -> Token pos ASSIGN }
"|"                            { \pos _ -> Token pos PIPE }
"*"                            { \pos _ -> Token pos STAR }
Some                           { \pos _ -> Token pos FORSOME }
forsome                        { \pos _ -> Token pos FORSOME }
"∃"                            { \pos _ -> Token pos FORSOME }
All                            { \pos _ -> Token pos FORALL }
forall                         { \pos _ -> Token pos FORALL }
"∀"                            { \pos _ -> Token pos FORALL }
case                           { \pos _ -> Token pos CASE }
of                             { \pos _ -> Token pos OF }
fix                            { \pos _ -> Token pos FIX }
in                             { \pos _ -> Token pos IN }
as                             { \pos _ -> Token pos AS }
let                            { \pos _ -> Token pos LET }
letrec                         { \pos _ -> Token pos LETREC }
nil                            { \pos _ -> Token pos NIL }
cons                           { \pos _ -> Token pos CONS }
isnil                          { \pos _ -> Token pos ISNIL }
head                           { \pos _ -> Token pos HEAD }
tail                           { \pos _ -> Token pos TAIL }
List                           { \pos _ -> Token pos TYLIST }
Nat                            { \pos _ -> Token pos TYNAT }
Bool                           { \pos _ -> Token pos TYBOOL }
Unit                           { \pos _ -> Token pos TYUNIT }
$digit+                        { \pos s -> Token pos (NUM $ read s) }
$aLower($alphNum|"_")*("\''")* { \pos s -> Token pos $ IDLOWER s }
$aUpper($alphNum|"_")*("\''")* { \pos s -> Token pos $ IDUPPER s }
.                              { \pos s -> Token pos $ ERR ("Lexing error: " ++ s) }

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
  | LSQUARE
  | RSQUARE
  | LANGLE
  | RANGLE
  | UNDER
  | ASSIGN
  | PIPE
  | STAR
  | FORSOME
  | FORALL
  | IN
  | AS
  | LET
  | LETREC
  | CASE
  | OF
  | FIX
  | NIL
  | CONS
  | ISNIL
  | HEAD
  | TAIL
  | TYLIST
  | TYARR
  | TYNAT
  | TYBOOL
  | TYUNIT
  | NUM Int
  | IDLOWER String
  | IDUPPER String
  | ERR String
  deriving (Show, Eq)
}


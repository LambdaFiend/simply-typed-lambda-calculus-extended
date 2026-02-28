{
module Parser where 
import Lexer
import Syntax
}

%monad { Either String } { (>>=) } { return }
%name parser
%tokentype { Token }
%error { parseError }

%token

if         { Token pos IF }
then       { Token pos THEN }
else       { Token pos ELSE }
succ       { Token pos SUCC }
"0"        { Token pos ZERO }
true       { Token pos TRUE }
false      { Token pos FALSE }
iszero     { Token pos ISZERO }
pred       { Token pos PRED }
unit       { Token pos UNIT }
"\\"       { Token pos LAMBDA }
"."        { Token pos DOT }
":"        { Token pos COLON }
";"        { Token pos SEMI }
"("        { Token pos LPAREN }
")"        { Token pos RPAREN }
"{"        { Token pos LBRACK }
"}"        { Token pos RBRACK }
"["        { Token pos LSQUARE }
"]"        { Token pos RSQUARE }
"<"        { Token pos LANGLE }
">"        { Token pos RANGLE }
","        { Token pos COMMA }
"_"        { Token pos UNDER }
"->"       { Token pos TYARR }
"="        { Token pos ASSIGN }
"|"        { Token pos PIPE }
"*"        { Token pos STAR }
forsome    { Token pos FORSOME }
forall     { Token pos FORALL }
in         { Token pos IN }
as         { Token pos AS }
let        { Token pos LET }
letrec     { Token pos LETREC }
case       { Token pos CASE }
of         { Token pos OF }
fix        { Token pos FIX }
nil        { Token pos NIL }
cons       { Token pos CONS }
isnil      { Token pos ISNIL }
head       { Token pos HEAD }
tail       { Token pos TAIL }
tynat      { Token pos TYNAT }
tybool     { Token pos TYBOOL }
tyunit     { Token pos TYUNIT }
tylist     { Token pos TYLIST }
num        { Token pos (NUM n) }
idlower    { Token pos (IDLOWER s) }
idupper    { Token pos (IDUPPER s) }

%%

Term
  : IfTE   { $1 }
  | Let    { $1 }
  | LetRec { $1 }
  | Unpack { $1 }
  | Case   { $1 }
  | Seq    { $1 }
  | Abst   { $1 }

Case
  : case Term of CaseBranches { TermNode (tokenPos $1) $ TmCase $2 $4 }

CaseBranches
  : CaseBranches "|" CaseBranch { $1 ++ $3 }
  | CaseBranch                  { $1 }

CaseBranch : "<" Name "=" Name ">" "->" Seq { [(snd $2, (snd $4, $7))] }

Seq
  : Seq ";" App { TermNode (getFI $1) $ TmSeq $1 $3 }
  | App         { $1 }

App
  : App Proj               { TermNode (getFI $1) $ TmApp $1 $2 }
  | App "[" TypeForAll "]" { TermNode (getFI $1) $ TmTyApp $1 $3 }
  | Proj                   { $1 }

Proj
  : Proj "." Num  { TermNode (getFI $1) $ TmProj $1 (show $ snd $3) }
  | Proj "." Name { TermNode (getFI $1) $ TmProj $1 (snd $3) }
  | Ascr          { $1 }

Ascr
  : Atom as TypeForAll { TermNode (getFI $1) $ TmAscribe $1 $3 }
  | Atom               { $1 }

Atom
  : Value          { $1 }
  | Succ           { $1 }
  | Pred           { $1 }
  | IsZero         { $1 }
  | Cons           { $1 }
  | IsNil          { $1 }
  | Head           { $1 }
  | Tail           { $1 }
  | Variant        { $1 }
  | Fix            { $1 }
  | Pack           { $1 }
  | "(" Term ")"   { $2 }
  | "{" Record "}" { TermNode (tokenPos $1) $ TmRecord $2 }

Unpack : let "{" TyVar "," Name "}" "=" Term in Term { TermNode (tokenPos $1) $ TmUnpack (snd $3) (snd $5) $8 $10 }

Pack : "{" "*" TypeForAll "," Term "}" as TypeForAll { TermNode (tokenPos $1) $ TmPack $3 $5 $8 }

Cons : cons "[" TypeForAll "]" Atom Atom { TermNode (tokenPos $1) $ TmCons $3 $5 $6 }

IsNil : isnil "[" TypeForAll "]" Atom { TermNode (tokenPos $1) $ TmIsNil $3 $5 }

Head : head "[" TypeForAll "]" Atom { TermNode (tokenPos $1) $ TmHead $3 $5 }

Tail : tail "[" TypeForAll "]" Atom { TermNode (tokenPos $1) $ TmTail $3 $5 }

Fix : fix "(" Term ")" { TermNode (tokenPos $1) $ TmFix $3 }

Variant
  : "<" Name "=" Term ">" as Type { TermNode (tokenPos $1) $ TmVariant (snd $2) $4 $7 }

Record
  : Record "," Name "=" Term { $1 ++ [(snd $3, $5)] }
  | Record "," Term          { $1 ++ [("", $3)] }
  | Name "=" Term            { [(snd $1, $3)] }
  | Term                     { [("", $1)] }

Value
  : true                   { TermNode (tokenPos $1) TmTrue }
  | false                  { TermNode (tokenPos $1) TmFalse }
  | "0"                    { TermNode (tokenPos $1) TmZero }
  | unit                   { TermNode (tokenPos $1) TmUnit }
  | nil "[" TypeForAll "]" { TermNode (tokenPos $1) $ TmNil $3 }
  | Name                   { TermNode (fst $1) $ TmVarRaw (snd $1) }
  | Num                    { convertNumToSuccs (snd $1) (fst $1) }

Name : idlower { (tokenPos $1, (\(IDLOWER s) -> s) $ tokenDat $1) }

TyVar : idupper { (tokenPos $1, (\(IDUPPER s) -> s) $ tokenDat $1) }

Num : num { (tokenPos $1, (\(NUM n) -> n) $ tokenDat $1) }

Type
  : tynat                 { TyNat }
  | tybool                { TyBool }
  | tyunit                { TyUnit }
  | TypeList              { $1 }
  | "{" TypeRecord "}"    { TyRecord $2 }
  | "<" TypeVariant ">"   { TyVariant $2 }
  | "(" TypeForAll ")"    { $2 }
  | "{" TypeForSome "}"   { $2 }
  | TyVar                 { TyVarFRaw (snd $1) }

TypeForSome : forsome TyVar "," TypeForAll { TyForSome (snd $2) $4 }

TypeList : tylist Type { TyList $2 }

TypeVariant
  : TypeVariant "," Name ":" TypeForAll { $1 ++ [(snd $3, $5)] }
  | TypeVariant "," TypeForAll          { $1 ++ [("", $3)] }
  | Name ":" TypeForAll                 { [(snd $1, $3)] }
  | TypeForAll                          { [("", $1)] }

TypeForAll
  : forall TyVar "." TypeForAll { TyForAll (snd $2) $4 }
  | TypeArr                     { $1 }

TypeArr
  : Type "->" TypeArr { TyArr $1 $3 }
  | Type              { $1 }

TypeRecord
  : TypeRecord "," Name ":" TypeForAll { $1 ++ [(snd $3, $5)] }
  | TypeRecord "," TypeForAll          { $1 ++ [("", $3)] }
  | Name ":" TypeForAll                { [(snd $1, $3)] }
  | TypeForAll                         { [("", $1)] }

Abst
  : "\\" Name ":" TypeForAll "." Term { TermNode (tokenPos $1) $ TmAbs (snd $2) $4 $6 }
  | "\\" "_" ":" TypeForAll "." Term  { TermNode (tokenPos $1) $ TmWildCard $4 $6 }
  | "\\" Name "." Term             { TermNode (tokenPos $1) $ TmAbs (snd $2) TyUnknown $4 }
  | "\\" TyVar "." Term            { TermNode (tokenPos $1) $ TmTyAbs (snd $2) $4 }

Pattern
  : Name                  { PVar $ snd $1 }
  | "{" PatternRecord "}" { PRecord $2 }
  
PatternRecord
  : PatternRecord "," Name "=" Pattern { $1 ++ [(snd $3, $5)] }
  | PatternRecord "," Pattern          { $1 ++ [("", $3)] }
  | Name "=" Pattern                   { [(snd $1, $3)] }
  | Pattern                            { [("", $1)] }

IfTE : if Term then Term else Term { TermNode (tokenPos $1) $ TmIf $2 $4 $6 }

Let : let Pattern "=" Term in Term { TermNode (tokenPos $1) $ TmLet $2 $4 $6 }

LetRec : letrec Name ":" TypeArr "=" Term in Term { TermNode (tokenPos $1) $ TmLet (PVar $ snd $2) (TermNode (tokenPos $1) $ TmFix (TermNode (tokenPos $1) $ TmAbs (snd $2) $4 $6)) $8 }

Succ : succ Atom { TermNode (tokenPos $1) $ TmSucc $2 }

Pred : pred Atom { TermNode (tokenPos $1) $ TmPred $2 }

IsZero : iszero Atom { TermNode (tokenPos $1) $ TmIsZero $2 }

{
parseError :: [Token] -> Either String a
parseError [] = Left ("Parsing error near the end of the file")
parseError ((Token fi _):tokens) = Left ("Parsing error at:" ++ showFileInfoHappy fi)
parseError (x:xs) = Left "Parsing error"

convertNumToSuccs :: Int -> AlexPosn -> TermNode
convertNumToSuccs 0 fi = TermNode fi TmZero
convertNumToSuccs n fi = TermNode fi $ TmSucc (convertNumToSuccs (n - 1) fi)

showFileInfoHappy :: AlexPosn -> String
showFileInfoHappy (AlexPn p l c) =
  "\n" ++"Absolute Offset: " ++ show p ++ "\n"
  ++ "Line: " ++ show l ++ "\n"
  ++ "Column: " ++ show c
}

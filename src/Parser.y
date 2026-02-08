{
module Parser where 
import Lexer
import Syntax
}

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
"<"        { Token pos LANGLE }
">"        { Token pos RANGLE }
","        { Token pos COMMA }
"_"        { Token pos UNDER }
"->"       { Token pos TYARR }
"="        { Token pos ASSIGN }
"|"        { Token pos PIPE }
in         { Token pos IN }
as         { Token pos AS }
let        { Token pos LET }
letrec     { Token pos LETREC }
case       { Token pos CASE }
of         { Token pos OF }
fix        { Token pos FIX }
tynat      { Token pos TYNAT }
tybool     { Token pos TYBOOL }
tyunit     { Token pos TYUNIT }
num        { Token pos (NUM n) }
var        { Token pos (VAR s) }
id         { Token pos (ID s) }


%%

Term
  : IfTE      { $1 }
  | Abst      { $1 }
  | Let       { $1 }
  | Letrec    { $1 }
  | ProjSeq   { $1 }
  | BeginCase { $1 }

BeginCase
  : case Term of ContinueCase { TermNode (tokenPos $1) $ TmCase $2 $4 }

ContinueCase
  : "<" Name "=" Name ">" "->" ProjSeq "|" ContinueCase { [(snd $2, (snd $4, $7))] ++ $9 }
  | "<" Name "=" Name ">" "->" ProjSeq                  { [(snd $2, (snd $4, $7))] }


ProjSeq
  : ProjSeq "." Num     { TermNode (getFI $1) $ TmProj $1 (show $ snd $3) }
  | ProjSeq "." Name    { TermNode (getFI $1) $ TmProj $1 (snd $3) }
  | ProjSeq ";" AscrApp { TermNode (getFI $1) $ TmSeq $1 $3 }
  | AscrApp             { $1 }

AscrApp
  : AscrApp as TypeArr { TermNode (getFI $1) $ TmAscribe $1 $3 }
  | AscrApp Atom       { TermNode (getFI $1) $ TmApp $1 $2 }
  | Atom               { $1 }


Atom
  : Value          { $1 }
  | Succ           { $1 }
  | Pred           { $1 }
  | IsZero         { $1 }
  | "(" Term ")"   { $2 }
  | "{" Record "}" { TermNode (tokenPos $1) $ TmRecord $2 }
  | Variant        { $1 }
  | Fix            { $1 }

Fix : fix "(" Term ")" { TermNode (tokenPos $1) $ TmFix $3 }

Variant
  : "<" Name "=" Term ">" as Type { TermNode (tokenPos $1) $ TmVariant (snd $2) $4 $7 }

Record
  : Record "," Name "=" Term { $1 ++ [(snd $3, $5)] }
  | Record "," Term          { $1 ++ [("", $3)] }
  | Name "=" Term            { [(snd $1, $3)] }
  | Term                     { [("", $1)] }

Value
  : true  { TermNode (tokenPos $1) TmTrue }
  | false { TermNode (tokenPos $1) TmFalse }
  | "0"   { TermNode (tokenPos $1) TmZero }
  | unit  { TermNode (tokenPos $1) TmUnit }
  | Name  { TermNode (fst $1) $ TmVarRaw (snd $1) }

Name
  : Id  { $1 }
  | Var { $1 }

Id : id { (tokenPos $1, (\(ID s) -> s) $ tokenDat $1) }

Var : var { (tokenPos $1, (\(VAR s) -> s) $ tokenDat $1) }

Num : num { (tokenPos $1, (\(NUM n) -> n) $ tokenDat $1) }

Type
  : tynat               { TyNat }
  | tybool              { TyBool }
  | tyunit              { TyUnit }
  | "(" TypeArr ")"     { $2 }
  | "{" TypeRecord "}"  { TyRecord $2 }
  | "<" TypeVariant ">" { TyVariant $2 }

TypeVariant
  : TypeVariant "," Name ":" Type { $1 ++ [(snd $3, $5)] }
  | TypeVariant "," Type          { $1 ++ [("", $3)] }
  | Name ":" Type                 { [(snd $1, $3)] }
  | Type                          { [("", $1)] }

TypeArr
  : Type "->" TypeArr { TyArr $1 $3 }
  | Type              { $1 }

TypeRecord
  : TypeRecord "," Name ":" Type { $1 ++ [(snd $3, $5)] }
  | TypeRecord "," Type          { $1 ++ [("", $3)] }
  | Name ":" Type                { [(snd $1, $3)] }
  | Type                         { [("", $1)] }

Abst
  : "\\" Var ":" TypeArr "." Term { TermNode (tokenPos $1) $ TmAbs (snd $2) $4 $6 }
  | "\\" "_" ":" TypeArr "." Term { TermNode (tokenPos $1) $ TmWildCard $4 $6 }

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

Letrec : letrec Name ":" TypeArr "=" Term in Term { TermNode (tokenPos $1) $ TmLet (PVar $ snd $2) (TermNode (tokenPos $1) $ TmFix (TermNode (tokenPos $1) $ TmAbs (snd $2) $4 $6)) $8 }

Succ : succ Atom { TermNode (tokenPos $1) $ TmSucc $2 }

Pred : pred Atom { TermNode (tokenPos $1) $ TmPred $2 }

IsZero : iszero Atom { TermNode (tokenPos $1) $ TmIsZero $2 }


{
parseError :: [Token] -> a
parseError _ = error "Parse error"
}

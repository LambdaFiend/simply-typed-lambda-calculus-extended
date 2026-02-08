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
","        { Token pos COMMA }
"_"        { Token pos UNDER }
"->"       { Token pos TYARR }
"="        { Token pos ASSIGN }
in         { Token pos IN }
as         { Token pos AS }
let        { Token pos LET }
tynat      { Token pos TYNAT }
tybool     { Token pos TYBOOL }
tyunit     { Token pos TYUNIT }
num        { Token pos (NUM n) }
var        { Token pos (VAR s) }
id         { Token pos (ID s) }


%%

Term
  : Seq  { $1 }
  | IfTE { $1 }
  | Abst { $1 }
  | Let  { $1 }
  | Proj { $1 }

Proj
  : Seq "." Num  { TermNode (getFI $1) $ TmProj $1 (show $ snd $3) }
  | Seq "." Name { TermNode (getFI $1) $ TmProj $1 (snd $3) }

Seq
  : Seq ";" Ascr { TermNode (getFI $1) $ TmSeq $1 $3 }
  | Ascr         { $1 }

Ascr
  : App as TypeArr { TermNode (getFI $1) $ TmAscribe $1 $3 }
  | App            { $1 }

App
  : App Atom { TermNode (getFI $1) $ TmApp $1 $2 }
  | Atom     { $1 }

Atom
  : Value          { $1 }
  | Succ           { $1 }
  | Pred           { $1 }
  | IsZero         { $1 }
  | "(" Term ")"   { $2 }
  | "{" Record "}" { TermNode (tokenPos $1) $ TmRecord $2 }

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
  | Var   { TermNode (fst $1) $ TmVarRaw (snd $1) }

Name
  : Id  { $1 }
  | Var { $1 }

Id : id { (tokenPos $1, (\(ID s) -> s) $ tokenDat $1) }

Var : var { (tokenPos $1, (\(VAR s) -> s) $ tokenDat $1) }

Num : num { (tokenPos $1, (\(NUM n) -> n) $ tokenDat $1) }

Type
  : tynat              { TyNat }
  | tybool             { TyBool }
  | tyunit             { TyUnit }
  | "(" TypeArr ")"    { $2 }
  | "{" TypeRecord "}" { TyRecord $2 }

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

IfTE : if Term then Term else Term { TermNode (tokenPos $1) $ TmIf $2 $4 $6 }

Let : let Var "=" Term in Term { TermNode (tokenPos $1) $ TmLet (snd $2) $4 $6 }

Succ : succ Atom { TermNode (tokenPos $1) $ TmSucc $2 }

Pred : pred Atom { TermNode (tokenPos $1) $ TmPred $2 }

IsZero : iszero Atom { TermNode (tokenPos $1) $ TmIsZero $2 }


{
parseError :: [Token] -> a
parseError _ = error "Parse error"
}

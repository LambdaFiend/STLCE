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
"->"       { Token pos TYARR }
tynat      { Token pos TYNAT }
tybool     { Token pos TYBOOL }
tyunit     { Token pos TYUNIT }
var        { Token pos (VAR s) }

%%

Term
  : Seq  { $1 }
  | IfTE { $1 }
  | Abst { $1 }

Seq
  : Seq ";" App { TermNode (getFI $1) $ TmSeq $1 $3 }
  | App         { $1 }

App
  : App Atom { TermNode (getFI $1) $ TmApp $1 $2 }
  | Atom     { $1 }

Atom
  : Value        { $1 }
  | Succ         { $1 }
  | Pred         { $1 }
  | IsZero       { $1 }
  | "(" Term ")" { $2 }

Value
  : true  { TermNode (tokenPos $1) TmTrue }
  | false { TermNode (tokenPos $1) TmFalse }
  | "0"   { TermNode (tokenPos $1) TmZero }
  | unit  { TermNode (tokenPos $1) TmUnit }
  | var   { TermNode (tokenPos $1) $ TmVarRaw ((\(VAR s) -> s) $ tokenDat $1) }

TypeArr
  : Type "->" TypeArr { TyArr $1 $3 }
  | Type              { $1 }

Type
  : tynat  { TyNat }
  | tybool { TyBool }
  | tyunit { TyUnit }
  | "(" TypeArr ")" { $2 }

Abst : "\\" var ":" TypeArr "." Term { TermNode (tokenPos $1) $ TmAbs ((\(VAR s) -> s) $ tokenDat $2) $4 $6 }

IfTE : if Term then Term else Term { TermNode (tokenPos $1) $ TmIf $2 $4 $6 }

Succ : succ Atom { TermNode (tokenPos $1) $ TmSucc $2 }

Pred : pred Atom { TermNode (tokenPos $1) $ TmPred $2 }

IsZero : iszero Atom { TermNode (tokenPos $1) $ TmIsZero $2 }



{
parseError :: [Token] -> a
parseError _ = error "Parse error"
}

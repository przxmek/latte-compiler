-- This Happy file was machine-generated by the BNF converter
{
{-# OPTIONS_GHC -fno-warn-incomplete-patterns -fno-warn-overlapping-patterns #-}
module ParLatte where
import AbsLatte
import LexLatte
import ErrM

}

%name pProgram Program
-- no lexer declaration
%monad { Err } { thenM } { returnM }
%tokentype {Token}
%token
  '!' { PT _ (TS _ 1) }
  '!=' { PT _ (TS _ 2) }
  '%' { PT _ (TS _ 3) }
  '&&' { PT _ (TS _ 4) }
  '(' { PT _ (TS _ 5) }
  ')' { PT _ (TS _ 6) }
  '*' { PT _ (TS _ 7) }
  '+' { PT _ (TS _ 8) }
  '++' { PT _ (TS _ 9) }
  ',' { PT _ (TS _ 10) }
  '-' { PT _ (TS _ 11) }
  '--' { PT _ (TS _ 12) }
  '.' { PT _ (TS _ 13) }
  '/' { PT _ (TS _ 14) }
  ':' { PT _ (TS _ 15) }
  ';' { PT _ (TS _ 16) }
  '<' { PT _ (TS _ 17) }
  '<=' { PT _ (TS _ 18) }
  '=' { PT _ (TS _ 19) }
  '==' { PT _ (TS _ 20) }
  '>' { PT _ (TS _ 21) }
  '>=' { PT _ (TS _ 22) }
  '[' { PT _ (TS _ 23) }
  '[]' { PT _ (TS _ 24) }
  ']' { PT _ (TS _ 25) }
  'boolean' { PT _ (TS _ 26) }
  'class' { PT _ (TS _ 27) }
  'else' { PT _ (TS _ 28) }
  'extends' { PT _ (TS _ 29) }
  'false' { PT _ (TS _ 30) }
  'for' { PT _ (TS _ 31) }
  'if' { PT _ (TS _ 32) }
  'int' { PT _ (TS _ 33) }
  'return' { PT _ (TS _ 34) }
  'string' { PT _ (TS _ 35) }
  'true' { PT _ (TS _ 36) }
  'void' { PT _ (TS _ 37) }
  'while' { PT _ (TS _ 38) }
  '{' { PT _ (TS _ 39) }
  '||' { PT _ (TS _ 40) }
  '}' { PT _ (TS _ 41) }

L_ident  { PT _ (TV $$) }
L_integ  { PT _ (TI $$) }
L_quoted { PT _ (TL $$) }


%%

Ident   :: { Ident }   : L_ident  { Ident $1 }
Integer :: { Integer } : L_integ  { (read ( $1)) :: Integer }
String  :: { String }  : L_quoted {  $1 }

Program :: { Program }
Program : ListTopDef { AbsLatte.Program $1 }
TopDef :: { TopDef }
TopDef : FuncDef { AbsLatte.TopDefFunc $1 }
       | ClassHead '{' ListMemberDecl '}' { AbsLatte.TopDefClass $1 (reverse $3) }
ListTopDef :: { [TopDef] }
ListTopDef : TopDef { (:[]) $1 } | TopDef ListTopDef { (:) $1 $2 }
FuncDef :: { FuncDef }
FuncDef : Type Ident '(' ListFArg ')' Block { AbsLatte.FuncDef $1 $2 $4 $6 }
FArg :: { FArg }
FArg : Type Ident { AbsLatte.FArg $1 $2 }
ListFArg :: { [FArg] }
ListFArg : {- empty -} { [] }
         | FArg { (:[]) $1 }
         | FArg ',' ListFArg { (:) $1 $3 }
ClassHead :: { ClassHead }
ClassHead : 'class' Ident { AbsLatte.ClassHeadDef $2 }
          | 'class' Ident 'extends' ClassType { AbsLatte.ClassHeadExtDef $2 $4 }
MemberDecl :: { MemberDecl }
MemberDecl : Type ListIdent ';' { AbsLatte.DeclField $1 $2 }
           | FuncDef { AbsLatte.FuncField $1 }
ListMemberDecl :: { [MemberDecl] }
ListMemberDecl : {- empty -} { [] }
               | ListMemberDecl MemberDecl { flip (:) $1 $2 }
ListIdent :: { [Ident] }
ListIdent : Ident { (:[]) $1 } | Ident ',' ListIdent { (:) $1 $3 }
Block :: { Block }
Block : '{' ListStmt '}' { AbsLatte.Block (reverse $2) }
ListStmt :: { [Stmt] }
ListStmt : {- empty -} { [] } | ListStmt Stmt { flip (:) $1 $2 }
Stmt :: { Stmt }
Stmt : ';' { AbsLatte.SEmpty }
     | Block { AbsLatte.SBlStmt $1 }
     | Type ListItem ';' { AbsLatte.SDecl $1 $2 }
     | Expr '=' Expr ';' { AbsLatte.SAss $1 $3 }
     | Expr '++' ';' { AbsLatte.SIncr $1 }
     | Expr '--' ';' { AbsLatte.SDecr $1 }
     | 'return' Expr ';' { AbsLatte.SRet $2 }
     | 'return' ';' { AbsLatte.SVRet }
     | 'if' '(' Expr ')' Stmt { AbsLatte.SCond $3 $5 }
     | 'if' '(' Expr ')' Stmt 'else' Stmt { AbsLatte.SCondElse $3 $5 $7 }
     | 'while' '(' Expr ')' Stmt { AbsLatte.SWhile $3 $5 }
     | 'for' '(' Type Ident ':' Expr ')' Stmt { AbsLatte.SFor $3 $4 $6 $8 }
     | Expr ';' { AbsLatte.SExp $1 }
Item :: { Item }
Item : Ident { AbsLatte.NoInit $1 }
     | Ident '=' Expr { AbsLatte.Init $1 $3 }
ListItem :: { [Item] }
ListItem : Item { (:[]) $1 } | Item ',' ListItem { (:) $1 $3 }
Type :: { Type }
Type : BaseType { AbsLatte.BaseTypeDef $1 }
     | ArrayType { AbsLatte.ArrayTypeDef $1 }
     | ClassType { AbsLatte.ClassTypeDef $1 }
ListType :: { [Type] }
ListType : {- empty -} { [] }
         | Type { (:[]) $1 }
         | Type ',' ListType { (:) $1 $3 }
BaseType :: { BaseType }
BaseType : 'int' { AbsLatte.TInt }
         | 'string' { AbsLatte.TStr }
         | 'boolean' { AbsLatte.TBool }
         | 'void' { AbsLatte.TVoid }
ArrayType :: { ArrayType }
ArrayType : Type '[]' { AbsLatte.TArray $1 }
ClassType :: { ClassType }
ClassType : Ident { AbsLatte.TClass $1 }
Expr6 :: { Expr }
Expr6 : Ident { AbsLatte.EVar $1 }
      | Integer { AbsLatte.ELitInt $1 }
      | 'true' { AbsLatte.ELitTrue }
      | 'false' { AbsLatte.ELitFalse }
      | Ident '(' ListExpr ')' { AbsLatte.EApp $1 $3 }
      | Ident '[' ListExpr ']' { AbsLatte.EArrSub $1 $3 }
      | Expr6 '.' Ident { AbsLatte.EMember $1 $3 }
      | String { AbsLatte.EString $1 }
      | '(' Expr ')' { $2 }
Expr5 :: { Expr }
Expr5 : '-' Expr6 { AbsLatte.Neg $2 }
      | '!' Expr6 { AbsLatte.Not $2 }
      | Expr6 { $1 }
Expr4 :: { Expr }
Expr4 : Expr4 MulOp Expr5 { AbsLatte.EMul $1 $2 $3 } | Expr5 { $1 }
Expr3 :: { Expr }
Expr3 : Expr3 AddOp Expr4 { AbsLatte.EAdd $1 $2 $3 } | Expr4 { $1 }
Expr2 :: { Expr }
Expr2 : Expr2 RelOp Expr3 { AbsLatte.ERel $1 $2 $3 } | Expr3 { $1 }
Expr1 :: { Expr }
Expr1 : Expr2 '&&' Expr1 { AbsLatte.EAnd $1 $3 } | Expr2 { $1 }
Expr :: { Expr }
Expr : Expr1 '||' Expr { AbsLatte.EOr $1 $3 } | Expr1 { $1 }
ListExpr :: { [Expr] }
ListExpr : {- empty -} { [] }
         | Expr { (:[]) $1 }
         | Expr ',' ListExpr { (:) $1 $3 }
AddOp :: { AddOp }
AddOp : '+' { AbsLatte.OpPlus } | '-' { AbsLatte.OpMinus }
MulOp :: { MulOp }
MulOp : '*' { AbsLatte.OpTimes }
      | '/' { AbsLatte.OpDiv }
      | '%' { AbsLatte.OpMod }
RelOp :: { RelOp }
RelOp : '<' { AbsLatte.OpLTH }
      | '<=' { AbsLatte.OpLE }
      | '>' { AbsLatte.OpGTH }
      | '>=' { AbsLatte.OpGE }
      | '==' { AbsLatte.OpEQU }
      | '!=' { AbsLatte.OpNE }
{

returnM :: a -> Err a
returnM = return

thenM :: Err a -> (a -> Err b) -> Err b
thenM = (>>=)

happyError :: [Token] -> Err a
happyError ts =
  Bad $ "syntax error at " ++ tokenPos ts ++ 
  case ts of
    [] -> []
    [Err _] -> " due to lexer error"
    _ -> " before " ++ unwords (map (id . prToken) (take 4 ts))

myLexer = tokens
}


-- Latte static checker
-- Przemysław Kuczyński (334685)
-- p.kuczynski@student.uw.edu.pl

module StaticChecker where


import           AbsLatte
import           Control.Monad
import           ErrM

import           Environment


type Result = Err String


failure :: Show a => a -> Result
failure x = Bad $ "Undefined case: " ++ show x


checkIdent :: Ident -> EnvState ()
checkIdent x = case x of
  Ident string -> return () -- @TODO


checkProgram :: Program -> EnvState ()
checkProgram (Program []) = return ()
checkProgram (Program (def:defs)) = do
  checkTopDef def
  checkProgram (Program defs)


checkTopDef :: TopDef -> EnvState ()
checkTopDef x = case x of
  TopDefFunc funcdef                -> return () -- @TODO
  TopDefClass classhead memberdecls -> return () -- @TODO

checkFuncDef :: FuncDef -> EnvState ()
checkFuncDef x = case x of
  FuncDef type_ ident fargs block -> return () -- @TODO

checkFArg :: FArg -> EnvState ()
checkFArg x = case x of
  FArg type_ ident -> return () -- @TODO

checkClassHead :: ClassHead -> EnvState ()
checkClassHead x = case x of
  ClassHeadDef ident              -> return () -- @TODO
  ClassHeadExtDef ident classtype -> return () -- @TODO

checkMemberDecl :: MemberDecl -> EnvState ()
checkMemberDecl x = case x of
  DeclField type_ idents -> return () -- @TODO
  FuncField funcdef      -> return () -- @TODO


checkStmt :: Stmt -> EnvState ()
checkStmt x = case x of
  SEmpty -> return ()
  SBlStmt (Block stmts) -> do
    newScope
    foldr ((>>) . checkStmt) (return ()) stmts
    exitScope
  SDecl type_ items -> return () -- @TODO
  SAss expr1 expr2 -> return () -- @TODO
  SIncr expr -> return () -- @TODO
  SDecr expr -> return () -- @TODO
  SRet expr -> return () -- @TODO
  SVRet -> return () -- @TODO
  SCond expr stmt -> checkStmt $ SCondElse expr stmt SEmpty
  SCondElse expr stmt1 stmt2 -> do
    cond <- checkExpr expr
    when (cond /= BaseTypeDef TBool) appendError
    checkStmt stmt1
    checkStmt stmt2
  SWhile expr stmt -> checkStmt $ SCond expr stmt
  SFor type_ ident expr stmt -> do
    arrType <- checkExpr $ EVar ident
    checkExpr expr -- @TODO check elem arr type
    checkStmt stmt
  SExp expr -> void $ checkExpr expr


checkItem :: Item -> EnvState ()
checkItem x = case x of
  NoInit ident    -> return () -- @TODO
  Init ident expr -> return () -- @TODO


checkType :: Type -> EnvState ()
checkType x = case x of
  BaseTypeDef basetype   -> return () -- @TODO
  ArrayTypeDef arraytype -> return () -- @TODO
  ClassTypeDef classtype -> return () -- @TODO

checkBaseType :: BaseType -> EnvState ()
checkBaseType x = case x of
  TInt  -> return () -- @TODO
  TStr  -> return () -- @TODO
  TBool -> return () -- @TODO
  TVoid -> return () -- @TODO

checkArrayType :: ArrayType -> EnvState ()
checkArrayType x = case x of
  TArray type_ -> return () -- @TODO

checkClassType :: ClassType -> EnvState ()
checkClassType x = case x of
  TClass ident -> return () -- @TODO


checkExpr :: Expr -> EnvState Type
checkExpr x = case x of
  EVar ident             -> return $ BaseTypeDef TVoid -- @TODO
  ELitInt integer        -> return $ BaseTypeDef TInt
  ELitTrue               -> return $ BaseTypeDef TBool
  ELitFalse              -> return $ BaseTypeDef TBool
  ENewClass classtype    -> return $ BaseTypeDef TVoid -- @TODO
  ENewArray type_ expr   -> return $ BaseTypeDef TVoid -- @TODO
  EApp ident exprs       -> return $ BaseTypeDef TVoid -- @TODO
  EArrSub ident exprs    -> return $ BaseTypeDef TVoid -- @TODO
  EMember expr ident     -> return $ BaseTypeDef TVoid -- @TODO
  EString string         -> return $ BaseTypeDef TVoid -- @TODO
  Neg expr               -> return $ BaseTypeDef TVoid -- @TODO
  Not expr               -> return $ BaseTypeDef TVoid -- @TODO
  EMul expr1 mulop expr2 -> return $ BaseTypeDef TVoid -- @TODO
  EAdd expr1 addop expr2 -> return $ BaseTypeDef TVoid -- @TODO
  ERel expr1 relop expr2 -> return $ BaseTypeDef TVoid -- @TODO
  EAnd expr1 expr2       -> return $ BaseTypeDef TVoid -- @TODO
  EOr expr1 expr2        -> return $ BaseTypeDef TVoid -- @TODO

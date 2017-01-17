-- Latte static checker
-- Przemysław Kuczyński (334685)
-- p.kuczynski@student.uw.edu.pl

module StaticChecker where


import           AbsLatte
import           Control.Monad

import           Environment


checkIdent :: Ident -> EnvState ()
checkIdent ident = do -- @TODO
  varType <- lookupVarType ident
  case varType of
    BaseTypeDef TVoid -> return () -- error
    _                 -> return () -- ok


checkProgram :: Program -> EnvState ()
checkProgram (Program []) = return ()
checkProgram (Program topDefs) = do
  saveTopDefs topDefs
  checkTopDefs topDefs


saveTopDefs :: [TopDef] -> EnvState ()
saveTopDefs [] = return ()
saveTopDefs (def:defs) = do
  case def of
    TopDefFunc (FuncDef type_ ident fargs _) ->
      newVar (FuncTypeDef (TFunc type_ [t | FArg t _ <- fargs])) ident
    TopDefClass _ _ ->
      return () -- @TODO
  saveTopDefs defs

checkTopDefs :: [TopDef] -> EnvState ()
checkTopDefs [] = return ()
checkTopDefs (def:defs) = do
  case def of
    TopDefFunc funcDef -> checkFuncDef funcDef
    TopDefClass _ _    -> return () -- @TODO
  checkTopDefs defs


checkFuncDef :: FuncDef -> EnvState ()
checkFuncDef (FuncDef type_ _ fargs block) = do
  newScope
  foldr ((>>) . defineArg) (return ()) fargs
  checkStmt type_ (SBlStmt block)
  exitScope
  where
    defineArg (FArg t var) = newVar t var

-- @TODO probably to remove
checkFArg :: FArg -> EnvState ()
checkFArg x = case x of
  FArg type_ ident -> return () -- @TODO


checkClassHead :: ClassHead -> EnvState ()
checkClassHead x = case x of
  ClassHeadDef ident              -> return () -- @TODO
  ClassHeadExtDef ident classtype -> return () -- @TODO

checkMemberDecls :: [MemberDecl] -> EnvState ()
checkMemberDecls [] = return ()
checkMemberDecls (x:xs) = case x of
  DeclField type_ idents -> return () -- @TODO
  FuncField funcdef      -> return () -- @TODO




checkStmt :: Type -> Stmt -> EnvState ()
checkStmt retType x = case x of
  SEmpty -> return ()
  SBlStmt (Block stmts) -> do
    newScope
    foldr ((>>) . checkStmt retType) (return ()) stmts
    exitScope
  SDecl type_ items -> checkStmtDecl type_ items
  SAss expr1 expr2 -> return () -- @TODO
  SIncr expr -> checkStmt retType (SAss expr $ ELitInt 0)
  SDecr expr -> checkStmt retType (SIncr expr)
  SRet expr -> do
    exprType <- checkExpr expr
    when (retType /= exprType) appendError
  SVRet ->
    when (retType /= BaseTypeDef TVoid) appendError
  SCond expr stmt -> checkStmt retType $ SCondElse expr stmt SEmpty
  SCondElse expr stmt1 stmt2 -> do
    cond <- checkExpr expr
    when (cond /= BaseTypeDef TBool) appendError
    checkStmt retType stmt1
    checkStmt retType stmt2
  SWhile expr stmt -> checkStmt retType $ SCond expr stmt
  SFor type_ ident expr stmt -> do
    arrType <- checkExpr expr
    verifyArrayElem type_ arrType
    newVar type_ ident
    checkStmt retType stmt
  SExp expr -> void $ checkExpr expr


checkStmtDecl :: Type -> [Item] -> EnvState ()
checkStmtDecl (BaseTypeDef TVoid) _ = appendError
checkStmtDecl type_ items = do
  let
    noInitVars = [i | NoInit i <- items]
    initVars = [i | Init i _ <- items]
    exprs = [e | Init _ e <- items]
  foldr ((>>) . compareExprType) (return ()) exprs
  foldr ((>>) . newVar type_) (return ()) (noInitVars ++ initVars)
  where
    compareExprType expr = do
      exprType <- checkExpr expr
      when (notVoid exprType && type_ /= exprType) appendError


-- @TODO probably to remove
checkItem :: Item -> EnvState ()
checkItem x = case x of
  NoInit ident    -> return () -- @TODO
  Init ident expr -> return () -- @TODO


-- @TODO probably to remove
checkType :: Type -> EnvState ()
checkType x = case x of
  BaseTypeDef basetype   -> return () -- @TODO
  ArrayTypeDef arraytype -> return () -- @TODO
  ClassTypeDef classtype -> return () -- @TODO

-- @TODO probably to remove
checkBaseType :: BaseType -> EnvState ()
checkBaseType x = case x of
  TInt  -> return () -- @TODO
  TStr  -> return () -- @TODO
  TBool -> return () -- @TODO
  TVoid -> return () -- @TODO

-- @TODO probably to remove
checkArrayType :: ArrayType -> EnvState ()
checkArrayType x = case x of
  TArray type_ -> return () -- @TODO

-- @TODO probably to remove
checkClassType :: ClassType -> EnvState ()
checkClassType x = case x of
  TClass ident -> return () -- @TODO




checkExpr :: Expr -> EnvState Type
checkExpr (EVar ident)             = return $ BaseTypeDef TVoid -- @TODO
checkExpr (ELitInt _)              = return $ BaseTypeDef TInt
checkExpr  ELitTrue                = return $ BaseTypeDef TBool
checkExpr  ELitFalse               = return $ BaseTypeDef TBool
checkExpr (EString _)              = return $ BaseTypeDef TStr
checkExpr (ENewClass classtype)    = return $ ClassTypeDef classtype
checkExpr (ENewArray type_ expr)   = checkNewArrayExpr type_ expr
checkExpr (EArrSub expr1 expr2)    = checkArraySubscript expr1 expr2
checkExpr (EApp expr exprs)        = checkApplicationExpr expr exprs
checkExpr (EMember expr ident)     = return $ BaseTypeDef TVoid -- @TODO
checkExpr (Neg expr)               = checkUnaryOp expr NegOp
checkExpr (Not expr)               = checkUnaryOp expr NotOp
checkExpr (EMul expr1 mulOp expr2) = checkBinaryOp expr1 expr2 (MulOp mulOp)
checkExpr (EAdd expr1 addOp expr2) = checkBinaryOp expr1 expr2 (AddOp addOp)
checkExpr (ERel expr1 relOp expr2) = checkBinaryOp expr1 expr2 (RelOp relOp)
checkExpr (EAnd expr1 expr2)       = checkBinaryOp expr1 expr2 LogOp
checkExpr (EOr expr1 expr2)        = checkBinaryOp expr1 expr2 LogOp


checkNewArrayExpr :: Type -> Expr -> EnvState Type
checkNewArrayExpr type_ expr = do
  _ <- checkUnaryOp expr ArrSubscOp -- @TODO ma to sens (nie wystarczy sprawdzic czy to int)?
  sizeType <- checkExpr expr
  when (sizeType /= BaseTypeDef TInt) appendError
  if type_ == BaseTypeDef TVoid
    then do
      appendError
      return $ BaseTypeDef TVoid
    else
      return $ ArrayTypeDef (TArray type_)


checkArraySubscript :: Expr -> Expr -> EnvState Type
checkArraySubscript expr1 expr2 = do
  _ <- checkUnaryOp expr2 ArrSubscOp -- @TODO ma to sens (nie wystarczy sprawdzic czy to int)?
  subType <- checkExpr expr2
  when (subType /= BaseTypeDef TInt) appendError

  arrTypeDef <- checkExpr expr1
  case arrTypeDef of
    ArrayTypeDef (TArray type_) -> return type_
    _ -> do
      appendError
      return $ BaseTypeDef TVoid


checkApplicationExpr :: Expr -> [Expr] -> EnvState Type
checkApplicationExpr expr exprs = do
  funcType <- checkExpr expr
  case funcType of
    FuncTypeDef (TFunc retType argsDefType) -> do
      argsActType <- checkExprs exprs
      when (argsDefType /= argsActType) appendError
      return retType
    _ -> do
      appendError
      return $ BaseTypeDef TVoid
    where
      checkExprs [] = return []
      checkExprs (expr':exprs') = do
        exprType <- checkExpr expr'
        rest <- checkExprs exprs'
        return $ exprType : rest


checkUnaryOp :: Expr -> UnaryOp -> EnvState Type
checkUnaryOp expr unaryOp = do
  exprType <- checkExpr expr
  if exprType `notElem` allowedOpTypes (UnaryOp unaryOp)
    then do
      appendError
      return $ BaseTypeDef TVoid
    else return exprType


checkBinaryOp :: Expr -> Expr -> BinOp -> EnvState Type
checkBinaryOp expr1 expr2 binOp = do
  lhsType <- checkExpr expr1
  rhsType <- checkExpr expr2
  let allowedTypes = allowedOpTypes $ BinOp binOp
  if (notVoid lhsType && notVoid rhsType && lhsType /= rhsType)
      || lhsType `notElem` allowedTypes || rhsType `notElem` allowedTypes
    then do
      appendError
      return $ BaseTypeDef TVoid
    else
      return lhsType


verifyArrayElem :: Type -> Type -> EnvState ()
verifyArrayElem elemType arrType = do
  when (arrType /= ArrayTypeDef (TArray elemType)) appendError
  return ()


notVoid :: Type -> Bool
notVoid t = t /= BaseTypeDef TVoid


allowedOpTypes :: Op -> [Type]
allowedOpTypes (UnaryOp unOp) = case unOp of
  NegOp      -> [BaseTypeDef TInt]
  NotOp      -> [BaseTypeDef TBool]
  ArrSubscOp -> [BaseTypeDef TInt]
allowedOpTypes (BinOp binOp) = BaseTypeDef TVoid : case binOp of
  AddOp OpPlus -> [BaseTypeDef TInt, BaseTypeDef TStr]
  AddOp _      -> [BaseTypeDef TInt]
  MulOp _      -> [BaseTypeDef TInt]
  RelOp _      -> [BaseTypeDef TInt]
  LogOp        -> [BaseTypeDef TBool]

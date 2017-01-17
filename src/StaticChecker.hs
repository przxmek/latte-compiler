-- Latte static checker
-- Przemysław Kuczyński (334685)
-- p.kuczynski@student.uw.edu.pl

module StaticChecker where

import           Control.Monad
import qualified Data.Map      as M

import           AbsLatte
import           Environment


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
    TopDefClass classHead membersDef ->
      newClass ident ext members
      where
        (ident, ext) = case classHead of
          ClassHeadDef i                  -> (i, BaseClass)
          ClassHeadExtDef i (TClass base) -> (i, ExtClass base)

        decls = [(dident, dtype) |
                  DeclField dtype idents <- membersDef,
                  dident <- idents]
        funcs = [(fident, FuncTypeDef (TFunc rtype (types fargs))) |
                  FuncField (FuncDef rtype fident fargs _) <- membersDef ]

        members = M.fromList (decls ++ funcs)

        types []                      = []
        types (FArg argtype _ : args) = argtype : types args

  saveTopDefs defs


checkTopDefs :: [TopDef] -> EnvState ()
checkTopDefs [] = return ()
checkTopDefs (def:defs) = do
  case def of
    TopDefFunc funcDef            -> checkFuncDef funcDef
    TopDefClass classHead members -> checkTopDefClass classHead members
  checkTopDefs defs


checkTopDefClass :: ClassHead -> [MemberDecl] -> EnvState ()
checkTopDefClass classHead memberDecls = do
  members <- getClassMembers className
  newScope
  foldr ((>>) . newVars) (return ()) members
  foldr ((>>) . checkFuncDef) (return ()) methods
  exitScope
  where
    className = case classHead of
      ClassHeadDef c      -> c
      ClassHeadExtDef c _ -> c
    newVars (v, t) = newVar t v
    methods = [f | FuncField f <- memberDecls]


checkFuncDef :: FuncDef -> EnvState ()
checkFuncDef (FuncDef type_ _ fargs block) = do
  newScope
  foldr ((>>) . defineArg) (return ()) fargs
  checkStmt type_ (SBlStmt block)
  exitScope
  where
    defineArg (FArg t var) = newVar t var




checkStmt :: Type -> Stmt -> EnvState ()
checkStmt retType x = case x of
  SEmpty -> return ()
  SBlStmt (Block stmts) -> do
    newScope
    foldr ((>>) . checkStmt retType) (return ()) stmts
    exitScope
  SDecl type_ items -> checkStmtDecl type_ items
  SAss expr1 expr2 -> checkStmtAssign expr1 expr2
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


checkStmtAssign :: Expr -> Expr -> EnvState ()
checkStmtAssign expr1 expr2 =
  if isLvalue expr1 then do
    lhs <- checkExpr expr1
    rhs <- checkExpr expr2
    when (notVoid lhs && notVoid rhs && lhs /= rhs) appendError
  else appendError
  where
    isLvalue expr = case expr of
      EVar _      -> True
      EMember _ _ -> True
      EArrSub _ _ -> True
      _           -> False



checkExpr :: Expr -> EnvState Type
checkExpr (EVar ident)             = lookupVarType ident
checkExpr (ELitInt _)              = return $ BaseTypeDef TInt
checkExpr  ELitTrue                = return $ BaseTypeDef TBool
checkExpr  ELitFalse               = return $ BaseTypeDef TBool
checkExpr (EString _)              = return $ BaseTypeDef TStr
checkExpr (EClassNull classtype)   = return $ ClassTypeDef classtype
checkExpr (ENewClass classtype)    = return $ ClassTypeDef classtype
checkExpr (ENewArray type_ expr)   = checkExprNewArray type_ expr
checkExpr (EArrSub expr1 expr2)    = checkExprArraySubscript expr1 expr2
checkExpr (EApp expr exprs)        = checkExprApplication expr exprs
checkExpr (EMember expr ident)     = checkExprMember expr ident
checkExpr (Neg expr)               = checkUnaryOp expr NegOp
checkExpr (Not expr)               = checkUnaryOp expr NotOp
checkExpr (EMul expr1 mulOp expr2) = checkBinaryOp expr1 expr2 (MulOp mulOp)
checkExpr (EAdd expr1 addOp expr2) = checkBinaryOp expr1 expr2 (AddOp addOp)
checkExpr (ERel expr1 relOp expr2) = checkBinaryOp expr1 expr2 (RelOp relOp)
checkExpr (EAnd expr1 expr2)       = checkBinaryOp expr1 expr2 LogOp
checkExpr (EOr expr1 expr2)        = checkBinaryOp expr1 expr2 LogOp


checkExprNewArray :: Type -> Expr -> EnvState Type
checkExprNewArray type_ expr = do
  _ <- checkUnaryOp expr ArrSubscOp
  if type_ == BaseTypeDef TVoid
    then do
      appendError
      return $ BaseTypeDef TVoid
    else
      return $ ArrayTypeDef (TArray type_)


checkExprArraySubscript :: Expr -> Expr -> EnvState Type
checkExprArraySubscript expr1 expr2 = do
  _ <- checkUnaryOp expr2 ArrSubscOp
  arrTypeDef <- checkExpr expr1
  case arrTypeDef of
    ArrayTypeDef (TArray type_) -> return type_
    _ -> do
      appendError
      return $ BaseTypeDef TVoid


checkExprApplication :: Expr -> [Expr] -> EnvState Type
checkExprApplication expr exprs = do
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


checkExprMember :: Expr -> Ident -> EnvState Type
checkExprMember expr memberIdent = do
  lhsType <- checkExpr expr
  case lhsType of
    ClassTypeDef (TClass classIdent) -> getClassMember classIdent memberIdent
    _ -> do
      appendError
      return $ BaseTypeDef TVoid


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


-- @TODO czy te voidy są konieczne?
allowedOpTypes :: Op -> [Type]
allowedOpTypes (UnaryOp unOp) = case unOp of
  NegOp      -> [BaseTypeDef TVoid, BaseTypeDef TInt]
  NotOp      -> [BaseTypeDef TVoid, BaseTypeDef TBool]
  ArrSubscOp -> [BaseTypeDef TInt]
allowedOpTypes (BinOp binOp) = BaseTypeDef TVoid : case binOp of
  AddOp OpPlus -> [BaseTypeDef TInt, BaseTypeDef TStr]
  AddOp _      -> [BaseTypeDef TInt]
  MulOp _      -> [BaseTypeDef TInt]
  RelOp _      -> [BaseTypeDef TInt]
  LogOp        -> [BaseTypeDef TBool]

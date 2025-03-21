module Frontend.StaticChecker where

import           Control.Monad
import qualified Data.Map             as M

import           AbsLatte
import           Frontend.Environment


checkProgram :: Program -> EnvState ()
checkProgram (Program []) = return ()
checkProgram (Program topDefs) = do
  saveTopDefs stdLibDefs
  saveTopDefs topDefs
  checkMainFunc
  checkTopDefs topDefs
  where
    stdLibDefs = [
      TopDefFunc printIntFuncDef,
      TopDefFunc printStringFuncDef,
      TopDefFunc readIntFuncDef,
      TopDefFunc readStringFuncDef]
    checkMainFunc = do
      mainType <- lookupVarType (Ident "main")
      case mainType of
        FuncTypeDef (TFunc retType _) -> case retType of
          BaseTypeDef TInt  -> return ()
          BaseTypeDef TVoid -> return ()
          _                 -> mainError
        _ -> mainError
      where
        mainError = appendError "Undefined reference to `main`"


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
        self = (Ident "self", ClassTypeDef (TClass ident))

        members = M.fromList $ self:(decls ++ funcs)

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
checkFuncDef (FuncDef retType _ fargs block) = do
  newScope
  foldr ((>>) . defineArg) (return ()) fargs
  retVal <- checkStmt retType (SBlStmt block)
  exitScope
  when (retVal /= retType) $
      appendError $ "Invalid return type\nCouldn't match expected type "
        ++ show retType  ++ "\n\twith actual type " ++ show retVal
  where
    defineArg (FArg t var) = newVar t var




checkStmt :: Type -> Stmt -> EnvState Type
checkStmt retType x = case x of
  SEmpty -> returnVoid
  SBlStmt (Block stmts) -> do
    newScope
    retVal <- checkStmts stmts
    exitScope
    return retVal
    where
      checkStmts [] = returnVoid
      checkStmts (stmt:stmts') = do
        retVal <- checkStmt retType stmt
        prevRetVal <- checkStmts stmts'
        if notVoid retVal
          then return retVal
          else return prevRetVal
  SDecl type_ items -> checkStmtDecl type_ items
  SAss expr1 expr2 -> checkStmtAssign expr1 expr2
  SIncr expr -> checkStmt retType (SAss expr $ ELitInt 0)
  SDecr expr -> checkStmt retType (SIncr expr)
  SRet expr -> do
    exprType <- checkExpr expr
    typesCheck <- checkTypes retType exprType
    unless typesCheck $
      appendError $ "Incorrect return value type. (expected "
        ++ show retType ++ " but got " ++ show exprType ++ ")."
    return retType
  SVRet -> do
    when (retType /= BaseTypeDef TVoid) $
      appendError $ "Incorrect return value type. (expected void"
        ++ show retType ++ " but got " ++ show (BaseTypeDef TVoid) ++ ")."
    returnVoid
  SCond expr stmt -> checkStmt retType $ SCondElse expr stmt SEmpty
  SCondElse expr stmt1 stmt2 -> do
    cond <- checkExpr expr
    when (cond /= BaseTypeDef TBool) $
      appendError $ "If statement condition must be of type Bool but got "
        ++ show cond
    retValT <- checkStmt retType stmt1
    retValF <- checkStmt retType stmt2
    case expr of
      ELitTrue  -> return retValT
      ELitFalse -> return retValF
      _         -> if notVoid retValT && notVoid retValF
                     then return retValT
                     else returnVoid
  SWhile expr stmt -> checkStmt retType $ SCond expr stmt
  SFor type_ ident expr stmt -> do
    arrType <- checkExpr expr
    verifyArrayElem type_ arrType
    newScope
    newVar type_ ident
    retVal <- checkStmt retType stmt
    exitScope
    return retVal
  SExp expr -> do
    _ <- checkExpr expr
    returnVoid


checkStmtDecl :: Type -> [Item] -> EnvState Type
checkStmtDecl (BaseTypeDef TVoid) _ = do
  appendError "Void type declaration is forbidden."
  returnVoid
checkStmtDecl type_ items = createVars items
  where
    createVars [] = returnVoid
    createVars (item:items') = do
      case item of
        NoInit var -> newVar type_ var
        Init var expr -> do
          compareExprType expr
          newVar type_ var
      createVars items'
    compareExprType expr = do
      exprType <- checkExpr expr
      typesCheck <- checkTypes type_ exprType
      when (notNoType exprType && not typesCheck) $
        appendError $ "Expression type " ++ show exprType
          ++ " does not match the type " ++ show type_ ++ " of the variable."


checkStmtAssign :: Expr -> Expr -> EnvState Type
checkStmtAssign expr1 expr2 =
  if isLvalue expr1 then do
    lhs <- checkExpr expr1
    rhs <- checkExpr expr2
    typesCheck <- checkTypes lhs rhs
    when (notNoType lhs && notNoType rhs && not typesCheck) $
      appendError $ "Right side expression type " ++ show rhs
        ++ " does not match the assignment type " ++ show lhs ++ "."
    returnVoid
  else do
    appendError $ "Left side expression must be a lvalue, but got "
      ++ show expr1 ++ "."
    returnVoid
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
      appendError "Array type must not be void"
      return NoTypeDef
    else
      return $ ArrayTypeDef (TArray type_)


checkExprArraySubscript :: Expr -> Expr -> EnvState Type
checkExprArraySubscript expr1 expr2 = do
  _ <- checkUnaryOp expr2 ArrSubscOp
  arrTypeDef <- checkExpr expr1
  case arrTypeDef of
    ArrayTypeDef (TArray type_) -> return type_
    _ -> do
      appendError $ "Expected array type but got " ++ show arrTypeDef
        ++ " from " ++ show expr1
      return NoTypeDef


checkExprApplication :: Expr -> [Expr] -> EnvState Type
checkExprApplication expr exprs = do
  funcType <- checkExpr expr
  case funcType of
    FuncTypeDef (TFunc retType argsDefType) -> do
      argsActType <- checkExprs exprs
      typesCheck <- checkTypesList argsDefType argsActType
      unless typesCheck $
        appendError $ "Function argument type does not match (expected "
          ++ show argsDefType ++ " but got " ++ show argsActType ++ ")."
      return retType
    _ -> do
      appendError $ "Function type exptected in " ++ show expr ++ "."
      return NoTypeDef
    where
      checkExprs [] = return []
      checkExprs (expr':exprs') = do
        exprType <- checkExpr expr'
        rest <- checkExprs exprs'
        return $ exprType : rest

      checkTypesList [] [] = return True
      checkTypesList []  _ = return False
      checkTypesList _  [] = return False
      checkTypesList (t1:l1) (t2:l2) = do
        rest <- checkTypesList l1 l2
        typesCheck <- checkTypes t1 t2
        return $ typesCheck && rest


checkExprMember :: Expr -> Ident -> EnvState Type
checkExprMember expr memberIdent = do
  lhsType <- checkExpr expr
  case lhsType of
    ClassTypeDef (TClass classIdent) -> getClassMember classIdent memberIdent
    ArrayTypeDef _ -> case memberIdent of
      Ident "length" -> return $ BaseTypeDef TInt
      _ -> do
        appendError $ "Array doesn't have a member " ++ show memberIdent
        return NoTypeDef
    _ -> do
      appendError $ show lhsType ++ " from " ++ show expr
        ++ " does not have a member " ++ show memberIdent ++ "."
      return NoTypeDef


checkUnaryOp :: Expr -> UnaryOp -> EnvState Type
checkUnaryOp expr unaryOp = do
  exprType <- checkExpr expr
  let allowed = allowedOpTypes (UnaryOp unaryOp)
  if exprType `notElem` allowed
    then do
      appendError $ "Couldn't match expected types " ++ show allowed
        ++ "\n\twith actual type " ++ show exprType
      return NoTypeDef
    else return exprType


checkBinaryOp :: Expr -> Expr -> BinOp -> EnvState Type
checkBinaryOp expr1 expr2 (RelOp _) = do
  lhs <- checkExpr expr1
  rhs <- checkExpr expr2
  when (notNoType lhs && notNoType rhs && lhs /= rhs) $
    appendError $ "LHS type " ++ show lhs ++ " does not match RHS type "
      ++ show rhs
  return $ BaseTypeDef TBool
checkBinaryOp expr1 expr2 binOp = do
  lhsType <- checkExpr expr1
  rhsType <- checkExpr expr2
  let allowed = allowedOpTypes $ BinOp binOp
  if (notNoType lhsType && notNoType rhsType && lhsType /= rhsType)
      || lhsType `notElem` allowed || rhsType `notElem` allowed
    then do
      appendError $ "Couldn't match expected types " ++ show allowed
        ++ "\n\twith actual type " ++ show lhsType ++ ", "
        ++ show rhsType ++ "."
      return NoTypeDef
    else
      return lhsType


checkTypes :: Type -> Type -> EnvState Bool
checkTypes t1 t2 =
  if t1 == t2
    then return True
    else case (t1, t2) of
      (ClassTypeDef (TClass c1), ClassTypeDef (TClass c2)) -> isSubclass c2 c1
      _ -> return False
    where
      isSubclass class_ superClass = do
        parent <- getParentClass class_
        case parent of
          Nothing -> return False
          Just parentClass ->
            if parentClass == superClass
              then return True
              else isSubclass parentClass superClass



verifyArrayElem :: Type -> Type -> EnvState ()
verifyArrayElem elemType arrType = do
  when (arrType /= ArrayTypeDef (TArray elemType)) $
    appendError $ "Couldn't match expected type "
      ++ show (ArrayTypeDef (TArray elemType))
      ++ "\n\twith actual type " ++ show arrType ++ "."
  return ()


notVoid :: Type -> Bool
notVoid t = t /= BaseTypeDef TVoid

notNoType :: Type -> Bool
notNoType t = t /= NoTypeDef

returnVoid :: EnvState Type
returnVoid = return $ BaseTypeDef TVoid


allowedOpTypes :: Op -> [Type]
allowedOpTypes (UnaryOp unOp) = NoTypeDef : case unOp of
  NegOp      -> [BaseTypeDef TInt]
  NotOp      -> [BaseTypeDef TBool]
  ArrSubscOp -> [BaseTypeDef TInt]
allowedOpTypes (BinOp binOp) = NoTypeDef : case binOp of
  AddOp OpPlus -> [BaseTypeDef TInt, BaseTypeDef TStr]
  AddOp _      -> [BaseTypeDef TInt]
  MulOp _      -> [BaseTypeDef TInt]
  RelOp _      -> [BaseTypeDef TBool, BaseTypeDef TInt]
  LogOp        -> [BaseTypeDef TBool]

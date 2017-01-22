module Generator.Generator where

import           Control.Monad         (liftM2)
import           Data.List             (intercalate)
import qualified Data.Map              as M

import           AbsLatte
import           Generator.Environment

type Result = EnvState Code
type ResultExpr = EnvState (Code, String, Type)

type ArgMap = M.Map Ident Type


todo :: Result
todo = return "NotImplemented\n"

todoExpr :: ResultExpr
todoExpr = return ("NotImplemented\n", [], NoTypeDef)


genStdLib :: EnvState Code
genStdLib = do
  saveFunType (Ident "printInt") (BaseTypeDef TVoid)
  genStdLibDecl

genStdLibDecl :: EnvState Code
genStdLibDecl = return $
  "declare void @printInt(i32)\n"
  ++ "\n"


genProgram :: Program -> Result
genProgram (Program topdefs) = do
  std <- genStdLib
  saveTopDefs topdefs
  code <- genTopDefs topdefs
  return $ std ++ code


saveTopDefs :: [TopDef] -> EnvState ()
saveTopDefs = foldr ((>>) . saveTopDef) (return ())

saveTopDef :: TopDef -> EnvState ()
saveTopDef (TopDefFunc (FuncDef type_ ident _ _)) = saveFunType ident type_
saveTopDef (TopDefClass _ _)                      = return () -- @TODO (extension - objects)

genTopDefs :: [TopDef] -> Result
genTopDefs = foldr (liftM2 (++) . genTopDef) (return [])

genTopDef :: TopDef -> Result
genTopDef (TopDefFunc funcdef)              = genFuncDef funcdef
genTopDef (TopDefClass classhead membdecls) = genClassDef classhead membdecls


genFuncDef :: FuncDef -> Result
genFuncDef (FuncDef type_ ident@(Ident f) fargs block) = do
  saveFunType ident type_
  code <- genStmt (argMap fargs) (SBlStmt block)
  let args = argsToLLVM fargs
  return $ "define " ++ typeToLLVM type_ ++ " @" ++ f ++ "(" ++ args ++ ") {\n"
    ++ code ++ "}\n\n"
  where
    argMap []                 = M.empty
    argMap (FArg t i : args') = M.insert i t $ argMap args'

    argsToLLVM args' = intercalate ", " $ argsToLLVM' args'
      where
        argsToLLVM' [] = []
        argsToLLVM' (FArg t (Ident x) : args'') =
          (typeToLLVM t ++ " %" ++ x) : argsToLLVM' args''


genClassDef :: ClassHead -> [MemberDecl] -> Result
genClassDef _ _ = todo -- @TODO (objects)

genClassHead :: ClassHead -> Result
genClassHead x = case x of
  ClassHeadDef _      -> todo -- @TODO (objects)
  ClassHeadExtDef _ _ -> todo -- @TODO (objects)

genMemberDecl :: MemberDecl -> Result
genMemberDecl x = case x of
  DeclField _ _ -> todo -- @TODO (objects)
  FuncField _   -> todo -- @TODO (objects)



genStmts :: ArgMap -> [Stmt] -> Result
genStmts args = foldr (liftM2 (++) . genStmt args) (return [])

genStmt :: ArgMap -> Stmt -> Result
genStmt _ SEmpty = return []
genStmt args (SBlStmt (Block stmts)) = do
  newScope
  code <- genStmts args stmts
  exitScope
  return code
genStmt args (SDecl type_ items) = genStmtDecl args type_ items
genStmt args (SAss expr1 expr2) = case expr1 of
  EVar var -> do
    (reg, _) <- getVar var
    (code, res, t) <- genExpr args expr2
    let t' = typeToLLVM t
    return $ code
      ++ "store " ++ t' ++ " " ++ res ++ ", " ++ t' ++ "* " ++ reg ++ "\n"
  _        -> todo -- @TODO
genStmt _ (SIncr expr) = case expr of
  EVar var -> do
    (reg, t) <- getVar var
    return $ "add " ++ typeToLLVM t ++ "* " ++ reg ++ ", 1\n"
  _        -> todo -- @TODO
genStmt _ (SDecr expr) = case expr of
  EVar var -> do
    (reg, t) <- getVar var
    return $ "sub " ++ typeToLLVM t ++ "* " ++ reg ++ ", 1\n"
  _        -> todo -- @TODO
genStmt args (SRet expr) = do
  (code, reg, t) <- genExpr args expr
  return $ code ++ "ret " ++ typeToLLVM t ++ " " ++ reg ++ "\n"
genStmt _ SVRet =
  return "ret void\n"
genStmt args (SCond expr stmt) = genStmt args (SCondElse expr stmt SEmpty)
genStmt _ (SCondElse _ _ _) = todo -- @TODO
genStmt _ (SWhile _ _)           = todo -- @TODO
genStmt _ (SFor _ _ _ _) = todo -- @TODO (for loop)
genStmt args (SExp expr) = do
  (code, _, _) <- genExpr args expr
  return code


genStmtDecl :: ArgMap -> Type -> [Item] -> Result
genStmtDecl _ _ [] = return []
genStmtDecl args type_ (item:items) = do
  code <- case item of
    NoInit var -> genVarAlloc var type_
    Init var expr -> do
      allocCode <- genVarAlloc var type_
      initCode <- genStmt args (SAss (EVar var) expr)
      return $ allocCode ++ initCode
  rest <- genStmtDecl args type_ items
  return $ code ++ rest


genVarAlloc :: Ident -> Type -> Result
genVarAlloc var type_ = do
  reg <- allocVar var type_
  return $ reg ++ " = alloca " ++ typeToLLVM type_ ++ "\n"




genExpr :: ArgMap -> Expr -> ResultExpr
genExpr args (EVar var) = do
  (reg, t) <- getVar var
  nreg <- getNextRegisterName
  case t of
    NoTypeDef -> case M.lookup var args of
      Nothing -> return ([], [], NoTypeDef)
      Just t' -> let Ident name = var in
        return ("", "%" ++ name, t')
    _ -> let t' = typeToLLVM t in
      return (nreg ++ " = load " ++ t' ++ "* " ++ reg ++ "\n", nreg, t)
genExpr _ (ELitInt n) = return ("", show n, BaseTypeDef TInt)
genExpr _ ELitTrue = return ("", "1", BaseTypeDef TBool)
genExpr _ ELitFalse = return ("", "0", BaseTypeDef TBool)
genExpr _ (EString _) = todoExpr -- @TODO
genExpr _ (EClassNull _) = todoExpr -- @TODO (objects)
genExpr args (EApp expr exprs) = genEApp args expr exprs
genExpr _ (EArrSub _ _) = todoExpr -- @TODO (arrays)
genExpr _ (EMember _ _) = todoExpr -- @TODO (objects)
genExpr args (Neg expr) = genUnaryOp args expr NegOp
genExpr args (Not expr) = genUnaryOp args expr NotOp
genExpr args (EMul expr1 mulop expr2) = genBinaryOp args expr1 expr2 (MulOp mulop)
genExpr args (EAdd expr1 addop expr2) = genBinaryOp args expr1 expr2 (AddOp addop)
genExpr args (ERel expr1 relop expr2) = genBinaryOp args expr1 expr2 (RelOp relop)
genExpr args (EAnd expr1 expr2) = genBinaryOp args expr1 expr2 LogOp
genExpr args (EOr expr1 expr2) = genBinaryOp args expr1 expr2 LogOp
genExpr _ (ENewClass _) = todoExpr -- @TODO (objects)
genExpr _ (ENewArray _ _) = todoExpr -- @TODO (arrays)


genEApp :: ArgMap -> Expr -> [Expr] -> ResultExpr
genEApp args (EVar ident) exprs = do
  (args', argsCode) <- genArgs exprs
  retType <- getFunType ident
  reg <- getNextRegisterName
  let Ident fname = ident
      prepArgs = intercalate ", " args'
  let callCode = reg ++ " = call " ++ typeToLLVM retType ++ " @" ++ fname
                 ++ "(" ++ prepArgs ++ ")\n"
  return (argsCode ++ callCode, reg, retType)
  where
    genArgs []           = return ([], [])
    genArgs (expr:args') = do
      (code, res, t) <- genExpr args expr
      (a, c) <- genArgs args'
      return ((typeToLLVM t ++ " " ++ res) : a, code ++ c)
genEApp _ _ _ = todoExpr -- @TODO


genUnaryOp :: ArgMap -> Expr -> UnaryOp -> ResultExpr
genUnaryOp _ _ _ = todoExpr -- @TODO

genBinaryOp :: ArgMap -> Expr -> Expr -> BinOp -> ResultExpr
genBinaryOp _ _ _ (RelOp _) = todoExpr -- @TODO
genBinaryOp args expr1 expr2 op = do
  (lhsCode, res1, t1) <- genExpr args expr1
  (rhsCode, res2, _) <- genExpr args expr2
  reg <- getNextRegisterName
  case t1 of
    BaseTypeDef TInt ->
      let opCode = reg ++ " = " ++ opToLLVM (BinOp op) ++ " i32 "
                   ++ res1 ++ ", " ++ res2 ++ "\n" in
        return (lhsCode ++ rhsCode ++ opCode, reg, BaseTypeDef TInt)
    _ -> todoExpr -- @TODO



opToLLVM :: Op -> String
opToLLVM (UnaryOp _) = notImplemented
opToLLVM (BinOp op) = case op of
  AddOp OpPlus  -> "add"
  AddOp OpMinus -> "sub"
  MulOp OpTimes -> "mul"
  MulOp OpDiv   -> "sdiv"
  _             -> notImplemented


typeToLLVM :: Type -> String
typeToLLVM (BaseTypeDef TVoid) = "void"
typeToLLVM (BaseTypeDef TBool) = "i1"
typeToLLVM (BaseTypeDef TInt)  = "i32"
typeToLLVM _                   = notImplemented


notImplemented :: String
notImplemented = "NotImplemented"

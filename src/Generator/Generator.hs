module Generator.Generator where

import           Control.Monad         (liftM2)
import           Data.List             (intercalate)

import           AbsLatte
import           Generator.Environment

type Result = EnvState Code
type ResultExpr = EnvState (Code, String, Type)

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
genProgram (Program topdefs) = liftM2 (++) genStdLib (genTopDefs topdefs)


genTopDefs :: [TopDef] -> Result
genTopDefs = foldr (liftM2 (++) . genTopDef) (return [])

genTopDef :: TopDef -> Result
genTopDef (TopDefFunc funcdef)              = genFuncDef funcdef
genTopDef (TopDefClass classhead membdecls) = genClassDef classhead membdecls


genFuncDef :: FuncDef -> Result
genFuncDef (FuncDef type_ ident@(Ident f) fargs block) = do
  saveFunType ident type_
  code <- genStmt (SBlStmt block)
  return $ "define " ++ typeToLLVM type_ ++ " @" ++ f ++ "() {\n"
    ++ code ++ "}\n\n"


genFArg :: FArg -> Result
genFArg x = case x of
  FArg type_ ident -> todo -- @TODO


genClassDef :: ClassHead -> [MemberDecl] -> Result
genClassDef classhead memberdecls = todo -- @TODO (objects)

genClassHead :: ClassHead -> Result
genClassHead x = case x of
  ClassHeadDef ident              -> todo -- @TODO
  ClassHeadExtDef ident classtype -> todo -- @TODO

genMemberDecl :: MemberDecl -> Result
genMemberDecl x = case x of
  DeclField type_ idents -> todo -- @TODO
  FuncField funcdef      -> todo -- @TODO



genStmts :: [Stmt] -> Result
genStmts = foldr (liftM2 (++) . genStmt) (return [])

genStmt :: Stmt -> Result
genStmt SEmpty = return []
genStmt (SBlStmt (Block stmts)) = do
  newScope
  code <- genStmts stmts
  exitScope
  return code
genStmt (SDecl type_ items) = genStmtDecl type_ items
genStmt (SAss expr1 expr2) = case expr1 of
  EVar var -> do
    (reg, _) <- getVar var
    (code, res, t) <- genExpr expr2
    let t' = typeToLLVM t
    return $ code
      ++ "store " ++ t' ++ " " ++ res ++ ", " ++ t' ++ "* " ++ reg ++ "\n"
  _        -> todo -- @TODO
genStmt (SIncr expr) = case expr of
  EVar var -> do
    (reg, t) <- getVar var
    return $ "add " ++ typeToLLVM t ++ "* " ++ reg ++ ", 1\n"
  _        -> todo -- @TODO
genStmt (SDecr expr) = case expr of
  EVar var -> do
    (reg, t) <- getVar var
    return $ "sub " ++ typeToLLVM t ++ "* " ++ reg ++ ", 1\n"
  _        -> todo -- @TODO
genStmt (SRet expr) = do
  (code, reg, t) <- genExpr expr
  return $ code ++ "ret " ++ typeToLLVM t ++ " " ++ reg ++ "\n"
genStmt SVRet =
  return "ret void\n"
genStmt (SCond expr stmt)            = genStmt (SCondElse expr stmt SEmpty)
genStmt (SCondElse expr stmt1 stmt2) = todo -- @TODO
genStmt (SWhile expr stmt)           = todo -- @TODO
genStmt (SFor type_ ident expr stmt) = todo -- @TODO (extension)
genStmt (SExp expr) = do
  (code, _, _) <- genExpr expr
  return code


genStmtDecl :: Type -> [Item] -> Result
genStmtDecl _ [] = return []
genStmtDecl type_ (item:items) = do
  code <- case item of
    NoInit var -> genVarAlloc var type_
    Init var expr -> do
      allocCode <- genVarAlloc var type_
      initCode <- genStmt (SAss (EVar var) expr)
      return $ allocCode ++ initCode
  rest <- genStmtDecl type_ items
  return $ code ++ rest


genVarAlloc :: Ident -> Type -> Result
genVarAlloc var type_ = do
  reg <- allocVar var type_
  return $ reg ++ " = alloca " ++ typeToLLVM type_ ++ "\n"




genExpr :: Expr -> ResultExpr
genExpr (EVar var) = do
  (reg, t) <- getVar var
  nextReg <- getNextRegisterName
  let t' = typeToLLVM t
  return (nextReg ++ " = load " ++ t' ++ "* " ++ reg ++ "\n", nextReg, t)
genExpr (ELitInt n) = return ("", show n, BaseTypeDef TInt)
genExpr ELitTrue = return ("", "1", BaseTypeDef TBool)
genExpr ELitFalse = return ("", "0", BaseTypeDef TBool)
genExpr (EString string) = todoExpr -- @TODO
genExpr (EClassNull classtype) = todoExpr -- @TODO (objects)
genExpr (EApp expr exprs) = genEApp expr exprs
genExpr (EArrSub expr1 expr2) = todoExpr -- @TODO (arrays)
genExpr (EMember expr ident) = todoExpr -- @TODO (objects)
genExpr (Neg expr) = genUnaryOp expr NegOp
genExpr (Not expr) = genUnaryOp expr NotOp
genExpr (EMul expr1 mulop expr2) = genBinaryOp expr1 expr2 (MulOp mulop)
genExpr (EAdd expr1 addop expr2) = genBinaryOp expr1 expr2 (AddOp addop)
genExpr (ERel expr1 relop expr2) = genBinaryOp expr1 expr2 (RelOp relop)
genExpr (EAnd expr1 expr2) = genBinaryOp expr1 expr2 LogOp
genExpr (EOr expr1 expr2) = genBinaryOp expr1 expr2 LogOp
genExpr (ENewClass classtype) = todoExpr -- @TODO (objects)
genExpr (ENewArray type_ expr) = todoExpr -- @TODO (arrays)


genEApp :: Expr -> [Expr] -> ResultExpr
genEApp (EVar ident) exprs = do
  (args, argsCode) <- genArgs exprs
  retType <- getFunType ident
  reg <- getNextRegisterName
  let Ident fname = ident
      prepArgs = intercalate ", " args
  let callCode = reg ++ " = call " ++ typeToLLVM retType ++ " @" ++ fname
                 ++ "(" ++ prepArgs ++ ")\n"
  return (argsCode ++ callCode, reg, retType)
  where
    genArgs []           = return ([], [])
    genArgs (expr:args') = do
      (code, res, t) <- genExpr expr
      (a, c) <- genArgs args'
      return ((typeToLLVM t ++ " " ++ res) : a, code ++ c)

genEApp _ _                = todoExpr -- @TODO


genAddOp :: AddOp -> ResultExpr
genAddOp x = case x of
  OpPlus  -> todoExpr -- @TODO
  OpMinus -> todoExpr -- @TODO

genMulOp :: MulOp -> ResultExpr
genMulOp x = case x of
  OpTimes -> todoExpr -- @TODO
  OpDiv   -> todoExpr -- @TODO
  OpMod   -> todoExpr -- @TODO

genRelOp :: RelOp -> ResultExpr
genRelOp x = case x of
  OpLTH -> todoExpr -- @TODO
  OpLE  -> todoExpr -- @TODO
  OpGTH -> todoExpr -- @TODO
  OpGE  -> todoExpr -- @TODO
  OpEQU -> todoExpr -- @TODO
  OpNE  -> todoExpr -- @TODO

genUnaryOp :: Expr -> UnaryOp -> ResultExpr
genUnaryOp expr op = todoExpr -- @TODO

genBinaryOp :: Expr -> Expr -> BinOp -> ResultExpr
genBinaryOp _ _ (RelOp _) = todoExpr -- @TODO
genBinaryOp expr1 expr2 op = do
  (lhsCode, res1, t1) <- genExpr expr1
  (rhsCode, res2, t2) <- genExpr expr2
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

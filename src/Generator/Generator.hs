module Generator.Generator where

import           AbsLatte
import           Generator.Environment

type Result = EnvState Code
type ResultExpr = EnvState (Code, String, Type)

todo :: Result
todo = return []

todoExpr :: ResultExpr
todoExpr = return ([], [], NoTypeDef)

-- @TODO remove?
genIdent :: Ident -> Result
genIdent x = case x of
  Ident string -> todo -- @TODO


genProgram :: Program -> Result
genProgram (Program topdefs) = genTopDefs topdefs


genTopDefs :: [TopDef] -> Result
genTopDefs = foldr ((>>) . genTopDef) (return [])

genTopDef :: TopDef -> Result
genTopDef (TopDefFunc funcdef)              = genFuncDef funcdef
genTopDef (TopDefClass classhead membdecls) = genClassDef classhead membdecls


genFuncDef :: FuncDef -> Result
genFuncDef (FuncDef type_ (Ident ident) fargs block) = do
  code <- genStmt 1 (SBlStmt block)
  return $ "define " ++ toLLVMType type_ ++ " @" ++ ident ++ "() {\n"
    ++ code ++ "}\n"


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



genStmts :: Integer -> [Stmt] -> Result
genStmts indent = foldr ((>>) . genStmt indent) (return [])


genStmt :: Integer -> Stmt -> Result
genStmt indent SEmpty = return []
genStmt indent (SBlStmt (Block stmts)) = genStmts indent stmts
genStmt indent (SDecl type_ items) = todo -- @TODO
genStmt indent (SAss expr1 expr2) = case expr1 of
  EVar var -> do
    (reg, _) <- getVar var
    (code, res, t) <- genExpr expr2
    let t' = toLLVMType t
    return $ addIndent indent ++ code
      ++ "store " ++ t' ++ " " ++ res ++ ", " ++ t' ++ "* " ++ reg ++ "\n"
  _        -> todo -- @TODO
genStmt indent (SIncr expr) = case expr of
  EVar var -> do
    (reg, t) <- getVar var
    let t' = toLLVMType t
    return $ addIndent indent ++ "add " ++ t' ++ "* " ++ reg ++ ", 1\n"
  _        -> todo -- @TODO
genStmt indent (SDecr expr) = case expr of
  EVar var -> do
    (reg, t) <- getVar var
    let t' = toLLVMType t
    return $ addIndent indent ++ "sub " ++ t' ++ "* " ++ reg ++ ", 1\n"
  _        -> todo -- @TODO
genStmt indent (SRet expr) = do
  (code, reg, t) <- genExpr expr
  let t' = toLLVMType t
  return $ addIndent indent ++ code
    ++ "ret " ++ t' ++ " " ++ reg ++ "\n"
genStmt indent SVRet =
  return $ addIndent indent ++ "ret void\n"
genStmt indent (SCond expr stmt)            = genStmt indent (SCondElse expr stmt SEmpty)
genStmt indent (SCondElse expr stmt1 stmt2) = todo -- @TODO
genStmt indent (SWhile expr stmt)           = todo -- @TODO
genStmt indent (SFor type_ ident expr stmt) = todo -- @TODO (extension)
genStmt indent (SExp expr) = do
  (code, _, _) <- genExpr expr
  return $ addIndent indent ++ code




genExpr :: Expr -> ResultExpr
genExpr (EVar var) = do
  (reg, t) <- getVar var
  resultReg <- getNextRegister
  let res = "%" ++ show resultReg
      t' = toLLVMType t
  return (res ++ " = load " ++ t' ++ "* " ++ reg, res, t)
genExpr (ELitInt n) = return ("", show n, BaseTypeDef TInt)
genExpr ELitTrue = return ("", "1", BaseTypeDef TBool)
genExpr ELitFalse = return ("", "0", BaseTypeDef TBool)
genExpr (EString string) = todoExpr -- @TODO
genExpr (EClassNull classtype) = todoExpr -- @TODO (objects)
genExpr (EApp expr exprs) = todoExpr -- @TODO
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
genBinaryOp expr1 expr2 op = todoExpr -- @TODO





toLLVMType :: Type -> String
toLLVMType (BaseTypeDef TBool) = "i1"
toLLVMType (BaseTypeDef TInt)  = "i32"
toLLVMType _                   = "Type not implemented"

addIndent :: Integer -> String
addIndent 0 = ""
addIndent n = "  " ++ addIndent (n - 1)

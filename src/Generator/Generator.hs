module Generator.Generator where

import           Control.Monad         (liftM2)
import           Data.Char             (isAlphaNum, ord)
import           Data.List             (intercalate)
import qualified Data.Map              as M
import           Text.Printf           (printf)

import           AbsLatte
import           Generator.Environment

type Result = EnvState Code
type ResultExpr = EnvState (Code, String, Type)

type ArgMap = M.Map Ident Type

data AndOr = And | Or


todo :: Result
todo = return $ printf "  %s\n" notImplemented

todoExpr :: ResultExpr
todoExpr = return (printf "  %s\n" notImplemented, [], NoTypeDef)


genStdLib :: Result
genStdLib = do
  saveFunType (Ident "printInt") (BaseTypeDef TVoid)
  saveFunType (Ident "printString") (BaseTypeDef TVoid)
  saveFunType (Ident "readInt") (BaseTypeDef TInt)
  saveFunType (Ident "readString") (BaseTypeDef TStr)
  genStdLibDecl

genStdLibDecl :: Result
genStdLibDecl = return
  "declare i8* @concat(i8*, i8*)\n\
  \declare i32 @strcmp(i8*, i8*)\n\
  \declare void @printInt(i32)\n\
  \declare void @printString(i8*)\n\
  \declare i32 @readInt()\n\
  \declare i8* @readString()\n\n\
  \@default_str = internal constant [1 x i8] c\"\\00\"\n\n\n"


saveStrings :: Result
saveStrings = do
  str <- getStringDefs
  let defs = intercalate [] str
  return $ defs ++ "\n\n"


genProgram :: Program -> Result
genProgram (Program topdefs) = do
  stdLib <- genStdLib
  saveTopDefs topdefs
  code <- genTopDefs topdefs
  strDefs <- saveStrings
  return $ stdLib ++ strDefs ++ code


saveTopDefs :: [TopDef] -> EnvState ()
saveTopDefs = foldr ((>>) . saveTopDef) (return ())

saveTopDef :: TopDef -> EnvState ()
saveTopDef (TopDefFunc (FuncDef type_ ident _ _)) = saveFunType ident type_
saveTopDef (TopDefClass _ _)                      = return () -- @TODO (objects)

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
      t' = typeToLLVM type_
  return $ printf "define %s @%s(%s) {\n%s%s}\n\n" t' f args code (ret code)
  where
    argMap []                 = M.empty
    argMap (FArg t i : args') = M.insert i t $ argMap args'

    argsToLLVM args' = intercalate ", " $ argsToLLVM' args'
      where
        argsToLLVM' [] = []
        argsToLLVM' (FArg t (Ident x) : args'') =
          (typeToLLVM t ++ " %" ++ x) : argsToLLVM' args''

    -- @TODO change this code
    ret c = case (last. lines) ('\n' : c) of
      ' ':' ':'r':'e':'t':_ -> ""
      _ -> case type_ of
        BaseTypeDef TVoid -> "  ret void\n"
        _ -> printf "  ret %s 0\n" $ typeToLLVM type_


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
-- genStmts args = foldr (liftM2 (++) . genStmt args) (return [])
genStmts _ [] = return []
genStmts args (stmt:stmts)= case stmt of
  SRet _ -> genStmt args stmt
  SVRet  -> genStmt args stmt
  _      -> liftM2 (++) (genStmt args stmt) (genStmts args stmts)

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
    return $ code ++ printf "  store %s %s, %s* %s\n" t' res t' reg
  _ -> todo -- @TODO
genStmt args (SIncr expr) = case expr of
  EVar _ -> genStmt args $ SAss expr $ EAdd expr OpPlus $ ELitInt 1
  _      -> todo -- @TODO (extension)
genStmt args (SDecr expr) = case expr of
  EVar _ -> genStmt args $ SAss expr $ EAdd expr OpMinus $ ELitInt 1
  _      -> todo -- @TODO (extension)
genStmt args (SRet expr) = do
  (code, reg, t) <- genExpr args expr
  let t' = typeToLLVM t
  return $ code ++ printf "  ret %s %s\n" t' reg
genStmt _ SVRet =
  return "  ret void\n"
genStmt args (SCond expr stmt) = genStmt args (SCondElse expr stmt SEmpty)
genStmt args (SCondElse expr stmt1 stmt2) = case expr of
  ELitTrue  -> genStmt args stmt1
  ELitFalse -> genStmt args stmt2
  _ -> do
  (condCode, reg, _) <- genExpr args expr
  lTrue <- getNewLabel -- if true label
  lFalse <- getNewLabel -- if false label
  lEnd <- getNewLabel -- end label
  sTrue <- genStmt args stmt1 -- if true stmt
  sFalse <- genStmt args stmt2 -- if false stmt
  return $ condCode ++ toLLVMCondJump reg lTrue lFalse
    ++ toLLVMLabel lTrue ++ sTrue ++ toLLVMJump lEnd
    ++ toLLVMLabel lFalse ++ sFalse ++ toLLVMJump lEnd
    ++ toLLVMLabel lEnd
genStmt args (SWhile expr stmt) = do
  (condCode, reg, _) <- genExpr args expr
  loopCode <- genStmt args stmt
  lCond <- getNewLabel
  lLoop <- getNewLabel
  lEnd <- getNewLabel
  return $ toLLVMJump lCond
    ++ toLLVMLabel lCond ++ condCode ++ toLLVMCondJump reg lLoop lEnd
    ++ toLLVMLabel lLoop ++ loopCode ++ toLLVMJump lCond
    ++ toLLVMLabel lEnd
genStmt _ (SFor _ _ _ _) = todo -- @TODO (for loop)
genStmt args (SExp expr) = do
  (code, _, _) <- genExpr args expr
  return code


genStmtDecl :: ArgMap -> Type -> [Item] -> Result
genStmtDecl _ _ [] = return []
genStmtDecl args type_ (item:items) = do
  let t' = typeToLLVM type_
      allocCode = printf "  %s = alloca %s\n"
  code <- case item of
    NoInit var -> do
      (defValCode, defVal) <- defaultInit
      res <- allocVar var type_
      let initCode = printf "  store %s %s, %s* %s\n" t' defVal t' res
      return $ allocCode res t' ++ defValCode ++ initCode
      where
        defaultInit = case type_ of
          BaseTypeDef TStr -> do
            reg <- getNewRegisterName
            let bitcast = "  %s = bitcast [1 x i8]* @default_str to i8*\n"
            return (printf bitcast reg, reg)
          _ -> return ("", "0")
    Init var expr -> do
      (exprCode, reg, _) <- genExpr args expr
      res <- allocVar var type_
      let initCode = printf "  store %s %s, %s* %s\n" t' reg t' res
      return $ exprCode ++ allocCode res t' ++ initCode
  rest <- genStmtDecl args type_ items
  return $ code ++ rest




genExpr :: ArgMap -> Expr -> ResultExpr
genExpr args (EVar var) = do
  (reg, t) <- getVar var
  nreg <- getNewRegisterName
  case t of
    NoTypeDef -> case M.lookup var args of
      Nothing -> return ([], [], NoTypeDef)
      Just t' -> let Ident name = var in
        return ("", "%" ++ name, t')
    _ ->
      let t' = typeToLLVM t
          code = printf "  %s = load %s, %s* %s\n" nreg t' t' reg in
      return (code, nreg, t)
genExpr _ (ELitInt n) = return ("", show n, BaseTypeDef TInt)
genExpr _ ELitTrue = return ("", "1", BaseTypeDef TBool)
genExpr _ ELitFalse = return ("", "0", BaseTypeDef TBool)
genExpr _ (EString str) = do
  strReg <- getNewRegister
  res <- getNewRegisterName
  let strName = "@str" ++ show strReg
      strLen = (toInteger . length) str
  newString $ llvmStrDef strName (length str + 1) (toASCII str)
  return (llvmStrBitcast res (strLen + 1) strName, res, BaseTypeDef TStr)
  where
    llvmStrDef = printf "%s = internal constant [%d x i8] c\"%s\\00\"\n"
    llvmStrBitcast = printf "  %s = bitcast [%d x i8]* %s to i8*\n"

    toASCII [] = []
    toASCII (c:cc) = (if isAlphaNum c
      then [c]
      else printf "\\%02X" (ord c)) ++ cc

genExpr _ (EClassNull _) = todoExpr -- @TODO (objects)
genExpr args (EApp expr exprs) = genEApp args expr exprs
genExpr _ (EArrSub _ _) = todoExpr -- @TODO (arrays)
genExpr _ (EMember _ _) = todoExpr -- @TODO (objects)
genExpr args (Neg expr) = do
  (code, res, t) <- genExpr args expr
  case expr of
    ELitInt _ -> return ("", '-':res, t)
    _ -> do
      reg <- getNewRegisterName
      let code' = code ++ printf "  %s = mul i32 %s, -1\n" reg res
      return (code', reg, t)
genExpr args (Not expr) = case expr of
  ELitTrue  -> return ("", "false", BaseTypeDef TBool)
  ELitFalse -> return ("", "true", BaseTypeDef TBool)
  _         -> do
    (code, res, t) <- genExpr args expr
    reg <- getNewRegisterName
    let code' = code ++ printf "  %s = xor i1 %s, 1\n" reg res
    return (code', reg, t)
genExpr args (EMul expr1 mulop expr2) = genBinaryOp args expr1 expr2 (MulOp mulop)
genExpr args (EAdd expr1 addop expr2) = genBinaryOp args expr1 expr2 (AddOp addop)
genExpr args (ERel expr1 relop expr2) = genBinaryOp args expr1 expr2 (RelOp relop)
genExpr args (EAnd expr1 expr2) = genEAndOr args And expr1 expr2
genExpr args (EOr  expr1 expr2) = genEAndOr args Or expr1 expr2
genExpr _ (ENewClass _) = todoExpr -- @TODO (objects)
genExpr _ (ENewArray _ _) = todoExpr -- @TODO (arrays)


genEApp :: ArgMap -> Expr -> [Expr] -> ResultExpr
genEApp args (EVar ident) exprs = do
  (args', argsCode) <- genArgs exprs
  retType <- getFunType ident
  reg <- getNewRegisterName
  let Ident fname = ident
      prepArgs = intercalate ", " args'
      t' = typeToLLVM retType
      callCode = if retType /= BaseTypeDef TVoid
        then printf "  %s = call %s @%s(%s)\n" reg t' fname prepArgs
        else printf "  call %s @%s(%s)\n" t' fname prepArgs
  return (argsCode ++ callCode, reg, retType)
  where
    genArgs []           = return ([], [])
    genArgs (expr:args') = do
      (code, res, t) <- genExpr args expr
      (a, c) <- genArgs args'
      return ((typeToLLVM t ++ " " ++ res) : a, code ++ c)
genEApp _ _ _ = todoExpr -- @TODO


genEAndOr :: ArgMap -> AndOr -> Expr -> Expr -> ResultExpr
genEAndOr args andor expr1 expr2 = do
  (c1, r1, _) <- genExpr args expr1
  (c2, r2, _) <- genExpr args expr2
  l2 <- getNewLabel
  lTrue <- getNewLabel
  lFalse <- getNewLabel
  lEnd <- getNewLabel
  reg <- getNewRegisterName
  let condJump = case andor of
        And -> toLLVMCondJump r1 l2 lFalse
        Or  -> toLLVMCondJump r1 lTrue l2
      code = c1 ++ condJump
        ++ toLLVMLabel l2 ++ c2 ++ toLLVMCondJump r2 lTrue lFalse
        ++ toLLVMLabel lTrue ++ toLLVMJump lEnd
        ++ toLLVMLabel lFalse ++ toLLVMJump lEnd
        ++ toLLVMLabel lEnd
        ++ printf "  %s = phi i1 [ 1, %%%s ], [ 0, %%%s ]\n" reg lTrue lFalse
  return (code, reg, BaseTypeDef TBool)


genBinaryOp :: ArgMap -> Expr -> Expr -> BinOp -> ResultExpr
genBinaryOp args expr1 expr2 op = do
  (lhsCode, res1, t1) <- genExpr args expr1
  (rhsCode, res2, _) <- genExpr args expr2
  reg <- getNewRegisterName
  case t1 of
    BaseTypeDef TStr -> case op of
      AddOp OpPlus ->
        let code = lhsCode ++ rhsCode ++ llvmConcatStr reg res1 res2 in
          return (code, reg, t1)
      _ -> do
        subReg <- getNewRegisterName
        let cmpStr = llvmStrCmp subReg res1 res2
            eqStr = (if op == RelOp OpEQU then llvmEq else llvmNe) reg subReg
            code = lhsCode ++ rhsCode ++ cmpStr ++ eqStr
        return (code, reg, BaseTypeDef TBool)
      where
        llvmConcatStr = printf "  %s = call i8* @concat(i8* %s, i8* %s)\n"
        llvmStrCmp = printf "  %s = call i32 @strcmp(i8* %s, i8* %s)\n"
        llvmEq = printf "  %s = icmp eq i32 %s, 0\n"
        llvmNe = printf "  %s = icmp ne i32 %s, 0\n"
    _ -> do
      let t' = typeToLLVM t1
          opLLVM = opToLLVM (BinOp op)
          opCode = printf "  %s = %s %s %s, %s\n" reg opLLVM t' res1 res2
          resultType = case op of
            RelOp _ -> BaseTypeDef TBool
            _       -> t1
      return (lhsCode ++ rhsCode ++ opCode, reg, resultType)



opToLLVM :: Op -> String
opToLLVM (UnaryOp _) = notImplemented
opToLLVM (BinOp op) = case op of
  AddOp OpPlus  -> "add"
  AddOp OpMinus -> "sub"
  MulOp OpTimes -> "mul"
  MulOp OpDiv   -> "sdiv"
  MulOp OpMod   -> "srem"
  RelOp relOp   -> "icmp " ++ case relOp of
    OpLTH -> "slt"
    OpLE  -> "sle"
    OpGTH -> "sgt"
    OpGE  -> "sge"
    OpEQU -> "eq"
    OpNE  -> "ne"
  _ -> notImplemented


typeToLLVM :: Type -> String
typeToLLVM (BaseTypeDef TVoid)     = "void"
typeToLLVM (BaseTypeDef TBool)     = "i1"
typeToLLVM (BaseTypeDef TInt)      = "i32"
typeToLLVM (BaseTypeDef TStr)      = "i8*"
typeToLLVM _ = notImplemented


toLLVMLabel :: String -> String
toLLVMLabel = printf "%s:\n"

toLLVMCondJump :: String -> String -> String -> String
toLLVMCondJump = printf "  br i1 %s, label %%%s, label %%%s\n"

toLLVMJump :: String -> String
toLLVMJump = printf "  br label %%%s\n"


notImplemented :: String
notImplemented = "NotImplemented"

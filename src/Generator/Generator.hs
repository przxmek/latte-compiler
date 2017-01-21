module Generator.Generator where

import           AbsLatte
import           Generator.Environment

type Result = EnvState Code

todo :: EnvState Code
todo = return []

-- @TODO remove?
genIdent :: Ident -> Result
genIdent x = case x of
  Ident string -> todo -- @TODO


genProgram :: Program -> Result
genProgram (Program topdefs) = genTopDefs topdefs


genTopDefs :: [TopDef] -> Result
genTopDefs = foldr ((>>) . genTopDef) (return [])

genTopDef :: TopDef -> Result
genTopDef x = case x of
  TopDefFunc funcdef                -> todo -- @TODO
  TopDefClass classhead memberdecls -> todo -- @TODO

genFuncDef :: FuncDef -> Result
genFuncDef x = case x of
  FuncDef type_ ident fargs block -> todo -- @TODO

genFArg :: FArg -> Result
genFArg x = case x of
  FArg type_ ident -> todo -- @TODO

genClassHead :: ClassHead -> Result
genClassHead x = case x of
  ClassHeadDef ident              -> todo -- @TODO
  ClassHeadExtDef ident classtype -> todo -- @TODO

genMemberDecl :: MemberDecl -> Result
genMemberDecl x = case x of
  DeclField type_ idents -> todo -- @TODO
  FuncField funcdef      -> todo -- @TODO

genBlock :: Block -> Result
genBlock x = case x of
  Block stmts -> todo -- @TODO


genStmt :: Stmt -> Result
genStmt  SEmpty                      = todo -- @TODO
genStmt (SBlStmt block)              = todo -- @TODO
genStmt (SDecl type_ items)          = todo -- @TODO
genStmt (SAss expr1 expr2)           = todo -- @TODO
genStmt (SIncr expr)                 = todo -- @TODO
genStmt (SDecr expr)                 = todo -- @TODO
genStmt (SRet expr)                  = todo -- @TODO
genStmt  SVRet                       = todo -- @TODO
genStmt (SCond expr stmt)            = genStmt (SCondElse expr stmt SEmpty)
genStmt (SCondElse expr stmt1 stmt2) = todo -- @TODO
genStmt (SWhile expr stmt)           = todo -- @TODO
genStmt (SFor type_ ident expr stmt) = todo -- @TODO
genStmt (SExp expr)                  = todo -- @TODO


genItem :: Item -> Result
genItem x = case x of
  NoInit ident    -> todo -- @TODO
  Init ident expr -> todo -- @TODO

genType :: Type -> Result
genType x = case x of
  BaseTypeDef basetype   -> todo -- @TODO
  ArrayTypeDef arraytype -> todo -- @TODO
  ClassTypeDef classtype -> todo -- @TODO
  FuncTypeDef functype   -> todo -- @TODO

genBaseType :: BaseType -> Result
genBaseType x = case x of
  TInt  -> todo -- @TODO
  TStr  -> todo -- @TODO
  TBool -> todo -- @TODO
  TVoid -> todo -- @TODO

genArrayType :: ArrayType -> Result
genArrayType x = case x of
  TArray type_ -> todo -- @TODO

genClassType :: ClassType -> Result
genClassType x = case x of
  TClass ident -> todo -- @TODO


genExpr :: Expr -> Result
genExpr (EVar ident)             = todo -- @TODO
genExpr (ELitInt integer)        = todo -- @TODO
genExpr  ELitTrue                = todo -- @TODO
genExpr  ELitFalse               = todo -- @TODO
genExpr (EString string)         = todo -- @TODO
genExpr (EClassNull classtype)   = todo -- @TODO
genExpr (EApp expr exprs)        = todo -- @TODO
genExpr (EArrSub expr1 expr2)    = todo -- @TODO
genExpr (EMember expr ident)     = todo -- @TODO
genExpr (Neg expr)               = genUnaryOp expr NegOp
genExpr (Not expr)               = genUnaryOp expr NotOp
genExpr (EMul expr1 mulop expr2) = genBinaryOp expr1 expr2 (MulOp mulop)
genExpr (EAdd expr1 addop expr2) = genBinaryOp expr1 expr2 (AddOp addop)
genExpr (ERel expr1 relop expr2) = genBinaryOp expr1 expr2 (RelOp relop)
genExpr (EAnd expr1 expr2)       = genBinaryOp expr1 expr2 LogOp
genExpr (EOr expr1 expr2)        = genBinaryOp expr1 expr2 LogOp
genExpr (ENewClass classtype)    = todo -- @TODO
genExpr (ENewArray type_ expr)   = todo -- @TODO


genAddOp :: AddOp -> Result
genAddOp x = case x of
  OpPlus  -> todo -- @TODO
  OpMinus -> todo -- @TODO

genMulOp :: MulOp -> Result
genMulOp x = case x of
  OpTimes -> todo -- @TODO
  OpDiv   -> todo -- @TODO
  OpMod   -> todo -- @TODO

genRelOp :: RelOp -> Result
genRelOp x = case x of
  OpLTH -> todo -- @TODO
  OpLE  -> todo -- @TODO
  OpGTH -> todo -- @TODO
  OpGE  -> todo -- @TODO
  OpEQU -> todo -- @TODO
  OpNE  -> todo -- @TODO

genUnaryOp :: Expr -> UnaryOp -> Result
genUnaryOp expr op = todo -- @TODO

genBinaryOp :: Expr -> Expr -> BinOp -> Result
genBinaryOp expr1 expr2 op = todo -- @TODO

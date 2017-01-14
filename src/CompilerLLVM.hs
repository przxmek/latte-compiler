module CompilerLLVM where


import Control.Monad.State
import qualified Data.Set as Set

import Parser.AbsInstant


type Result      = StateT Env IO ProgramTxt
type ResultExp   = StateT Env IO (ProgramTxt, ExpRet)
type Env         = (RegisterNum, Set.Set String)
type RegisterNum = Integer
type ProgramTxt  = String
data ExpRet      = Reg Integer | Val Integer

instance Show ExpRet where
  show (Reg r) = "%" ++ show r
  show (Val i) = show i




initEnv :: Env
initEnv = (1, Set.empty)


nextReg :: StateT Env IO ExpRet
nextReg = do
  (reg, vars) <- get
  put (reg + 1, vars)
  return $ Reg reg


compile :: Program -> String -> IO String
compile program _ = do
  (mainTxt, _) <- runStateT (transProgram program) initEnv
  return (
    "@dnl = internal constant [4 x i8] c\"%d\\0a\\00\"\n\n" ++
    "declare i32 @printf(i8*, ...) nounwind\n\n" ++
    "define void @printInt(i32 %i) {\n" ++
    "entry:  %t0 = getelementptr [4 x i8], [4 x i8]* @dnl, i32 0, i32 0\n" ++
    formatInstr "call i32 (i8*, ...) @printf(i8* %t0, i32 %i)" ++
    formatInstr "ret void" ++
    "}\n\n" ++
    "define i32 @main() #0 {\n" ++ mainTxt ++ formatInstr "ret i32 0" ++ "}\n")




transProgram :: Program -> Result
transProgram (Prog stmts) = do
  results <- mapM transStmt stmts
  return $ concat results


transStmt :: Stmt -> Result
transStmt (SAss (Ident ident) expr) = do
  (expTxt, expRet) <- transExp expr
  (reg, vars) <- get
  allocTxt <- 
    if Set.member ident vars then return "" else do
      put (reg, Set.insert ident vars)
      return $ formatInstr $ "%" ++ ident ++ " = alloca i32"
  let assTxt = allocTxt ++ formatInstr ("store i32 " ++ show expRet ++ ", i32* %" ++ ident)
  return $ expTxt ++ assTxt
transStmt (SExp expr) = do
  (expTxt, expRet) <- transExp expr
  let printTxt = formatInstr $ "call void @printInt(i32 " ++ show expRet ++ ")"
  return $ expTxt ++ printTxt


transExp :: Exp -> ResultExp
transExp (ExpAdd exp1 exp2)     = transBinaryOp  "add" exp1 exp2
transExp (ExpSub exp1 exp2)     = transBinaryOp  "sub" exp1 exp2
transExp (ExpMul exp1 exp2)     = transBinaryOp  "mul" exp1 exp2
transExp (ExpDiv exp1 exp2)     = transBinaryOp "sdiv" exp1 exp2
transExp (ExpLit num)           = return ("", Val num)
transExp (ExpVar (Ident ident)) = do
  reg <- nextReg
  (_, vars) <- get
  if Set.member ident vars
    then return (formatInstr $ show reg ++ " = load i32, i32* %" ++ ident, reg)
    else error $ "Error: undefined variable `" ++ ident ++ "`" 


transBinaryOp :: String -> Exp -> Exp -> ResultExp
transBinaryOp op exp1 exp2 = do
  (e1Txt, e1Ret) <- transExp exp1
  (e2Txt, e2Ret) <- transExp exp2
  reg   <- nextReg
  let opTxt = formatInstr $ regWrap (op ++ " i32 " ++ show e1Ret ++ ", " ++ show e2Ret) reg
  return (e1Txt ++ e2Txt ++ opTxt, reg)


regWrap :: String -> ExpRet -> String
regWrap txt reg = show reg ++ " = " ++ txt


formatInstr :: String -> String
formatInstr   instr  = "    " ++ instr ++ "\n"

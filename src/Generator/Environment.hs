module Generator.Environment where

import           Control.Monad.State
import qualified Data.Map            as M

import           AbsLatte


type Register = Integer
type Code = String
type Var = (Register, Type)
type VarInfo = (String, Type)

type Store = M.Map Ident Var

type Environment = ([Store], Register)

type EnvState a = State Environment a


initEnv :: Environment
initEnv = ([M.empty], 0)

getNextRegister :: EnvState Register
getNextRegister = do
  (stores, reg) <- get
  let nextReg = reg + 1
  put (stores, nextReg)
  return nextReg

getVar :: Ident -> EnvState VarInfo
getVar var = do
  (stores, _) <- get
  lookupStores stores where
    lookupStores stores = case stores of
      [] -> return ("Var not found", NoTypeDef)
      s:ss' -> case M.lookup var s of
        Just (r, t) -> return ("%" ++ show r, t)
        Nothing     -> lookupStores ss'

allocVar :: Ident -> Type -> EnvState Register
allocVar var type_ = do
  (s:ss, _) <- get
  reg <- getNextRegister
  put (M.insert var (reg, type_) s : ss, reg)
  return reg

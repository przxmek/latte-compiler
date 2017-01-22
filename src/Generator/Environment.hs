module Generator.Environment where

import           Control.Monad.State
import qualified Data.Map            as M

import           AbsLatte


type Register = Integer
type Code = String
type Var = (String, Type)

type Store = M.Map Ident Var
type FunStore = M.Map Ident Type

type Environment = ([Store], FunStore, Register)

type EnvState a = State Environment a


initEnv :: Environment
initEnv = ([M.empty], M.empty, 0)


newScope :: EnvState ()
newScope = do
  (stores, fstore, reg) <- get
  put (M.empty : stores, fstore, reg)

exitScope :: EnvState ()
exitScope = do
  (_:stores, fstore, reg) <- get
  put (stores, fstore, reg)


getNextRegister :: EnvState Register
getNextRegister = do
  (stores, fstore, reg) <- get
  let nextReg = reg + 1
  put (stores, fstore, nextReg)
  return nextReg

getNextRegisterName :: EnvState String
getNextRegisterName = do
  reg <- getNextRegister
  return $ "%i" ++ show reg

getVar :: Ident -> EnvState Var
getVar var = do
  (stores, _, _) <- get
  lookupStores stores where
    lookupStores stores = case stores of
      [] -> return ("Var not found", NoTypeDef)
      s:ss' -> case M.lookup var s of
        Just (r, t) -> return (r, t)
        Nothing     -> lookupStores ss'

allocVar :: Ident -> Type -> EnvState String
allocVar var type_ = do
  (s:ss, fstore, oldReg) <- get
  let reg = oldReg + 1
  let reg' = "%i" ++ show reg
  put (M.insert var (reg', type_) s : ss, fstore, reg)
  return reg'

saveFunType :: Ident -> Type -> EnvState ()
saveFunType ident type_ = do
  (stores, fstore, reg) <- get
  put (stores, M.insert ident type_ fstore, reg)


getFunType :: Ident -> EnvState Type
getFunType ident = do
  (_, fstore, _) <- get
  case M.lookup ident fstore of
    Just t  -> return t
    Nothing -> return NoTypeDef

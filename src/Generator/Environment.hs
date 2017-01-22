module Generator.Environment where

import           Control.Monad.State
import qualified Data.Map            as M

import           AbsLatte


type Register = Integer
type LabelNumber = Integer
type Code = String
type Var = (String, Type)

type Store = M.Map Ident Var
type FunStore = M.Map Ident Type

type Environment = ([Store], FunStore, Register, LabelNumber)

type EnvState a = State Environment a


initEnv :: Environment
initEnv = ([M.empty], M.empty, 0, 0)


newScope :: EnvState ()
newScope = do
  (ss, fs, r, l) <- get
  put (M.empty : ss, fs, r, l)

exitScope :: EnvState ()
exitScope = do
  (_:ss, fs, r, l) <- get
  put (ss, fs, r, l)


getNewRegister :: EnvState Register
getNewRegister = do
  (ss, fs, r, l) <- get
  let nextReg = r + 1
  put (ss, fs, nextReg, l)
  return nextReg

getNewRegisterName :: EnvState String
getNewRegisterName = do
  reg <- getNewRegister
  return $ "%i" ++ show reg

getNewLabel :: EnvState String
getNewLabel = do
  (ss, fs, r, l) <- get
  let nextLabel = l + 1
  put (ss, fs, r, nextLabel)
  return $ "LABEL" ++ show nextLabel


getVar :: Ident -> EnvState Var
getVar var = do
  (ss, _, _, _) <- get
  lookupStores ss where
    lookupStores stores = case stores of
      [] -> return ("Var not found", NoTypeDef)
      s:ss' -> case M.lookup var s of
        Just (r, t) -> return (r, t)
        Nothing     -> lookupStores ss'

allocVar :: Ident -> Type -> EnvState String
allocVar var type_ = do
  (s:ss, fs, oldReg, l) <- get
  let reg = oldReg + 1
  let reg' = "%i" ++ show reg
  put (M.insert var (reg', type_) s : ss, fs, reg, l)
  return reg'

saveFunType :: Ident -> Type -> EnvState ()
saveFunType ident type_ = do
  (ss, fs, r, l) <- get
  put (ss, M.insert ident type_ fs, r, l)


getFunType :: Ident -> EnvState Type
getFunType ident = do
  (_, fs, _, _) <- get
  case M.lookup ident fs of
    Just t  -> return t
    Nothing -> return NoTypeDef

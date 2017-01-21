module Generator.Environment where

import           Control.Monad.State
import qualified Data.Map            as M

import           AbsLatte


type Register = Integer
type Code = [String]

type Store = M.Map Ident Register

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

getVarReg :: Ident -> EnvState Register
getVarReg var = do
  (stores, _) <- get
  lookupStores stores where
    lookupStores stores = case stores of
      [] -> return 0
      s:ss' -> case M.lookup var s of
        Just v  -> return v
        Nothing -> lookupStores ss'

allocVar :: Ident -> EnvState Register
allocVar var = do
  (s:ss', _) <- get
  reg <- getNextRegister
  put (M.insert var reg s : ss', reg)
  return reg

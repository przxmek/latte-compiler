module Frontend.Environment where

import           AbsLatte
import           Control.Monad.State
import qualified Data.Map            as M


data ClassExt = BaseClass | ExtClass Ident
type ClassMemberStore = M.Map Ident Type

type Store = M.Map Ident Type
type ClassStore = M.Map Ident (ClassExt, ClassMemberStore)
type ErrorMsg = String

type Environment = ([Store], ClassStore, [ErrorMsg])

type EnvState a = State Environment a


initEnv :: Environment
initEnv = ([M.empty], M.empty, [])


newScope :: EnvState ()
newScope = do
  (s, c, e) <- get
  put(M.empty : s, c, e)

exitScope :: EnvState ()
exitScope = do
  (s, c, e) <- get
  put(tail s, c, e)


appendError :: String -> EnvState ()
appendError errorMsg = do
  (s, c, e) <- get
  put(s, c, errorMsg : e)


lookupVarType :: Ident -> EnvState Type
lookupVarType ident = do
  (stores, _, _) <- get
  lookupStores ident stores where
    lookupStores ident' stores' = case stores' of
      [] -> do
        appendError $ "Variable " ++ show ident' ++ " does not exist."
        return $ BaseTypeDef TVoid
      s:ss -> case M.lookup ident' s of
        Just v  -> return v
        Nothing -> lookupStores ident' ss


newVar :: Type -> Ident -> EnvState ()
newVar newType ident = do
  (store:stores, c, e) <- get
  case M.lookup ident store of
    Just _ -> appendError $ "Variable " ++ show ident ++ " is already defined."
    Nothing ->
      put(M.insert ident newType store:stores, c, e)


newClass :: Ident -> ClassExt -> ClassMemberStore -> EnvState ()
newClass ident ext members = do
  (s, c, e) <- get
  let c' = M.insert ident (ext, members) c
  put (s, c', e)


getClassMember :: Ident -> Ident -> EnvState Type
getClassMember classIdent memberIdent = do
  (_, c, _) <- get
  case M.lookup classIdent c of
    Nothing -> do
      appendError $ "Class " ++ show classIdent ++ " does not exist."
      return $ BaseTypeDef TVoid
    Just (ext, members) -> case M.lookup memberIdent members of
      Nothing -> case ext of
        BaseClass -> do
          appendError $ "Member " ++ show memberIdent ++ " of class " ++ show classIdent ++ " does not exist."
          return $ BaseTypeDef TVoid
        ExtClass extClass -> getClassMember extClass memberIdent
      Just member -> return member

getClassMembers :: Ident -> EnvState [(Ident, Type)]
getClassMembers classIdent = do
  (_, c, _) <- get
  case M.lookup classIdent c of
    Just (classExt, members) -> case classExt of
      BaseClass -> return $ M.toList members
      ExtClass extClass -> do
        extMembers <- getClassMembers extClass
        inheritedMembers <- filterM inherited extMembers
        return $ M.toList members ++ inheritedMembers
        where
          inherited (i, _) = case M.lookup i members of
            Just _  -> return False
            Nothing -> return True
    Nothing -> do
      appendError $ "Class " ++ show classIdent ++ " does not exist."
      return []


getParentClass :: Ident -> EnvState (Maybe Ident)
getParentClass ident = do
  (_, c, _) <- get
  case M.lookup ident c of
    Nothing -> return Nothing
    Just (classExt, _) -> case classExt of
      BaseClass          -> return Nothing
      ExtClass baseClass -> return $ Just baseClass




--- Standard library functions -------------------------------------------------

printIntFuncDef :: FuncDef
printIntFuncDef = FuncDef retType ident fargs (Block []) where
  retType = BaseTypeDef TVoid
  ident = Ident "printInt"
  fargs  = [FArg (BaseTypeDef TInt) (Ident "arg1")]


printStringFuncDef :: FuncDef
printStringFuncDef = FuncDef retType ident fargs (Block []) where
  retType = BaseTypeDef TVoid
  ident = Ident "printString"
  fargs  = [FArg (BaseTypeDef TStr) (Ident "arg1")]


readIntFuncDef :: FuncDef
readIntFuncDef = FuncDef retType ident fargs (Block []) where
  retType = BaseTypeDef TInt
  ident = Ident "readInt"
  fargs  = []


readStringFuncDef :: FuncDef
readStringFuncDef = FuncDef retType ident fargs (Block []) where
  retType = BaseTypeDef TStr
  ident = Ident "readString"
  fargs  = []

-- End of standard library functions -------------------------------------------

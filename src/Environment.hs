module Environment where

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


appendError :: EnvState ()
appendError = do
  (s, c, e) <- get
  put(s, c, "error" : e)


lookupVarType :: Ident -> EnvState Type
lookupVarType ident = do
  (stores, _, _) <- get
  lookupStores ident stores where
    lookupStores ident' stores' = case stores' of
      [] -> do
        appendError
        return $ BaseTypeDef TVoid
      s:ss -> case M.lookup ident' s of
        Just v  -> return v
        Nothing -> lookupStores ident' ss


newVar :: Type -> Ident -> EnvState ()
newVar newType ident = do
  (store:stores, c, e) <- get
  case M.lookup ident store of
    Just _ -> appendError
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
      appendError
      return $ BaseTypeDef TVoid
    Just (ext, members) -> case M.lookup memberIdent members of
      Nothing -> case ext of
        BaseClass -> do
          appendError
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
      appendError
      return []

module Environment where

import           AbsLatte
import           Control.Monad.State
import qualified Data.Map            as M


data ClassExt = BaseClass | ExtClass Ident
data ClassMember = Var Type | Method Type [Type]
type ClassMemberStore = M.Map Ident ClassMember

type Store = M.Map Ident Type
type ClassStore = M.Map Ident (ClassExt, ClassMemberStore)
type ErrorMsg = String

type Environment = ([Store], ClassStore, ErrorMsg)

type EnvState a = State Environment a


getStores :: Environment -> [Store]
getStores env = let (s, _, _) = env in s

getClassStores :: Environment -> ClassStore
getClassStores env = let (_, c, _) = env in c

getErrorMsg :: Environment -> ErrorMsg
getErrorMsg env = let (_, _, e) = env in e


newScope :: EnvState ()
newScope = do
  (s, c, e) <- get
  put(M.empty : s, c, e)

exitScope :: EnvState ()
exitScope = do
  (s, c, e) <- get
  put(tail s, c, e)

appendError :: EnvState ()
appendError = return ()


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
newClass ident ext fields = do
  (s, c, e) <- get
  let c' = M.insert ident (ext, fields) c
  put (s, c', e)

getClassMember :: Ident -> Ident -> EnvState ClassMember
getClassMember classIdent memberIdent = do
  (_, c, _) <- get
  case M.lookup classIdent c of
    Nothing -> do
      appendError
      return $ Var $ BaseTypeDef TVoid
    Just (ext, members) -> case M.lookup memberIdent members of
      Nothing -> case ext of
        BaseClass -> do
          appendError
          return $ Var $ BaseTypeDef TVoid
        ExtClass extClass -> getClassMember extClass memberIdent
      Just member -> return member

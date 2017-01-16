module Environment where

import           AbsLatte
import           Control.Monad.State
import qualified Data.Map            as M

type Store = M.Map Ident Type

data ClassMember = Var Type | Method Type [Type]
type ClassStore = M.Map Ident (M.Map Ident ClassMember)

type ErrorMsg = String
type Environment = ([Store], ClassStore, ErrorMsg)
type EnvState a = State Environment a

newScope :: EnvState ()
newScope = return () -- @TODO

exitScope :: EnvState ()
exitScope = return () -- @TODO

appendError :: EnvState ()
appendError = return () -- @TODO

module AbsLatte where


newtype Ident = Ident String deriving (Eq, Ord, Show, Read)
data Program = Program [TopDef]
  deriving (Eq, Ord, Show, Read)

data TopDef
    = TopDefFunc FuncDef | TopDefClass ClassHead [MemberDecl]
  deriving (Eq, Ord, Show, Read)

data FuncDef = FuncDef Type Ident [FArg] Block
  deriving (Eq, Ord, Show, Read)

data FArg = FArg Type Ident
  deriving (Eq, Ord, Show, Read)

data ClassHead
    = ClassHeadDef Ident | ClassHeadExtDef Ident ClassType
  deriving (Eq, Ord, Show, Read)

data MemberDecl = DeclField Type [Ident] | FuncField FuncDef
  deriving (Eq, Ord, Show, Read)

data Block = Block [Stmt]
  deriving (Eq, Ord, Show, Read)

data Stmt
    = SEmpty
    | SBlStmt Block
    | SDecl Type [Item]
    | SAss Expr Expr
    | SIncr Expr
    | SDecr Expr
    | SRet Expr
    | SVRet
    | SCond Expr Stmt
    | SCondElse Expr Stmt Stmt
    | SWhile Expr Stmt
    | SFor Type Ident Expr Stmt
    | SExp Expr
  deriving (Eq, Ord, Show, Read)

data Item = NoInit Ident | Init Ident Expr
  deriving (Eq, Ord, Show, Read)

data Type
    = NoTypeDef
    | BaseTypeDef BaseType
    | FuncTypeDef FuncType
    | ArrayTypeDef ArrayType
    | ClassTypeDef ClassType
  deriving (Eq, Ord, Show, Read)

data BaseType = TInt | TStr | TBool | TVoid
  deriving (Eq, Ord, Show, Read)

data FuncType = TFunc Type [Type]
  deriving (Eq, Ord, Show, Read)

data ArrayType = TArray Type
  deriving (Eq, Ord, Show, Read)

data ClassType = TClass Ident
  deriving (Eq, Ord, Show, Read)

data Expr
    = EVar Ident
    | ELitInt Integer
    | ELitTrue
    | ELitFalse
    | EString String
    | EClassNull ClassType
    | EApp Expr [Expr]
    | EArrSub Expr Expr
    | EMember Expr Ident
    | Neg Expr
    | Not Expr
    | EMul Expr MulOp Expr
    | EAdd Expr AddOp Expr
    | ERel Expr RelOp Expr
    | EAnd Expr Expr
    | EOr Expr Expr
    | ENewClass ClassType
    | ENewArray Type Expr
  deriving (Eq, Ord, Show, Read)

data Op = UnaryOp UnaryOp | BinOp BinOp
  deriving (Eq, Ord, Show, Read)
data UnaryOp = NegOp | NotOp | ArrSubscOp
  deriving (Eq, Ord, Show, Read)
data BinOp = AddOp AddOp | MulOp MulOp | RelOp RelOp | LogOp
  deriving (Eq, Ord, Show, Read)

data AddOp = OpPlus | OpMinus
  deriving (Eq, Ord, Show, Read)

data MulOp = OpTimes | OpDiv | OpMod
  deriving (Eq, Ord, Show, Read)

data RelOp = OpLTH | OpLE | OpGTH | OpGE | OpEQU | OpNE
  deriving (Eq, Ord, Show, Read)

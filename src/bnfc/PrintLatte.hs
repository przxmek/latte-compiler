{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
module PrintLatte where

-- pretty-printer generated by the BNF converter

import AbsLatte
import Data.Char


-- the top-level printing method
printTree :: Print a => a -> String
printTree = render . prt 0

type Doc = [ShowS] -> [ShowS]

doc :: ShowS -> Doc
doc = (:)

render :: Doc -> String
render d = rend 0 (map ($ "") $ d []) "" where
  rend i ss = case ss of
    "["      :ts -> showChar '[' . rend i ts
    "("      :ts -> showChar '(' . rend i ts
    "{"      :ts -> showChar '{' . new (i+1) . rend (i+1) ts
    "}" : ";":ts -> new (i-1) . space "}" . showChar ';' . new (i-1) . rend (i-1) ts
    "}"      :ts -> new (i-1) . showChar '}' . new (i-1) . rend (i-1) ts
    ";"      :ts -> showChar ';' . new i . rend i ts
    t  : "," :ts -> showString t . space "," . rend i ts
    t  : ")" :ts -> showString t . showChar ')' . rend i ts
    t  : "]" :ts -> showString t . showChar ']' . rend i ts
    t        :ts -> space t . rend i ts
    _            -> id
  new i   = showChar '\n' . replicateS (2*i) (showChar ' ') . dropWhile isSpace
  space t = showString t . (\s -> if null s then "" else (' ':s))

parenth :: Doc -> Doc
parenth ss = doc (showChar '(') . ss . doc (showChar ')')

concatS :: [ShowS] -> ShowS
concatS = foldr (.) id

concatD :: [Doc] -> Doc
concatD = foldr (.) id

replicateS :: Int -> ShowS -> ShowS
replicateS n f = concatS (replicate n f)

-- the printer class does the job
class Print a where
  prt :: Int -> a -> Doc
  prtList :: Int -> [a] -> Doc
  prtList i = concatD . map (prt i)

instance Print a => Print [a] where
  prt = prtList

instance Print Char where
  prt _ s = doc (showChar '\'' . mkEsc '\'' s . showChar '\'')
  prtList _ s = doc (showChar '"' . concatS (map (mkEsc '"') s) . showChar '"')

mkEsc :: Char -> Char -> ShowS
mkEsc q s = case s of
  _ | s == q -> showChar '\\' . showChar s
  '\\'-> showString "\\\\"
  '\n' -> showString "\\n"
  '\t' -> showString "\\t"
  _ -> showChar s

prPrec :: Int -> Int -> Doc -> Doc
prPrec i j = if j<i then parenth else id


instance Print Integer where
  prt _ x = doc (shows x)


instance Print Double where
  prt _ x = doc (shows x)


instance Print Ident where
  prt _ (Ident i) = doc (showString ( i))
  prtList _ [x] = (concatD [prt 0 x])
  prtList _ (x:xs) = (concatD [prt 0 x, doc (showString ","), prt 0 xs])


instance Print Program where
  prt i e = case e of
    Program topdefs -> prPrec i 0 (concatD [prt 0 topdefs])

instance Print TopDef where
  prt i e = case e of
    TopDefFunc funcdef -> prPrec i 0 (concatD [prt 0 funcdef])
    TopDefClass classhead memberdecls -> prPrec i 0 (concatD [prt 0 classhead, doc (showString "{"), prt 0 memberdecls, doc (showString "}")])
  prtList _ [x] = (concatD [prt 0 x])
  prtList _ (x:xs) = (concatD [prt 0 x, prt 0 xs])
instance Print FuncDef where
  prt i e = case e of
    FuncDef type_ id fargs block -> prPrec i 0 (concatD [prt 0 type_, prt 0 id, doc (showString "("), prt 0 fargs, doc (showString ")"), prt 0 block])

instance Print FArg where
  prt i e = case e of
    FArg type_ id -> prPrec i 0 (concatD [prt 0 type_, prt 0 id])
  prtList _ [] = (concatD [])
  prtList _ [x] = (concatD [prt 0 x])
  prtList _ (x:xs) = (concatD [prt 0 x, doc (showString ","), prt 0 xs])
instance Print ClassHead where
  prt i e = case e of
    ClassHeadDef id -> prPrec i 0 (concatD [doc (showString "class"), prt 0 id])
    ClassHeadExtDef id classtype -> prPrec i 0 (concatD [doc (showString "class"), prt 0 id, doc (showString "extends"), prt 0 classtype])

instance Print MemberDecl where
  prt i e = case e of
    DeclField type_ ids -> prPrec i 0 (concatD [prt 0 type_, prt 0 ids, doc (showString ";")])
    FuncField funcdef -> prPrec i 0 (concatD [prt 0 funcdef])
  prtList _ [] = (concatD [])
  prtList _ (x:xs) = (concatD [prt 0 x, prt 0 xs])
instance Print Block where
  prt i e = case e of
    Block stmts -> prPrec i 0 (concatD [doc (showString "{"), prt 0 stmts, doc (showString "}")])

instance Print Stmt where
  prt i e = case e of
    SEmpty -> prPrec i 0 (concatD [doc (showString ";")])
    SBlStmt block -> prPrec i 0 (concatD [prt 0 block])
    SDecl type_ items -> prPrec i 0 (concatD [prt 0 type_, prt 0 items, doc (showString ";")])
    SAss expr1 expr2 -> prPrec i 0 (concatD [prt 0 expr1, doc (showString "="), prt 0 expr2, doc (showString ";")])
    SIncr expr -> prPrec i 0 (concatD [prt 0 expr, doc (showString "++"), doc (showString ";")])
    SDecr expr -> prPrec i 0 (concatD [prt 0 expr, doc (showString "--"), doc (showString ";")])
    SRet expr -> prPrec i 0 (concatD [doc (showString "return"), prt 0 expr, doc (showString ";")])
    SVRet -> prPrec i 0 (concatD [doc (showString "return"), doc (showString ";")])
    SCond expr stmt -> prPrec i 0 (concatD [doc (showString "if"), doc (showString "("), prt 0 expr, doc (showString ")"), prt 0 stmt])
    SCondElse expr stmt1 stmt2 -> prPrec i 0 (concatD [doc (showString "if"), doc (showString "("), prt 0 expr, doc (showString ")"), prt 0 stmt1, doc (showString "else"), prt 0 stmt2])
    SWhile expr stmt -> prPrec i 0 (concatD [doc (showString "while"), doc (showString "("), prt 0 expr, doc (showString ")"), prt 0 stmt])
    SFor type_ id expr stmt -> prPrec i 0 (concatD [doc (showString "for"), doc (showString "("), prt 0 type_, prt 0 id, doc (showString ":"), prt 0 expr, doc (showString ")"), prt 0 stmt])
    SExp expr -> prPrec i 0 (concatD [prt 0 expr, doc (showString ";")])
  prtList _ [] = (concatD [])
  prtList _ (x:xs) = (concatD [prt 0 x, prt 0 xs])
instance Print Item where
  prt i e = case e of
    NoInit id -> prPrec i 0 (concatD [prt 0 id])
    Init id expr -> prPrec i 0 (concatD [prt 0 id, doc (showString "="), prt 0 expr])
  prtList _ [x] = (concatD [prt 0 x])
  prtList _ (x:xs) = (concatD [prt 0 x, doc (showString ","), prt 0 xs])
instance Print Type where
  prt i e = case e of
    BaseTypeDef basetype -> prPrec i 0 (concatD [prt 0 basetype])
    ArrayTypeDef arraytype -> prPrec i 0 (concatD [prt 0 arraytype])
    ClassTypeDef classtype -> prPrec i 0 (concatD [prt 0 classtype])

instance Print BaseType where
  prt i e = case e of
    TInt -> prPrec i 0 (concatD [doc (showString "int")])
    TStr -> prPrec i 0 (concatD [doc (showString "string")])
    TBool -> prPrec i 0 (concatD [doc (showString "boolean")])
    TVoid -> prPrec i 0 (concatD [doc (showString "void")])

instance Print ArrayType where
  prt i e = case e of
    TArray type_ -> prPrec i 0 (concatD [prt 0 type_, doc (showString "[]")])

instance Print ClassType where
  prt i e = case e of
    TClass id -> prPrec i 0 (concatD [prt 0 id])

instance Print Expr where
  prt i e = case e of
    EVar id -> prPrec i 9 (concatD [prt 0 id])
    ELitInt n -> prPrec i 8 (concatD [prt 0 n])
    ELitTrue -> prPrec i 8 (concatD [doc (showString "true")])
    ELitFalse -> prPrec i 8 (concatD [doc (showString "false")])
    EString str -> prPrec i 8 (concatD [prt 0 str])
    EClassNull classtype -> prPrec i 7 (concatD [doc (showString "("), prt 0 classtype, doc (showString ")null")])
    EApp expr exprs -> prPrec i 7 (concatD [prt 6 expr, doc (showString "("), prt 0 exprs, doc (showString ")")])
    EArrSub expr1 expr2 -> prPrec i 7 (concatD [prt 6 expr1, doc (showString "["), prt 0 expr2, doc (showString "]")])
    EMember expr id -> prPrec i 7 (concatD [prt 6 expr, doc (showString "."), prt 0 id])
    Neg expr -> prPrec i 5 (concatD [doc (showString "-"), prt 6 expr])
    Not expr -> prPrec i 5 (concatD [doc (showString "!"), prt 6 expr])
    EMul expr1 mulop expr2 -> prPrec i 4 (concatD [prt 4 expr1, prt 0 mulop, prt 5 expr2])
    EAdd expr1 addop expr2 -> prPrec i 3 (concatD [prt 3 expr1, prt 0 addop, prt 4 expr2])
    ERel expr1 relop expr2 -> prPrec i 2 (concatD [prt 2 expr1, prt 0 relop, prt 3 expr2])
    EAnd expr1 expr2 -> prPrec i 1 (concatD [prt 2 expr1, doc (showString "&&"), prt 1 expr2])
    EOr expr1 expr2 -> prPrec i 0 (concatD [prt 1 expr1, doc (showString "||"), prt 0 expr2])
    ENewClass classtype -> prPrec i 0 (concatD [doc (showString "new"), prt 0 classtype])
    ENewArray type_ expr -> prPrec i 0 (concatD [doc (showString "new"), prt 0 type_, doc (showString "["), prt 0 expr, doc (showString "]")])
  prtList _ [] = (concatD [])
  prtList _ [x] = (concatD [prt 0 x])
  prtList _ (x:xs) = (concatD [prt 0 x, doc (showString ","), prt 0 xs])
instance Print AddOp where
  prt i e = case e of
    OpPlus -> prPrec i 0 (concatD [doc (showString "+")])
    OpMinus -> prPrec i 0 (concatD [doc (showString "-")])

instance Print MulOp where
  prt i e = case e of
    OpTimes -> prPrec i 0 (concatD [doc (showString "*")])
    OpDiv -> prPrec i 0 (concatD [doc (showString "/")])
    OpMod -> prPrec i 0 (concatD [doc (showString "%")])

instance Print RelOp where
  prt i e = case e of
    OpLTH -> prPrec i 0 (concatD [doc (showString "<")])
    OpLE -> prPrec i 0 (concatD [doc (showString "<=")])
    OpGTH -> prPrec i 0 (concatD [doc (showString ">")])
    OpGE -> prPrec i 0 (concatD [doc (showString ">=")])
    OpEQU -> prPrec i 0 (concatD [doc (showString "==")])
    OpNE -> prPrec i 0 (concatD [doc (showString "!=")])



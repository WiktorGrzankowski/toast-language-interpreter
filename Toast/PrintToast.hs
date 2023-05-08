-- File generated by the BNF Converter (bnfc 2.9.4).

{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
#if __GLASGOW_HASKELL__ <= 708
{-# LANGUAGE OverlappingInstances #-}
#endif

-- | Pretty-printer for Toast.

module Toast.PrintToast where

import Prelude
  ( ($), (.)
  , Bool(..), (==), (<)
  , Int, Integer, Double, (+), (-), (*)
  , String, (++)
  , ShowS, showChar, showString
  , all, elem, foldr, id, map, null, replicate, shows, span
  )
import Data.Char ( Char, isSpace )
import qualified Toast.AbsToast

-- | The top-level printing method.

printTree :: Print a => a -> String
printTree = render . prt 0

type Doc = [ShowS] -> [ShowS]

doc :: ShowS -> Doc
doc = (:)

render :: Doc -> String
render d = rend 0 False (map ($ "") $ d []) ""
  where
  rend
    :: Int        -- ^ Indentation level.
    -> Bool       -- ^ Pending indentation to be output before next character?
    -> [String]
    -> ShowS
  rend i p = \case
      "["      :ts -> char '[' . rend i False ts
      "("      :ts -> char '(' . rend i False ts
      "{"      :ts -> onNewLine i     p . showChar   '{'  . new (i+1) ts
      "}" : ";":ts -> onNewLine (i-1) p . showString "};" . new (i-1) ts
      "}"      :ts -> onNewLine (i-1) p . showChar   '}'  . new (i-1) ts
      [";"]        -> char ';'
      ";"      :ts -> char ';' . new i ts
      t  : ts@(s:_) | closingOrPunctuation s
                   -> pending . showString t . rend i False ts
      t        :ts -> pending . space t      . rend i False ts
      []           -> id
    where
    -- Output character after pending indentation.
    char :: Char -> ShowS
    char c = pending . showChar c

    -- Output pending indentation.
    pending :: ShowS
    pending = if p then indent i else id

  -- Indentation (spaces) for given indentation level.
  indent :: Int -> ShowS
  indent i = replicateS (2*i) (showChar ' ')

  -- Continue rendering in new line with new indentation.
  new :: Int -> [String] -> ShowS
  new j ts = showChar '\n' . rend j True ts

  -- Make sure we are on a fresh line.
  onNewLine :: Int -> Bool -> ShowS
  onNewLine i p = (if p then id else showChar '\n') . indent i

  -- Separate given string from following text by a space (if needed).
  space :: String -> ShowS
  space t s =
    case (all isSpace t', null spc, null rest) of
      (True , _   , True ) -> []              -- remove trailing space
      (False, _   , True ) -> t'              -- remove trailing space
      (False, True, False) -> t' ++ ' ' : s   -- add space if none
      _                    -> t' ++ s
    where
      t'          = showString t []
      (spc, rest) = span isSpace s

  closingOrPunctuation :: String -> Bool
  closingOrPunctuation [c] = c `elem` closerOrPunct
  closingOrPunctuation _   = False

  closerOrPunct :: String
  closerOrPunct = ")],;"

parenth :: Doc -> Doc
parenth ss = doc (showChar '(') . ss . doc (showChar ')')

concatS :: [ShowS] -> ShowS
concatS = foldr (.) id

concatD :: [Doc] -> Doc
concatD = foldr (.) id

replicateS :: Int -> ShowS -> ShowS
replicateS n f = concatS (replicate n f)

-- | The printer class does the job.

class Print a where
  prt :: Int -> a -> Doc

instance {-# OVERLAPPABLE #-} Print a => Print [a] where
  prt i = concatD . map (prt i)

instance Print Char where
  prt _ c = doc (showChar '\'' . mkEsc '\'' c . showChar '\'')

instance Print String where
  prt _ = printString

printString :: String -> Doc
printString s = doc (showChar '"' . concatS (map (mkEsc '"') s) . showChar '"')

mkEsc :: Char -> Char -> ShowS
mkEsc q = \case
  s | s == q -> showChar '\\' . showChar s
  '\\' -> showString "\\\\"
  '\n' -> showString "\\n"
  '\t' -> showString "\\t"
  s -> showChar s

prPrec :: Int -> Int -> Doc -> Doc
prPrec i j = if j < i then parenth else id

instance Print Integer where
  prt _ x = doc (shows x)

instance Print Double where
  prt _ x = doc (shows x)

instance Print Toast.AbsToast.Ident where
  prt _ (Toast.AbsToast.Ident i) = doc $ showString i
instance Print (Toast.AbsToast.Program' a) where
  prt i = \case
    Toast.AbsToast.Prg _ stmts -> prPrec i 0 (concatD [prt 0 stmts])

instance Print (Toast.AbsToast.Arg' a) where
  prt i = \case
    Toast.AbsToast.Ar _ type_ id_ -> prPrec i 0 (concatD [prt 0 type_, prt 0 id_])
    Toast.AbsToast.ArgRef _ type_ id_ -> prPrec i 0 (concatD [prt 0 type_, doc (showString "&"), prt 0 id_])

instance Print [Toast.AbsToast.Arg' a] where
  prt _ [] = concatD []
  prt _ [x] = concatD [prt 0 x]
  prt _ (x:xs) = concatD [prt 0 x, doc (showString ","), prt 0 xs]

instance Print (Toast.AbsToast.Block' a) where
  prt i = \case
    Toast.AbsToast.Blk _ stmts -> prPrec i 0 (concatD [doc (showString "{"), prt 0 stmts, doc (showString "}")])

instance Print [Toast.AbsToast.Stmt' a] where
  prt _ [] = concatD []
  prt _ (x:xs) = concatD [prt 0 x, prt 0 xs]

instance Print (Toast.AbsToast.Stmt' a) where
  prt i = \case
    Toast.AbsToast.Empty _ -> prPrec i 0 (concatD [doc (showString ";")])
    Toast.AbsToast.BStmt _ block -> prPrec i 0 (concatD [prt 0 block])
    Toast.AbsToast.Decl _ type_ items -> prPrec i 0 (concatD [prt 0 type_, prt 0 items, doc (showString ";")])
    Toast.AbsToast.Ass _ id_ expr -> prPrec i 0 (concatD [prt 0 id_, doc (showString "="), prt 0 expr, doc (showString ";")])
    Toast.AbsToast.Incr _ id_ -> prPrec i 0 (concatD [prt 0 id_, doc (showString "++"), doc (showString ";")])
    Toast.AbsToast.Decr _ id_ -> prPrec i 0 (concatD [prt 0 id_, doc (showString "--"), doc (showString ";")])
    Toast.AbsToast.Ret _ expr -> prPrec i 0 (concatD [doc (showString "return"), prt 0 expr, doc (showString ";")])
    Toast.AbsToast.Cond _ expr block -> prPrec i 0 (concatD [doc (showString "if"), doc (showString "("), prt 0 expr, doc (showString ")"), prt 0 block])
    Toast.AbsToast.CondElse _ expr block1 block2 -> prPrec i 0 (concatD [doc (showString "if"), doc (showString "("), prt 0 expr, doc (showString ")"), prt 0 block1, doc (showString "else"), prt 0 block2])
    Toast.AbsToast.While _ expr block -> prPrec i 0 (concatD [doc (showString "while"), doc (showString "("), prt 0 expr, doc (showString ")"), prt 0 block])
    Toast.AbsToast.Break _ -> prPrec i 0 (concatD [doc (showString "break"), doc (showString ";")])
    Toast.AbsToast.Continue _ -> prPrec i 0 (concatD [doc (showString "continue"), doc (showString ";")])
    Toast.AbsToast.SPrint _ expr -> prPrec i 0 (concatD [doc (showString "Print"), doc (showString "("), prt 0 expr, doc (showString ")"), doc (showString ";")])
    Toast.AbsToast.FnDef _ type_ id_ args block -> prPrec i 0 (concatD [prt 0 type_, prt 0 id_, doc (showString "("), prt 0 args, doc (showString ")"), prt 0 block])

instance Print (Toast.AbsToast.Item' a) where
  prt i = \case
    Toast.AbsToast.NoInit _ id_ -> prPrec i 0 (concatD [prt 0 id_])
    Toast.AbsToast.Init _ id_ expr -> prPrec i 0 (concatD [prt 0 id_, doc (showString "="), prt 0 expr])

instance Print [Toast.AbsToast.Item' a] where
  prt _ [] = concatD []
  prt _ [x] = concatD [prt 0 x]
  prt _ (x:xs) = concatD [prt 0 x, doc (showString ","), prt 0 xs]

instance Print (Toast.AbsToast.Type' a) where
  prt i = \case
    Toast.AbsToast.TInt _ -> prPrec i 0 (concatD [doc (showString "int")])
    Toast.AbsToast.TStr _ -> prPrec i 0 (concatD [doc (showString "string")])
    Toast.AbsToast.TBool _ -> prPrec i 0 (concatD [doc (showString "boolean")])

instance Print [Toast.AbsToast.Type' a] where
  prt _ [] = concatD []
  prt _ [x] = concatD [prt 0 x]
  prt _ (x:xs) = concatD [prt 0 x, doc (showString ","), prt 0 xs]

instance Print (Toast.AbsToast.ExprArg' a) where
  prt i = \case
    Toast.AbsToast.EArg _ expr -> prPrec i 0 (concatD [prt 0 expr])
    Toast.AbsToast.EArgRef _ id_ -> prPrec i 0 (concatD [doc (showString "&"), prt 0 id_])

instance Print [Toast.AbsToast.ExprArg' a] where
  prt _ [] = concatD []
  prt _ [x] = concatD [prt 0 x]
  prt _ (x:xs) = concatD [prt 0 x, doc (showString ","), prt 0 xs]

instance Print (Toast.AbsToast.Expr' a) where
  prt i = \case
    Toast.AbsToast.EVar _ id_ -> prPrec i 6 (concatD [prt 0 id_])
    Toast.AbsToast.ELitInt _ n -> prPrec i 6 (concatD [prt 0 n])
    Toast.AbsToast.ELitTrue _ -> prPrec i 6 (concatD [doc (showString "true")])
    Toast.AbsToast.ELitFalse _ -> prPrec i 6 (concatD [doc (showString "false")])
    Toast.AbsToast.EApp _ id_ exprargs -> prPrec i 6 (concatD [prt 0 id_, doc (showString "("), prt 0 exprargs, doc (showString ")")])
    Toast.AbsToast.EString _ str -> prPrec i 6 (concatD [printString str])
    Toast.AbsToast.Neg _ expr -> prPrec i 5 (concatD [doc (showString "-"), prt 6 expr])
    Toast.AbsToast.Not _ expr -> prPrec i 5 (concatD [doc (showString "!"), prt 6 expr])
    Toast.AbsToast.EMul _ expr1 mulop expr2 -> prPrec i 4 (concatD [prt 4 expr1, prt 0 mulop, prt 5 expr2])
    Toast.AbsToast.EAdd _ expr1 addop expr2 -> prPrec i 3 (concatD [prt 3 expr1, prt 0 addop, prt 4 expr2])
    Toast.AbsToast.ERel _ expr1 relop expr2 -> prPrec i 2 (concatD [prt 2 expr1, prt 0 relop, prt 3 expr2])
    Toast.AbsToast.EAnd _ expr1 expr2 -> prPrec i 1 (concatD [prt 2 expr1, doc (showString "&&"), prt 1 expr2])
    Toast.AbsToast.EOr _ expr1 expr2 -> prPrec i 0 (concatD [prt 1 expr1, doc (showString "||"), prt 0 expr2])

instance Print [Toast.AbsToast.Expr' a] where
  prt _ [] = concatD []
  prt _ [x] = concatD [prt 0 x]
  prt _ (x:xs) = concatD [prt 0 x, doc (showString ","), prt 0 xs]

instance Print (Toast.AbsToast.AddOp' a) where
  prt i = \case
    Toast.AbsToast.Plus _ -> prPrec i 0 (concatD [doc (showString "+")])
    Toast.AbsToast.Minus _ -> prPrec i 0 (concatD [doc (showString "-")])

instance Print (Toast.AbsToast.MulOp' a) where
  prt i = \case
    Toast.AbsToast.Times _ -> prPrec i 0 (concatD [doc (showString "*")])
    Toast.AbsToast.Div _ -> prPrec i 0 (concatD [doc (showString "/")])
    Toast.AbsToast.Mod _ -> prPrec i 0 (concatD [doc (showString "%")])

instance Print (Toast.AbsToast.RelOp' a) where
  prt i = \case
    Toast.AbsToast.LTH _ -> prPrec i 0 (concatD [doc (showString "<")])
    Toast.AbsToast.LE _ -> prPrec i 0 (concatD [doc (showString "<=")])
    Toast.AbsToast.GTH _ -> prPrec i 0 (concatD [doc (showString ">")])
    Toast.AbsToast.GE _ -> prPrec i 0 (concatD [doc (showString ">=")])
    Toast.AbsToast.EQU _ -> prPrec i 0 (concatD [doc (showString "==")])
    Toast.AbsToast.NE _ -> prPrec i 0 (concatD [doc (showString "!=")])

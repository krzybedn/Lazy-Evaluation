{-# LANGUAGE Safe #-}
module CheckWellFormed(checkWellFormed, FormnesError(..)) where

import AST
import Data.Maybe
import Text.Parsec.Pos

data FormnesError
  = DuplicateVariable SourcePos AST.Var
  | UnknownVariable SourcePos AST.Var
  deriving(Show)
instance Eq FormnesError where
   DuplicateVariable _ x1 == DuplicateVariable _ x2 = x1 == x2
   UnknownVariable _ x1   == UnknownVariable _ x2   = x1 == x2
   _ == _ = False


checkWellFormed :: AST.Expr SourcePos -> Maybe FormnesError
checkWellFormed  = aux [] 

aux :: [AST.Var] -> AST.Expr SourcePos -> Maybe FormnesError
aux cv (EApp p t1 t2) = 
  let vs1 = aux cv t1 in
  if isNothing vs1 then 
    let vs2 = aux cv t2 in
    if isNothing vs2 then
      Nothing
    else
      vs2
  else
    vs1

aux cv (ECFn p f e) = aux cv e 
aux cv (ECVl p b) = Nothing
aux cv (ELam p x e) = 
  if Prelude.elem x cv then
    Just $ DuplicateVariable p x
  else 
    let vs = aux (x:cv) e in
    if isNothing vs then
      Nothing
    else
      vs
aux cv (EVar p x) = 
  if Prelude.elem x cv then
    Nothing
  else 
    Just $ UnknownVariable p x


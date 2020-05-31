-- {-# LANGUAGE Safe #-}
module AbstractMachine where

import AST
import Data.Maybe
import Data.Sequence
import Control.Monad.State.Lazy

import qualified Parser
import Text.Parsec.Pos
import System.Environment



{-========================================
    TYPES
========================================-}

data NVar = NVar {var :: Var, count :: Int}


data Term 
  = TApp Term Term
  | TCFn CFn Term
  | TCVl CVl
  | TLam NVar Term
  | TVar NVar

data Val 
  = VCVl CVl
  | VLam NVar Term 

data Ans
  = Ans (Seq Frame) Val
  deriving(Eq)

data Frame
  = FApp Term
  | FCont NVar Context
  | FCFn CFn
  | FLam NVar Term
  deriving(Eq)

type Context = Seq Frame


data Redex 
  = RApp Ans Term
  | RCont NVar Context Ans
  | RCFn CFn Ans

data Config
  = SRefocus [NVar] Context Term
  | SRebuild [NVar] Context Val
  | SNeed [NVar] Context NVar
  | SReduce [NVar] Context Redex
  | SAns Ans


{-========================================
    MAIN CALCULATIONS
========================================-}

step :: Config -> State Int Config 
step (SRefocus vs c to) =  return $
  case to of
    TApp t1 t2  -> SRefocus vs (c |> (FApp t2)) t1
    TCFn f t    -> SRefocus vs (c |> (FCFn f)) t
    TCVl b      -> SRebuild vs c (VCVl b)
    TLam x t    -> SRebuild vs c (VLam x t)
    TVar x      -> SNeed vs c x

step (SRebuild vs c v) =  return $
  case aux c of
    (_, Nothing, _)             -> SAns $ Ans c v
    (c1, Just (FApp t), cb)     -> SReduce vs c1 (RApp (Ans cb v) t)  
    (c1, Just (FCont x c2), cb) -> SReduce vs c1 (RCont x c2 (Ans cb v))  
    (c1, Just (FCFn f), cb)     -> SReduce vs c1 (RCFn f (Ans cb v))  
    (c1, Just (FLam x t), cb)   -> error "Impossible because of implementation of aux"
  where 
    aux :: Context -> (Context, Maybe Frame, Context)
    aux Empty = (Empty, Nothing, Empty)
    aux (cs :|> c) = case c of 
      FApp _     -> (cs, Just c, Empty)
      FCont _ _  -> (cs, Just c, Empty)
      FCFn _     -> (cs, Just c, Empty)
      FLam _ _   -> let (c1, fr, c2) = aux cs in (c1, fr, c2 |> c)

step (SNeed vs c x) =  return $
  case aux c of
    (c1, Just (FLam x t), c2) -> SRefocus vs (c1 |> (FCont x c2)) t
    _                         -> error $ "Something went wrong - wrong args in need rule: " ++ show (SNeed vs  c x) 
  where 
    aux :: Context -> (Context, Maybe Frame, Context)
    aux Empty = (Empty, Nothing, Empty)
    aux (cs :|> c) = case c of 
      FLam xp _  -> if x == xp then (cs, Just c, Empty)
                    else let (c1, fr, c2) = aux cs in (c1, fr, c2 |> c)
      _         -> let (c1, fr, c2) = aux cs in (c1, fr, c2 |> c)

step (SReduce vs c1 r) = 
  case r of
    RApp a t2     ->  let (Ans c2 (VLam x t1)) = a in
                      do
                        n <- get
                        put (n+1)
                        let xp = NVar (var x) (n + 1) in
                          do return $ SRefocus (xp:vs) ((c1 >< c2) |> (FLam xp t2)) (rename t1 x xp)
    RCont x c2 a  -> let (Ans c3 v) = a in return $ SRebuild vs (c1 >< c3 >< ((FLam x $ valToTerm v) <| c2)) v
    RCFn f  a      -> let (Ans cb v) = a in return $ SRebuild vs (c1 >< cb) (delta v f)

step (SAns a) = return $ SAns a

delta :: Val -> CFn -> Val
delta (VLam x t) f  = VLam x (TCFn f t)
delta (VCVl b) "succ" = (VCVl $ 's':b)
delta (VCVl b) f = error $ "Unknown constant function: " ++ f

rename :: Term -> NVar -> NVar -> Term
rename (TApp t1 t2) x1 x2 = TApp (rename t1 x1 x2) (rename t2 x1 x2)
rename (TLam x t) x1 x2 = TLam x (rename t x1 x2)
rename (TCVl b) _ _ = TCVl b
rename (TCFn f t) x1 x2 = TCFn f (rename t x1 x2)
rename (TVar x) x1 x2 = if x == x1 then TVar x2 else TVar x


runMachine :: Config -> State Int Config
runMachine (SAns a) = return $ SAns a
runMachine s = do
  let q = step s in do
    w <- q
    runMachine w


{-========================================
    RUN CALCUALTIONS
========================================-}

findAns :: Term -> Ans
findAns t = let (SAns a) = evalState (runMachine $ SRefocus [] Empty t) 0 in a
  where
    aux (SAns a) = return $ SAns a
    aux s = step s


getFinalAns :: Ans -> Term
getFinalAns (Ans c v) = 
  case v of
    (VCVl b)    -> TCVl b
    (VLam _ _)  -> ansToTerm (Ans c v)

findVal :: Term -> Val
findVal = ansToVal . findAns

evaluateTerm :: AST.Expr p -> Ans
evaluateTerm = findAns . exprToTerm



{-========================================
    CONVERSIONS
========================================-}

exprToTerm :: AST.Expr p -> Term
exprToTerm (EApp _ e1 e2)  = TApp (exprToTerm e1) (exprToTerm e2)
exprToTerm (ECFn _ f e)    = TCFn f (exprToTerm e)
exprToTerm (ECVl _ b)      = TCVl b
exprToTerm (ELam _ x e)    = TLam (NVar x 0) (exprToTerm e)
exprToTerm (EVar _ x)      = TVar $ NVar x 0

termToExpr :: Term -> AST.Expr ()
termToExpr (TApp t1 t2) = EApp () (termToExpr t1) (termToExpr t2)
termToExpr (TCFn f t)   = ECFn () f (termToExpr t) 
termToExpr (TCVl b)     = ECVl () b 
termToExpr (TLam x t)   = ELam () ((var x) ++ (show $ count x)) (termToExpr t) 
termToExpr (TVar x)     = EVar () ((var x) ++ (show $ count x))

valToTerm :: Val -> Term
valToTerm (VLam x t)  = TLam x t
valToTerm (VCVl b)    = TCVl b

ansToTerm :: Ans -> Term
ansToTerm (Ans c v) = plug c $ valToTerm v
  where
    plug Empty t = t
    plug (cs :|> c) t = case c of
      FApp t2   -> plug cs $ TApp t t2
      FCont _ _ -> error "Impossible"
      FLam x t2 -> plug cs $ TApp (TLam x t) t2

ansToVal :: Ans -> Val
ansToVal (Ans _ v) = v

{-========================================
    EQUALITY
========================================-}

instance Eq NVar where
  xl == xr = var xl == var xr  && count xl == count xr

instance Eq Term where
  tl == tr = aux (enumVars 0 [] tl) (enumVars 0 [] tr)
    where
      aux (TApp t1_l t2_l)  (TApp t1_r t2_r)  = aux t1_l t1_r && aux t2_l t2_r
      aux (TCFn f_l t_l)    (TCFn f_r t_r)    = f_l == f_r && aux t_l t_r
      aux (TCVl b_l)        (TCVl b_r)        = b_l == b_r
      aux (TLam x_l t_l)    (TLam x_r t_r)    = x_l == x_r && aux t_l t_r
      aux (TVar x_l)        (TVar x_r)        = x_l == x_r
      aux _ _ = False

      enumVars :: Int -> [(NVar, NVar)] -> Term -> Term
      enumVars n xs (TApp t1 t2) = TApp (enumVars n xs t1) (enumVars n xs t2)
      enumVars n xs (TCFn f t) = TCFn f (enumVars n xs t)
      enumVars _ _  (TCVl b) = TCVl b
      enumVars n xs (TLam x t) = TLam (NVar (var x) n) (enumVars (n+1) ((x, NVar (var x) n):xs) t)
      enumVars _ xs (TVar x) = TVar $ fromJust $ Prelude.lookup x xs

instance Eq Val where
  vl == vr = valToTerm vl == valToTerm vr

{-========================================
    PREATY PRINTER
========================================-}

instance Show NVar where
  showsPrec _ (NVar x n) = showString x . showString "_" . showsPrec 0 n

instance Show Term where
  showsPrec p (TApp t1 t2) = showString "A" . 
    showParen (p > 100)
      (showString "(" . showsPrec 100 t1 . showString ") -> " . showsPrec 101 t2)
  showsPrec _ (TCVl  b)    = showsPrec 0 b
  showsPrec p (TCFn  f t)  = 
    showParen (p > 0)
      (showString f . showString " " . showsPrec p t)
  showsPrec p (TLam x t) =
    showParen (p > 0)
      (showString "λ (" . showsPrec 0 x . showString "). " . showsPrec 0 t)
  showsPrec _ (TVar x)     = showsPrec 0 x


instance Show Val where
  showsPrec _ (VCVl  b)   = showsPrec 0 b
  showsPrec p (VLam x t)  =
    showParen (p > 0)
      (showString "λ (" . showsPrec 0 x . showString "). " . showsPrec 0 t)

instance Show Ans where
  showsPrec p (Ans cs v)  = showString "[\n" . showContext cs . showString "]\n" . showsPrec p v
    where 
      showContext Empty = showString ""
      showContext (c :<| Empty) = showsPrec 0 c
      showContext (c :<| cs) = showsPrec 0 c . showString ",\n" . showContext cs


instance Show Frame where
  showsPrec p (FApp t)     = showString "□  " . showsPrec p t
  showsPrec p (FCont x c)  = showString "(κ " .  showsPrec 0 x . showString "." . showsPrec p c . showString ")"
  showsPrec p (FCFn f)     = showString "/"   .  showsPrec 0 f 
  showsPrec _ (FLam x t)   = (showString "(λ(" . showsPrec 0 x . showString "). " . showString "□ ) " . showsPrec 0 t)


instance Show Redex where
  showsPrec p (RApp a t)     = showString "(" . showsPrec p a . showString ") " . showsPrec p t
  showsPrec p (RCont x c a)  = showString "(κ " . showsPrec 0 x . showString "." . showsPrec p c . showString ") " . showsPrec p a
  showsPrec p (RCFn f a)     = showString "(/" . showsPrec 0 f . showString ") " . showsPrec p a

instance Show Config where
  showsPrec p (SRefocus vs c t)     = showString "<" . showsPrec p vs . showsPrec p c . showString ", " . showsPrec p t . showString ">f"
  showsPrec p (SRebuild vs c a)     = showString "<" . showsPrec p vs . showsPrec p c . showString ", " . showsPrec p a . showString ">b"
  showsPrec p (SNeed vs c x)        = showString "<" . showsPrec p vs . showsPrec p c . showString ", " . showsPrec 0 x . showString ">n"
  showsPrec p (SReduce vs c r)      = showString "<" . showsPrec p vs . showsPrec p c . showString ", " . showsPrec p r . showString ">d"
  showsPrec p (SAns a)              = showString "<" . showsPrec p a  . showString ">a"
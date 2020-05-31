{-# LANGUAGE Safe, DeriveFunctor #-}
module AST where


{-========================================
    TYPES
========================================-}

type CFn = String
type CVl = String
type Var = String

data Expr p 
    = EApp p (Expr p) (Expr p)
    | ECFn p CFn (Expr p)
    | ECVl p CVl
    | ELam p Var (Expr p)
    | EVar p Var
    deriving (Functor)

data Program p = Program (Expr p)
  deriving (Functor, Eq)



{-========================================
    DATA OPERATIONS
========================================-}

updateData :: (p -> p) -> Expr p -> Expr p
updateData f (EApp    p e1 e2)  = EApp    (f p) e1 e2
updateData f (ECFn    p x e)    = ECFn    (f p) x e
updateData f (ECVl    p b)      = ECVl    (f p) b
updateData f (ELam    p x e)    = ELam    (f p) x e
updateData f (EVar    p x)      = EVar    (f p) x
-- updateData f (ENum    p n)        = ENum    (f p) n
-- updateData f (EUnit   p)          = EUnit   (f p)
-- updateData f (EPair   p e1 e2)    = EPair   (f p) e1 e2
-- updateData f (EFst    p  e)       = EFst    (f p) e
-- updateData f (ESnd    p  e)       = ESnd    (f p) e

setData :: p -> Expr p -> Expr p
setData p = updateData (\ _ -> p)

getData :: Expr p -> p
getData (EApp    p _ _)   = p
getData (ECFn    p _ _)   = p
getData (ECVl    p _)     = p
getData (ELam    p _ _)   = p
getData (EVar    p _)     = p
-- getData (ENum    p _)     = p
-- getData (EUnit   p)       = p
-- getData (EPair   p _ _)   = p
-- getData (EFst    p _)     = p
-- getData (ESnd    p _)     = p

dropData :: Expr p -> Expr ()
dropData (EApp    _ e1 e2)  = EApp () (dropData e1) (dropData e2)
dropData (ECFn    _ f e)    = ECFn () f (dropData e)
dropData (ECVl    _ b)      = ECVl () b
dropData (ELam    _ x e)    = ELam () x (dropData e)
dropData (EVar    _ x)      = EVar () x




{-========================================
    PREATY PRINTER
========================================-}

instance Show (Expr p) where
  showsPrec p (EApp _ e1 e2)  =
    showParen (p > 100)
      (showsPrec 100 e1 . showString " -> " . showsPrec 101 e2)
  showsPrec p (ECFn _ f e)    = 
    showParen (p > 100)
      (showString ("/" ++ f) . showString " " . showsPrec 101 e)
  showsPrec _ (ECVl _ b)      = showString "_" . showsPrec 0 b
  showsPrec p (ELam _ x e)    =
    showParen (p > 0)
      (showString "λ (" . showString x . showString "). " . showsPrec 0 e)
  showsPrec _ (EVar  _ x)     = showString x



instance Eq (Expr p) where
  (EApp _ ll al)  == (EApp _ lr ar)   = ll == lr && al == ar
  (ECFn _ xl al)  == (ECFn _ xr ar)   = xl == xr && al == ar
  (ECVl _ xl)     == (ECVl _ xr)      = xl == xr
  (ELam _ xl el)  == (ELam _ xr er)   = xl == xr && el == er
  (EVar _ xl)     == (EVar _ xr)      = xl == xr
  _ == _ = False

  -- showsPrec _ (EUnit _) = showString "()"
  
  -- showsPrec _ (EPair _ e1 e2) =
  --   showString "(" . showsPrec 0 e1 .
  --   showString ", " . showsPair e2 . showString ")"
  --   where
  --     showsPair (EPair _ e1 e2) =
  --       showsPrec 0 e1 . showString ", " . showsPair e2
  --     showsPair e = showsPrec 0 e

  -- showsPrec p (EFst _ e) =
  --   showParen (p > 100) (showString "fst " . showsPrec 101 e)

  -- showsPrec p (ESnd _ e) =
  --   showParen (p > 100) (showString "snd " . showsPrec 101 e)

-- instance (Show p) => Show (Program p) where
instance Show (Program p) where
  showsPrec _ (Program e) = shows e

        
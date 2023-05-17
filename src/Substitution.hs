{-# LANGUAGE FlexibleInstances #-}

module Substitution where

import Syntax
import qualified Data.Set as Set

class Substitutable a where
  substitute :: Var -> Var -> a -> a

instance Substitutable Var  where
    substitute x y z | z == x = y
    substitute _ _ z = z

instance Substitutable Expr where
    substitute x y e = 
        case e of
            (EVar z)        -> EVar (substitute x y z)
            (EApp e1 z)     -> EApp (substitute x y e1) (substitute x y z)
            (EAbs m)        -> EAbs (substitute x y m)
            (ECons c xs)    -> ECons c (substitute x y xs)
            (ELet z e1 e2)  -> ELet (substitute x y z) (substitute x y e1) (substitute x y e2)

instance Substitutable Match where
    substitute x y m = 
        case m of
            (MRet e)        -> MRet (substitute x y e)
            (MPat p m)      -> MPat p (substitute x y m)
            (MApp z m)      -> MApp (substitute x y z) (substitute x y m)
            (MAlt m1 m2)    -> MAlt (substitute x y m1) (substitute x y m2)
            (MGuard e p m)  -> MGuard (substitute x y e) p (substitute x y m)

instance Substitutable (Set.Set Var) where
    substitute x y vars = Set.map (substitute x y) vars

instance Substitutable ([Var]) where
    substitute x y vars = map (substitute x y) vars
{-# LANGUAGE FlexibleInstances #-}

module Substitution where

import Syntax
import qualified Data.Set as Set

class Substitutable a where
  substitute :: Var -> Var -> a -> a

instance Substitutable Expr where
    substitute x y e = 
        case e of
            (EVar z)       | x==z   -> EVar y
            (EApp e1 z)    | x==z   -> EApp (substitute x y e1) y
            (EApp e1 z)             -> EApp (substitute x y e1) z
            (EAbs m)                -> EAbs (substitute x y m)
            (ECons c xs)            -> ECons c (substitute x y xs)
            (ELet z e1 e2) | x==z   -> ELet y (substitute x y e1) (substitute x y e2)
            (ELet z e1 e2)          -> ELet z (substitute x y e1) (substitute x y e2)
            otherwise -> e

instance Substitutable Match where
    substitute x y m = 
        case m of
            (MRet e)            -> MRet (substitute x y e)
            (MPat p m)          -> MPat p (substitute x y m)
            (MApp z m) | z==x   -> MApp y (substitute x y m)
            (MApp z m)          -> MApp z (substitute x y m)
            (MAlt m1 m2)     -> MAlt (substitute x y m1) (substitute x y m2)
            (MGuard e p m)      -> MGuard (substitute x y e) p (substitute x y m)
            otherwise -> m

instance Substitutable (Set.Set Var) where
    substitute x y vars = Set.map (\z -> if x == z then y else z) vars

instance Substitutable ([Var]) where
    substitute x y vars = map (\z -> if x == z then y else z) vars
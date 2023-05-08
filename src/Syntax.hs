module Syntax where

import qualified Data.Map as Map

type Var  = String
type Cons = String

data Expr
    = EVar Var
    | EApp Expr Var
    | EAbs Match
    | ECons Cons [Var]
    | ELet Var Expr Expr
    deriving Eq

instance Show Expr where
    show (EVar x) = show x
    show (EApp e x) = show e ++ " " ++ x
    show (EAbs m) = "λ(" ++ show m ++ ")"
    show (ECons cons xs) = case xs of 
        [] -> cons
        _  -> cons ++ "(" ++ unwords xs ++ ")"
    show (ELet x e1 e2) = "let " ++ x ++ " = " ++ show e1 ++ " in " ++ show e2

data Match
    = MRet Expr
    | MFail
    | MPat Pat Match
    | MApp Var Match
    | MAlt Match Match
    | MGuard Expr Pat Match
    deriving Eq

instance Show Match where
    show (MRet e) = "⌈" ++ show e ++ "⌉"
    show MFail = "↯"
    show (MPat p m) = show p ++ " => " ++ show m
    show (MApp x m) = x ++ " ▷ " ++ show m
    show (MAlt m1 m2) = show m1 ++ " | " ++ show m2
    show (MGuard e p m) = show e ++ " ▷ " ++ show p ++ " => " ++ show m

data Pat
    = PVar Var
    | PCons Cons [Pat]
    deriving Eq

instance Show Pat where
    show (PVar x) = x
    show (PCons cons ps) = case ps of
        [] -> cons
        _  -> cons ++ "(" ++ (unwords . map show) ps ++ ")"

buildMatch :: [Var] -> [Pat] -> Match -> Match
buildMatch []  []  m = m
buildMatch [y] [p] m = MApp y (MPat p m)
buildMatch (y:ys) (p:ps) m = MApp y (MPat p (buildMatch ys ps m))

type Heap = Map.Map Var Expr

emptyHeap = Map.empty

extend :: Var -> Expr -> Heap -> Heap
extend x e h = Map.insert x e h

restrict :: Var -> Heap -> Heap
restrict x h = Map.delete x h

extract :: Var -> Heap -> Maybe Expr
extract x h = Map.lookup x h

arity :: Match -> Int
arity MFail             = 0
arity (MRet _)          = 0
arity (MPat _ m)        = 1 + arity m
arity (MApp _ m)        = max 0 ((arity m) - 1)
arity (MAlt m _)     = arity m
arity (MGuard _ _ m)    = arity m

whnf :: Expr -> Bool
whnf e = case e of
    (EAbs m) | arity m > 0  -> True
    (ECons _ _)             -> True
    otherwise               -> False

data Control
    = E Expr
    | M [Var] Match
    deriving (Eq, Show)

data Continuation
    = CArg Var
    | CUpd Var
    | CEnd
    | CAlt [Var] Match
    | CPat [Var] Match
    deriving (Eq, Show)

type Stack = [Continuation]

emptyStack = []

type Configuration = (Heap, Control, Stack)

data EvalError
    = MatchError String
    | EvalError String
    | Stuck
    | UndefinedVariable
    deriving (Show, Eq)
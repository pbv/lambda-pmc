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

data Match
    = MRet Expr
    | MFail
    | MPat Pat Match
    | MApp Var Match
    | MAlt Match Match
    | MGuard Expr Pat Match
    deriving Eq

data Pat
    = PVar Var
    | PCons Cons [Pat]
    deriving Eq

data Control
    = E Expr
    | M [Var] Match
    deriving (Eq)

data Continuation
    = CArg Var
    | CUpd Var
    | CEnd
    | CAlt [Var] Match
    | CPat [Var] Match
    deriving (Eq)

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
arity (MAlt m _)        = arity m
arity (MGuard _ _ m)    = arity m

whnf :: Expr -> Bool
whnf e = case e of
    (EAbs m) | arity m > 0  -> True
    (ECons _ _)             -> True
    otherwise               -> False

type Stack = [Continuation]

emptyStack = []

type Configuration = (Heap, Control, Stack)

data EvalError
    = MatchError String
    | EvalError String
    | Stuck
    | Loop
    | UndefinedVariable
    deriving (Show, Eq)
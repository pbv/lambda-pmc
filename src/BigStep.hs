module BigStep (eval) where

import Syntax
import Substitution

import Control.Monad
import Control.Monad.State
import Control.Monad.Except
import qualified Data.Set as Set

type Eval = ExceptT EvalError (State EvalState)

data EvalState = EvalState { count :: Int }
  deriving (Show, Eq)

initialState = EvalState { count = 0 }

fresh :: Eval Var
fresh = do
    s <- get
    put s {count = count s + 1}
    return (typeVars !! count s)

typeVars = [1..] >>= flip replicateM letters

letters = ['A'..'Z']

eval e env = do 
    (_, res) <- evalState (runExceptT $ evalExpr env Set.empty e) initialState
    return res

evalExpr :: Heap -> Set.Set Var -> Expr -> Eval (Heap, Expr)
evalExpr heap _ expr
    | whnf expr = return (heap, expr)

evalExpr heap l (EAbs match) 
    | arity match == 0 
    = do 
        (heap', res) <- evalMatch heap l [] match
        case res of
            (MRet expr) -> evalExpr heap' l expr
            otherwise -> throwError $ EvalError "Match Failure @SAT"

evalExpr heap l (EVar name) = do
    case extract name heap of
        Nothing -> throwError $ EvalError ("Variable not found " ++ name )
        Just heapExpr -> do
            (heap', expr) <- evalExpr (restrict name heap) (Set.insert name l) heapExpr
            return (extend name expr heap', expr)

evalExpr heap l (EApp expr name) = do 
    (heap', res) <- evalExpr heap l expr
    case res of
        (EAbs match) -> evalExpr heap' l (EAbs (MApp name match))
        otherwise -> throwError $ EvalError $ "Application failure " -- ++ show res

evalExpr heap l (ELet x expr body) = do
    y <- fresh
    let expr' = substitute x y expr
        body' = substitute x y body
        heap' = extend y expr' heap
    evalExpr heap' l body'

evalExpr _ _ _ = throwError $ EvalError "eval"

evalMatch :: Heap -> Set.Set Var -> [Var] -> Match -> Eval (Heap, Match)
evalMatch heap l stack MFail = return (heap, MFail)

evalMatch heap l stack (MRet expr) = do
    return (heap, MRet (foldl EApp expr stack))

evalMatch heap l stack (MApp name match) =
    evalMatch heap l (name : stack) match

evalMatch heap l (y : stack) (MPat (PVar x) match) =
    evalMatch heap l stack (substitute x y match)

evalMatch heap l (y : stack) (MPat (PCons pcons patterns) match) = do
    (heap', res) <- evalExpr heap l (EVar y)
    case res of
        (ECons econs exprs) 
            | pcons == econs
            && (length patterns == length exprs) 
            -> evalMatch heap' l stack (buildMatch exprs patterns match)
        (ECons _ _) -> return (heap', MFail) 
        otherwise -> throwError $ MatchError $ "Constructor mismatch: " ++ pcons -- ++ " and " ++ show res 

evalMatch heap l stack (MAlt lhs rhs) = do
    (heap', res) <- evalMatch heap l stack lhs
    case res of
        (MRet _) -> return (heap', res)
        MFail    -> evalMatch heap' l stack rhs
        -- _ -> throwError $ MatchError "alternative"

evalMatch heap l stack (MGuard e (PCons pcons patterns) match) = do
    (heap', res) <- evalExpr heap l e
    case res of
        (ECons econs exprs)
            | pcons == econs
            && (length patterns == length exprs) 
            -> evalMatch heap' l stack (buildMatch exprs patterns match)
        (ECons _ _) -> return (heap', MFail)
        otherwise -> throwError $ MatchError $ "Constructor mismatch: " ++ pcons -- ++ " and " ++ show res 

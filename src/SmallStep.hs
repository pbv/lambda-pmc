module SmallStep (run) where

import Syntax
-- import LatexPrinter
import PrettyPrinter
import Substitution
import qualified Data.Map as Map
import Control.Monad.Except
import Control.Monad.State
import Debug.Trace

type Eval = ExceptT EvalError (State EvalState)

data EvalState = EvalState { count :: Int, startingEnv :: Heap }

initialState env = EvalState { count = 0, startingEnv = env }

fresh :: Eval Var
fresh = do
    s <- get
    put s {count = count s + 1}
    return (names !! count s)

names = zipWith (\a -> \b -> a ++ show b) (repeat "l") [1..]

run :: Expr -> Heap -> Either EvalError Configuration
run e env = evalState (runExceptT $ eval (env, E e, [])) (initialState env)

eval :: Configuration -> Eval Configuration
eval config@(_, E e, []) | whnf e = return config
eval (_, M _ MFail, (CEnd) : _)   = throwError Stuck
eval config = do
    config'@(heap, control, stack) <- eval' config
    h <- gets startingEnv
    traceM $ showStep (heap Map.\\ h, control, stack)
    when (config == config') $ throwError Loop
    eval config'

eval' :: Configuration -> Eval Configuration
-- App1
eval' (heap, E (EApp e y), stack) = 
    return (heap, E e, (CArg y) : stack)

--App2
eval' (heap, E (EAbs m), (CArg y) : stack)
    | arity m > 0
    = return (heap, E (EAbs (MApp y m)), stack)

--Sat
eval' (heap, E (EAbs m), stack)
    | arity m == 0
    = return (heap, M [] m, CEnd : stack)

--Var
eval' (heap, E (EVar y), stack) = 
    case extract y heap of
        Nothing -> throwError UndefinedVariable
        -- Just e | whnf e -> return (heap, E e, stack) -- small optimization
        Just e -> return (restrict y heap, E e, (CUpd y) : stack)

--Update
eval' (heap, E w, (CUpd y : stack))
    | whnf w
    = return (extend y w heap, E w, stack) 

--Let
eval' (heap, (E (ELet x expr body)), stack) = do
    y <- fresh
    let expr' = substitute x y expr
        body' = substitute x y body
        heap' = extend y expr' heap
    return (heap', E body', stack)

--Return1A
eval' (heap, (M args (MRet e)), stack)
    | args /= []
    = return (heap, M [] (MRet (foldl EApp e args)), stack)

-- Return1B
eval' (heap, (M [] (MRet e)), (CEnd : stack)) = 
    return (heap, E e, stack)

--Return1C
eval' (heap, (M args MFail), stack)
    | args /= []
    = return (heap, M [] MFail, stack)

--Return2
eval' (heap, (M [] (MRet e)), ((CAlt _ m) : stack))
    = return (heap, M [] (MRet e), stack)

--Bind
eval' (heap, (M (y : args) (MPat (PVar x) m)), stack) =
    return (heap, M args (substitute x y m), stack)

--Cons1
eval' (heap, M (y : args) m@(MPat (PCons _ _) _), stack) =
    return (heap, E (EVar y), ((CPat args m) : stack))

--Cons2
eval' (heap, E (ECons econs vars), ((CPat args (MPat (PCons pcons pats) m)) : stack))
    | length pats == length vars
    && econs == pcons
    = return (heap, M args (buildMatch vars pats m), stack)

--Fail
eval' (heap, E (ECons econs vars), ((CPat args (MPat (PCons pcons pats) m)) : stack))
    | length pats /= length vars
    || econs /= pcons
    = return (heap, M [] MFail, stack)

--Guard
eval' (heap, M args (MGuard expr p@(PCons _ _) m), stack) =
    return (heap, E expr, CPat args (MPat p m) : stack)

--Arg
eval' (heap, M args (MApp y m), stack) =
    return (heap, M (y : args) m, stack)

--Alt1
eval' (heap, M args (MAlt m1 m2), stack)
    = return (heap, M args m1, (CAlt args m2) : stack)

--Alt2
eval' (heap, M [] MFail, (CAlt args m) : stack)
    = return (heap, M args m, stack)
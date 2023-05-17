module PrettyPrinter where

import Syntax
import qualified Data.Map as Map

instance Show Expr where
    show (EVar x) = show x
    show (EApp e x) = show e ++ " " ++ x
    show (EAbs m) = "λ(" ++ show m ++ ")"
    show (ECons cons xs) = case xs of 
        [] -> cons
        _  -> cons ++ "(" ++ commas xs ++ ")"
    show (ELet x e1 e2) = "let " ++ x ++ " = " ++ show e1 ++ " in " ++ show e2

instance Show Match where
    show (MRet e) = "⌈" ++ show e ++ "⌉"
    show MFail = "↯"
    show (MPat p m) = show p ++ " => " ++ show m
    show (MApp x m) = x ++ " ▷ " ++ show m
    show (MAlt m1 m2) = show m1 ++ " | " ++ show m2
    show (MGuard e p m) = show e ++ " ▷ " ++ show p ++ " => " ++ show m

instance Show Pat where
    show (PVar x) = x
    show (PCons cons ps) = case ps of
        [] -> cons
        _  -> cons ++ "(" ++ (commas . map show) ps ++ ")"

instance Show Continuation where
    show (CArg x) = x
    show (CUpd x) = "!" ++ x
    show CEnd = "$"
    show (CAlt args m) = "?(" ++ commas args ++ "," ++ show m ++ ")"
    show (CPat args m) = "@(" ++ commas args ++ "," ++ show m ++ ")"

instance Show Control where
    show (E e) = "E " ++ show e
    show (M args m) = "M " ++ commas args ++ " " ++ show m

showStep :: Configuration -> String
showStep = show

commas :: [String] -> String
commas [] = []
commas [x] = x
commas (x:xs) = x ++ ", " ++ commas xs

printConfig env (h,c,s) = 
    putStr "Heap:\t" >> print (h Map.\\ env) 
    >> putStr "Stack:\t" >> print s 
    >> putStr "Result:\t" >> print c 

printBegin :: IO ()
printBegin = return ()

printEnd :: IO ()
printEnd = return ()

printInitial e = print e
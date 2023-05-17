module LatexPrinter where

import Syntax
import qualified Data.Map as Map

instance Show Expr where
    show (EVar x) = x
    show (EApp e x) = "\\app{" ++ show e ++ "}{" ++ x ++ "}"
    show (EAbs m) = "\\abstr{" ++ show m ++ "}"
    show (ECons ":" xs) = case xs of 
        [] -> "\\constr{\\cons{nil}}"
        (h:t:[])  -> "(" ++ h ++ " : " ++ t ++ ")"
    show (ECons cons []) = "\\constr{\\cons{" ++ cons ++ "}}{}"
    show (ECons cons [x,y]) = "\\constr{\\cons{" ++ cons ++ "}}{" ++ x ++ "," ++ y ++ "}"
    show (ELet x e1 e2) = 
        "\\llet{" ++ x ++ " = " ++ show e1 ++ "}{" ++ show e2 ++ "}"

instance Show Match where
    show (MRet e) = "\\matchreturn{" ++ show e ++"}"
    show MFail = "\\matchfail"
    show (MPat p m) = "\\matchpat{" ++ show p ++ "}{" ++ show m ++ "}"
    show (MApp x m) = "\\matcharg{" ++ x ++ "}{" ++ show m ++ "}"
    show (MAlt m1 m2) = "\\matchalt{" ++ show m1 ++ "}{" ++ show m2 ++ "}"
    show (MGuard e p m) = show e ++ " \\amatcharg " ++ show p ++ " \\amatchpat " ++ show m

instance Show Pat where
    show (PVar x) = x
    show (PCons ":" xs) = case xs of 
        [] -> "\\textsf{nil}"
        (h:t) -> "(" ++ show h ++ " : " ++ (commas . map show) t ++")"
    show (PCons cons (h:t)) = "\\textsf{" ++ cons ++ "}" ++ "(" ++ show h ++ "," ++ (commas . map show) t ++")"
    show (PCons cons []) = "\\textsf{" ++ cons ++ "}"

instance Show Continuation where
    show (CArg x) = x
    show (CUpd x) = "!" ++ x
    show CEnd = "\\$"
    show (CAlt args m) = "?([" ++ commas args ++ "]," ++ show m ++ ")"
    show (CPat args m) = "@([" ++ commas args ++ "]," ++ show m ++ ")"

instance Show Control where
    show (E e) = "\\eval{" ++ show e ++ "}"
    show (M args m) = "\\match{[" ++ commas args ++ "]}{" ++ show m ++ "}"


showStep :: Configuration -> String
showStep (h,c,s) = showHeap (Map.toList h) ++ " & " ++ show c ++ " & " ++ show s ++ "& () \\\\"

showHeap [] = "\\Gamma"
showHeap h = "\\{" ++ (commas . map (\(a,b) -> "(" ++ a ++ "\\mapsto" ++ show b ++ ")")) h ++ "\\}"

commas :: [String] -> String
commas [] = []
commas [x] = x
commas (x:xs) = x ++ ",~" ++ commas xs

printBegin = putStrLn "\\begin{figure*}\n\\[ \n\\begin{array}{llll}\n\\hline\n\\text{Heap} & \\text{Control} & \\text{RetStack} & \\text{rule} \\\\\n\\hline"
printEnd = putStrLn "\\end{array}\n\\]\n\\end{figure*}"
printInitial e = putStrLn $ "\\Gamma & " ++ show (E e) ++ " & [] & () \\\\"

printConfig env (h,c,s) = 
    putStr "Heap:\t" >> print (h Map.\\ env) 
    >> putStr "Stack:\t" >> print s 
    >> putStr "Result:\t" >> print c 
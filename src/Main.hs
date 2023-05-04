module Main where

import Syntax
import qualified BigStep
import qualified SmallStep

import qualified Data.Map as Map

startingEnv = Map.fromList 
    [ ("empty", ECons ":" [])
    , ("True", ECons "True" [])
    , ("False", ECons "False" [])
    , ("1", ECons "1" [])
    , ("2", ECons "2" [])
    , ("3", ECons "3" [])
    , ("4", ECons "4" [])
    -- , ("list1", mkList "1" "empty")
    -- , ("list2", mkList "2" "empty")
    -- , ("list3", mkList "3" "empty")
    -- , ("list4", mkList "4" "empty")
    -- , ("list12", mkList "1" "list2")
    -- , ("list13", mkList "1" "list3")
    -- , ("list23", mkList "2" "list3")
    -- , ("list24", mkList "2" "list4")
    -- , ("list123", mkList "1" "list23")
    , ("zipWith", zipWithPMC)
    ]

-- Evaluators

eval e = BigStep.eval e startingEnv
run e = mapM_ printConfig (SmallStep.run e startingEnv)

printConfig (h,c,s) = 
    putStr "Heap:\t" >> print (h Map.\\ startingEnv) 
    >> putStr "Stack:\t" >> print s 
    >> putStr "Result:\t" >> print c 

-- helpers
mkList x xs = ECons ":" [x, xs]
mkPair x y  = ECons "()" [x, y]
appZipWith f xs ys = (EApp (EApp (EApp zipWithPMC f) xs) ys)

apply f e = ELet "res" e  (EApp f "res")

apply2 f e1 e2 = 
    ELet "res" e1 
    (ELet "res'" e2 
    (EApp (EApp f "res") "res'"))

apply3 f e1 e2 e3 = 
    ELet "res" e1 
    (ELet "res'" e2 
    (ELet "res''" e3 
    (EApp (EApp (EApp f "res") "res'") "res''")))

-- lambda-PMC functions

mkPairPMC = EAbs (MPat (PVar "x") (MPat (PVar "y") (MRet (mkPair "x" "y"))))
constPMC  = EAbs (MPat (PVar "x") (MRet (EVar "1")))
idPMC     = EAbs (MPat (PVar "x") (MRet (EVar "x")))

isShortPMC = 
    EAbs (MAlt 
        (MPat (PCons ":" [PVar "x", PCons ":" [PVar "y", PVar "ys"]]) 
            ((MRet (ECons "False" []))))
        (MPat (PVar "ys") 
            ((MRet (ECons "True" [])))))

zipWithPMC =
    -- ELet "zipWith"
        (EAbs (MAlt 
            (MPat (PVar "f") 
            (MPat (PCons ":" [PVar "x", PVar "xs"])
            (MPat (PCons ":" [PVar "y", PVar "ys"]) 
                (MRet (ELet "head" (EApp (EApp (EVar "f") "x") "y")
                      (ELet "tail" (EApp (EApp (EApp (EVar "zipWith") "f") "xs") "ys")
                      (ECons ":" ["head", "tail"])))))))                              
            (MPat (PVar "f") (MPat (PVar "xs'") (MPat (PVar "ys'") (MRet (ECons ":" [])))))))
        -- (EVar "zipWith")

headPMC = EAbs (MPat (PCons ":" [PVar "x", PVar "xs"]) (MRet (EVar "x")))
tailPMC = EAbs (MPat (PCons ":" [PVar "x", PVar "xs"]) (MRet (EVar "xs")))

-- Example 1: isShort (foo True)
example1 = apply isShortPMC (EApp foo "True")
    where foo = EAbs (MPat (PVar "x") (MRet (mkList "x" "empty"))) 

-- Example 2: zipWith mkPair [] (tail [])
example2 = --apply3 zipWithPMC mkPairPMC (ECons ":" []) (apply tailPMC (ECons ":" []))
    ELet "res" (EApp tailPMC "empty") 
        (EApp (EApp (EApp zipWithPMC "mkPair") "empty") "res")

main :: IO ()
main = return ()
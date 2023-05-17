module Main where

import Syntax
import LatexPrinter
-- import PrettyPrinter
import qualified BigStep
import qualified SmallStep

import qualified Data.Map as Map

startingEnv = Map.fromList 
    [ ("nil",  nil)
    , ("zero", zero)
    , ("one",  one)
    , ("two",  two)
    , ("isShort", isShortPMC)
    , ("zipWith", zipWithPMC)
    , ("nodups",  nodupsPMC)
    , ("isEqual", isEqualPMC)
    , ("single", singlePMC)
    , ("singleton", mkList "one" "nil")
    , ("double", mkList "two" "single")
    , ("triple", mkList "one" "double")
    ]

-- Evaluators

eval e = BigStep.eval e startingEnv

run e = do
    printBegin
    printInitial e
    mapM_ return $ SmallStep.run e startingEnv
    printEnd

-- helpers
mkList x xs = ECons ":" [x, xs]
mkPair x y  = ECons "()" [x, y]

apply  f e        = ELet "res" e  (EApp f "res")
apply2 f e1 e2    = ELet "res" e1 (ELet "res2" e2 (EApp (EApp f "res") "res2"))
apply3 f e1 e2 e3 = ELet "res" e1 (ELet "res2" e2 (ELet "res3" e3 (EApp (EApp (EApp f "res") "res2") "res3")))

-- literals
true  = ECons "True"  []
false = ECons "False" []
zero  = ECons "0" []
one   = ECons "S" ["zero"]
two   = ECons "S" ["one"]
nil   = ECons ":" []

isNil   = PCons ":" []
isTrue  = PCons "True"  []
isFalse = PCons "False" []
isList [x,y] = PCons ":" [PVar x, PVar y]
isList (x:ys) =  PCons ":" [PVar x, isList ys]

-- lambda-PMC functions

singlePMC = EAbs (MPat (PVar "x") (MRet $ mkList "x" "nil"))
mkPairPMC = EAbs (MPat (PVar "x") (MPat (PVar "y") (MRet (mkPair "x" "y"))))
idPMC     = EAbs (MPat (PVar "x") (MRet (EVar "x")))
succPMC   = EAbs (MPat (PCons "S" [PVar "x"]) (MRet (ECons "S" ["x"])))
predPMC   = EAbs (MPat (PCons "S" [PVar "x"]) (MRet (EVar "x")))

isShortPMC = EAbs (MAlt (MPat (isList ["x","y","ys"]) (MRet false)) (MPat (PVar "ys") (MRet true)))

zipWithPMC =
    (EAbs (MAlt 
        (MPat (PVar "f") 
        (MPat (PCons ":" [PVar "x", PVar "xs"])
        (MPat (PCons ":" [PVar "y", PVar "ys"]) 
            (MRet (ELet "head" (EApp (EApp (EVar "f") "x") "y")
                  (ELet "tail" (EApp (EApp (EApp (EVar "zipWith") "f") "xs") "ys")
                  (ECons ":" ["head", "tail"])))))))                              
        (MPat (PVar "f") (MPat (PVar "xs'") (MPat (PVar "ys'") (MRet nil))))))

headPMC = EAbs (MPat (PCons ":" [PVar "x", PVar "xs"]) (MRet (EVar "x")))
tailPMC = EAbs (MPat (PCons ":" [PVar "x", PVar "xs"]) (MRet (EVar "xs")))

isEqualPMC = EAbs (MAlt 
          (MPat (PCons "zero" []) (MPat (PCons "zero" []) (MRet true)))
    (MAlt (MPat (PCons "S" [PVar "x"]) (MPat (PCons "S" [PVar "y"]) (MRet (apply2 (EVar "isEqual") (EVar "x") (EVar "y")))))
          (MPat (PVar "x") (MPat (PVar "y") (MRet false)))))

nodupsPMC = EAbs (MAlt pat1 (MAlt pat2 pat3))
    where
        pat1 = MPat (PCons ":" [PVar "x", PVar "xs"]) 
              (MApp "xs"
              (MPat (PCons ":" [PVar "y", PVar "xs'"]) 
              (MGuard 
                (apply2 isEqualPMC (EVar "x") (EVar "y"))
                isTrue 
                (MRet (EApp (EVar "nodups") "xs")))))
        pat2 = MPat (PCons ":" [PVar "x", PVar "xs"]) 
                    (MRet (ELet "res" (EApp (EVar "nodups") "xs") (mkList "x" "res")))
        pat3 = MPat isNil (MRet nil)

-- Example 1: isShort [1]
example1 = apply isShortPMC (mkList "one" "nil")

-- Example 2: zipWith mkPair [] (tail [])
example2 = apply (EApp (EApp zipWithPMC "mkPair") "nil") (EApp tailPMC "nil")

-- Example 3: nodups [1,1,2,1]
example3 = EApp nodupsPMC "list1121"

main :: IO ()
main = return ()
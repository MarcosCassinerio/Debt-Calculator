module PrettyPrinter where

import Common
import Text.PrettyPrint.HughesPJ
import Prelude hiding ( (<>) )

printEnv :: Env -> Doc
printEnv [] = text ""
printEnv ((n, (l, o)):xs) = if [n] == l
                            then text n 
                                 <> if o == []
                                    then text " []"
                                    else text " [\n"
                                         <> printOps o
                                         <> text "]\n\n"
                                 <> printEnv xs
                            else text n
                                 <> text " "
                                 <> text (show l)
                                 <> if o == []
                                    then text " []"
                                    else text " [\n"
                                         <> printOps o
                                         <> text "]\n\n"
                                 <> printEnv xs

printOps :: [Op] -> Doc
printOps [] = text ""
printOps [x] = text "  "
               <> text (show x)
               <> text "\n"
printOps (x:xs) = text "  "
                  <> text (show x)
                  <> text ","
                  <> text "\n"
                  <> printOps xs

printArcs :: [Arc] -> Doc
printArcs [] = text ""
printArcs (((p1, p2), i):xs) = text (show p1)
                               <> text " -- "
                               <> text (show i)
                               <> text " --> "
                               <> text (show p2)
                               <> text "\n"
                               <> printArcs xs
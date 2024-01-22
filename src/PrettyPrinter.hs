module PrettyPrinter where

import           Common
import           Text.PrettyPrint.HughesPJ
import           Prelude                 hiding ( (<>) )

print :: Exp -> Doc
print (DefineP n) =
  text "DEFINEP"
    <> text n
print (DefineG n nl) =
  text "DEFINEP"
print (DebtP n i) = 
  text "DEBTP"
    <> text n
    <> text "1"
print (DebtG g n i) = 
  text "DEBTG"
    <> text g
    <> text n
    <> text "1"
print (Expense n i) = 
  text "EXPENSE"
    <> text n
    <> text "1"
print (Calculate n) = 
  text "CALCULATE"
    <> text n
print CalculateAll = 
  text "CALCULATEALL"
print (Registry n) =
  text "REGISTRY"
    <> text n
print (Members n) = 
  text "MEMBERS"
    <> text n
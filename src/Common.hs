module Common where

import qualified Data.Map.Strict as M

-- Comandos interactivos o de archivos
data Stmt e = Def e
            | Eval e
  deriving (Show)

instance Functor Stmt where
  fmap f (Def i) = Def (f i)
  fmap f (Eval i)  = Eval (f i)

data Person = Self | Other Name
  deriving (Eq, Show, Ord)

type Name = String

type Op = (Person, Int)

type Env = [(Name, ([Name], [Op]))]

data Exp = DefineP Name
          | DefineG Name [Name]
          | DebtP Name Int
          | DebtG Name Name Int
          | Expense Name Int
          | Calculate Name
          | CalculateAll
          | Registry Name
          | Members Name
  deriving (Eq, Show)

type Graph = ([Person], Operations)

type Operations = M.Map Person (M.Map Person Int)

type Arc = ((Person, Person), Int)

data Error = NameNotFound | NameAlreadyExists | WrongName | NameNotInGroup
  deriving (Eq, Show)
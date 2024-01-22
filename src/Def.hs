module Def where

import           Common
import           Monads
import           Data.Maybe
import           Control.Monad                  ( liftM
                                                , ap
                                                )

-- Mónada estado, con manejo de errores
newtype StateError a =
  StateError { runStateError :: Env -> Either Error (a, Env) }

-- Para calmar al GHC
instance Functor StateError where
  fmap = liftM

instance Applicative StateError where
  pure  = return
  (<*>) = ap

instance Monad StateError where
  return x = StateError (\e -> Right (x, e))
  m >>= f = StateError (\e -> case runStateError m e of
                                   Left e -> Left e
                                   Right (v, e') -> runStateError (f v) e')

instance MonadState StateError where
  create n l = StateError $ create' n l
    where create' :: Name -> [Name] -> Env -> Either Error ((), Env)
          create' n l s = case lookup n s of
                               Nothing -> Right ((), (n, (l, [])):s)
                               Just _ -> Left (NameAlreadyExists n)
  addOp n o = StateError $ checkEnv' n o
    where checkEnv' :: Name -> Op -> Env -> Either Error ((), Env)
          checkEnv' n o s = case lookup n s of
                                 Nothing -> Left (NameNotFound n)
                                 Just (l, o') -> case fst o of
                                                      Self -> Right ((), addOp' n o s)
                                                      Other name -> if elem name l
                                                                    then Right ((), addOp' n o s)
                                                                    else Left (NameNotInGroup name n)
          addOp' :: Name -> Op -> Env -> Env
          addOp' n o (v@(n', (l', o')):xs) = if n == n' 
                                           then (n, (l', o' ++ [o])):xs
                                           else v:(addOp' n o xs)

instance MonadError StateError where
  throw e = StateError (\_ -> Left e)

-- Toma una expresion y un entorno
-- Devuelve el entorno resultante
-- O el error adecuando en caso de que haya habido alguno
def :: Exp -> Env -> Either Error Env
def exp env = case runStateError (def' exp) env of
                   Left err -> Left err
                   Right (_, env') -> Right env'

-- Toma una expresion
-- Devuelve la monada resultante de haber agregado la expresion
def' :: (MonadState m, MonadError m) => Exp -> m ()
def' (DefineP n) = create n [n]
def' (DefineG n l) = create n l
def' (DebtP n m) = addOp n (Other n, m)
def' (DebtG n g m) = addOp g (Other n, m)
def' (Expense n m) = addOp n (Self, m)
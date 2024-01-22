module Monads where

import           Common

-- Clases de mónadas que proveen las operaciones necesarias
-- para implementar los evaluadores.

-- Clase para representar mónadas con estado de variables
class Monad m => MonadState m where
    -- Cambia el valor de una variable
    create :: Name -> [Name] -> m ()
    -- Agrega una nueva operacion
    addOp :: Name -> Op -> m ()

-- Clase para representar mónadas que lanzan errores
class Monad m => MonadError m where
    -- Lanza un error
    throw :: Error -> m a
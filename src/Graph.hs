module Graph where

import qualified Data.Map.Strict as M
import Common

eval :: Exp -> Env -> String
eval (Calculate n) env = show (calculate env n)
eval CalculateAll env = show (calculateAll env)
eval (Registry n) env = show (registry env n)
eval (Members n) env = show (members env n)

-- Toma un entorno
-- Devuelve las deudas correspondientes de ese entorno
transformToHash :: Env -> Operations
transformToHash [] = M.empty
transformToHash ((n, (l, o)):xs) = transformToHash' (concat (map (generateDebt l) o))
  where transformToHash' :: [Arc] -> Operations
        transformToHash' [] = M.empty
        transformToHash' (((p1, p2), i):xs) = let m = transformToHash' xs
                                              in M.insertWith (M.unionWith (+)) p1 (M.insert p2 i M.empty) m
        generateDebt :: [Name] -> Op -> [Arc]
        generateDebt l (p, i) = let len = ((length l) + 1)
                                    val = div i len
                                in case p of
                                        Self -> map (\x -> ((Other x, Self), val)) l
                                        Other a -> ((Self, p), val):(map (\x -> ((Other x, p), val)) (delete a l))

delete :: Eq a => a -> [a] -> [a]
delete x [] = []
delete x (y:ys) = if x == y then ys else y:(delete x ys)

-- Toma un entorno y un nombre
-- Genera las operaciones de ese grupo o persona y devuelve las deudas (aristas) simplificando tanto los ciclos como los caminos de igual coste
-- Si no existe ese nombre devuelve el error adecuado
calculate :: Env -> Name -> Either Error [Arc]
calculate [] _ = Left NameNotFound
calculate (e@(n, _):xs) s = if n == s0 3
                            then let (_, ops) = deleteSameCostPaths (deleteAllCicles ([Self, Other n], transformToHash [e]))
                                 in Right $ operationsToArcs ops
                            else calculate xs s

-- Toma un entorno
-- Genera todas las operaciones y devuelve las deudas (aristas) simplificando tanto los ciclos como los caminos de igual coste
-- Si no existe ese nombre devuelve el error adecuado
calculateAll :: Env -> [Arc]
calculateAll xs = let (l, ops) = deleteSameCostPaths (deleteAllCicles (Self:(map (\x -> Other (fst x)) xs), transformToHash xs))
                  in operationsToArcs ops

-- 
-- Transforma las operaciones a aristas (deudas)
operationsToArcs :: Operations -> [Arc]
operationsToArcs ops = let l = M.toList ops
                       in [((p1, p2), i) | (p1, m) <- l, (p2, i) <- M.toList m]

deleteSameCostPaths :: Graph -> Graph
deleteSameCostPaths g@(l@(x:xs), ops) = deleteSameCostPaths' g l (maybe [] M.toList (M.lookup x ops))
  where deleteSameCostPaths' :: Graph -> [Person] -> [(Person, Int)] -> Graph
        deleteSameCostPaths' g [_] [] = g
        deleteSameCostPaths' g@(l, ops) (y:z:zs) [] = deleteSameCostPaths' g (z:zs) (maybe [] M.toList (M.lookup z ops))
        deleteSameCostPaths' g@(l, ops) (x:xs) ((y, i):ys) = case M.lookup y ops of
                                                                  Nothing -> deleteSameCostPaths' g (x:xs) ys
                                                                  Just m -> case getSameCostArc (M.toList m) i of
                                                                                 Nothing -> deleteSameCostPaths' g (x:xs) ys
                                                                                 Just z -> deleteSameCostPaths' (l, updateOperations ops x y z i) (x:xs) (addPath ys (z, i))
        getSameCostArc :: [(Person, Int)] -> Int -> Maybe Person
        getSameCostArc [] _ = Nothing
        getSameCostArc ((p, i):xs) i' = if i == i' then Just p else getSameCostArc xs i'
        updateOperations :: Operations -> Person -> Person -> Person -> Int -> Operations
        updateOperations ops p1 p2 p3 n = case M.lookup p1 ops of 
                                               Nothing -> ops
                                               Just m -> case M.lookup p2 ops of
                                                              Nothing -> ops
                                                              Just m' -> M.insert p1 (M.insertWith (+) p3 n (M.delete p2 m)) (M.insert p2 (M.delete p3 m') ops)
        addPath :: [(Person, Int)] -> (Person, Int) -> [(Person, Int)]
        addPath [] x = [x]
        addPath ((p, i):xs) v@(p', i') = if p == p'
                                         then ((p, i + i'):xs)
                                         else ((p, i):(addPath xs v))

deleteAllCicles :: Graph -> Graph
deleteAllCicles g = deleteAllCicles' g (fst g) (length (fst g))
  where deleteAllCicles' :: Graph -> [Person] -> Int -> Graph
        deleteAllCicles' g _ 1 = g
        deleteAllCicles' g [] n = deleteAllCicles' g (fst g) (n - 1)
        deleteAllCicles' g@(l, ops) (x:xs) n = case M.lookup x ops of
                                                    Nothing -> g
                                                    Just m -> let arcs = getNLengthCicle' (l, ops) (M.toList m) n x x
                                                              in if (length arcs) == n
                                                                 then deleteAllCicles' (l, reduceDebt ops arcs) (x:xs) n
                                                                 else deleteAllCicles' g xs n
        getNLengthCicle' :: Graph -> [(Person, Int)] -> Int -> Person -> Person -> [Arc]
        getNLengthCicle' _ [] _ _ _ = []
        getNLengthCicle' g ((p, i):xs) 1 pi pf = if p == pf
                                                 then [((pi, pf), i)]
                                                 else getNLengthCicle' g xs 1 pi pf
        getNLengthCicle' g@(l, ops) ((p, i):xs) n pi pf = if elem p l
                                                          then ((pi, p), i):(getNLengthCicle' (delete p l, ops) (maybe [] M.toList (M.lookup p ops)) (n - 1) p pf)
                                                          else getNLengthCicle' g xs n pi pf        
        reduceDebt :: Operations -> [Arc] -> Operations
        reduceDebt ops arcs = reduceDebt' ops arcs (getMinDebt arcs)
        getMinDebt :: [Arc] -> Int
        getMinDebt arcs = minimum (map snd arcs)
        reduceDebt' :: Operations -> [Arc] -> Int -> Operations
        reduceDebt' ops [] _ = ops
        reduceDebt' ops (((p1, p2), i):xs) i' = case M.lookup p1 ops of
                                                     Nothing -> reduceDebt' ops xs i'
                                                     Just m -> if i == i'
                                                               then reduceDebt' (M.insert p1 (M.delete p2 m) ops) xs i'
                                                               else reduceDebt' (M.insert p1 (M.insert p2 (i - i') m) ops) xs i'

-- Toma un entorno y un nombre
-- Devuelve las operaciones hechas en ese grupo o persona
registry :: Env -> Name -> Either Error [(Name, (Person, Int))]
registry [] _ = Left NameNotFound
registry ((n, (_, o)):xs) n' = if n == n'
                          then Right [(n, (p, i)) | (p, i) <- o]
                          else registry xs n'

-- Toma un entorno y un nombre
-- Devuelve los nombres de ese grupo o el mismo nombre de no ser un grupo.
-- Si no existe ese nombre devuelve el error adecuado
members :: Env -> Name -> Either Error [Name]
members [] n = Left NameNotFound
members ((n, (l, _)):xs) n' = if n == n'
                              then Right l
                              else members xs n'
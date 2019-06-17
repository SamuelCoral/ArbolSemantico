module Expresion where

import qualified Data.Map as Map
import Data.Foldable
import Data.Maybe


insertarSeguroMapa :: (Ord k, Eq v) => k -> v -> Map.Map k v -> Maybe (Map.Map k v)
insertarSeguroMapa k v m = case Map.lookup k m of
    Nothing -> return $ Map.insert k v m
    Just f -> if f == v then return m else Nothing


unirSeguroMapa :: (Ord k, Eq v) => Map.Map k v -> Map.Map k v -> Maybe (Map.Map k v)
unirSeguroMapa m = foldrM (uncurry insertarSeguroMapa) m . Map.assocs


infixl 6 :|
infixl 7 :&
infixr 8 :>
data Expresion a =
    At a
    | No (Expresion a)
    | Expresion a :| Expresion a
    | Expresion a :& Expresion a
    | Expresion a :> Expresion a
    deriving (Eq, Read, Ord)


type Rama a = Map.Map (Expresion a) Bool

data Satisfacibilidad = Tautologia | Satisfacible | Insatisfacible 
    deriving Show


instance Show a => Show (Expresion a) where
    show e = case e of
        At p -> show p
        No p -> '~' : show p
        p :& q -> mostrarPar p q " ^ "
        p :| q -> mostrarPar p q " v "
        p :> q -> mostrarPar p q " -> "
        where mostrarPar p q o = "( " ++ show p ++ o ++ show q ++ " )"


equivalente :: Expresion a -> Expresion a
equivalente e = case e of
    No f -> case f of
        No p -> equivalente p
        p :& q -> No p :| No q
        p :| q -> No p :& No q
        p :> q -> p :& No q
        p -> e
    p :> q -> No p :| q
    p -> p


obtenerSigno :: Expresion a -> (Expresion a, Bool)
obtenerSigno e = case e of
    No p -> not <$> obtenerSigno p
    p -> (p, True)


crearArbolSemantico :: Ord a => Expresion a -> [Rama a]
crearArbolSemantico e = case equivalente e of
    p :| q -> crearArbolSemantico p ++ crearArbolSemantico q
    p :& q -> catMaybes [ unirSeguroMapa r1 r2 |
                r1 <- crearArbolSemantico p, r2 <- crearArbolSemantico q ]
    p -> [ uncurry Map.singleton $ obtenerSigno p ]


obtenerSatisfacibilidad :: Ord a => Expresion a -> (Satisfacibilidad, [Rama a])
obtenerSatisfacibilidad e =
    let ap = crearArbolSemantico e
        an = crearArbolSemantico $ No e
        s | null an = Tautologia
          | null ap = Insatisfacible
          | otherwise = Satisfacible
    in (s, ap)


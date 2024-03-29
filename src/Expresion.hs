{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase #-}
module Expresion where

import qualified Data.Map as Map
import qualified Data.Set as Set
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
    deriving (Eq, Read, Ord, Functor)


instance Applicative Expresion where
    pure = At
    f <*> t = case f of
        No p -> No $ p <*> t
        p :| q -> (p <*> t) :| (q <*> t)
        p :& q -> (p <*> t) :& (q <*> t)
        p :> q -> (p <*> t) :> (q <*> t)
        At p -> p <$> t


instance Monad Expresion where
    t >>= f = case t of
        No p -> No $ p >>= f
        p :| q -> (p >>= f) :| (q >>= f)
        p :& q -> (p >>= f) :& (q >>= f)
        p :> q -> (p >>= f) :> (q >>= f)
        At p -> f p


type ArbolSemantico a = Set.Set (Map.Map (Expresion a) Bool)

data Satisfacibilidad = Tautologia | Satisfacible | Insatisfacible 
    deriving Show


instance Show a => Show (Expresion a) where
    show = \ case
        At p -> show p
        No p -> '~' : show p
        p :& q -> mostrarPar p q " ^ "
        p :| q -> mostrarPar p q " v "
        p :> q -> mostrarPar p q " -> "
        where mostrarPar p q o = "( " ++ show p ++ o ++ show q ++ " )"


equivalente :: Expresion a -> Expresion a
equivalente = \ case
    No f -> case f of
        No p -> equivalente p
        p :& q -> No p :| No q
        p :| q -> No p :& No q
        p :> q -> p :& No q
        p -> No p
    p :> q -> No p :| q
    p -> p


obtenerSigno :: Expresion a -> (Expresion a, Bool)
obtenerSigno = \ case
    No p -> not <$> obtenerSigno p
    p -> (p, True)


crearArbolSemantico :: Ord a => Expresion a -> ArbolSemantico a
crearArbolSemantico e = case equivalente e of
    p :| q -> Set.union (crearArbolSemantico p) (crearArbolSemantico q)
    p :& q -> Set.fromList $ catMaybes [ unirSeguroMapa r1 r2 |
                r1 <- Set.toList $ crearArbolSemantico p,
                r2 <- Set.toList $ crearArbolSemantico q ]
    p -> Set.singleton $ uncurry Map.singleton $ obtenerSigno p


obtenerSatisfacibilidad :: Ord a => Expresion a -> (Satisfacibilidad, ArbolSemantico a)
obtenerSatisfacibilidad e =
    let ap = crearArbolSemantico e
        an = crearArbolSemantico $ No e
        s | null an = Tautologia
          | null ap = Insatisfacible
          | otherwise = Satisfacible
    in (s, ap)


evaluarExpresion :: Expresion Bool -> Bool
evaluarExpresion = \ case
    No p -> not $ evaluarExpresion p
    p :| q -> evaluarExpresion p || evaluarExpresion q
    p :& q -> evaluarExpresion p && evaluarExpresion q
    p :> q -> not (evaluarExpresion p) || evaluarExpresion q
    At p -> p


formatearPrefija :: Expresion String -> String
formatearPrefija e =
    let (d, s) = obtenerSigno e
        l | s = '+'
          | otherwise = '-'
    in l : case d of
        p :& q -> "&(" ++ formatearPrefija p ++ "," ++ formatearPrefija q ++ ")"
        p :| q -> "|(" ++ formatearPrefija p ++ "," ++ formatearPrefija q ++ ")"
        p :> q -> "/(" ++ formatearPrefija p ++ "," ++ formatearPrefija q ++ ")"
        At p -> p ++ "."


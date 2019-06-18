{-# LANGUAGE LambdaCase #-}
module Main where

import Expresion
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Maybe
import Text.Read
import System.Environment


formatearSatisfacibilidad :: (Ord a, Show a) => Expresion a -> String
formatearSatisfacibilidad e =
    let (s, a) = obtenerSatisfacibilidad e
    in unlines $ show e : show s : (show . Map.assocs <$> Set.toList a)


main :: IO ()
main = getArgs >>= \ case
    entrada : salida : _ -> do
        archivo <- readFile entrada
        let expresiones = catMaybes $ readMaybe <$> lines archivo :: [Expresion Char]
        writeFile salida $ unlines $ formatearSatisfacibilidad <$> expresiones
    _ -> putStrLn "Especifique un archivo de entrada y otro de salida"


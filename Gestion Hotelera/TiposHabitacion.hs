module TiposHabitacion where

import Modelos
import System.Directory (doesFileExist) -- Librería estándar de Haskell


separarPorComas :: String -> [String]
separarPorComas [] = []
separarPorComas texto =
    let (parte, resto) = break (== ',') texto
    in parte : if null resto then [] else separarPorComas (tail resto)

crearTipoDesdeLinea :: String -> TipoHabitacion
crearTipoDesdeLinea linea =
    let partes = separarPorComas linea
    in TipoHabitacion
        (partes !! 0)
        (partes !! 1)
        (read (partes !! 2) :: Int)

parsearLineas :: [String] -> [TipoHabitacion]
parsearLineas [] = []
parsearLineas (linea:resto) =
    crearTipoDesdeLinea linea : parsearLineas resto


leerArchivo :: FilePath -> IO [String]
leerArchivo ruta = do
    contenido <- readFile ruta
    return (lines contenido)

cargarTiposDesdeArchivo :: FilePath -> IO [TipoHabitacion]
cargarTiposDesdeArchivo ruta = do
    lineas <- leerArchivo ruta
    return (parsearLineas lineas)


obtenerNombres :: [TipoHabitacion] -> [String]
obtenerNombres [] = []
obtenerNombres (t:resto) = nombreTH t : obtenerNombres resto

nombreExiste :: String -> [String] -> Bool
nombreExiste _ [] = False
nombreExiste nombre (n:resto) =
    if nombre == n
        then True
        else nombreExiste nombre resto

filtrarNuevos :: [TipoHabitacion] -> [String] -> [TipoHabitacion]
filtrarNuevos [] _ = []
filtrarNuevos (t:resto) nombresExistentes =
    if nombreExiste (nombreTH t) nombresExistentes
        then filtrarNuevos resto nombresExistentes
        else t : filtrarNuevos resto nombresExistentes

procesarCargaTipos :: [TipoHabitacion] -> FilePath -> IO [TipoHabitacion]
procesarCargaTipos tiposExistentes ruta = do
    -- 1. Verificamos si el archivo existe antes de leerlo
    existe <- doesFileExist ruta
    
    if not existe
        then do
            putStrLn "\n[ERROR] El archivo no existe. Verifique la ruta e intente nuevamente."
            return tiposExistentes -- Devolvemos la lista sin cambios
        else do
            -- 2. Si existe, procedemos con la carga normal
            tiposDelArchivo <- cargarTiposDesdeArchivo ruta
            let nombresExistentes = obtenerNombres tiposExistentes
            let tiposNuevos = filtrarNuevos tiposDelArchivo nombresExistentes
            let tiposFinales = tiposExistentes ++ tiposNuevos
            
            putStrLn ("\n[OK] Se leyeron " ++ show (length tiposDelArchivo) ++ " tipos del archivo.")
            putStrLn ("[OK] Se agregaron " ++ show (length tiposNuevos) ++ " tipos nuevos.")
            putStrLn ("[OK] Total de tipos en el sistema: " ++ show (length tiposFinales))
            
            return tiposFinales

mostrarTipos :: [TipoHabitacion] -> IO ()
mostrarTipos [] = putStrLn "[AVISO] No hay tipos de habitación cargados."
mostrarTipos tipos = do
    putStrLn "--- Tipos de Habitación ---"
    mapM_ mostrarUnTipo tipos

mostrarUnTipo :: TipoHabitacion -> IO ()
mostrarUnTipo tipo = do
    putStrLn ("  - " ++ nombreTH tipo)
    putStrLn ("    Descripción: " ++ descripcion tipo)
    putStrLn ("    Máximo huéspedes: " ++ show (maxHuespedes tipo))
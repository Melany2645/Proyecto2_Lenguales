module TiposHabitacion where

import Modelos
import System.Directory (doesFileExist)
import System.IO (openFile, IOMode(ReadMode, WriteMode), hSetEncoding, latin1, hGetContents, hPutStr, hClose)

-- ============================================
-- 1. FUNCIONES AUXILIARES DE PARSING (Puras)
-- ============================================

separarPorComas :: String -> [String]
separarPorComas [] = []
separarPorComas texto =
    case break (== ',') texto of
        (parte, []) -> [parte]
        (parte, _:resto) -> parte : separarPorComas resto

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

-- ============================================
-- 2. FUNCIONES DE LECTURA Y ESCRITURA (IO)
-- ============================================

-- | Lee el archivo de forma segura (latin1 evita errores de Windows/WSL)
leerArchivo :: FilePath -> IO [String]
leerArchivo ruta = do
    handle <- openFile ruta ReadMode
    hSetEncoding handle latin1
    contenido <- hGetContents handle
    let lineas = lines contenido
    length lineas `seq` hClose handle
    return lineas

cargarTiposDesdeArchivo :: FilePath -> IO [TipoHabitacion]
cargarTiposDesdeArchivo ruta = do
    lineas <- leerArchivo ruta
    return (parsearLineas lineas)

-- | NUEVA FUNCIÓN: Convierte un TipoHabitacion a una línea de texto
tipoALinea :: TipoHabitacion -> String
tipoALinea tipo = 
    nombreTH tipo ++ "," ++ descripcion tipo ++ "," ++ show (maxHuespedes tipo)

-- | NUEVA FUNCIÓN: Guarda la lista de tipos en un archivo
guardarTiposEnArchivo :: [TipoHabitacion] -> FilePath -> IO ()
guardarTiposEnArchivo tipos ruta = do
    handle <- openFile ruta WriteMode
    hSetEncoding handle latin1
    let contenido = unlines (map tipoALinea tipos)
    hPutStr handle contenido
    hClose handle
    putStrLn ("[OK] Tipos de habitación guardados persistentemente en: " ++ ruta)

-- ============================================
-- 3. FUNCIONES DE FILTRADO (Puras)
-- ============================================

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

-- ============================================
-- 4. FUNCIÓN PRINCIPAL DE CARGA (Con persistencia)
-- ============================================

procesarCargaTipos :: [TipoHabitacion] -> FilePath -> IO [TipoHabitacion]
procesarCargaTipos tiposExistentes ruta = do
    existe <- doesFileExist ruta
    
    if not existe
        then do
            putStrLn "\n[ERROR] El archivo no existe. Verifique la ruta e intente nuevamente."
            return tiposExistentes
        else do
            tiposDelArchivo <- cargarTiposDesdeArchivo ruta
            let nombresExistentes = obtenerNombres tiposExistentes
            let tiposNuevos = filtrarNuevos tiposDelArchivo nombresExistentes
            let tiposFinales = tiposExistentes ++ tiposNuevos
            
            putStrLn ("\n[OK] Se leyeron " ++ show (length tiposDelArchivo) ++ " tipos del archivo.")
            putStrLn ("[OK] Se agregaron " ++ show (length tiposNuevos) ++ " tipos nuevos.")
            putStrLn ("[OK] Total de tipos en el sistema: " ++ show (length tiposFinales))
            
            -- AQUÍ ESTÁ LA MAGIA DE LA PERSISTENCIA (Puntos extra)
            -- Guardamos la lista actualizada en un archivo llamado "tipos_persistencia.txt"
            guardarTiposEnArchivo tiposFinales "tipos_persistencia.txt"
            
            return tiposFinales

-- ============================================
-- 5. FUNCIONES DE MOSTRAR
-- ============================================

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
module Tarifas where

import Modelos
import System.Directory (doesFileExist)
import System.IO (openFile, IOMode(ReadMode, WriteMode), hSetEncoding, latin1, hGetContents, hPutStr, hClose)
import Text.Read (readMaybe)
import Data.Maybe (mapMaybe)


-- 1. convertir una línea de texto en una tarifa
-- E: Una lista de líneas de texto, cada línea con formato "codigo,monto"
-- S: Una lista de tarifas válidas, ignorando líneas inválidas
-- R: Valores válidos
-- F: Se encarga de separar la línea por comas, convertir a números y validar el rango de código y monto.
separarPorComas :: String -> [String]
separarPorComas [] = []
separarPorComas texto =
    case break (== ',') texto of
        (parte, []) -> [parte]
        (parte, _:resto) -> parte : separarPorComas resto

-- E: Una línea de texto con formato "codigo,monto"
-- S: Maybe Tarifa, donde Just Tarifa si es válida, Nothing si no lo es
-- R: Valores válidos
-- F: Se encarga de separar la línea por comas, convertir a números y validar el rango de código y monto.
crearTarifaDesdeLinea :: String -> Maybe Tarifa
crearTarifaDesdeLinea linea =
    case separarPorComas linea of
        [codigoTexto, montoTexto] ->
            case (readMaybe codigoTexto, readMaybe montoTexto) of
                (Just codigo, Just monto) ->
                    if codigo >= 1 && codigo <= 8 && monto > 0
                        then Just (Tarifa codigo monto)
                        else Nothing
                _ -> Nothing
        _ -> Nothing

parsearTarifas :: [String] -> [Tarifa]
parsearTarifas lineas = mapMaybe crearTarifaDesdeLinea lineas


-- 2. LEER TARIFAS DESDE EL ARCHIVO


-- E: Una ruta de archivo
-- S: Una lista de líneas de texto leídas del archivo
-- R: Valores válidos
-- F: Se encarga de abrir el archivo, leer su contenido y devolver las líneas como una lista de strings.
leerArchivoTarifas :: FilePath -> IO [String]
leerArchivoTarifas ruta = do
    handle <- openFile ruta ReadMode
    hSetEncoding handle latin1  -- Evita errores de decodificación en Windows/WSL
    contenido <- hGetContents handle
    let lineas = lines contenido
    length lineas `seq` hClose handle
    return lineas

cargarTarifasDesdeArchivo :: FilePath -> IO [Tarifa]
cargarTarifasDesdeArchivo ruta = do
    lineas <- leerArchivoTarifas ruta
    return (parsearTarifas lineas)

-- 3. GUARDAR TARIFAS EN EL ARCHIVO

-- E: Una tarifa
-- S: Una línea de texto con formato "codigo,monto"
-- R: Valores válidos
-- F: Se encarga de convertir una tarifa en una línea de texto para guardarla en el archivo.
tarifaALinea :: Tarifa -> String
tarifaALinea tarifa =
    show (codigoTarifa tarifa) ++ "," ++ show (montoTarifa tarifa)


-- E: Una lista de tarifas y una ruta de archivo
-- S: Las tarifas guardadas en el archivo
-- R: Valores válidos
-- F: Se encarga de guardar una lista de tarifas en un archivo de texto.
guardarTarifasEnArchivo :: [Tarifa] -> FilePath -> IO ()
guardarTarifasEnArchivo tarifas ruta = do
    handle <- openFile ruta WriteMode
    hSetEncoding handle latin1
    hPutStr handle (unlines (map tarifaALinea tarifas))
    hClose handle
    putStrLn ("[OK] Tarifas guardadas persistentemente en: " ++ ruta)


-- 4. FUNCIÓN PRINCIPAL DE CARGA (Reemplazo total)

-- E: Una ruta de archivo
-- S: Una lista de tarifas cargadas
-- R: Valores válidos
-- F: Carga las tarifas desde un archivo, REEMPLAZA las anteriores y guarda en persistencia
procesarCargaTarifas :: FilePath -> IO [Tarifa]
procesarCargaTarifas ruta = do
    existe <- doesFileExist ruta
    
    if not existe
        then do
            putStrLn "\n[ERROR] El archivo no existe. Verifique la ruta."
            return []
        else do
            tarifasNuevas <- cargarTarifasDesdeArchivo ruta
            
            -- Validación: el PDF pide exactamente 8 tarifas
            if length tarifasNuevas == 8
                then putStrLn "\n[OK] Se cargaron las 8 tarifas correctamente."
                else do
                    putStrLn "\n[AVISO] El archivo no contiene exactamente 8 tarifas válidas."
                    putStrLn ("[INFO] Se cargaron " ++ show (length tarifasNuevas) ++ " tarifas.")
            
            -- REEMPLAZO TOTAL: Guardamos las nuevas en el archivo de persistencia
            guardarTarifasEnArchivo tarifasNuevas "tarifas_persistencia.txt"
            
            return tarifasNuevas


-- 5. BUSCAR UNA TARIFA POR SU CÓDIGO (Útil para Reservaciones)

-- E: Un código de tarifa y una lista de tarifas
-- S: Maybe Tarifa, donde Just Tarifa si se encuentra, Nothing si no
-- R: Valores válidos
-- F: Busca una tarifa por su código en una lista de tarifas.
buscarTarifa :: Int -> [Tarifa] -> Maybe Tarifa
buscarTarifa codigo [] = Nothing
buscarTarifa codigo (tarifa:resto) =
    if codigo == codigoTarifa tarifa
        then Just tarifa
        else buscarTarifa codigo resto

-- E: Un código de tarifa y una lista de tarifas
-- S: El monto de la tarifa si se encuentra, 0.0 si no
-- R: Valores válidos
-- F: Obtiene el monto de una tarifa por su código.
obtenerMontoTarifa :: Int -> [Tarifa] -> Double
obtenerMontoTarifa codigo tarifas =
    case buscarTarifa codigo tarifas of
        Just tarifa -> montoTarifa tarifa
        Nothing -> 0.0


-- 6. MOSTRAR LAS TARIFAS

-- E: Una lista de tarifas
-- S: Muestra en pantalla las tarifas del hotel
-- R: Valores válidos
-- F: Muestra en pantalla las tarifas del hotel.
mostrarTarifas :: [Tarifa] -> IO ()
mostrarTarifas [] = putStrLn "[AVISO] No hay tarifas cargadas."
mostrarTarifas tarifas = do
    putStrLn "\n========== TARIFAS DEL HOTEL =========="
    mapM_ mostrarUnaTarifa tarifas
    putStrLn "========================================"

mostrarUnaTarifa :: Tarifa -> IO ()
mostrarUnaTarifa tarifa = do
    putStrLn ("  Código: " ++ show (codigoTarifa tarifa) 
           ++ " | Monto: CRC" ++ show (montoTarifa tarifa))


-- 7. REGISTRAR O ACTUALIZAR UNA TARIFA MANUALMENTE (Bonus)

-- E: Una lista de tarifas
-- S: Una lista de tarifas actualizada después de registrar o actualizar una tarifa manualmente
-- R: Valores válidos
-- F: Permite al usuario registrar o actualizar una tarifa manualmente, validando el código y el monto, y guardando los cambios en el archivo de persistencia.
registrarTarifaManual :: [Tarifa] -> IO [Tarifa]
registrarTarifaManual tarifas = do
    putStrLn "\n========== REGISTRAR TARIFA MANUAL =========="
    putStrLn "Ingrese un código del 1 al 8:"
    codigoTexto <- getLine

    case readMaybe codigoTexto of
        Nothing -> do
            putStrLn "[ERROR] Debe ingresar un número entero."
            return tarifas
        Just codigo ->
            if codigo < 1 || codigo > 8
                then do
                    putStrLn "[ERROR] El código debe estar entre 1 y 8."
                    return tarifas
                else do
                    putStrLn "Ingrese el monto de la tarifa:"
                    montoTexto <- getLine

                    case readMaybe montoTexto of
                        Nothing -> do
                            putStrLn "[ERROR] El monto no es válido."
                            return tarifas
                        Just monto ->
                            if monto <= 0
                                then do
                                    putStrLn "[ERROR] El monto debe ser mayor que cero."
                                    return tarifas
                                else do
                                    let nuevaTarifa = Tarifa codigo monto
                                    -- Reemplaza la tarifa si ya existía, o la agrega
                                    let otrasTarifas = filter (\t -> codigoTarifa t /= codigo) tarifas
                                    let tarifasActualizadas = otrasTarifas ++ [nuevaTarifa]
                                    
                                    guardarTarifasEnArchivo tarifasActualizadas "tarifas_persistencia.txt"
                                    putStrLn "[OK] Tarifa registrada y guardada."
                                    return tarifasActualizadas
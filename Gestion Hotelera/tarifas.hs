
module Tarifas where

import Modelos
import System.Directory (doesFileExist)
import System.IO (openFile, IOMode(ReadMode, WriteMode), hSetEncoding, latin1, hGetContents, hPutStr, hClose)
import Text.Read (readMaybe)
import Data.Maybe (mapMaybe)

-- ============================================
-- 1. CONVERTIR LINEAS A TARIFAS
-- ============================================

separarPorComas :: String -> [String]
separarPorComas [] = []
separarPorComas texto =
    case break (== ',') texto of
        (parte, []) -> [parte]
        (parte, _:resto) -> parte : separarPorComas resto

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
parsearTarifas lineas =
    mapMaybe crearTarifaDesdeLinea lineas

-- ============================================
-- 2. LEER TARIFAS DESDE EL ARCHIVO
-- ============================================

leerArchivoTarifas :: FilePath -> IO [String]
leerArchivoTarifas ruta = do
    handle <- openFile ruta ReadMode
    hSetEncoding handle latin1
    contenido <- hGetContents handle
    let lineas = lines contenido
    length lineas `seq` hClose handle
    return lineas

cargarTarifas :: FilePath -> IO [Tarifa]
cargarTarifas ruta = do
    existe <- doesFileExist ruta

    if existe
        then do
            lineas <- leerArchivoTarifas ruta
            return (parsearTarifas lineas)
        else
            return []

-- ============================================
-- 3. GUARDAR TARIFAS EN EL ARCHIVO
-- ============================================

tarifaALinea :: Tarifa -> String
tarifaALinea tarifa =
    show (codigoTarifa tarifa) ++ "," ++ show (montoTarifa tarifa)

guardarTarifas :: [Tarifa] -> FilePath -> IO ()
guardarTarifas tarifas ruta = do
    handle <- openFile ruta WriteMode
    hSetEncoding handle latin1
    hPutStr handle (unlines (map tarifaALinea tarifas))
    hClose handle
    putStrLn "[OK] Tarifas guardadas correctamente."

-- ============================================
-- 4. BUSCAR UNA TARIFA POR SU CODIGO
-- ============================================

buscarTarifa :: Int -> [Tarifa] -> Maybe Tarifa
buscarTarifa codigo [] = Nothing
buscarTarifa codigo (tarifa:resto) =
    if codigo == codigoTarifa tarifa
        then Just tarifa
        else buscarTarifa codigo resto

obtenerMontoTarifa :: Int -> [Tarifa] -> Double
obtenerMontoTarifa codigo tarifas =
    case buscarTarifa codigo tarifas of
        Just tarifa -> montoTarifa tarifa
        Nothing -> 0

-- ============================================
-- 5. MOSTRAR LAS TARIFAS
-- ============================================

mostrarTarifas :: [Tarifa] -> IO ()
mostrarTarifas [] =
    putStrLn "[AVISO] No hay tarifas cargadas."

mostrarTarifas tarifas = do
    putStrLn "\n========== TARIFAS DEL HOTEL =========="
    mapM_ mostrarUnaTarifa tarifas

mostrarUnaTarifa :: Tarifa -> IO ()
mostrarUnaTarifa tarifa = do
    putStrLn
        ("Codigo: " ++ show (codigoTarifa tarifa)
        ++ " | Monto: " ++ show (montoTarifa tarifa))

-- ============================================
-- 6. REGISTRAR O ACTUALIZAR UNA TARIFA
-- ============================================

registrarTarifa :: [Tarifa] -> IO [Tarifa]
registrarTarifa tarifas = do
    putStrLn "\n========== REGISTRAR TARIFA =========="
    putStrLn "Ingrese un codigo del 1 al 8:"
    codigoTexto <- getLine

    case readMaybe codigoTexto of
        Nothing -> do
            putStrLn "[ERROR] Debe ingresar un numero entero."
            return tarifas

        Just codigo ->
            if codigo < 1 || codigo > 8
                then do
                    putStrLn "[ERROR] El codigo debe estar entre 1 y 8."
                    return tarifas
                else do
                    putStrLn "Ingrese el monto de la tarifa:"
                    montoTexto <- getLine

                    case readMaybe montoTexto of
                        Nothing -> do
                            putStrLn "[ERROR] El monto no es valido."
                            return tarifas

                        Just monto ->
                            if monto <= 0
                                then do
                                    putStrLn "[ERROR] El monto debe ser mayor que cero."
                                    return tarifas
                                else do
                                    let nuevaTarifa = Tarifa codigo monto
                                    let otrasTarifas =
                                            filter
                                                (\t -> codigoTarifa t /= codigo)
                                                tarifas
                                    let tarifasActualizadas =
                                            otrasTarifas ++ [nuevaTarifa]

                                    guardarTarifas tarifasActualizadas "tarifas.txt"
                                    putStrLn "[OK] Tarifa registrada."
                                    return tarifasActualizadas

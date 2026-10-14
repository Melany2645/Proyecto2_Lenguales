module Reservaciones where

-- PERSISTENCIA DE RESERVAS

-- Este archivo complementa la gestión de reservas con la capa de almacenamiento.
-- Tiene como objetivo cargar y guardar la lista de reservas desde un archivo de texto,
-- de tal modo que la información del sistema pueda persistir entre ejecuciones.

-- Su función principal es permitir que el sistema hotelero pueda iniciar con un historial de reservas existente,
-- o bien crear un nuevo historial si no existe un archivo previo.

import Modelos
import System.Directory (doesFileExist)
import Text.Read (readMaybe)

-- CARGA Y GUARDADO DE RESERVACIONES

-- E: Ruta del archivo de reservas
-- S: Lista de reservas cargadas
-- R: Una ruta válida
-- F: Se encarga de leer el archivo de reservas y convertirlo en una lista de reservas.
cargarReservaciones :: FilePath -> IO [Reserva]
cargarReservaciones ruta = do
    existe <- doesFileExist ruta
    if not existe
        then do
            putStrLn "[INFO] No existe archivo de reservaciones. Iniciando vacío."
            return []
        else do
            contenido <- readFile ruta
            let lineas = filter (not . null) (lines contenido)
            if null lineas
                then return []
                else case traverse readMaybe lineas of
                    Just reservas -> return reservas
                    Nothing -> do
                        putStrLn "[ERROR] El archivo de reservaciones tiene formato inválido."
                        putStrLn "[INFO] Iniciando con lista vacía."
                        return []

-- E: La ruta de archivo y la lista de reservas a guardar
-- S: Ninguna salida, pero se guarda en el archivo
-- R: Una ruta válida y permisos de escritura
-- F: Se encarga de escribir la lista de reservas en el archivo especificado, sobrescribiendo cualquier contenido previo.
guardarReservaciones :: FilePath -> [Reserva] -> IO ()
guardarReservaciones ruta reservas = do
    writeFile ruta (unlines (map show reservas))
    putStrLn ("[OK] Reservaciones guardadas en: " ++ ruta)

-- Consultar reservaciones

-- E: Lista de reservas a consultar
-- S: Salida por pantalla con la información de las reservas
-- R: Una lista de reservas válida
-- F: Se encarga de mostrar la información completa de cada reserva en la consola.
consultarReservaciones :: [Reserva] -> IO ()
consultarReservaciones [] = do
    putStrLn "\n[AVISO] No hay reservaciones registradas."

consultarReservaciones reservas = do
    putStrLn "\n========== HISTORIAL DE RESERVACIONES =========="
    putStrLn ("Total: " ++ show (length reservas) ++ " reservaciones\n")
    mapM_ mostrarReservaCompleta reservas
    putStrLn "================================================"

-- E: Una reserva
-- S: Salida por pantalla con la información de la reserva
-- R: Una reserva válida
-- F: Se encarga de mostrar la información completa de una reserva en la consola.
mostrarReservaCompleta :: Reserva -> IO ()
mostrarReservaCompleta reserva = do
    putStrLn ("--- Reserva #" ++ show (idReserva reserva) ++ " ---")
    putStrLn ("  Cliente: " ++ nombreCliente reserva)
    putStrLn ("  Fecha de registro: " ++ fechaReserva reserva)
    putStrLn ("  Entrada: " ++ fechaEntrada reserva)
    putStrLn ("  Salida: " ++ fechaSalida reserva)
    putStrLn ("  Adultos: " ++ show (totalAdultos reserva))
    putStrLn ("  Niños: " ++ show (totalNinos reserva))
    putStrLn ("  Estado: " ++ show (estadoReserva reserva))
    putStrLn ("  Subtotal: CRC" ++ show (subtotalReserva reserva))
    putStrLn ("  Habitaciones:")
    mapM_ mostrarOcupacionDetalle (ocupaciones reserva)
    putStrLn ""

-- E: Una ocupación de habitación
-- S: Salida por pantalla con la información de la ocupación
-- R: Una ocupación válida
-- F: Se encarga de mostrar la información detallada de la ocupación de una habitación dentro de una reserva.
mostrarOcupacionDetalle :: OcupacionHab -> IO ()
mostrarOcupacionDetalle ocupacion = do
    let hab = habitacionOcupada ocupacion
    putStrLn ("    - " ++ idHabitacion hab 
           ++ " (" ++ nombreTH (tipoHabitacion hab) ++ ")"
           ++ " | Adultos: " ++ show (adultosHab ocupacion)
           ++ " | Niños: " ++ show (ninosHab ocupacion))
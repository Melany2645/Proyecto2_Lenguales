module Reservaciones where

import Modelos
import System.Directory (doesFileExist)
import Text.Read (readMaybe)

-- ============================================
-- CARGA Y GUARDADO DE RESERVACIONES
-- ============================================

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

guardarReservaciones :: FilePath -> [Reserva] -> IO ()
guardarReservaciones ruta reservas = do
    writeFile ruta (unlines (map show reservas))
    putStrLn ("[OK] Reservaciones guardadas en: " ++ ruta)

-- ============================================
-- CONSULTAR RESERVACIONES (Historial completo)
-- ============================================

consultarReservaciones :: [Reserva] -> IO ()
consultarReservaciones [] = do
    putStrLn "\n[AVISO] No hay reservaciones registradas."

consultarReservaciones reservas = do
    putStrLn "\n========== HISTORIAL DE RESERVACIONES =========="
    putStrLn ("Total: " ++ show (length reservas) ++ " reservaciones\n")
    mapM_ mostrarReservaCompleta reservas
    putStrLn "================================================"

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
    putStrLn ("  Subtotal: ₡" ++ show (subtotalReserva reserva))
    putStrLn ("  Habitaciones:")
    mapM_ mostrarOcupacionDetalle (ocupaciones reserva)
    putStrLn ""

mostrarOcupacionDetalle :: OcupacionHab -> IO ()
mostrarOcupacionDetalle ocupacion = do
    let hab = habitacionOcupada ocupacion
    putStrLn ("    - " ++ idHabitacion hab 
           ++ " (" ++ nombreTH (tipoHabitacion hab) ++ ")"
           ++ " | Adultos: " ++ show (adultosHab ocupacion)
           ++ " | Niños: " ++ show (ninosHab ocupacion))
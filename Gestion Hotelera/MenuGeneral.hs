module MenuGeneral where

import Modelos
import qualified ReservasUsuario as R
import qualified Ocupacion as O
import qualified Facturacion as F


-- F: menuGeneral
-- E: Estado actual del sistema
-- S: Estado actualizado del sistema
-- O: Mostrar las opciones generales del hotel
--    y guardar los cambios realizados

menuGeneral :: EstadoSistema -> IO EstadoSistema
menuGeneral sistema = do

    putStrLn ""
    putStrLn "========================================"
    putStrLn "          OPCIONES GENERALES"
    putStrLn "========================================"
    putStrLn "1. Ver disponibilidad"
    putStrLn "2. Reservacion"
    putStrLn "3. Anular reservacion"
    putStrLn "4. Facturar reservacion"
    putStrLn "5. Volver"
    putStrLn "========================================"
    putStrLn "Seleccione una opcion:"

    opcion <- getLine

    case opcion of

        -- OPCION 1: VER DISPONIBILIDAD
        "1" -> do
            O.verDisponibilidad
                (estadoHab sistema)
                (estadoReservas sistema)

            menuGeneral sistema


        -- OPCION 2: REGISTRAR RESERVACION
        "2" -> do
            nuevoSistema <- R.registrarReservacion sistema

            menuGeneral nuevoSistema


        -- OPCION 3: ANULAR RESERVACION
        "3" -> do
            nuevoSistema <- R.cancelarReservacion sistema

            menuGeneral nuevoSistema


        -- OPCION 4: FACTURAR RESERVACION
        "4" -> do
            nuevoSistema <- F.menuFacturacion sistema

            menuGeneral nuevoSistema


        -- OPCION 5: VOLVER
        "5" -> do
            putStrLn "Regresando al menu anterior..."

            return sistema


        -- OPCION INCORRECTA
        _ -> do
            putStrLn "ERROR: Opcion no valida."
            putStrLn "Seleccione una opcion del 1 al 5."

            menuGeneral sistema
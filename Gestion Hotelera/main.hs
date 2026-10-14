module Main where

import Modelos

import qualified MenuGeneral as G
import qualified MenuAdministrativo as A
import qualified TiposHabitacion as T
import qualified CantDeHabitaciones as H
import qualified Reservaciones as R
import qualified Facturacion as F

import System.Directory (doesFileExist)
import System.IO (hFlush, stdout)


-- ============================================
-- CARGAR DATOS DEL SISTEMA
-- ============================================

-- F: cargarSistema
-- E: Archivos TXT del proyecto
-- S: EstadoSistema con datos cargados
-- O: Recuperar la informacion guardada

cargarSistema :: IO EstadoSistema
cargarSistema = do

    -- CARGAR TIPOS DE HABITACION

    existeTipos <- doesFileExist "tipos_persistencia.txt"

    tipos <-
        if existeTipos
            then do

                lineas <- T.leerArchivo "tipos_persistencia.txt"

                return (T.parsearLineas lineas)

            else return []


    -- CARGAR HABITACIONES

    existeHabitaciones <-
        doesFileExist "habitaciones_persistencia.txt"

    habitaciones <-
        if existeHabitaciones
            then do

                contenido <-
                    readFile "habitaciones_persistencia.txt"

                let lineas =
                        filter (not . null) (lines contenido)

                return (map H.lineaAHabitacion lineas)

            else return []


    -- CARGAR TARIFAS

    existeTarifas <-
        doesFileExist "tarifas_persistencia.txt"

    tarifas <-
        if existeTarifas
            then do

                contenido <-
                    readFile "tarifas_persistencia.txt"

                let lineas =
                        filter (not . null) (lines contenido)

                return (map A.parsearLineaTarifa lineas)

            else return []


    -- CARGAR RESERVACIONES

    reservas <-
        R.cargarReservaciones "reservaciones.txt"


    -- CARGAR FACTURAS

    facturas <- F.cargarFacturas

    -- CALCULAR SIGUIENTE NUMERO DE FACTURA
    let siguienteFactura =
            if null facturas
                then 1
                else maximum (map idFactura facturas) + 1

    putStrLn ("Facturas cargadas: " ++ show (length facturas))


    -- CALCULAR SIGUIENTE NUMERO DE RESERVACION

    let ultimoIdReserva =
            if null reservas
                then 0
                else maximum (map idReserva reservas)


    -- CALCULAR SIGUIENTE NUMERO DE FACTURA

    let ultimoIdFactura =
            if null facturas
                then 0
                else maximum (map idFactura facturas)


    -- MOSTRAR RESUMEN

    putStrLn ""
    putStrLn "========================================"
    putStrLn "           DATOS CARGADOS"
    putStrLn "========================================"

    putStrLn
        ("Tipos de habitacion: " ++ show (length tipos))

    putStrLn
        ("Habitaciones: " ++ show (length habitaciones))

    putStrLn
        ("Tarifas: " ++ show (length tarifas))

    putStrLn
        ("Reservaciones: " ++ show (length reservas))

    putStrLn
        ("Facturas: " ++ show (length facturas))

    putStrLn "========================================"


    -- CREAR ESTADO DEL SISTEMA

    return estadoInicial
        {
          estadoTipos = tipos

        , estadoHab = habitaciones

        , estadoTarifas = tarifas

        , estadoReservas = reservas

        , estadoFacturas = facturas

        , nextIdReserva = ultimoIdReserva + 1

        , nextIdFactura = ultimoIdFactura + 1
        }


-- ============================================
-- MENU PRINCIPAL
-- ============================================

-- F: menuPrincipal
-- E: Estado del sistema
-- S: Ejecucion del programa
-- O: Conectar las opciones generales y administrativas

menuPrincipal :: EstadoSistema -> IO ()
menuPrincipal sistema = do

    putStrLn ""
    putStrLn "========================================"
    putStrLn "       SISTEMA DE GESTION HOTELERA"
    putStrLn "========================================"
    putStrLn "1. Opciones generales"
    putStrLn "2. Opciones administrativas"
    putStrLn "3. Salir"
    putStrLn "========================================"

    putStr "Seleccione una opcion: "
    hFlush stdout

    opcion <- getLine

    case opcion of

        -- OPCIONES GENERALES

        "1" -> do

            nuevoSistema <- G.menuGeneral sistema

            menuPrincipal nuevoSistema


        -- OPCIONES ADMINISTRATIVAS

        "2" -> do

            nuevoSistema <- A.menuAdministrativo sistema

            menuPrincipal nuevoSistema


        -- SALIR

        "3" -> do

            putStrLn ""
            putStrLn "Programa finalizado."


        -- OPCION INVALIDA

        _ -> do

            putStrLn "[ERROR] Opcion no valida."

            menuPrincipal sistema


-- ============================================
-- INICIAR PROGRAMA
-- ============================================

-- F: main
-- E: Ninguna
-- S: Programa en ejecucion
-- O: Cargar los datos y mostrar el menu principal

main :: IO ()
main = do

    putStrLn ""
    putStrLn "Iniciando sistema de gestion hotelera..."

    sistema <- cargarSistema

    menuPrincipal sistema

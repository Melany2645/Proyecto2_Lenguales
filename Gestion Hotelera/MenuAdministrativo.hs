
module MenuAdministrativo where

import Modelos
import HotelInformacion hiding (pausar)
import TiposHabitacion
import CantDeHabitaciones
import System.Directory (doesFileExist)

import qualified Tarifas as T
import qualified Reservaciones as R

-- ============================================
-- FUNCIONES AUXILIARES
-- ============================================

limpiarPantalla :: IO ()
limpiarPantalla = putStr "\ESC[2J\ESC[H"

pausar :: IO ()
pausar = do
    putStrLn "\nPresione ENTER para continuar..."
    _ <- getLine
    return ()

mostrarMenu :: IO String
mostrarMenu = do
    limpiarPantalla
    putStrLn "=================================================="
    putStrLn "       SISTEMA DE GESTION HOTELERA"
    putStrLn "             MENU ADMINISTRATIVO"
    putStrLn "=================================================="
    putStrLn "1. Informacion del hotel"
    putStrLn "2. Cargar tipos de habitaciones"
    putStrLn "3. Asignar habitaciones por tipo"
    putStrLn "4. Ver habitaciones"
    putStrLn "5. Carga de tarifas"
    putStrLn "6. Consultar reservaciones"
    putStrLn "7. Consultar disponibilidad"
    putStrLn "8. Estadisticas de ocupacion"
    putStrLn "9. Salir"
    putStrLn "=================================================="
    putStr "Seleccione una opcion: "
    getLine

-- ============================================
-- OPCION 1: INFORMACION DEL HOTEL
-- ============================================

opcionInfoHotel :: Maybe HotelInfo -> IO (Maybe HotelInfo)
opcionInfoHotel infoActual = do
    limpiarPantalla
    putStrLn "=== INFORMACION DEL HOTEL ==="
    putStrLn "1. Cargar informacion del hotel"
    putStrLn "2. Ver informacion actual"
    putStrLn "3. Guardar informacion en archivo"
    putStrLn "4. Cargar informacion desde archivo"
    putStrLn "5. Volver"
    putStr "Seleccione una opcion: "
    opcion <- getLine

    case opcion of
        "1" -> do
            nuevaInfo <- cargarInfoHotel infoActual
            pausar
            return nuevaInfo

        "2" -> do
            mostrarInfoHotel infoActual
            pausar
            return infoActual

        "3" -> do
            guardarInfoHotel infoActual "hotel_info.txt"
            pausar
            return infoActual

        "4" -> do
            infoLeida <- leerInfoHotel infoActual "hotel_info.txt"
            pausar
            return infoLeida

        _ -> return infoActual

-- ============================================
-- OPCION 2: CARGAR TIPOS
-- ============================================

opcionCargarTipos :: [TipoHabitacion] -> IO [TipoHabitacion]
opcionCargarTipos tiposActuales = do
    limpiarPantalla
    putStrLn "=== CARGAR TIPOS DE HABITACION ==="

    mostrarTipos tiposActuales

    putStr "\nRuta del archivo de tipos: "
    ruta <- getLine

    if null ruta || all (== ' ') ruta
        then do
            putStrLn "La ruta no puede estar vacia."
            pausar
            return tiposActuales
        else do
            nuevosTipos <- procesarCargaTipos tiposActuales ruta
            pausar
            return nuevosTipos

-- ============================================
-- OPCION 3: ASIGNAR HABITACIONES
-- ============================================

opcionAsignarHabitaciones ::
    [Habitacion] -> [TipoHabitacion] -> IO [Habitacion]

opcionAsignarHabitaciones habitaciones tipos = do
    limpiarPantalla
    putStrLn "=== ASIGNAR HABITACIONES ==="
    nuevasHabitaciones <- asignarHabitaciones habitaciones tipos
    pausar
    return nuevasHabitaciones

-- ============================================
-- OPCION 4: VER HABITACIONES
-- ============================================

opcionVerHabitaciones :: [Habitacion] -> IO ()
opcionVerHabitaciones habitaciones = do
    limpiarPantalla
    putStrLn "=== HABITACIONES DEL HOTEL ==="
    CantDeHabitaciones.mostrarHabitaciones habitaciones
    pausar

-- ============================================
-- OPCION 5: TARIFAS
-- ============================================

opcionCargarTarifas :: [Tarifa] -> IO [Tarifa]
opcionCargarTarifas tarifasActuales = do
    limpiarPantalla
    putStrLn "=== CARGA DE TARIFAS ==="

    T.mostrarTarifas tarifasActuales

    putStrLn "\n1. Registrar o actualizar tarifa"
    putStrLn "2. Volver"
    putStr "Seleccione una opcion: "
    opcion <- getLine

    case opcion of
        "1" -> do
            tarifasNuevas <- T.registrarTarifa tarifasActuales
            pausar
            return tarifasNuevas

        _ -> return tarifasActuales

-- ============================================
-- OPCION 6: RESERVACIONES
-- ============================================

opcionConsultarReservas :: EstadoSistema -> IO EstadoSistema
opcionConsultarReservas sistema = do
    limpiarPantalla
    putStrLn "=== GESTION DE RESERVACIONES ==="

    putStrLn "\n1. Consultar todas las reservaciones"
    putStrLn "2. Registrar reservacion"
    putStrLn "3. Cancelar reservacion"
    putStrLn "4. Volver"
    putStr "Seleccione una opcion: "
    opcion <- getLine

    case opcion of
        "1" -> do
            R.consultarReservaciones (estadoReservas sistema)
            pausar
            return sistema

        "2" -> do
            sistemaNuevo <- R.registrarReservacion sistema
            pausar
            return sistemaNuevo

        "3" -> do
            sistemaNuevo <- R.cancelarReservacion sistema
            pausar
            return sistemaNuevo

        _ -> return sistema

-- ============================================
-- OPCION 7: DISPONIBILIDAD
-- ============================================

opcionConsultarOcupacion :: EstadoSistema -> IO ()
opcionConsultarOcupacion sistema = do
    limpiarPantalla
    R.consultarDisponibilidad sistema
    pausar

-- ============================================
-- OPCION 8: ESTADISTICAS
-- ============================================

opcionEstadisticas :: IO ()
opcionEstadisticas = do
    limpiarPantalla
    putStrLn "=== ESTADISTICAS DE OCUPACION ==="
    putStrLn "Esta opcion esta pendiente de integracion."
    pausar

-- ============================================
-- MENU PRINCIPAL
-- ============================================

menuAdministrativo :: EstadoSistema -> IO EstadoSistema
menuAdministrativo sistema = do
    opcion <- mostrarMenu

    case opcion of
        "1" -> do
            nuevaInfo <- opcionInfoHotel (estadoInfo sistema)
            let sistemaNuevo = sistema { estadoInfo = nuevaInfo }
            menuAdministrativo sistemaNuevo

        "2" -> do
            nuevosTipos <- opcionCargarTipos (estadoTipos sistema)
            let sistemaNuevo = sistema { estadoTipos = nuevosTipos }
            menuAdministrativo sistemaNuevo

        "3" -> do
            nuevasHabitaciones <-
                opcionAsignarHabitaciones
                    (estadoHab sistema)
                    (estadoTipos sistema)

            let sistemaNuevo = sistema { estadoHab = nuevasHabitaciones }
            menuAdministrativo sistemaNuevo

        "4" -> do
            opcionVerHabitaciones (estadoHab sistema)
            menuAdministrativo sistema

        "5" -> do
            nuevasTarifas <- opcionCargarTarifas (estadoTarifas sistema)
            let sistemaNuevo = sistema { estadoTarifas = nuevasTarifas }
            menuAdministrativo sistemaNuevo

        "6" -> do
            sistemaNuevo <- opcionConsultarReservas sistema
            menuAdministrativo sistemaNuevo

        "7" -> do
            opcionConsultarOcupacion sistema
            menuAdministrativo sistema

        "8" -> do
            opcionEstadisticas
            menuAdministrativo sistema

        "9" -> do
            limpiarPantalla
            putStrLn "Saliendo del menu administrativo..."
            return sistema

        _ -> do
            putStrLn "Opcion no valida."
            pausar
            menuAdministrativo sistema

-- ============================================
-- CARGA INICIAL
-- ============================================

main :: IO ()
main = do
    putStrLn "Iniciando sistema de gestion hotelera..."

    -- Cargar tipos de habitacion
    existeTipos <- doesFileExist "tipos_persistencia.txt"

    tiposIniciales <-
        if existeTipos
            then do
                lineas <- leerArchivo "tipos_persistencia.txt"
                return (parsearLineas lineas)
            else
                return []

    -- Cargar habitaciones
    existeHabitaciones <-
        doesFileExist "habitaciones_persistencia.txt"

    habitacionesIniciales <-
        if existeHabitaciones
            then do
                contenido <- readFile "habitaciones_persistencia.txt"

                let lineas = filter (not . null) (lines contenido)

                return (map lineaAHabitacion lineas)
            else
                return []

    -- Cargar tarifas
    tarifasIniciales <- T.cargarTarifas "tarifas.txt"

    -- Cargar reservaciones
    reservasIniciales <- R.cargarReservaciones "reservaciones.txt"

    -- Construir el estado inicial del sistema
    let sistemaInicial = estadoInicial
            { estadoTipos = tiposIniciales
            , estadoHab = habitacionesIniciales
            , estadoTarifas = tarifasIniciales
            , estadoReservas = reservasIniciales
            , nextIdReserva =
                1 + maximum (0 : map idReserva reservasIniciales)
            }

    putStrLn "\nResumen de carga:"
    putStrLn ("Tipos de habitacion: " ++ show (length tiposIniciales))
    putStrLn ("Habitaciones: " ++ show (length habitacionesIniciales))
    putStrLn ("Tarifas: " ++ show (length tarifasIniciales))
    putStrLn ("Reservaciones: " ++ show (length reservasIniciales))

    _ <- menuAdministrativo sistemaInicial

    putStrLn "Programa finalizado."

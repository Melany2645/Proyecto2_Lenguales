module MenuAdministrativo where

import Modelos
import HotelInformacion hiding (pausar)
import TiposHabitacion
import CantDeHabitaciones
import System.Directory (doesFileExist)
import Data.List (find)
import Text.Read (readMaybe)

import qualified Reservaciones as R
import qualified Estadisticas as E
import System.IO (hFlush, stdout)

-- Funciones auxiliares

-- E: Ninguna entrada
-- S: Limpia la pantalla de la consola
-- R: Ninguna
-- F: Se encarga de limpiar la pantalla de la consola para mejorar la presentación del menú y las opciones.
limpiarPantalla :: IO ()
limpiarPantalla = putStr "\ESC[2J\ESC[H"

-- E: Ninguna entrada
-- S: Pausa la ejecución hasta que el usuario presione ENTER
-- R: Ninguna
-- F: Se encarga de pausar la ejecución del programa hasta que el usuario presione ENTER.
pausar :: IO ()
pausar = do
    putStrLn "\nPresione ENTER para continuar..."
    _ <- getLine
    return ()

-- E: Una lista de líneas de texto
-- S: Una lista de tipos de habitación parseados
-- R: Una lista de líneas de texto válidas
-- F: Se encarga de convertir una lista de líneas de texto en una lista de tipos de habitación, 
-- ignorando líneas vacías y manejando errores de parseo.
separarPorComasLocal :: String -> [String]
separarPorComasLocal [] = []
separarPorComasLocal texto =
    case break (== ',') texto of
        (parte, []) -> [parte]
        (parte, _:resto) -> parte : separarPorComasLocal resto

parsearLineaTarifa :: String -> Tarifa
parsearLineaTarifa linea =
    let partes = separarPorComasLocal linea
    in case partes of
    [codigo, monto] ->
        Tarifa (read codigo :: Int) (read monto :: Double)
    _ ->
        error "Formato incorrecto en tarifas_persistencia.txt"

leerEntero :: String -> Maybe Int
leerEntero texto = readMaybe texto


-- MENÚ PRINCIPAL


-- E: Ninguna entrada
-- S: Nos despliega el menú principal y nos permite seleccionar una opción
-- R: Ninguna
-- F: Se encarga de mostrar el menú principal del sistema de gestión hotelera y permite al usuario seleccionar una opción para realizar diferentes acciones administrativas, como gestionar información del hotel, tipos de habitaciones, tarifas, reservaciones y estadísticas.
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
    putStrLn "6. Gestion de reservaciones"
    putStrLn "7. Consultar disponibilidad"
    putStrLn "8. Estadisticas de ocupacion"
    putStrLn "9. Salir"
    putStrLn "=================================================="
    putStr "Seleccione una opcion: "
    getLine


-- OPCION 1: INFORMACION DEL HOTEL


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
        "1" -> do nuevaInfo <- cargarInfoHotel infoActual; pausar; return nuevaInfo
        "2" -> do mostrarInfoHotel infoActual; pausar; return infoActual
        "3" -> do guardarInfoHotel infoActual "hotel_info.txt"; pausar; return infoActual
        "4" -> do infoLeida <- leerInfoHotel infoActual "hotel_info.txt"; pausar; return infoLeida
        _ -> return infoActual


-- OPCION 2: CARGAR TIPOS


-- F: opcionCargarTipos
-- E: Lista de tipos de habitaciones existentes
-- S: Lista actualizada de tipos
-- O: Cargar nuevos tipos desde un archivo TXT

opcionCargarTipos :: [TipoHabitacion] -> IO [TipoHabitacion]
opcionCargarTipos tiposActuales = do

    limpiarPantalla

    putStrLn "========================================"
    putStrLn "       CARGAR TIPOS DE HABITACION"
    putStrLn "========================================"

    putStr "Ingrese la ruta del archivo TXT: "
    hFlush stdout

    ruta <- getLine

    if null ruta || all (== ' ') ruta
        then do
            putStrLn "[ERROR] La ruta no puede estar vacia."
            pausar
            return tiposActuales

        else do
            nuevosTipos <- procesarCargaTipos tiposActuales ruta

            pausar

            return nuevosTipos

-- OPCION 3: ASIGNAR HABITACIONES

-- E: Habitaciones existentes y tipos de habitaciones
-- S: Lista actualizada de habitaciones
-- R: Valores válidos
-- F: Se encarga de asignar nuevas habitaciones al sistema, generando IDs únicos y guardando los cambios en un archivo de persistencia.
opcionAsignarHabitaciones :: [Habitacion] -> [TipoHabitacion] -> IO [Habitacion]
opcionAsignarHabitaciones habitaciones tipos = do
    limpiarPantalla
    putStrLn "=== ASIGNAR HABITACIONES ==="
    nuevasHabitaciones <- asignarHabitaciones habitaciones tipos
    pausar
    return nuevasHabitaciones


-- OPCION 4: VER HABITACIONES

-- E: Una lista de habitaciones
-- S: Muestra en pantalla las habitaciones del hotel
-- R: Valores válidos
-- F: Se encarga de mostrar en pantalla la lista de habitaciones del hotel, incluyendo su ID y tipo, para que el usuario pueda visualizar la información actual de las habitaciones disponibles en el sistema.
opcionVerHabitaciones :: [Habitacion] -> IO ()
opcionVerHabitaciones habitaciones = do
    limpiarPantalla
    putStrLn "=== HABITACIONES DEL HOTEL ==="
    CantDeHabitaciones.mostrarHabitaciones habitaciones
    pausar


-- OPCION 5: TARIFAS

-- E: Una entrada de usuario para la ruta del archivo de tarifas
-- S: Una lista de tarifas cargadas desde el archivo
-- R: Valores válidos
-- F: Se encarga de cargar las tarifas desde un archivo de texto, reemplazando las tarifas existentes en el sistema y guardando los cambios en un archivo de persistencia.
opcionCargarTarifas :: [Tarifa] -> IO [Tarifa]
opcionCargarTarifas _ = do
    limpiarPantalla
    putStrLn "=== CARGA DE TARIFAS ==="
    putStrLn "Las tarifas existentes seran reemplazadas por las del archivo."
    putStrLn "Ingrese la ruta del archivo (ej: tarifas.txt): "
    ruta <- getLine
    
    if null ruta || all (== ' ') ruta
        then do
            putStrLn "[ERROR] La ruta no puede estar vacia."
            pausar
            return []
        else do
            existe <- doesFileExist ruta
            if not existe
                then do
                    putStrLn "[ERROR] El archivo no existe."
                    pausar
                    return []
                else do
                    contenido <- readFile ruta
                    let lineas = filter (not . null) (lines contenido)
                    let nuevasTarifas = map parsearLineaTarifa lineas
                    putStrLn ("[OK] Se cargaron " ++ show (length nuevasTarifas) ++ " tarifas.")
                    writeFile "tarifas_persistencia.txt" (unlines (map (\t -> show (codigoTarifa t) ++ "," ++ show (montoTarifa t)) nuevasTarifas))
                    putStrLn "[OK] Tarifas guardadas en persistencia."
                    pausar
                    return nuevasTarifas


-- OPCION 6: SUBMENU DE RESERVACIONES


opcionGestionReservas :: EstadoSistema -> IO EstadoSistema
opcionGestionReservas sistema = do
    limpiarPantalla
    putStrLn "=== GESTION DE RESERVACIONES ==="
    putStrLn "\n1. Consultar historial de reservaciones"
    putStrLn "2. Volver al menu administrativo"
    putStr "\nSeleccione una opcion: "
    opcion <- getLine

    case opcion of
        "1" -> do
            limpiarPantalla
            R.consultarReservaciones (estadoReservas sistema)
            pausar
            opcionGestionReservas sistema
        
        "2" -> return sistema
        
        _ -> do
            putStrLn "Opcion no valida."
            pausar
            opcionGestionReservas sistema


-- OPCION 7: CONSULTAR DISPONIBILIDAD POR FECHA

-- E: Estado actual del sistema
-- S: Estado del sistema sin modificaciones
-- R: Valores válidos
-- F: Permite al usuario consultar la disponibilidad de habitaciones en una fecha específica, mostrando las habitaciones ocupadas y disponibles, y validando el formato de la fecha ingresada.
opcionConsultarDisponibilidad :: EstadoSistema -> IO EstadoSistema
opcionConsultarDisponibilidad sistema = do
    limpiarPantalla
    putStrLn "=== CONSULTAR DISPONIBILIDAD ===\n"
    
    if null (estadoHab sistema)
        then do
            putStrLn "[AVISO] No hay habitaciones en el sistema."
            pausar
            return sistema
        else do
            putStrLn "Ingrese una fecha para consultar (AAAA-MM-DD):"
            fechaTexto <- getLine
            
            if not (validarFormatoFecha fechaTexto)
                then do
                    putStrLn "[ERROR] Formato de fecha invalido. Use AAAA-MM-DD"
                    pausar
                    return sistema
                else do
                    putStrLn ""
                    putStrLn ("--- Habitaciones OCUPADAS en " ++ fechaTexto ++ " ---")
                    let listaOcupadas = habitacionesOcupadasEnFecha sistema fechaTexto
                    if null listaOcupadas
                        then putStrLn "  (Ninguna)"
                        else mapM_ mostrarHabitacionOcupada listaOcupadas
                    putStrLn ""
                    putStrLn ("--- Habitaciones DISPONIBLES en " ++ fechaTexto ++ " ---")
                    let listaDisponibles = habitacionesDisponiblesEnFecha sistema fechaTexto
                    if null listaDisponibles
                        then putStrLn "  (Ninguna)"
                        else mapM_ mostrarHabitacionDisponible listaDisponibles
                    pausar
                    return sistema

-- | Valida el formato AAAA-MM-DD
validarFormatoFecha :: String -> Bool
validarFormatoFecha fecha =
    length fecha == 10 &&
    fecha !! 4 == '-' &&
    fecha !! 7 == '-' &&
    all (`elem` "0123456789") (take 4 fecha) &&
    all (`elem` "0123456789") (take 2 (drop 5 fecha)) &&
    all (`elem` "0123456789") (drop 8 fecha)

-- E: Una fecha en formato AAAA-MM-DD
-- S: Un valor booleano que indica si la fecha es válida
-- R: Una fecha con el formato correcto
-- F: Valida que una fecha tenga el formato AAAA-MM-DD
-- | Verifica si una fecha está dentro del rango de una reserva
fechaDentroDeReserva :: String -> String -> String -> Bool
fechaDentroDeReserva fecha entrada salida =
    fecha >= entrada && fecha < salida

-- E: Estado actual del sistema y una fecha en formato AAAA-MM-DD
-- S: Una lista de tuplas que contienen habitaciones ocupadas y el ID de la reserva correspondiente
-- R: Las habitaciones ocupadas en la fecha especificada
-- F: Obtiene las habitaciones ocupadas en una fecha específica
habitacionesOcupadasEnFecha :: EstadoSistema -> String -> [(Habitacion, Int)]
habitacionesOcupadasEnFecha sistema fecha =
    let reservasActivas = filter (\r -> estadoReserva r == Activa && 
                                        fechaDentroDeReserva fecha (fechaEntrada r) (fechaSalida r))
                                 (estadoReservas sistema)
        listaOcupaciones = concatMap (\r -> map (\oc -> (habitacionOcupada oc, idReserva r)) (Modelos.ocupaciones r)) reservasActivas
    in listaOcupaciones

-- | Obtiene las habitaciones disponibles en una fecha específica
habitacionesDisponiblesEnFecha :: EstadoSistema -> String -> [Habitacion]
habitacionesDisponiblesEnFecha sistema fecha =
    let ocupadas = map fst (habitacionesOcupadasEnFecha sistema fecha)
        idsOcupadas = map idHabitacion ocupadas
    in filter (\h -> idHabitacion h `notElem` idsOcupadas) (estadoHab sistema)

-- E: Una tupla que contiene una habitación y el ID de la reserva correspondiente
-- S: Muestra en pantalla la información de una habitación ocupada junto con el ID de la reserva
-- R: Una habitación ocupada con su código de reserva
-- F: Muestra en pantalla la información de una habitación ocupada junto con el ID de la reserva correspondiente.
-- | Muestra una habitación ocupada con su código de reserva
mostrarHabitacionOcupada :: (Habitacion, Int) -> IO ()
mostrarHabitacionOcupada (hab, idReserva) = do
    putStrLn ("  " ++ idHabitacion hab 
           ++ " - " ++ nombreTH (tipoHabitacion hab)
           ++ " | Reservada en reserva #" ++ show idReserva)

-- | Muestra toda la informacion de una habitacion disponible
mostrarHabitacionDisponible :: Habitacion -> IO ()
mostrarHabitacionDisponible hab = do
    putStrLn ("Habitacion: " ++ idHabitacion hab)
    putStrLn ("Tipo: " ++ nombreTH (tipoHabitacion hab))
    putStrLn ("Descripcion: " ++ descripcion (tipoHabitacion hab))
    putStrLn ("Capacidad maxima: " ++
        show (maxHuespedes (tipoHabitacion hab)) ++ " huespedes")
    putStrLn "----------------------------------------"


-- OPCION 8: ESTADISTICAS

    
-- F: opcionEstadisticas
-- E: Estado actual del sistema
-- S: Estado del sistema sin modificaciones
-- O: Mostrar las estadisticas de ocupacion del hotel

opcionEstadisticas :: EstadoSistema -> IO EstadoSistema
opcionEstadisticas sistema = do
    limpiarPantalla

    E.menuEstadisticas sistema

    return sistema


-- MENU PRINCIPAL

-- E: Estado actual del sistema
-- S: Estado del sistema actualizado después de realizar las acciones seleccionadas por el usuario
-- R: Valores válidos
-- F: Se encarga de mostrar el menú principal del sistema de gestión hotelera y permite al usuario seleccionar una opción para realizar diferentes acciones administrativas, como gestionar información del hotel, tipos de habitaciones, tarifas, reservaciones y estadísticas.
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
            nuevasHabitaciones <- opcionAsignarHabitaciones (estadoHab sistema) (estadoTipos sistema)
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
            sistemaNuevo <- opcionGestionReservas sistema
            menuAdministrativo sistemaNuevo

        "7" -> do
            sistemaNuevo <- opcionConsultarDisponibilidad sistema
            menuAdministrativo sistemaNuevo

        "8" -> do
            sistemaNuevo <- opcionEstadisticas sistema
            menuAdministrativo sistemaNuevo

        "9" -> do
            limpiarPantalla
            putStrLn "Saliendo del menu administrativo..."
            return sistema

        _ -> do
            putStrLn "Opcion no valida."
            pausar
            menuAdministrativo sistema


-- MAIN

-- E: Ninguna entrada
-- S: Inicia el sistema de gestión hotelera y permite al usuario interactuar con el menú administrativo para realizar diversas acciones.
-- R: Ninguna
-- F: Se encarga de iniciar el sistema de gestión hotelera, cargando la información inicial desde archivos de persistencia si existen, y luego muestra el menú administrativo para que el usuario pueda realizar diversas acciones relacionadas con la gestión del hotel.
main :: IO ()
main = do
    putStrLn "Iniciando sistema de gestion hotelera..."
    putStrLn ""

    -- 1. Cargar tipos
    existeTipos <- doesFileExist "tipos_persistencia.txt"
    tiposIniciales <- if existeTipos
        then do
            putStrLn "[OK] Cargando tipos de habitación guardados..."
            lineas <- leerArchivo "tipos_persistencia.txt"
            let tipos = parsearLineas lineas
            putStrLn ("[INFO] Se cargaron " ++ show (length tipos) ++ " tipos.")
            return tipos
        else do
            putStrLn "[AVISO] No hay tipos guardados."
            return []

    -- 2. Cargar habitaciones
    existeHabitaciones <- doesFileExist "habitaciones_persistencia.txt"
    habitacionesIniciales <- if existeHabitaciones
        then do
            putStrLn "[OK] Cargando habitaciones guardadas..."
            contenido <- readFile "habitaciones_persistencia.txt"
            let lineas = filter (not . null) (lines contenido)
            let habs = map lineaAHabitacion lineas
            putStrLn ("[INFO] Se cargaron " ++ show (length habs) ++ " habitaciones.")
            return habs
        else do
            putStrLn "[AVISO] No hay habitaciones guardadas."
            return []

    -- 3. Cargar tarifas
    existeTarifas <- doesFileExist "tarifas_persistencia.txt"
    tarifasIniciales <- if existeTarifas
        then do
            putStrLn "[OK] Cargando tarifas guardadas..."
            contenido <- readFile "tarifas_persistencia.txt"
            let lineas = filter (not . null) (lines contenido)
            putStrLn ("[INFO] El archivo tiene " ++ show (length lineas) ++ " líneas.")
            let tarifas = map parsearLineaTarifa lineas
            putStrLn ("[INFO] Se parsearon " ++ show (length tarifas) ++ " tarifas válidas.")
            return tarifas
        else do
            putStrLn "[AVISO] No hay tarifas guardadas."
            return []

    -- 4. Cargar reservaciones
    putStrLn "[OK] Cargando reservaciones..."
    reservasIniciales <- R.cargarReservaciones "reservaciones.txt"
    putStrLn ("[INFO] Se cargaron " ++ show (length reservasIniciales) ++ " reservaciones.")

    -- 5. Construir estado inicial
    let maxIdReserva = if null reservasIniciales
                       then 0
                       else maximum (map idReserva reservasIniciales)
        sistemaInicial = estadoInicial
            { estadoInfo     = Nothing
            , estadoTipos    = tiposIniciales
            , estadoHab      = habitacionesIniciales
            , estadoTarifas  = tarifasIniciales
            , estadoReservas = reservasIniciales
            , estadoFacturas = []
            , nextIdReserva  = 1 + maxIdReserva
            , nextIdFactura  = 1
            }

    putStrLn ""
    putStrLn "=================================================="
    putStrLn " Resumen de carga inicial:"
    putStrLn ("   - Tipos de habitacion: " ++ show (length tiposIniciales))
    putStrLn ("   - Habitaciones: " ++ show (length habitacionesIniciales))
    putStrLn ("   - Tarifas: " ++ show (length tarifasIniciales))
    putStrLn ("   - Reservaciones: " ++ show (length reservasIniciales))
    putStrLn "=================================================="
    putStrLn ""

    _ <- menuAdministrativo sistemaInicial

    putStrLn "Programa finalizado."
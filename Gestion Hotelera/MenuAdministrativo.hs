module MenuAdministrativo where

import Modelos
import HotelInformacion
import TiposHabitacion
import CantDeHabitaciones
import System.Directory (doesFileExist)

-- ============================================
-- FUNCIONES AUXILIARES LOCALES
-- ============================================

limpiarPantalla :: IO ()
limpiarPantalla = putStr "\ESC[2J\ESC[H"

mostrarMenu :: IO String
mostrarMenu = do
    limpiarPantalla
    putStrLn "=================================================="
    putStrLn "  SISTEMA DE GESTIÓN HOTELERA"
    putStrLn "  Menú Administrativo"
    putStrLn "=================================================="
    putStrLn "1. Información del hotel"
    putStrLn "2. Cargar tipo de habitaciones"
    putStrLn "3. Asignar Cantidad de habitaciones por tipo"
    putStrLn "4. Ver habitaciones"
    putStrLn "5. Carga de Tarifas"
    putStrLn "6. Consultar Reservaciones"
    putStrLn "7. Consultar estado de ocupación"
    putStrLn "8. Estadísticas de ocupación"
    putStrLn "9. Salir"
    putStrLn "=================================================="
    putStr "\nSeleccione una opción: "
    getLine

-- ============================================
-- OPCIÓN 1: INFORMACIÓN DEL HOTEL
-- ============================================
opcionInfoHotel :: Maybe HotelInfo -> IO (Maybe HotelInfo)
opcionInfoHotel infoActual = do
    limpiarPantalla
    putStrLn "=== INFORMACIÓN DEL HOTEL ===\n"
    putStrLn "1. Cargar información del hotel"
    putStrLn "2. Ver información actual"
    putStrLn "3. Guardar información en archivo"
    putStrLn "4. Cargar información desde archivo"
    putStrLn "5. Volver al menú administrativo"
    putStr "\nSeleccione una opción: "
    opcion <- getLine
    
    case opcion of
        "1" -> do nuevaInfo <- cargarInfoHotel infoActual; pausar; return nuevaInfo
        "2" -> do mostrarInfoHotel infoActual; pausar; return infoActual
        "3" -> do guardarInfoHotel infoActual "hotel_info.txt"; pausar; return infoActual
        "4" -> do infoLeida <- leerInfoHotel infoActual "hotel_info.txt"; pausar; return infoLeida
        _ -> return infoActual

-- ============================================
-- OPCIÓN 2: CARGAR TIPOS DE HABITACIÓN
-- ============================================
opcionCargarTipos :: [TipoHabitacion] -> IO [TipoHabitacion]
opcionCargarTipos tiposActuales = do
    limpiarPantalla
    putStrLn "=== CARGAR TIPO DE HABITACIONES ===\n"
    putStrLn "Tipos actualmente en el sistema:"
    mostrarTipos tiposActuales
    putStr "\nIngrese la ruta del archivo (ej: tipos.txt): "
    ruta <- getLine
    
    if null ruta || all (== ' ') ruta
        then do
            putStrLn "\n[ERROR] La ruta no puede estar vacía."
            pausar
            return tiposActuales
        else do
            nuevosTipos <- procesarCargaTipos tiposActuales ruta
            putStrLn "\nLista actual de tipos:"
            mostrarTipos nuevosTipos
            pausar
            return nuevosTipos

-- ============================================
-- OPCIÓN 3: ASIGNAR CANTIDAD DE HABITACIONES
-- ============================================
opcionAsignarHabitaciones :: [Habitacion] -> [TipoHabitacion] -> IO [Habitacion]
opcionAsignarHabitaciones habsActuales tipos = do
    limpiarPantalla
    putStrLn "=== ASIGNAR CANTIDAD DE HABITACIONES ===\n"
    nuevasHabs <- asignarHabitaciones habsActuales tipos
    pausar
    return nuevasHabs

-- ============================================
-- OPCIÓN 4: VER HABITACIONES
-- ============================================
opcionVerHabitaciones :: [Habitacion] -> IO ()
opcionVerHabitaciones habs = do
    limpiarPantalla
    putStrLn "=== VER HABITACIONES ===\n"
    mostrarHabitaciones habs
    pausar

-- ============================================
-- OTRAS OPCIONES (EN DESARROLLO)
-- ============================================
opcionCargarTarifas :: IO ()
opcionCargarTarifas = do limpiarPantalla; putStrLn "=== CARGA DE TARIFAS ===\n[EN DESARROLLO]"; pausar

opcionConsultarReservas :: IO ()
opcionConsultarReservas = do limpiarPantalla; putStrLn "=== CONSULTAR RESERVACIONES ===\n[EN DESARROLLO]"; pausar

opcionConsultarOcupacion :: IO ()
opcionConsultarOcupacion = do limpiarPantalla; putStrLn "=== CONSULTAR ESTADO DE OCUPACIÓN ===\n[EN DESARROLLO]"; pausar

opcionEstadisticas :: IO ()
opcionEstadisticas = do limpiarPantalla; putStrLn "=== ESTADÍSTICAS DE OCUPACIÓN ===\n[EN DESARROLLO]"; pausar

-- ============================================
-- MENÚ PRINCIPAL (Maneja 3 estados)
-- ============================================
menuAdministrativo :: Maybe HotelInfo -> [TipoHabitacion] -> [Habitacion] -> IO (Maybe HotelInfo, [TipoHabitacion], [Habitacion])
menuAdministrativo infoHotel tiposHab habs = do
    opcion <- mostrarMenu
    
    case opcion of
        "1" -> do
            nuevaInfo <- opcionInfoHotel infoHotel
            menuAdministrativo nuevaInfo tiposHab habs
        "2" -> do
            nuevosTipos <- opcionCargarTipos tiposHab
            menuAdministrativo infoHotel nuevosTipos habs
        "3" -> do
            nuevasHabs <- opcionAsignarHabitaciones habs tiposHab
            menuAdministrativo infoHotel tiposHab nuevasHabs
        "4" -> do
            opcionVerHabitaciones habs
            menuAdministrativo infoHotel tiposHab habs
        "5" -> do
            opcionCargarTarifas
            menuAdministrativo infoHotel tiposHab habs
        "6" -> do
            opcionConsultarReservas
            menuAdministrativo infoHotel tiposHab habs
        "7" -> do
            opcionConsultarOcupacion
            menuAdministrativo infoHotel tiposHab habs
        "8" -> do
            opcionEstadisticas
            menuAdministrativo infoHotel tiposHab habs
        "9" -> do
            limpiarPantalla
            putStrLn "Saliendo del Menú Administrativo..."
            return (infoHotel, tiposHab, habs)
        _ -> do
            putStrLn "\n[ERROR] Opción no válida."
            menuAdministrativo infoHotel tiposHab habs

-- ============================================
-- MAIN (Con carga automática de persistencia mejorada)
-- ============================================
main :: IO ()
main = do
    putStrLn "Iniciando sistema..."
    putStrLn ""
    
    -- 1. Cargar tipos de habitación guardados
    existeTipos <- doesFileExist "tipos_persistencia.txt"
    tiposIniciales <- if existeTipos
        then do
            putStrLn "[OK] Cargando tipos de habitación guardados..."
            lineas <- leerArchivo "tipos_persistencia.txt"
            let tipos = parsearLineas lineas
            putStrLn ("[INFO] Se cargaron " ++ show (length tipos) ++ " tipos desde persistencia.")
            return tipos
        else do
            putStrLn "[AVISO] No hay tipos guardados. Iniciando vacío."
            return []
    
    putStrLn ""
    
    -- 2. Cargar habitaciones guardadas
    existeHabs <- doesFileExist "habitaciones_persistencia.txt"
    habsIniciales <- if existeHabs
        then do
            putStrLn "[OK] Cargando habitaciones guardadas..."
            contenido <- readFile "habitaciones_persistencia.txt"
            let lineas = lines contenido
            putStrLn ("[INFO] El archivo tiene " ++ show (length lineas) ++ " líneas.")
            
            -- Filtrar líneas vacías
            let lineasValidas = filter (not . null) lineas
            putStrLn ("[INFO] Líneas válidas: " ++ show (length lineasValidas))
            
            if null lineasValidas
                then do
                    putStrLn "[ERROR] El archivo de habitaciones está vacío o corrupto."
                    putStrLn "[AVISO] Iniciando sin habitaciones."
                    return []
                else do
                    let habs = map lineaAHabitacion lineasValidas
                    putStrLn ("[INFO] Se cargaron " ++ show (length habs) ++ " habitaciones desde persistencia.")
                    return habs
        else do
            putStrLn "[AVISO] No hay habitaciones guardadas. Iniciando vacío."
            return []
    
    putStrLn ""
    putStrLn "=================================================="
    putStrLn " Resumen de carga inicial:"
    putStrLn ("   - Tipos de habitación: " ++ show (length tiposIniciales))
    putStrLn ("   - Habitaciones: " ++ show (length habsIniciales))
    putStrLn "=================================================="
    putStrLn ""
    
    -- 3. Iniciar el menú con todo cargado
    _ <- menuAdministrativo Nothing tiposIniciales habsIniciales
    
    putStrLn "\nPrograma finalizado."
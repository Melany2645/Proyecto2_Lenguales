module MenuAdministrativo where

import Modelos
import HotelInformacion
import TiposHabitacion

-- ============================================
-- FUNCIONES AUXILIARES LOCALES
-- ============================================

-- | Limpia la pantalla usando secuencias ANSI
limpiarPantalla :: IO ()
limpiarPantalla = putStr "\ESC[2J\ESC[H"

-- | Muestra el menú administrativo completo
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
    
    -- Validamos que no haya presionado Enter sin escribir nada
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
-- OTRAS OPCIONES (EN DESARROLLO)
-- ============================================

opcionAsignarHabitaciones :: IO ()
opcionAsignarHabitaciones = do
    limpiarPantalla
    putStrLn "=== ASIGNAR CANTIDAD DE HABITACIONES ===\n"
    putStrLn "[EN DESARROLLO]"
    pausar

opcionVerHabitaciones :: IO ()
opcionVerHabitaciones = do
    limpiarPantalla
    putStrLn "=== VER HABITACIONES ===\n"
    putStrLn "[EN DESARROLLO]"
    pausar

opcionCargarTarifas :: IO ()
opcionCargarTarifas = do
    limpiarPantalla
    putStrLn "=== CARGA DE TARIFAS ===\n"
    putStrLn "[EN DESARROLLO]"
    pausar

opcionConsultarReservas :: IO ()
opcionConsultarReservas = do
    limpiarPantalla
    putStrLn "=== CONSULTAR RESERVACIONES ===\n"
    putStrLn "[EN DESARROLLO]"
    pausar

opcionConsultarOcupacion :: IO ()
opcionConsultarOcupacion = do
    limpiarPantalla
    putStrLn "=== CONSULTAR ESTADO DE OCUPACIÓN ===\n"
    putStrLn "[EN DESARROLLO]"
    pausar

opcionEstadisticas :: IO ()
opcionEstadisticas = do
    limpiarPantalla
    putStrLn "=== ESTADÍSTICAS DE OCUPACIÓN ===\n"
    putStrLn "[EN DESARROLLO]"
    pausar

-- ============================================
-- MENÚ PRINCIPAL 
-- ============================================

menuAdministrativo :: Maybe HotelInfo -> [TipoHabitacion] -> IO (Maybe HotelInfo, [TipoHabitacion])
menuAdministrativo infoHotel tiposHab = do
    opcion <- mostrarMenu
    
    case opcion of
        "1" -> do
            nuevaInfo <- opcionInfoHotel infoHotel
            menuAdministrativo nuevaInfo tiposHab
        
        "2" -> do
            nuevosTipos <- opcionCargarTipos tiposHab
            menuAdministrativo infoHotel nuevosTipos
        
        "3" -> do
            opcionAsignarHabitaciones
            menuAdministrativo infoHotel tiposHab
        
        "4" -> do
            opcionVerHabitaciones
            menuAdministrativo infoHotel tiposHab
        
        "5" -> do
            opcionCargarTarifas
            menuAdministrativo infoHotel tiposHab
        
        "6" -> do
            opcionConsultarReservas
            menuAdministrativo infoHotel tiposHab
        
        "7" -> do
            opcionConsultarOcupacion
            menuAdministrativo infoHotel tiposHab
        
        "8" -> do
            opcionEstadisticas
            menuAdministrativo infoHotel tiposHab
        
        "9" -> do
            limpiarPantalla
            putStrLn "Saliendo del Menú Administrativo..."
            return (infoHotel, tiposHab)
        
        _ -> do
            putStrLn "\n[ERROR] Opción no válida."
            menuAdministrativo infoHotel tiposHab

-- ============================================
-- MAIN
-- ============================================

main :: IO ()
main = do
    _ <- menuAdministrativo Nothing []
    putStrLn "\nPrograma finalizado."
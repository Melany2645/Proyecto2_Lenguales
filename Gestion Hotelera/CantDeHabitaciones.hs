module CantDeHabitaciones where

import Modelos
import System.IO (openFile, IOMode(ReadMode, WriteMode), hSetEncoding, latin1, hGetContents, hPutStr, hClose)
import Text.Read (readMaybe)

-- ============================================
-- 1. FUNCIONES AUXILIARES DE GENERACIÓN (Puras)
-- ============================================

formatearNumero :: Int -> String
formatearNumero n
    | n < 10    = "00" ++ show n
    | n < 100   = "0" ++ show n
    | otherwise = show n

crearIdHabitacion :: Int -> String
crearIdHabitacion numero = "H" ++ formatearNumero numero

generarListaIds :: Int -> Int -> [String]
generarListaIds idInicial cantidad = 
    [crearIdHabitacion n | n <- [idInicial .. idInicial + cantidad - 1]]

crearHabitacionesDeUnTipo :: TipoHabitacion -> [String] -> [Habitacion]
crearHabitacionesDeUnTipo tipo ids = 
    map (\id -> Habitacion id tipo) ids

generarHabitacionesRecursivo :: Int -> [(TipoHabitacion, Int)] -> [Habitacion]
generarHabitacionesRecursivo _ [] = []
generarHabitacionesRecursivo idActual ((tipo, cantidad):resto) =
    let idsNuevos = generarListaIds idActual cantidad
        habsNuevas = crearHabitacionesDeUnTipo tipo idsNuevos
        siguienteId = idActual + cantidad
    in habsNuevas ++ generarHabitacionesRecursivo siguienteId resto

-- | Obtiene el siguiente ID disponible
obtenerSiguienteId :: [Habitacion] -> Int
obtenerSiguienteId [] = 1
obtenerSiguienteId habs =
    case idsNumericos of
        [] -> 1
        _ -> maximum idsNumericos + 1
    where
        idsNumericos =
            [ numero
            | hab <- habs
            , Just numero <- [extraerNumero (idHabitacion hab)]
            ]

        extraerNumero :: String -> Maybe Int
        extraerNumero identificador = do
            numero <- case identificador of
                'H':sufijo -> readMaybe sufijo
                _ -> readMaybe identificador
            if numero > 0 then Just numero else Nothing

-- | Obtiene los nombres de los tipos que ya tienen habitaciones
obtenerTiposConHabitaciones :: [Habitacion] -> [String]
obtenerTiposConHabitaciones habs =
    map (nombreTH . tipoHabitacion) habs

-- | Filtra solo los tipos que NO tienen habitaciones aún
obtenerTiposSinHabitaciones :: [TipoHabitacion] -> [Habitacion] -> [TipoHabitacion]
obtenerTiposSinHabitaciones tipos habs =
    let tiposConHabs = obtenerTiposConHabitaciones habs
    in filter (\t -> nombreTH t `notElem` tiposConHabs) tipos

-- ============================================
-- 2. FUNCIONES DE INTERACCIÓN (IO)
-- ============================================

pedirCantidadParaTipo :: TipoHabitacion -> IO (TipoHabitacion, Int)
pedirCantidadParaTipo tipo = do
    putStr ("Cantidad de habitaciones tipo '" ++ nombreTH tipo ++ "' (0 para saltar): ")
    entrada <- getLine
    let cantidad = if null entrada || not (all (`elem` "0123456789") entrada) 
                   then 0 
                   else read entrada :: Int
    return (tipo, cantidad)

pedirCantidadesRecursivo :: [TipoHabitacion] -> IO [(TipoHabitacion, Int)]
pedirCantidadesRecursivo [] = return []
pedirCantidadesRecursivo (t:ts) = do
    par <- pedirCantidadParaTipo t
    resto <- pedirCantidadesRecursivo ts
    return (par : resto)

-- | Función principal: Solo asigna habitaciones a tipos que no las tienen
asignarHabitaciones :: [Habitacion] -> [TipoHabitacion] -> IO [Habitacion]
asignarHabitaciones habitacionesExistentes tipos = do
    -- Filtrar solo los tipos que NO tienen habitaciones aún
    let tiposNuevos = obtenerTiposSinHabitaciones tipos habitacionesExistentes
    
    if null tiposNuevos
        then do
            putStrLn "\n[AVISO] Todos los tipos ya tienen habitaciones asignadas."
            putStrLn "No es posible agregar más habitaciones a tipos existentes."
            return habitacionesExistentes
        else do
            if null habitacionesExistentes
                then putStrLn "\n=== Asignar Habitaciones (Primera vez) ==="
                else do
                    putStrLn "\n=== Asignar Habitaciones a Tipos Nuevos ==="
                    putStrLn "Los siguientes tipos no tienen habitaciones aún:"
                    mapM_ (\t -> putStrLn ("  - " ++ nombreTH t)) tiposNuevos
                    putStrLn ""
            
            putStrLn "Indique cuántas habitaciones desea para cada tipo nuevo (0 para saltar):\n"
            
            paresTipoCantidad <- pedirCantidadesRecursivo tiposNuevos
            let paresValidos = filter (\(_, c) -> c > 0) paresTipoCantidad
            
            if null paresValidos
                then do
                    putStrLn "\n[AVISO] No se asignó ninguna habitación nueva."
                    return habitacionesExistentes
                else do
                    let siguienteId = obtenerSiguienteId habitacionesExistentes
                    let nuevasHabitaciones = generarHabitacionesRecursivo siguienteId paresValidos
                    let todasLasHabitaciones = habitacionesExistentes ++ nuevasHabitaciones
                    
                    putStrLn ("\n[OK] Se generaron " ++ show (length nuevasHabitaciones) ++ " habitaciones nuevas.")
                    putStrLn ("[OK] Total de habitaciones en el sistema: " ++ show (length todasLasHabitaciones))
                    
                    guardarHabitacionesEnArchivo todasLasHabitaciones "habitaciones_persistencia.txt"
                    
                    return todasLasHabitaciones

-- ============================================
-- 3. FUNCIONES DE PERSISTENCIA
-- ============================================

habitacionALinea :: Habitacion -> String
habitacionALinea hab =
    let id = idHabitacion hab
        tipo = tipoHabitacion hab
    in id ++ "," ++ nombreTH tipo ++ "," ++ descripcion tipo ++ "," ++ show (maxHuespedes tipo)

guardarHabitacionesEnArchivo :: [Habitacion] -> FilePath -> IO ()
guardarHabitacionesEnArchivo habs ruta = do
    handle <- openFile ruta WriteMode
    hSetEncoding handle latin1
    let contenido = unlines (map habitacionALinea habs)
    hPutStr handle contenido
    hClose handle
    putStrLn ("[OK] Habitaciones guardadas persistentemente en: " ++ ruta)

leerHabitacionesDesdeArchivo :: FilePath -> IO [Habitacion]
leerHabitacionesDesdeArchivo ruta = do
    handle <- openFile ruta ReadMode
    hSetEncoding handle latin1
    contenido <- hGetContents handle
    let lineas = lines contenido
    length lineas `seq` hClose handle
    return (map lineaAHabitacion lineas)

lineaAHabitacion :: String -> Habitacion
lineaAHabitacion linea =
    let partes = separarPorComas linea
        idHab = partes !! 0
        tipo = TipoHabitacion (partes !! 1) (partes !! 2) (read (partes !! 3) :: Int)
    in Habitacion idHab tipo

separarPorComas :: String -> [String]
separarPorComas [] = []
separarPorComas texto =
    case break (== ',') texto of
        (parte, []) -> [parte]
        (parte, _:resto) -> parte : separarPorComas resto

-- ============================================
-- 4. FUNCIONES DE MOSTRAR
-- ============================================

mostrarHabitaciones :: [Habitacion] -> IO ()
mostrarHabitaciones [] = putStrLn "[AVISO] No hay habitaciones generadas en el sistema."
mostrarHabitaciones habs = do
    putStrLn "--- Listado de Habitaciones ---"
    putStrLn ("Total de habitaciones: " ++ show (length habs))
    mapM_ mostrarUnaHabitacion habs

mostrarUnaHabitacion :: Habitacion -> IO ()
mostrarUnaHabitacion hab = do
    putStrLn ("  ID: " ++ idHabitacion hab ++ " | Tipo: " ++ nombreTH (tipoHabitacion hab))

contarHabitacionesPorTipo :: [Habitacion] -> [(TipoHabitacion, Int)]
contarHabitacionesPorTipo [] = []
contarHabitacionesPorTipo habs =
    let tiposUnicos = obtenerTiposUnicos habs
    in map (\tipo -> (tipo, contarTipo tipo habs)) tiposUnicos

obtenerTiposUnicos :: [Habitacion] -> [TipoHabitacion]
obtenerTiposUnicos [] = []
obtenerTiposUnicos (h:resto) =
    let tipo = tipoHabitacion h
        tiposRestantes = obtenerTiposUnicos resto
    in if tipo `elem` tiposRestantes
        then tiposRestantes
        else tipo : tiposRestantes

contarTipo :: TipoHabitacion -> [Habitacion] -> Int
contarTipo tipo habs = length (filter (\h -> tipoHabitacion h == tipo) habs)

mostrarCantidadesPorTipo :: [Habitacion] -> IO ()
mostrarCantidadesPorTipo [] = putStrLn "[AVISO] No hay habitaciones para contar."
mostrarCantidadesPorTipo habs = do
    putStrLn "--- Cantidad de Habitaciones por Tipo ---"
    let conteo = contarHabitacionesPorTipo habs
    mapM_ mostrarUnConteo conteo

mostrarUnConteo :: (TipoHabitacion, Int) -> IO ()
mostrarUnConteo (tipo, cantidad) = do
    putStrLn ("  " ++ nombreTH tipo ++ ": " ++ show cantidad ++ " habitaciones")
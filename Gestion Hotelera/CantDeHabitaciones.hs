module CantDeHabitaciones where

import Modelos
import System.IO (openFile, IOMode(ReadMode, WriteMode), hSetEncoding, latin1, hGetContents, hPutStr, hClose)
import Text.Read (readMaybe)


-- 1. FUNCIONES AUXILIARES DE GENERACIÓN

-- E: Un número entero
-- S: Una cadena de texto con el número formateado como "HXXX" donde XXX es el número con ceros a la izquierda
-- R: Un identificador de habitación único
-- F: Se encarga de formatear un número entero para generar un identificador de habitación único en el formato "HXXX", donde XXX es el número con ceros a la izquierda si es menor que 100.
formatearNumero :: Int -> String
formatearNumero n
    | n < 10    = "00" ++ show n
    | n < 100   = "0" ++ show n
    | otherwise = show n

-- E: Un número entero
-- S: Una cadena de texto con el identificador de habitación generado
-- R: Un identificador de habitación único
-- F: Se encarga de crear un identificador de habitación único en el formato "HXXX" a partir de un número entero, utilizando la función formatearNumero para asegurar que el número tenga tres dígitos con ceros a la izquierda si es necesario.
crearIdHabitacion :: Int -> String
crearIdHabitacion numero = "H" ++ formatearNumero numero

-- E: Una lista de tuplas que contienen un tipo de habitación y la cantidad de habitaciones a generar para ese tipo
-- S: Una lista de habitaciones generadas a partir de los tipos y cantidades especificados
-- R: Una lista de habitaciones generadas
-- F: Se encarga de generar habitaciones a partir de una lista de tuplas que contienen un tipo de habitación y la cantidad de habitaciones a generar para ese tipo, utilizando identificadores únicos generados secuencialmente.
generarListaIds :: Int -> Int -> [String]
generarListaIds idInicial cantidad = 
    [crearIdHabitacion n | n <- [idInicial .. idInicial + cantidad - 1]]

-- E: Un tipo de habitación y una lista de identificadores de habitaciones
-- S: Una lista de habitaciones del tipo especificado con los identificadores dados
-- R: Una lista de habitaciones generadas
-- F: Se encarga de crear una lista de habitaciones de un tipo específico utilizando los identificadores proporcionados.
crearHabitacionesDeUnTipo :: TipoHabitacion -> [String] -> [Habitacion]
crearHabitacionesDeUnTipo tipo ids = 
    map (\id -> Habitacion id tipo) ids

-- E: Un número entero y una lista de tuplas que contienen un tipo de habitación y la cantidad de habitaciones a generar para ese tipo
-- S: Una lista de habitaciones generadas a partir de los tipos y cantidades especificados, con identificadores únicos generados secuencialmente
-- R: Una lista de habitaciones generadas
-- F: Se encarga de generar habitaciones a partir de una lista de tuplas que contienen un tipo de habitación y la cantidad de habitaciones a generar para ese tipo, utilizando identificadores únicos generados secuencialmente, comenzando desde el número entero proporcionado como identificador inicial.
generarHabitacionesRecursivo :: Int -> [(TipoHabitacion, Int)] -> [Habitacion]
generarHabitacionesRecursivo _ [] = []
generarHabitacionesRecursivo idActual ((tipo, cantidad):resto) =
    let idsNuevos = generarListaIds idActual cantidad
        habsNuevas = crearHabitacionesDeUnTipo tipo idsNuevos
        siguienteId = idActual + cantidad
    in habsNuevas ++ generarHabitacionesRecursivo siguienteId resto

-- E: Una lista de habitaciones y una ruta de archivo
-- S: Guarda las habitaciones en un archivo de texto en la ruta especificada
-- R: Una lista de habitaciones guardadas en un archivo de texto
-- F: Se encarga de guardar una lista de habitaciones en un archivo de texto en la ruta especificada, utilizando la función habitacionALinea para convertir cada habitación en una línea de texto con formato adecuado.
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

-- E: Una lista de tipos de habitación y una lista de habitaciones
-- S: Una lista de tipos de habitación que no tienen habitaciones asignadas
-- R: Una lista de tipos de habitación que no tienen habitaciones asignadas
-- F: Se encarga de filtrar los tipos de habitación que no tienen habitaciones asignadas, comparando la lista de tipos con la lista de habitaciones existentes y devolviendo solo aquellos tipos que no están presentes en las habitaciones.
-- | Filtra solo los tipos que NO tienen habitaciones aún
obtenerTiposSinHabitaciones :: [TipoHabitacion] -> [Habitacion] -> [TipoHabitacion]
obtenerTiposSinHabitaciones tipos habs =
    let tiposConHabs = obtenerTiposConHabitaciones habs
    in filter (\t -> nombreTH t `notElem` tiposConHabs) tipos


-- 2. FUNCIONES DE INTERACCIÓN

-- E: Un tipo de habitación
-- S: Una tupla que contiene el tipo de habitación y la cantidad de habitaciones a generar para ese tipo
-- R: Una tupla que contiene el tipo de habitación y la cantidad de habitaciones a generar para ese tipo
-- F: Se encarga de solicitar al usuario la cantidad de habitaciones que desea generar para un tipo específico de habitación, mostrando un mensaje con el nombre del tipo y esperando la entrada del usuario. Si el usuario ingresa un valor no numérico o vacío, se considera como 0.
pedirCantidadParaTipo :: TipoHabitacion -> IO (TipoHabitacion, Int)
pedirCantidadParaTipo tipo = do
    putStr ("Cantidad de habitaciones tipo '" ++ nombreTH tipo ++ "' (0 para saltar): ")
    entrada <- getLine
    let cantidad = if null entrada || not (all (`elem` "0123456789") entrada) 
                   then 0 
                   else read entrada :: Int
    return (tipo, cantidad)

-- E: Una lista de tipos de habitación
-- S: Una lista de tuplas que contienen cada tipo de habitación y la cantidad de habitaciones a generar para ese tipo
-- R: Una lista de tuplas que contienen cada tipo de habitación y la cantidad de habitaciones a generar para ese tipo
-- F: Se encarga de solicitar al usuario la cantidad de habitaciones que desea generar para cada tipo de habitación en una lista, utilizando la función pedirCantidadParaTipo para cada tipo y devolviendo una lista de tuplas con los resultados.
pedirCantidadesRecursivo :: [TipoHabitacion] -> IO [(TipoHabitacion, Int)]
pedirCantidadesRecursivo [] = return []
pedirCantidadesRecursivo (t:ts) = do
    par <- pedirCantidadParaTipo t
    resto <- pedirCantidadesRecursivo ts
    return (par : resto)

-- E: Una lista de habitaciones y una ruta de archivo
-- S: Guarda las habitaciones en un archivo de texto en la ruta especificada
-- R: Una lista de habitaciones guardadas en un archivo de texto
-- F: Se encarga de guardar una lista de habitaciones en un archivo de texto en la ruta especificada, utilizando la función habitacionALinea para convertir cada habitación en una línea de texto con formato adecuado.
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


-- 3. FUNCIONES DE PERSISTENCIA


habitacionALinea :: Habitacion -> String
habitacionALinea hab =
    let id = idHabitacion hab
        tipo = tipoHabitacion hab
    in id ++ "," ++ nombreTH tipo ++ "," ++ descripcion tipo ++ "," ++ show (maxHuespedes tipo)

-- E: Una lista de habitaciones y una ruta de archivo
-- S: Guarda las habitaciones en un archivo de texto en la ruta especificada
-- R: Una lista de habitaciones guardadas en un archivo de texto
-- F: Se encarga de guardar una lista de habitaciones en un archivo de texto en la ruta especificada, utilizando la función habitacionALinea para convertir cada habitación en una línea de texto con formato adecuado.
guardarHabitacionesEnArchivo :: [Habitacion] -> FilePath -> IO ()
guardarHabitacionesEnArchivo habs ruta = do
    handle <- openFile ruta WriteMode
    hSetEncoding handle latin1
    let contenido = unlines (map habitacionALinea habs)
    hPutStr handle contenido
    hClose handle
    putStrLn ("[OK] Habitaciones guardadas persistentemente en: " ++ ruta)

-- E: Una ruta de archivo
-- S: Una lista de habitaciones leídas desde el archivo
-- R: Una lista de habitaciones leídas desde el archivo
-- F: Se encarga de leer una lista de habitaciones desde un archivo de texto en la ruta especificada, utilizando la función lineaAHabitacion para convertir cada línea en una habitación.
leerHabitacionesDesdeArchivo :: FilePath -> IO [Habitacion]
leerHabitacionesDesdeArchivo ruta = do
    handle <- openFile ruta ReadMode
    hSetEncoding handle latin1
    contenido <- hGetContents handle
    let lineas = lines contenido
    length lineas `seq` hClose handle
    return (map lineaAHabitacion lineas)

-- E: Una línea de texto con formato "id,tipo,descripcion,maxHuespedes"
-- S: Una habitación creada a partir de la línea de texto
-- R: Una habitación creada a partir de la línea de texto
-- F: Se encarga de convertir una línea de texto con formato "id,tipo,descripcion,maxHuespedes" en una habitación, extrayendo los valores correspondientes y creando un objeto Habitacion con el tipo de habitación asociado.
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


-- 4. FUNCIONES DE MOSTRAR

-- E: Una lista de habitaciones
-- S: Muestra en pantalla la información de cada habitación en la lista
-- R: Una lista de habitaciones mostradas en pantalla
-- F: Se encarga de mostrar en pantalla la información de cada habitación en una lista, incluyendo el ID y el tipo de habitación, o un mensaje de aviso si no hay habitaciones generadas en el sistema.
mostrarHabitaciones :: [Habitacion] -> IO ()
mostrarHabitaciones [] = putStrLn "[AVISO] No hay habitaciones generadas en el sistema."
mostrarHabitaciones habs = do
    putStrLn "--- Listado de Habitaciones ---"
    putStrLn ("Total de habitaciones: " ++ show (length habs))
    mapM_ mostrarUnaHabitacion habs

mostrarUnaHabitacion :: Habitacion -> IO ()
mostrarUnaHabitacion hab = do
    putStrLn ("  ID: " ++ idHabitacion hab ++ " | Tipo: " ++ nombreTH (tipoHabitacion hab))

-- E: Una lista de habitaciones
-- S: Muestra en pantalla la cantidad de habitaciones por tipo, incluyendo el nombre del tipo y la cantidad correspondiente
-- R: Una lista de habitaciones mostradas en pantalla con la cantidad por tipo
-- F: Se encarga de mostrar en pantalla la cantidad de habitaciones por tipo, contando cuántas habitaciones hay de cada tipo y mostrando el nombre del tipo junto con la cantidad correspondiente, o un mensaje de aviso si no hay habitaciones para contar.
contarHabitacionesPorTipo :: [Habitacion] -> [(TipoHabitacion, Int)]
contarHabitacionesPorTipo [] = []
contarHabitacionesPorTipo habs =
    let tiposUnicos = obtenerTiposUnicos habs
    in map (\tipo -> (tipo, contarTipo tipo habs)) tiposUnicos

-- E: Una lista de habitaciones
-- S: Muestra en pantalla la cantidad de habitaciones por tipo, incluyendo el nombre del tipo y la cantidad correspondiente
-- R: Una lista de habitaciones mostradas en pantalla con la cantidad por tipo
-- F: Se encarga de mostrar en pantalla la cantidad de habitaciones por tipo, contando cuántas habitaciones hay de cada tipo y mostrando el nombre del tipo junto con la cantidad correspondiente, o un mensaje de aviso si no hay habitaciones para contar.
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
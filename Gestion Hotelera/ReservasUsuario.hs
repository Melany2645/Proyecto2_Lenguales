module ReservasUsuario where


-- COMPONENTE 2: Gestión de reservas y disponibilidad de habitaciones

-- Este archivo concentra la lógica de operación del hotel para registrar reservas.
-- Valida fechas, controla la disponibilidad de habitaciones, verifica la capacidad
-- por huésped, calcula el subtotal de la estadía y orquesta el flujo principal de
-- creación de una reserva.
--
-- Su responsabilidad principal es transformar la información ingresada por el usuario
-- en una reserva coherente con las reglas del negocio y con las restricciones del hotel.

import Modelos
import Reservaciones (guardarReservaciones)
import Data.List (find)
import Text.Read (readMaybe)
import Data.Time
    ( Day, DayOfWeek(..), defaultTimeLocale, parseTimeM
    , formatTime, getCurrentTime, getZonedTime, localDay
    , zonedTimeToLocalTime
    , addDays, dayOfWeek, toGregorian
    )
import System.IO (hFlush, stdout)

-- Funciones de validaciones

-- E: Un texto 
-- S: Nos devuelve la fecha en formato Day si es válida, o Nothing si no lo es
-- R: Que la fecha ingresada sea válida y esté en el formato AAAA-MM-DD
-- F: Función que valida y convierte una fecha ingresada por el usuario en un tipo Day de Haskell.
leerFecha :: String -> Maybe Day
leerFecha texto = parseTimeM True defaultTimeLocale "%Y-%m-%d" texto

-- E: Un texto
-- S: Nos devuelve True si el texto es válido (no vacío y no solo espacios), False en caso contrario
-- R: Que el texto ingresado no esté vacío ni contenga solo espacios
-- F: Función que valida si un texto ingresado por el usuario es válido, es decir, no está vacío ni contiene solo espacios.
textoValido :: String -> Bool
textoValido texto = not (null texto) && any (/= ' ') texto

-- E: Un texto
-- S: Nos devuelve Just Int si el texto es un número entero válido, Nothing en caso contrario
-- R: Tiene que ser un número entero válido
-- F: Función que valida y convierte un texto ingresado por el usuario en un tipo Int de Haskell.
leerEntero :: String -> Maybe Int
leerEntero texto = readMaybe texto

validarFechas :: Day -> Day -> Day -> Bool
validarFechas hoy entrada salida = entrada >= hoy && entrada < salida

-- F: pedirRangoFechas
-- E: Fechas ingresadas por el usuario
-- S: Fechas de entrada y salida
-- O: Solicitar y validar las fechas de la reservacion

pedirRangoFechas :: IO (Day, Day, String, String)
pedirRangoFechas = do

    putStr "Fecha de entrada (AAAA-MM-DD): "
    hFlush stdout
    entradaTexto <- getLine

    putStr "Fecha de salida (AAAA-MM-DD): "
    hFlush stdout
    salidaTexto <- getLine

    case (leerFecha entradaTexto, leerFecha salidaTexto) of

        (Just entrada, Just salida) -> do

            ahora <- getZonedTime
            let hoy = localDay (zonedTimeToLocalTime ahora)

            if entrada < hoy
                then do
                    putStrLn "La fecha de entrada no puede ser anterior a hoy."
                    pedirRangoFechas

                else if not (validarFechas hoy entrada salida)
                    then do
                        putStrLn "La salida debe ser posterior a la entrada."
                        pedirRangoFechas

                    else
                        return (entrada, salida, entradaTexto, salidaTexto)

        _ -> do
            putStrLn "Fecha invalida. Use formato AAAA-MM-DD."
            pedirRangoFechas

-- E: Cuatro fechas, dos salidas y dos entradas
-- S: Un booleano indicando si las fechas se cruzan
-- R: Las fechas deben ser válidas
-- F: Función que persmite saber si dos rangos de fechas se cruzan
seCruzan :: Day -> Day -> Day -> Day -> Bool
seCruzan entrada1 salida1 entrada2 salida2 =
    entrada1 < salida2 && entrada2 < salida1

-- E: Una habitación, cantidad de adultos y cantidad de niños
-- S: Un booleano indicando si la ocupación es válida
-- R: Los parámetros deben ser válidos
-- F: Función que valida la ocupación de una habitación
validarHuespedes :: Habitacion -> Int -> Int -> Bool
validarHuespedes hab adultos ninos =
    adultos >= 0
    && ninos >= 0
    && adultos + ninos > 0
    && adultos + ninos <= maxHuespedes (tipoHabitacion hab)


-- E: Una lista de ocupaciones
-- S: Un booleano indicando si todas las ocupaciones son válidas
-- R: Las ocupaciones deben ser válidas
-- F: Función que valida una lista de ocupaciones
validarOcupaciones :: [OcupacionHab] -> Bool
validarOcupaciones [] = True
validarOcupaciones (ocupacion:resto) =
    validarHuespedes (habitacionOcupada ocupacion) (adultosHab ocupacion) (ninosHab ocupacion)
    && validarOcupaciones resto

-- E: Una habitación, fechas de entrada y salida, y una lista de reservas
-- S: Un booleano indicando si la habitación está ocupada en las fechas dadas
-- R: Los parámetros deben ser válidos
-- F: Función que verifica si una habitación está ocupada en un rango de fechas
habitacionOcupadaEnFechas :: Habitacion -> Day -> Day -> [Reserva] -> Bool
habitacionOcupadaEnFechas hab entrada salida reservas =
    any estaReservada reservas
    where
        estaReservada reserva =
            estadoReserva reserva /= Cancelada
            && any mismaHabitacion (ocupaciones reserva)
            && seCruzaConReserva reserva

        mismaHabitacion ocupacion =
            idHabitacion hab == idHabitacion (habitacionOcupada ocupacion)

        seCruzaConReserva reserva =
            case (leerFecha (fechaEntrada reserva), leerFecha (fechaSalida reserva)) of
                (Just entradaReserva, Just salidaReserva) ->
                    seCruzan entrada salida entradaReserva salidaReserva
                _ -> False


-- E: Un sistema de estado, fechas de entrada y salida
-- S: Una lista de habitaciones disponibles en las fechas dadas
-- R: Los parámetros deben ser válidos
-- F: Función que obtiene las habitaciones disponibles en un rango de fechas
obtenerHabitacionesDisponibles :: EstadoSistema -> Day -> Day -> [Habitacion]
obtenerHabitacionesDisponibles sistema entrada salida =
    filter disponible (estadoHab sistema)
    where
        disponible hab = not (habitacionOcupadaEnFechas hab entrada salida (estadoReservas sistema))


-- E: Una habitación
-- S: Muestra la información de la habitación en consola
-- R: Los parámetros deben ser válidos
-- F: Función que muestra la información de una habitación
mostrarHabitacion :: Habitacion -> IO ()
mostrarHabitacion hab = do
    putStrLn ("  " ++ idHabitacion hab 
           ++ " - " ++ nombreTH (tipoHabitacion hab)
           ++ " (Capacidad: " ++ show (maxHuespedes (tipoHabitacion hab)) ++ ")")


-- Selección de habitaciones

-- F: seleccionarHabitaciones
-- E: Habitaciones disponibles y seleccionadas
-- S: Lista de habitaciones seleccionadas
-- O: Permitir seleccionar habitaciones y validar huespedes

seleccionarHabitaciones :: [Habitacion] -> [OcupacionHab] -> IO [OcupacionHab]
seleccionarHabitaciones disponibles seleccionadas = do

    putStrLn "\nIngrese el ID de una habitacion (o ENTER para terminar):"
    putStr "ID: "
    hFlush stdout
    codigo <- getLine


-- Aquí se valida si el usuario ingresó un código de habitación o presionó ENTER para finalizar la selección. Si se ingresó un código, 
-- se busca la habitación correspondiente en la lista de disponibles y se valida que no haya sido seleccionada antes. 
-- Luego, se solicita la cantidad de adultos y niños para esa habitación y se valida que no exceda la capacidad máxima. 
-- Si todo es válido, se agrega la ocupación a la lista de seleccionadas y se repite el proceso hasta que el usuario decida finalizar.
    if null codigo
        then return (reverse seleccionadas)

        else case find (\hab -> idHabitacion hab == codigo) disponibles of

            Nothing -> do
                putStrLn "[ERROR] Habitacion no disponible o ID incorrecto."
                seleccionarHabitaciones disponibles seleccionadas

            Just hab ->
                if any (\oc -> idHabitacion (habitacionOcupada oc) == codigo) seleccionadas
                    then do
                        putStrLn "[ERROR] Esa habitacion ya fue seleccionada."
                        seleccionarHabitaciones disponibles seleccionadas

                    else do
                        putStr "Cantidad de adultos: "
                        hFlush stdout
                        adultosTexto <- getLine

                        putStr "Cantidad de ninos: "
                        hFlush stdout
                        ninosTexto <- getLine

                        case (leerEntero adultosTexto, leerEntero ninosTexto) of

                            (Just adultos, Just ninos) ->
                                if validarHuespedes hab adultos ninos
                                    then do
                                        let nuevaOcupacion = OcupacionHab hab adultos ninos
                                        putStrLn "Habitacion agregada."
                                        seleccionarHabitaciones disponibles (nuevaOcupacion : seleccionadas)

                                    else do
                                        putStrLn "[ERROR] Capacidad excedida o huespedes invalidos."
                                        seleccionarHabitaciones disponibles seleccionadas

                            _ -> do
                                putStrLn "[ERROR] Ingrese numeros validos."
                                seleccionarHabitaciones disponibles seleccionadas

-- Calcular el subtotal de la estadía

-- E: El código de la tarifa, la cantidad de adultos y niños, y el número de días
-- S: El costo total de la estadía
-- R: La función calcula el costo total de la estadía basado en las tarifas y la cantidad de huéspedes
-- F: Función que calcula el subtotal de la estadía según las tarifas y la cantidad de huéspedes
buscarTarifa :: Int -> [Tarifa] -> Maybe Tarifa
buscarTarifa codigo [] = Nothing
buscarTarifa codigo (tarifa:resto) =
    if codigoTarifa tarifa == codigo
        then Just tarifa
        else buscarTarifa codigo resto

codigoTarifaPara :: Day -> Bool -> Int
codigoTarifaPara dia esAdulto =
    let (_, mes, _) = toGregorian dia
        temporadaVerde = mes >= 4 && mes <= 10
        finDeSemana = dayOfWeek dia `elem` [Friday, Saturday]
        codigoBase = if temporadaVerde
                     then if finDeSemana then 2 else 1
                     else if finDeSemana then 6 else 5
    in if esAdulto then codigoBase else codigoBase + 2

-- E: La lista de tarifas, la fecha de entrada, la fecha de salida y las ocupaciones
-- S: El subtotal de la estadía, o Nothing si no se puede calcular
-- R: La función calcula el subtotal de la estadía basado en las tarifas y la cantidad de huéspedes
-- F: Función que calcula el subtotal de la estadía según las tarifas y la cantidad de huéspedes
calcularCostoDia :: [Tarifa] -> Day -> [OcupacionHab] -> Maybe Double
calcularCostoDia tarifas dia ocupacionesDia = do
    costos <- mapM calcularCosto ocupacionesDia
    return (sum costos)
    where
        calcularCosto ocupacion = do
            let codigoAdulto = codigoTarifaPara dia True
            let codigoNino = codigoTarifaPara dia False
            tarifaAdulto <- buscarTarifa codigoAdulto tarifas
            tarifaNino <- buscarTarifa codigoNino tarifas
            return (fromIntegral (adultosHab ocupacion) * montoTarifa tarifaAdulto
                  + fromIntegral (ninosHab ocupacion) * montoTarifa tarifaNino)

-- E: La lista de tarifas, la fecha de entrada, la fecha de salida y las ocupaciones
-- S: El subtotal de la estadía, o Nothing si no se puede calcular
-- R: La función calcula el subtotal de la estadía basado en las tarifas y la cantidad de huéspedes
-- F: Función que calcula el subtotal de la estadía según las tarifas y la cantidad de huéspedes
calcularSubtotal :: [Tarifa] -> Day -> Day -> [OcupacionHab] -> Maybe Double
calcularSubtotal tarifas entrada salida ocupaciones =
    if entrada >= salida || null ocupaciones
        then Nothing
        else calcularPorDia entrada
    where
        calcularPorDia dia
            | dia >= salida = Just 0
            | otherwise = do
                costoHoy <- calcularCostoDia tarifas dia ocupaciones
                costoRestante <- calcularPorDia (addDays 1 dia)
                return (costoHoy + costoRestante)

-- Registrar una nueva reservación


-- E: Un sistema de estado
-- S: El sistema de estado actualizado con la nueva reservación
-- R: La función registra una nueva reservación en el sistema
-- F: Función que registra una nueva reservación en el sistema
registrarReservacion :: EstadoSistema -> IO EstadoSistema
registrarReservacion sistema = do
    putStrLn "\n=== REGISTRAR NUEVA RESERVACIÓN ===\n"

-- aquí se valida que existan habitaciones y tarifas cargadas en el sistema antes de permitir
-- registrar una nueva reservación.
    if null (estadoHab sistema)
        then do
            putStrLn "[ERROR] No hay habitaciones en el sistema."
            return sistema
        else if null (estadoTarifas sistema)
            then do
                putStrLn "[ERROR] No hay tarifas cargadas."
                return sistema
            else do
                putStr "Nombre del cliente: "
                hFlush stdout
                nombre <- getLine

                if not (textoValido nombre)
                    then do
                        putStrLn "[ERROR] El nombre no puede estar vacío."
                        return sistema
                    else do
                        (entrada, salida, entradaTexto, salidaTexto) <- pedirRangoFechas

                        let disponibles = obtenerHabitacionesDisponibles sistema entrada salida
-- Aquí se valida si hay habitaciones disponibles para las fechas ingresadas. 
-- Si no hay, se muestra un mensaje de error y se retorna el sistema sin cambios. 
-- Si hay habitaciones disponibles, se muestran al usuario y se procede a la selección de habitaciones.
                        if null disponibles
                            then do
                                putStrLn "[ERROR] No hay habitaciones disponibles para esas fechas."
                                return sistema
                            else do
                                putStrLn "\nHabitaciones disponibles:"
                                mapM_ mostrarHabitacion disponibles

                                ocupaciones <- seleccionarHabitaciones disponibles []

                                if not (validarOcupaciones ocupaciones)
                                    then do
                                        putStrLn "[ERROR] Ocupaciones inválidas."
                                        return sistema
                                    else case calcularSubtotal (estadoTarifas sistema) entrada salida ocupaciones of
                                        Nothing -> do
                                            putStrLn "[ERROR] No se puede calcular el monto."
                                            return sistema
                                        Just subtotal -> do
                                            momento <- getCurrentTime
                                            let nuevaReserva = Reserva
                                                    { idReserva = nextIdReserva sistema
                                                    , nombreCliente = nombre
                                                    , fechaReserva = formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" momento
                                                    , fechaEntrada = entradaTexto
                                                    , fechaSalida = salidaTexto
                                                    , totalAdultos = sum (map adultosHab ocupaciones)
                                                    , totalNinos = sum (map ninosHab ocupaciones)
                                                    , ocupaciones = ocupaciones
                                                    , estadoReserva = Activa
                                                    , subtotalReserva = subtotal
                                                    }

                                            let sistemaActualizado = sistema
                                                    { estadoReservas = estadoReservas sistema ++ [nuevaReserva]
                                                    , nextIdReserva = nextIdReserva sistema + 1
                                                    }

                                            guardarReservaciones "reservaciones.txt" (estadoReservas sistemaActualizado)

                                            putStrLn "\n[OK] ¡Reservación registrada exitosamente!"
                                            mostrarComprobante nuevaReserva
                                            return sistemaActualizado


-- E: La reservación
-- S: Muestra el comprobante de la reservación en consola
-- R: La función muestra un resumen de la reservación con todos los detalles relevantes
-- F: Función que muestra un resumen de la reservación con todos los detalles relevantes
mostrarComprobante :: Reserva -> IO ()
mostrarComprobante reserva = do
    putStrLn "\n========== COMPROBANTE DE RESERVA =========="
    putStrLn ("Número de reserva: " ++ show (idReserva reserva))
    putStrLn ("Cliente: " ++ nombreCliente reserva)
    putStrLn ("Fecha de registro: " ++ fechaReserva reserva)
    putStrLn ("Entrada: " ++ fechaEntrada reserva)
    putStrLn ("Salida: " ++ fechaSalida reserva)
    putStrLn ("Adultos: " ++ show (totalAdultos reserva))
    putStrLn ("Niños: " ++ show (totalNinos reserva))
    putStrLn "Habitaciones:"
    mapM_ mostrarOcupacionComprobante (ocupaciones reserva)
    putStrLn ("Subtotal: " ++ show (subtotalReserva reserva))
    putStrLn ("Estado: " ++ show (estadoReserva reserva))
    putStrLn "============================================"

-- E: La ocupación de la habitación
-- S: Muestra los detalles de la ocupación en consola
-- R: La función muestra la información de la habitación ocupada junto con el número de adultos y niños
-- F: Función que muestra los detalles de la ocupación de la habitación
mostrarOcupacionComprobante :: OcupacionHab -> IO ()
mostrarOcupacionComprobante ocupacion = do
    let hab = habitacionOcupada ocupacion
    putStrLn ("  - " ++ idHabitacion hab 
           ++ " (" ++ nombreTH (tipoHabitacion hab) ++ ")"
           ++ " | Adultos: " ++ show (adultosHab ocupacion)
           ++ " | Niños: " ++ show (ninosHab ocupacion))

-- Cancelar una reservación


-- E: El sistema de estado
-- S: Muestra un mensaje de confirmación o error
-- R: La función cancela la reservación si está en estado activo
-- F: Función que cancela una reservación existente
cancelarReservacion :: EstadoSistema -> IO EstadoSistema
cancelarReservacion sistema = do
    putStrLn "\n=== CANCELAR RESERVACIÓN ===\n"

    if null (estadoReservas sistema)
        then do
            putStrLn "[AVISO] No hay reservaciones para cancelar."
            return sistema
        else do
            putStr "Ingrese el número de reserva a cancelar: "
            hFlush stdout
            idTexto <- getLine
-- Aquí se valida que el número de reserva ingresado sea un entero válido y que exista en la lista de reservas del sistema.
-- Si la reserva existe y está activa, se cambia su estado a Cancelada y se guarda la lista 
-- actualizada en el archivo de reservaciones. Si la reserva ya estaba cancelada o no existe, se muestra un mensaje de error.
            case leerEntero idTexto of
                Nothing -> do
                    putStrLn "[ERROR] Número de reserva inválido."
                    return sistema
                Just idBuscado ->
                    case find (\r -> idReserva r == idBuscado) (estadoReservas sistema) of
                        Nothing -> do
                            putStrLn "[ERROR] No se encontró esa reservación."
                            return sistema
                        Just reserva ->
                            if estadoReserva reserva == Cancelada
                                then do
                                    putStrLn "[AVISO] Esa reservación ya está cancelada."
                                    return sistema
                                else do
                                    let reservasActualizadas = map (\r ->
                                            if idReserva r == idBuscado
                                                then r { estadoReserva = Cancelada }
                                                else r) (estadoReservas sistema)

                                    let sistemaActualizado = sistema { estadoReservas = reservasActualizadas }

                                    guardarReservaciones "reservaciones.txt" reservasActualizadas
                                    putStrLn "[OK] Reservación cancelada exitosamente."
                                    return sistemaActualizado
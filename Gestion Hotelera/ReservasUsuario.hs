module ReservasUsuario where

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

-- ============================================
-- FUNCIONES AUXILIARES
-- ============================================

leerFecha :: String -> Maybe Day
leerFecha texto = parseTimeM True defaultTimeLocale "%Y-%m-%d" texto

textoValido :: String -> Bool
textoValido texto = not (null texto) && any (/= ' ') texto

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

-- ============================================
-- VALIDACIONES DE OCUPACIÓN
-- ============================================

seCruzan :: Day -> Day -> Day -> Day -> Bool
seCruzan entrada1 salida1 entrada2 salida2 =
    entrada1 < salida2 && entrada2 < salida1

validarHuespedes :: Habitacion -> Int -> Int -> Bool
validarHuespedes hab adultos ninos =
    adultos >= 0
    && ninos >= 0
    && adultos + ninos > 0
    && adultos + ninos <= maxHuespedes (tipoHabitacion hab)

validarOcupaciones :: [OcupacionHab] -> Bool
validarOcupaciones [] = True
validarOcupaciones (ocupacion:resto) =
    validarHuespedes (habitacionOcupada ocupacion) (adultosHab ocupacion) (ninosHab ocupacion)
    && validarOcupaciones resto

-- ============================================
-- DISPONIBILIDAD DE HABITACIONES
-- ============================================

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

obtenerHabitacionesDisponibles :: EstadoSistema -> Day -> Day -> [Habitacion]
obtenerHabitacionesDisponibles sistema entrada salida =
    filter disponible (estadoHab sistema)
    where
        disponible hab = not (habitacionOcupadaEnFechas hab entrada salida (estadoReservas sistema))

mostrarHabitacion :: Habitacion -> IO ()
mostrarHabitacion hab = do
    putStrLn ("  " ++ idHabitacion hab 
           ++ " - " ++ nombreTH (tipoHabitacion hab)
           ++ " (Capacidad: " ++ show (maxHuespedes (tipoHabitacion hab)) ++ ")")

-- ============================================
-- SELECCIÓN DE HABITACIONES
-- ============================================

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

-- ============================================
-- CÁLCULO DE TARIFAS
-- ============================================

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

-- ============================================
-- REGISTRAR RESERVACIÓN
-- ============================================

registrarReservacion :: EstadoSistema -> IO EstadoSistema
registrarReservacion sistema = do
    putStrLn "\n=== REGISTRAR NUEVA RESERVACIÓN ===\n"

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

mostrarOcupacionComprobante :: OcupacionHab -> IO ()
mostrarOcupacionComprobante ocupacion = do
    let hab = habitacionOcupada ocupacion
    putStrLn ("  - " ++ idHabitacion hab 
           ++ " (" ++ nombreTH (tipoHabitacion hab) ++ ")"
           ++ " | Adultos: " ++ show (adultosHab ocupacion)
           ++ " | Niños: " ++ show (ninosHab ocupacion))

-- ============================================
-- CANCELAR RESERVACIÓN
-- ============================================

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
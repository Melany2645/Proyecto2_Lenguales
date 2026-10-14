
module Reservaciones where

import Modelos
import Tarifas (buscarTarifa)
import Data.Time
    ( Day, DayOfWeek(..), defaultTimeLocale, parseTimeM
    , formatTime, getCurrentTime, addDays, dayOfWeek, toGregorian
    )
import Text.Read (readMaybe)
import Data.List (find)

-- Validaciones y funciones auxiliares para manejar reservaciones en el sistema de gestión hotelera.

leerFecha :: String -> Maybe Day
leerFecha texto =
    parseTimeM True defaultTimeLocale "%Y-%m-%d" texto

textoValido :: String -> Bool
textoValido texto =
    not (null texto) && any (/= ' ') texto

leerEntero :: String -> Maybe Int
leerEntero texto = readMaybe texto

validarFechas :: Day -> Day -> Bool
validarFechas entrada salida =
    entrada < salida

seCruzan :: Day -> Day -> Day -> Day -> Bool
seCruzan entrada1 salida1 entrada2 salida2 =
    entrada1 < salida2 && entrada2 < salida1

-- Validaciones de ocupación de habitaciones y reservas.

validarHuespedes :: Habitacion -> Int -> Int -> Bool
validarHuespedes hab adultos ninos =
    adultos >= 0
    && ninos >= 0
    && adultos + ninos > 0
    && adultos + ninos <= maxHuespedes (tipoHabitacion hab)

validarOcupaciones :: [OcupacionHab] -> Bool
validarOcupaciones [] = False
validarOcupaciones (ocupacion:resto) =
    validarHuespedes
        (habitacionOcupada ocupacion)
        (adultosHab ocupacion)
        (ninosHab ocupacion)
    && validarOcupaciones resto

-- Validar si una habitación está ocupada en un rango de fechas dado.

habitacionOcupadaEnFechas ::
    Habitacion -> Day -> Day -> [Reserva] -> Bool

habitacionOcupadaEnFechas hab entrada salida reservas =
    any estaReservada reservas
    where
        estaReservada reserva =
            estadoReserva reserva /= Cancelada
            && any mismaHabitacion (ocupaciones reserva)
            && seCruzaConReserva reserva

        mismaHabitacion ocupacion =
            idHabitacion hab
            == idHabitacion (habitacionOcupada ocupacion)

        seCruzaConReserva reserva =
            case (leerFecha (fechaEntrada reserva),
                  leerFecha (fechaSalida reserva)) of
                (Just entradaReserva, Just salidaReserva) ->
                    seCruzan entrada salida entradaReserva salidaReserva
                _ -> False

-- Consultar disponibilidad de habitaciones y registrar nuevas reservaciones.

obtenerHabitacionesDisponibles ::
    EstadoSistema -> Day -> Day -> [Habitacion]

obtenerHabitacionesDisponibles sistema entrada salida =
    filter disponible (estadoHab sistema)
    where
        disponible hab =
            not (habitacionOcupadaEnFechas
                hab entrada salida (estadoReservas sistema))

consultarDisponibilidad :: EstadoSistema -> IO ()
consultarDisponibilidad sistema = do
    putStrLn "\n=== CONSULTAR DISPONIBILIDAD ==="

    putStr "Fecha de entrada (AAAA-MM-DD): "
    entradaTexto <- getLine

    putStr "Fecha de salida (AAAA-MM-DD): "
    salidaTexto <- getLine

    case (leerFecha entradaTexto, leerFecha salidaTexto) of
        (Just entrada, Just salida) ->
            if validarFechas entrada salida
                then do
                    let disponibles =
                            obtenerHabitacionesDisponibles sistema entrada salida

                    if null disponibles
                        then putStrLn "No hay habitaciones disponibles."
                        else mostrarHabitaciones disponibles
                else
                    putStrLn "La salida debe ser posterior a la entrada."

        _ -> putStrLn "Formato de fecha incorrecto."

mostrarHabitaciones :: [Habitacion] -> IO ()
mostrarHabitaciones [] =
    putStrLn "No hay habitaciones para mostrar."

mostrarHabitaciones habitaciones =
    mapM_ mostrarHabitacion habitaciones

mostrarHabitacion :: Habitacion -> IO ()
mostrarHabitacion hab = do
    putStrLn
        ("Habitacion: " ++ idHabitacion hab
        ++ " | Tipo: " ++ nombreTH (tipoHabitacion hab)
        ++ " | Capacidad: "
        ++ show (maxHuespedes (tipoHabitacion hab)))

-- Seleccionar ocupaciones de habitaciones para una nueva reserva.

seleccionarOcupaciones ::
    [Habitacion] -> [OcupacionHab] -> IO [OcupacionHab]

seleccionarOcupaciones disponibles seleccionadas = do
    putStrLn "\nIngrese el ID de una habitacion."
    putStrLn "Presione ENTER para terminar la seleccion."
    putStr "ID: "
    codigo <- getLine

    if null codigo
        then return (reverse seleccionadas)
        else
            case find (\hab -> idHabitacion hab == codigo) disponibles of
                Nothing -> do
                    putStrLn "Habitacion no disponible o ID incorrecto."
                    seleccionarOcupaciones disponibles seleccionadas

                Just hab ->
                    if any
                        (\ocupacion ->
                            idHabitacion (habitacionOcupada ocupacion)
                            == idHabitacion hab)
                        seleccionadas
                    then do
                        putStrLn "Esa habitacion ya fue seleccionada."
                        seleccionarOcupaciones disponibles seleccionadas
                    else do
                        putStr "Cantidad de adultos: "
                        adultosTexto <- getLine

                        putStr "Cantidad de ninos: "
                        ninosTexto <- getLine

                        case (leerEntero adultosTexto, leerEntero ninosTexto) of
                            (Just adultos, Just ninos) ->
                                if validarHuespedes hab adultos ninos
                                    then do
                                        let nuevaOcupacion =
                                                OcupacionHab hab adultos ninos

                                        seleccionarOcupaciones
                                            disponibles
                                            (nuevaOcupacion : seleccionadas)
                                    else do
                                        putStrLn
                                            "Cantidad de huespedes no valida para esta habitacion."
                                        seleccionarOcupaciones
                                            disponibles seleccionadas

                            _ -> do
                                putStrLn "Debe ingresar cantidades enteras."
                                seleccionarOcupaciones
                                    disponibles seleccionadas

-- Calcular costos de reservaciones según tarifas y fechas.

codigoTarifaPara :: Day -> Bool -> Int
codigoTarifaPara dia esAdulto =
    let
        (_, mes, _) = toGregorian dia
        temporadaVerde = mes >= 4 && mes <= 10
        finDeSemana = dayOfWeek dia `elem` [Friday, Saturday]

        codigoBase =
            if temporadaVerde
                then if finDeSemana then 2 else 1
                else if finDeSemana then 6 else 5
    in
        if esAdulto then codigoBase else codigoBase + 2

calcularCostoDia ::
    [Tarifa] -> Day -> [OcupacionHab] -> Maybe Double

calcularCostoDia tarifas dia ocupacionesDia = do
    costos <- mapM calcularCosto ocupacionesDia
    return (sum costos)
    where
        calcularCosto ocupacion = do
            let cantidadAdultos = adultosHab ocupacion
            let cantidadNinos = ninosHab ocupacion

            let codigoAdulto = codigoTarifaPara dia True
            let codigoNino = codigoTarifaPara dia False

            tarifaAdulto <- buscarTarifa codigoAdulto tarifas
            tarifaNino <- buscarTarifa codigoNino tarifas

            return
                (fromIntegral cantidadAdultos * montoTarifa tarifaAdulto
                + fromIntegral cantidadNinos * montoTarifa tarifaNino)

-- Calcular el subtotal de una reservación considerando todas las ocupaciones y días.

calcularSubtotal ::
    [Tarifa] -> Day -> Day -> [OcupacionHab] -> Maybe Double

calcularSubtotal tarifas entrada salida ocupacionesDia =
    if entrada >= salida || null ocupacionesDia
        then Nothing
        else calcularPorDia entrada
    where
        calcularPorDia dia
            | dia >= salida = Just 0

            | otherwise = do
                costoHoy <- calcularCostoDia tarifas dia ocupacionesDia
                costoRestante <- calcularPorDia (addDays 1 dia)
                return (costoHoy + costoRestante)

-- Registrar, consultar y cancelar reservaciones en el sistema de gestión hotelera.

registrarReservacion :: EstadoSistema -> IO EstadoSistema
registrarReservacion sistema = do
    putStrLn "\n=== REGISTRAR RESERVACION ==="

    putStr "Nombre del cliente: "
    nombre <- getLine

    if not (textoValido nombre)
        then do
            putStrLn "El nombre no puede estar vacio."
            return sistema
        else do
            putStr "Fecha de entrada (AAAA-MM-DD): "
            entradaTexto <- getLine

            putStr "Fecha de salida (AAAA-MM-DD): "
            salidaTexto <- getLine

            case (leerFecha entradaTexto, leerFecha salidaTexto) of
                (Just entrada, Just salida) ->
                    if not (validarFechas entrada salida)
                        then do
                            putStrLn "La salida debe ser posterior a la entrada."
                            return sistema
                        else continuarRegistro
                            sistema nombre entrada salida
                            entradaTexto salidaTexto

                _ -> do
                    putStrLn "Una de las fechas no tiene el formato correcto."
                    return sistema

continuarRegistro ::
    EstadoSistema -> String -> Day -> Day -> String -> String
    -> IO EstadoSistema

continuarRegistro sistema nombre entrada salida entradaTexto salidaTexto = do
    let disponibles =
            obtenerHabitacionesDisponibles sistema entrada salida

    if null disponibles
        then do
            putStrLn "No hay habitaciones disponibles para esas fechas."
            return sistema
        else if null (estadoTarifas sistema)
            then do
                putStrLn "No hay tarifas cargadas."
                return sistema
            else do
                putStrLn "\nHabitaciones disponibles:"
                mostrarHabitaciones disponibles

                ocupacionesSeleccionadas <-
                    seleccionarOcupaciones disponibles []

                if not (validarOcupaciones ocupacionesSeleccionadas)
                    then do
                        putStrLn "Debe seleccionar habitaciones y huespedes validos."
                        return sistema
                    else
                        case calcularSubtotal
                            (estadoTarifas sistema)
                            entrada salida ocupacionesSeleccionadas of

                            Nothing -> do
                                putStrLn
                                    "No se puede calcular el monto. Revise las ocho tarifas."
                                return sistema

                            Just subtotal -> do
                                momento <- getCurrentTime

                                let nuevaReserva = Reserva
                                        { idReserva = nextIdReserva sistema
                                        , nombreCliente = nombre
                                        , fechaReserva =
                                            formatTime defaultTimeLocale
                                                "%Y-%m-%d %H:%M:%S" momento
                                        , fechaEntrada = entradaTexto
                                        , fechaSalida = salidaTexto
                                        , totalAdultos =
                                            sum (map adultosHab ocupacionesSeleccionadas)
                                        , totalNinos =
                                            sum (map ninosHab ocupacionesSeleccionadas)
                                        , ocupaciones = ocupacionesSeleccionadas
                                        , estadoReserva = Activa
                                        , subtotalReserva = subtotal
                                        }

                                let sistemaActualizado = sistema
                                        { estadoReservas =
                                            estadoReservas sistema ++ [nuevaReserva]
                                        , nextIdReserva = nextIdReserva sistema + 1
                                        }

                                putStrLn "\nReserva registrada correctamente."
                                mostrarComprobante nuevaReserva
                                return sistemaActualizado

-- Mostrar comprobante de reservación y detalles de ocupación.

mostrarComprobante :: Reserva -> IO ()
mostrarComprobante reserva = do
    putStrLn "\n========== COMPROBANTE DE RESERVA =========="
    putStrLn ("Numero de reserva: " ++ show (idReserva reserva))
    putStrLn ("Cliente: " ++ nombreCliente reserva)
    putStrLn ("Fecha de registro: " ++ fechaReserva reserva)
    putStrLn ("Entrada: " ++ fechaEntrada reserva)
    putStrLn ("Salida: " ++ fechaSalida reserva)
    putStrLn ("Adultos: " ++ show (totalAdultos reserva))
    putStrLn ("Ninos: " ++ show (totalNinos reserva))

    putStrLn "Habitaciones:"
    mapM_ mostrarOcupacion (ocupaciones reserva)

    putStrLn ("Subtotal: " ++ show (subtotalReserva reserva))
    putStrLn ("Estado: " ++ show (estadoReserva reserva))
    putStrLn "============================================"

mostrarOcupacion :: OcupacionHab -> IO ()
mostrarOcupacion ocupacion = do
    putStrLn
        ("  ID: " ++ idHabitacion (habitacionOcupada ocupacion)
        ++ " | Tipo: "
        ++ nombreTH (tipoHabitacion (habitacionOcupada ocupacion))
        ++ " | Adultos: " ++ show (adultosHab ocupacion)
        ++ " | Ninos: " ++ show (ninosHab ocupacion))

-- consultar, mostrar y cancelar reservaciones en el sistema de gestión hotelera.

consultarReservaciones :: [Reserva] -> IO ()
consultarReservaciones [] =
    putStrLn "No hay reservaciones registradas."

consultarReservaciones reservas = do
    putStrLn "\n========== RESERVACIONES =========="
    mapM_ mostrarComprobante reservas

-- Cancelar una reservación existente en el sistema de gestión hotelera.

cancelarReservacion :: EstadoSistema -> IO EstadoSistema
cancelarReservacion sistema = do
    putStrLn "\n=== CANCELAR RESERVACION ==="
    putStr "Ingrese el numero de reserva: "
    textoId <- getLine

    case leerEntero textoId of
        Nothing -> do
            putStrLn "El numero de reserva no es valido."
            return sistema

        Just codigo ->
            case find (\reserva -> idReserva reserva == codigo)
                (estadoReservas sistema) of

                Nothing -> do
                    putStrLn "No se encontro esa reservacion."
                    return sistema

                Just reserva ->
                    if estadoReserva reserva == Cancelada
                        then do
                            putStrLn "La reservacion ya esta cancelada."
                            return sistema
                        else do
                            let reservasActualizadas =
                                    map cancelarSiCoincide
                                        (estadoReservas sistema)

                            let sistemaActualizado = sistema
                                    { estadoReservas = reservasActualizadas }

                            putStrLn "Reservacion cancelada correctamente."
                            return sistemaActualizado
                    where
                        cancelarSiCoincide reserva =
                            if idReserva reserva == codigo
                                then reserva { estadoReserva = Cancelada }
                                else reserva

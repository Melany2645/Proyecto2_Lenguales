module Estadisticas where

import Modelos
import Ocupacion (habitacionesOcupadas, habitacionesLibres)
import Data.Time
import Data.Time.Calendar.WeekDate (toWeekDate)
import Data.List (sortBy)
import Data.Ord (comparing)


-- ==========================================
-- CALCULO DEL MONTO RECAUDADO
-- ==========================================

-- F: totalConImpuesto
-- E: una reservacion
-- S: monto total con impuesto
-- O: calcular el subtotal mas el 13%

totalConImpuesto :: Reserva -> Double
totalConImpuesto reserva =
    let subtotal = subtotalReserva reserva
        impuesto = fromIntegral (round (subtotal * 0.13))
    in subtotal + impuesto


-- F: montoRecaudado
-- E: lista de reservaciones
-- S: monto total recaudado
-- O: sumar los montos de las reservaciones facturadas

montoRecaudado :: [Reserva] -> Double
montoRecaudado reservas =
    sum [ totalConImpuesto reserva
        | reserva <- reservas
        , estadoReserva reserva == Facturada
        ]


-- ==========================================
-- TOTAL DE HUESPEDES
-- ==========================================

-- F: totalHuespedes
-- E: mes, año y lista de reservaciones
-- S: cantidad total de huespedes
-- O: contar adultos y niños por mes y año de entrada

totalHuespedes :: String -> String -> [Reserva] -> Int
totalHuespedes mes anio reservas =
    sum [ totalAdultos reserva + totalNinos reserva
        | reserva <- reservas
        , estadoReserva reserva /= Cancelada
        , take 2 (drop 5 (fechaEntrada reserva)) == mes
        , take 4 (fechaEntrada reserva) == anio
        ]


-- ==========================================
-- MANEJO DE FECHAS
-- ==========================================

-- F: convertirFecha
-- E: fecha escrita como texto
-- S: fecha convertida o Nothing
-- O: convertir una fecha al tipo Day

convertirFecha :: String -> Maybe Day
convertirFecha fecha =
    parseTimeM True defaultTimeLocale "%Y-%m-%d" fecha


-- F: diasReserva
-- E: una reservacion
-- S: lista de dias ocupados
-- O: obtener los dias desde la entrada hasta antes de la salida

diasReserva :: Reserva -> [Day]
diasReserva reserva =
    case (convertirFecha (fechaEntrada reserva),
          convertirFecha (fechaSalida reserva)) of

        (Just entrada, Just salida) ->
            [entrada .. addDays (-1) salida]

        _ -> []


-- ==========================================
-- ESTADISTICAS DE OCUPACION
-- ==========================================

-- F: contarOcupacion
-- E: numero del dia de la semana y reservaciones
-- S: cantidad de habitaciones ocupadas
-- O: contar ocupaciones para un dia de la semana

contarOcupacion :: Int -> [Reserva] -> Int
contarOcupacion numero reservas =
    sum [ length (ocupaciones reserva)
        | reserva <- reservas
        , estadoReserva reserva /= Cancelada
        , dia <- diasReserva reserva
        , let (_, _, diaSemana) = toWeekDate dia
        , diaSemana == numero
        ]


-- F: topTresDias
-- E: lista de reservaciones
-- S: tres dias con mayor ocupacion
-- O: ordenar los dias segun la cantidad de ocupaciones

topTresDias :: [Reserva] -> [(String, Int)]
topTresDias reservas =
    take 3 (sortBy (flip (comparing snd)) datos)
    where
        nombres =
            ["Lunes", "Martes", "Miercoles",
             "Jueves", "Viernes", "Sabado", "Domingo"]

        datos =
            [ (nombre, contarOcupacion numero reservas)
            | (numero, nombre) <- zip [1..7] nombres
            ]


-- ==========================================
-- VALIDACIONES
-- ==========================================

-- F: validarMes
-- E: mes ingresado por el usuario
-- S: True o False
-- O: aceptar solamente meses del 01 al 12

validarMes :: String -> Bool
validarMes mes =
    mes `elem` ["01", "02", "03", "04", "05", "06",
                "07", "08", "09", "10", "11", "12"]


-- F: validarAnio
-- E: año ingresado por el usuario
-- S: True o False
-- O: comprobar que el año tenga cuatro digitos

validarAnio :: String -> Bool
validarAnio anio =
    length anio == 4
    && all (`elem` ['0'..'9']) anio
    && anio /= "0000"


-- F: validarFechaEst
-- E: fecha ingresada por el usuario
-- S: True o False
-- O: comprobar que la fecha exista y tenga el formato correcto

validarFechaEst :: String -> Bool
validarFechaEst fecha =
    length fecha == 10
    && fecha !! 4 == '-'
    && fecha !! 7 == '-'
    && case convertirFecha fecha of
        Just _ -> True
        Nothing -> False


-- ==========================================
-- MOSTRAR HABITACIONES
-- ==========================================

-- F: mostrarHabitaciones
-- E: lista de habitaciones
-- S: muestra los codigos
-- O: mostrar las habitaciones sin mensajes adicionales

mostrarHabitaciones :: [Habitacion] -> IO ()
mostrarHabitaciones [] = return ()

mostrarHabitaciones (habitacion:resto) = do
    putStrLn (idHabitacion habitacion)
    mostrarHabitaciones resto


-- ==========================================
-- MENU DE ESTADISTICAS
-- ==========================================

-- F: menuEstadisticas
-- E: estado actual del sistema
-- S: muestra las estadisticas del hotel
-- O: permitir consultar las estadisticas

menuEstadisticas :: EstadoSistema -> IO ()
menuEstadisticas sistema = do

    putStrLn ""
    putStrLn "================================"
    putStrLn "      MENU DE ESTADISTICAS"
    putStrLn "================================"
    putStrLn "1. Total de huespedes por mes y año"
    putStrLn "2. Habitaciones ocupadas por dia"
    putStrLn "3. Habitaciones libres por dia"
    putStrLn "4. Monto recaudado"
    putStrLn "5. Top 3 de dias con mas ocupacion"
    putStrLn "6. Volver"
    putStrLn "================================"
    putStrLn "Seleccione una opcion:"

    opcion <- getLine

    case opcion of

        -- OPCION 1: TOTAL DE HUESPEDES
        "1" -> do
            putStrLn "Ingrese el mes (01 al 12):"
            mes <- getLine

            putStrLn "Ingrese el año (AAAA):"
            anio <- getLine

            if validarMes mes && validarAnio anio
                then do
                    let total =
                            totalHuespedes mes anio
                                (estadoReservas sistema)

                    putStrLn ""
                    putStrLn ("Total de huespedes: " ++ show total)

                else do
                    putStrLn "ERROR: Mes o año incorrecto."
                    putStrLn "El mes debe ser del 01 al 12."
                    putStrLn "El año debe tener cuatro digitos."

            menuEstadisticas sistema


        -- OPCION 2: HABITACIONES OCUPADAS
        "2" -> do
            putStrLn "Ingrese una fecha (AAAA-MM-DD):"
            fecha <- getLine

            if validarFechaEst fecha
                then do
                    let ocupadas =
                            habitacionesOcupadas fecha
                                (estadoHab sistema)
                                (estadoReservas sistema)

                    putStrLn ""
                    putStrLn "HABITACIONES OCUPADAS"
                    mostrarHabitaciones ocupadas

                    putStrLn
                        ("Total ocupadas: " ++ show (length ocupadas))

                else
                    putStrLn "ERROR: Fecha incorrecta. Use AAAA-MM-DD."

            menuEstadisticas sistema


        -- OPCION 3: HABITACIONES LIBRES
        "3" -> do
            putStrLn "Ingrese una fecha (AAAA-MM-DD):"
            fecha <- getLine

            if validarFechaEst fecha
                then do
                    let libres =
                            habitacionesLibres fecha
                                (estadoHab sistema)
                                (estadoReservas sistema)

                    putStrLn ""
                    putStrLn "HABITACIONES DISPONIBLES"
                    mostrarHabitaciones libres

                    putStrLn
                        ("Total disponibles: " ++ show (length libres))

                else
                    putStrLn "ERROR: Fecha incorrecta. Use AAAA-MM-DD."

            menuEstadisticas sistema


        -- OPCION 4: MONTO RECAUDADO
        "4" -> do
            let monto =
                    montoRecaudado (estadoReservas sistema)

            putStrLn ""
            putStrLn "MONTO RECAUDADO"
            putStrLn ("Total recaudado: " ++ show monto)

            menuEstadisticas sistema


        -- OPCION 5: TOP 3 DE DIAS
        "5" -> do
            putStrLn ""
            putStrLn "TOP 3 DE DIAS CON MAS OCUPACION"

            let resultado =
                    topTresDias (estadoReservas sistema)

            mapM_ mostrarDia resultado

            menuEstadisticas sistema


        -- OPCION 6: VOLVER
        "6" -> do
            putStrLn "Regresando al menu administrativo..."
            return ()


        -- OPCION INCORRECTA
        _ -> do
            putStrLn "ERROR: Opcion no valida."
            putStrLn "Seleccione un numero del 1 al 6."

            menuEstadisticas sistema


-- ==========================================
-- MOSTRAR DIAS DEL TOP 3
-- ==========================================

-- F: mostrarDia
-- E: nombre del dia y cantidad de ocupaciones
-- S: muestra el resultado
-- O: presentar los dias con mayor ocupacion

mostrarDia :: (String, Int) -> IO ()
mostrarDia (nombre, cantidad) =
    putStrLn
        (nombre ++ ": " ++ show cantidad ++ " ocupaciones")
module Estadisticas where

import Modelos


-- F: totalConImpuesto
-- E: una reservacion
-- S: monto de la reserva incluyendo el 13%
-- O: obtener el total de una reserva con impuesto
totalConImpuesto :: Reserva -> Double
totalConImpuesto reserva =
    subtotalReserva reserva * 1.13


-- F: montoRecaudado
-- E: lista de reservaciones
-- S: dinero total recaudado
-- O: sumar los montos de las reservas que no se encuentran canceladas
montoRecaudado :: [Reserva] -> Double
montoRecaudado reservas =
    sum
        [ totalConImpuesto reserva
        | reserva <- reservas
        , estadoReserva reserva /= Cancelada
        ]


-- F: menuEstadisticas
-- E: ninguna
-- S: muestra las opciones de estadisticas
-- O: permitir al usuario seleccionar una estadistica
menuEstadisticas :: IO ()
menuEstadisticas = do

    putStrLn ""
    putStrLn "=============================="
    putStrLn "     MENU DE ESTADISTICAS"
    putStrLn "=============================="

    putStrLn "1. Total de huespedes por mes y año"
    putStrLn "2. Habitaciones ocupadas por dia"
    putStrLn "3. Habitaciones libres por dia"
    putStrLn "4. Monto recaudado"
    putStrLn "5. Top 3 de dias con mas ocupacion"
    putStrLn "6. Volver"

    putStrLn "Seleccione una opcion:"
    opcion <- getLine

    case opcion of

        "1" -> do
            putStrLn "Total de huespedes"
            menuEstadisticas

        "2" -> do
            putStrLn "Habitaciones ocupadas"
            menuEstadisticas

        "3" -> do
            putStrLn "Habitaciones libres"
            menuEstadisticas

        "4" -> do
            putStrLn "Monto recaudado"
            menuEstadisticas

        "5" -> do
            putStrLn "Top 3 de dias"
            menuEstadisticas

        "6" ->
            return ()

        _ -> do
            putStrLn "Opcion no valida"
            menuEstadisticas


-- F: totalHuespedes
-- E: mes, año y lista de reservas
-- S: cantidad total de huespedes
-- O: contar los huespedes de las reservas del mes y año

totalHuespedes :: String -> String -> [Reserva] -> Int
totalHuespedes mes anio reservas =

    sum [ adultos reserva + ninos reserva
        | reserva <- reservas

        -- Revisamos que la reserva no este cancelada
        , estadoReserva reserva /= Cancelada

        -- Revisamos el mes
        , take 2 (drop 5 (fechaEntrada reserva)) == mes

        -- Revisamos el año
        , take 4 (fechaEntrada reserva) == anio
        ]
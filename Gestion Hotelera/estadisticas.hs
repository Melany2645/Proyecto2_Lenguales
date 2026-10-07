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
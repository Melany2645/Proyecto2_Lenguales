
module Ocupacion where

import Modelos


-- F: habitacionOcupada
-- E: habitacion, fecha y lista de reservas
-- S: True si esta ocupada, False si esta libre
-- O: revisar si una habitacion esta ocupada en una fecha

habitacionOcupada :: Habitacion -> String -> [Reserva] -> Bool
habitacionOcupada habitacion fecha reservas =

    or [ idHabitacion habitacion `elem` map idHabitacion (habitaciones reserva)
       && fecha >= fechaEntrada reserva
       && fecha < fechaSalida reserva
       && estadoReserva reserva /= Cancelada
       | reserva <- reservas
       ]


-- F: habitacionesOcupadas
-- E: fecha, habitaciones y reservaciones
-- S: lista de habitaciones ocupadas
-- O: obtener las habitaciones ocupadas en una fecha

habitacionesOcupadas :: String -> [Habitacion] -> [Reserva] -> [Habitacion]
habitacionesOcupadas fecha listaHabitaciones reservas =

    [ habitacion
    | habitacion <- listaHabitaciones
    , habitacionOcupada habitacion fecha reservas
    ]


-- F: habitacionesLibres
-- E: fecha, habitaciones y reservaciones
-- S: lista de habitaciones libres
-- O: obtener las habitaciones disponibles en una fecha

habitacionesLibres :: String -> [Habitacion] -> [Reserva] -> [Habitacion]
habitacionesLibres fecha listaHabitaciones reservas =

    [ habitacion
    | habitacion <- listaHabitaciones
    , not (habitacionOcupada habitacion fecha reservas)
    ]

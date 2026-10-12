module Ocupacion where

import Modelos


-- F: estaOcupada
-- E: habitacion, fecha y lista de reservas
-- S: True si esta ocupada, False si esta libre
-- O: revisar si una habitacion esta ocupada en una fecha

estaOcupada :: Habitacion -> String -> [Reserva] -> Bool
estaOcupada habitacion fecha reservas =

    or [ idHabitacion habitacion == idHabitacion (habitacionOcupada ocupacion)
       && fecha >= fechaEntrada reserva
       && fecha < fechaSalida reserva
       && estadoReserva reserva /= Cancelada

       | reserva <- reservas
       , ocupacion <- ocupaciones reserva
       ]


-- F: habitacionesOcupadas
-- E: fecha, lista de habitaciones y reservas
-- S: lista de habitaciones ocupadas
-- O: mostrar las habitaciones ocupadas en una fecha

habitacionesOcupadas :: String -> [Habitacion] -> [Reserva] -> [Habitacion]
habitacionesOcupadas fecha listaHabitaciones reservas =

    [ habitacion
    | habitacion <- listaHabitaciones
    , estaOcupada habitacion fecha reservas
    ]


-- F: habitacionesLibres
-- E: fecha, lista de habitaciones y reservas
-- S: lista de habitaciones libres
-- O: mostrar las habitaciones disponibles en una fecha

habitacionesLibres :: String -> [Habitacion] -> [Reserva] -> [Habitacion]
habitacionesLibres fecha listaHabitaciones reservas =

    [ habitacion
    | habitacion <- listaHabitaciones
    , not (estaOcupada habitacion fecha reservas)
    ]

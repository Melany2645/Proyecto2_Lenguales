module Ocupacion where

import Modelos
import Data.Time (Day, defaultTimeLocale, parseTimeM)


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

-- F: validarFecha
-- E: fecha ingresada por el usuario
-- S: True si la fecha existe, False si no
-- O: evitar fechas incorrectas

validarFecha :: String -> Bool
validarFecha fecha =
    case parseTimeM True defaultTimeLocale "%Y-%m-%d" fecha :: Maybe Day of
        Just _ -> length fecha == 10
        Nothing -> False


-- F: pedirFecha
-- E: fecha escrita por el usuario
-- S: fecha valida
-- O: solicitar una fecha hasta que sea correcta

pedirFecha :: IO String
pedirFecha = do
    putStrLn "Ingrese una fecha (AAAA-MM-DD):"
    fecha <- getLine

    if validarFecha fecha
        then return fecha
        else do
            putStrLn "ERROR: Fecha incorrecta. Use AAAA-MM-DD y una fecha real."
            pedirFecha


-- F: verDisponibilidad
-- E: habitaciones y reservaciones
-- S: lista de habitaciones disponibles
-- O: mostrar las habitaciones libres para una fecha

verDisponibilidad :: [Habitacion] -> [Reserva] -> IO ()
verDisponibilidad habitaciones reservas = do
    fecha <- pedirFecha

    let libres = habitacionesLibres fecha habitaciones reservas

    putStrLn ""
    putStrLn "HABITACIONES DISPONIBLES"

    if null libres
        then putStrLn "No hay habitaciones disponibles para esa fecha."
        else mapM_ (\h -> do
            putStrLn ("Habitacion: " ++ idHabitacion h)
            putStrLn ("Tipo: " ++ nombreTH (tipoHabitacion h))
            putStrLn ("Descripcion: " ++ descripcion (tipoHabitacion h))
            putStrLn ("Capacidad maxima: " ++ show (maxHuespedes (tipoHabitacion h)) ++ " huespedes")
            putStrLn "----------------------------------------"
            ) libres


-- F: consultarOcupacion
-- E: habitaciones y reservaciones
-- S: estado de ocupacion
-- O: mostrar habitaciones ocupadas y disponibles

consultarOcupacion :: [Habitacion] -> [Reserva] -> IO ()
consultarOcupacion habitaciones reservas = do
    fecha <- pedirFecha

    let ocupadas = habitacionesOcupadas fecha habitaciones reservas
    let libres = habitacionesLibres fecha habitaciones reservas

    putStrLn ""
    putStrLn "ESTADO DE OCUPACION"

    putStrLn "Habitaciones ocupadas:"
    if null ocupadas
        then putStrLn "No hay habitaciones ocupadas."
        else mapM_ (\h -> do
            putStrLn ("Habitacion: " ++ idHabitacion h)
            putStrLn ("Tipo: " ++ nombreTH (tipoHabitacion h))
            putStrLn ("Descripcion: " ++ descripcion (tipoHabitacion h))
            putStrLn ("Capacidad maxima: " ++
                show (maxHuespedes (tipoHabitacion h)) ++ " huespedes")
            putStrLn "----------------------------------------"
            ) ocupadas

    putStrLn ""
    putStrLn "Habitaciones disponibles:"
    if null libres
        then putStrLn "No hay habitaciones disponibles."
        else mapM_ (\h -> do
            putStrLn ("Habitacion: " ++ idHabitacion h)
            putStrLn ("Tipo: " ++ nombreTH (tipoHabitacion h))
            putStrLn ("Descripcion: " ++ descripcion (tipoHabitacion h))
            putStrLn ("Capacidad maxima: " ++
                show (maxHuespedes (tipoHabitacion h)) ++ " huespedes")
            putStrLn "----------------------------------------"
            ) libres

    putStrLn ""
    putStrLn ("Total ocupadas: " ++ show (length ocupadas))
    putStrLn ("Total disponibles: " ++ show (length libres))
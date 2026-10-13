module Reservaciones where

import Data.Time (Day, parseTimeM, defaultTimeLocale)
import Data.List
import Data.Char (toLower, IsSpace)
import Text.Read (readMaybe)

-- Primero los tipos de datos

data Estado = Disponible | Reservado | Cancelado deriving (Show, Eq) 
data Habitacion = Habitacion {
    idHabitacion :: Int,
    tipoHabitacion :: String,
    maxHuespedes :: Int
} deriving (Show, Eq)

data Ocupacion = Ocupacion {
    habitacion :: Habitacion,
    adultos :: Int,
    ninos :: Int
} deriving (Show, Eq)

data Reservacion = Reservacion {
    idReservacion :: Int,
    nombreReserva :: String,
    fechaEntrada :: Day,
    fechaSalida :: Day,
    cantidadAdultos :: Int,
    cantidadNinos :: Int,
    habitacionesReservadas :: [Ocupacion],
    estadoReservacion :: Estado,
    montoReservacion :: Double
} deriving (Show, Eq)


-- Todo tipo de validaciones, enteros, positivos, chars, fechas, cantidad por habitación
-- Validación de cantidades de huéspedes
-- Validación de espacios vacios

-- Validamos que el texto no esté vacío ni contenga solo espacios
textoValido :: String -> Bool
textoValido texto = not (all isSpace texto)

-- Comparar los textos sin distinguir mayúsculas y minúsculas
igualSinDistinguir :: String -> String -> Bool
igualSinDistinguir texto1 texto2 =
    map toLower texto1 == map toLower texto2

-- Validación de enteros, no se aceptan ni letras ni decimales
leerEntero :: String -> Maybe Int
leerEntero texto = 
    readMaybe texto

-- Validación de positivos
esPositivo :: Int -> Bool
esPositivo n = n > 0

-- Validación de que sea cero o positivo
esCeroOPositivo :: Int -> Bool
esCeroOPositivo n = n >= 0

-- Validación de fechas, que sean válidas y que la fecha de entrada sea menor a la de salida
leerFecha :: String -> Maybe Day
leerFecha texto =
    parseTimeM True defaultTimeLocale "%Y-%m-%d" texto

validarHuespedes :: Habitacion -> Int -> Int -> Bool
validarHuespedes habitacion adultos ninos =
    (adultos + ninos) <= maxHuespedes habitacion
    && adultos + ninos > 0

-- Validación de fechas

validarFechas :: Day -> Day -> Bool
validarFechas fechaEntrada fechaSalida =
    fechaEntrada < fechaSalida

-- Validación de si dos periodos se cruzan

seCruzan :: Day -> Day -> Day -> Day -> Bool
seCruzan entrada1 salida1 entrada2 salida2 =
    entrada1 < salida2 && entrada2 < salida1

-- Validación de disponibilidad de una habitación

habitacionOcupada :: Habitacion -> Day -> Day -> [Reservacion] -> Bool
habitacionOcupada habita fechaEntrada fechaSalida reservas = 
    any estaOcupada reservas
    where 
        estaOcupada reserva = 
            estadoReservacion reserva == Activa
            && any (\ocupacion -> 
                idHabitacion (habitacion ocupacion) == idHabitacion habita
                && seCruzan fechaEntrada fechaSalida (fechaEntrada (reserva)) (fechaSalida (reserva))) (habitacionesReservadas reserva)

-- Consultar disponibilidad de una habitación

habitacionesDisponibilidad :: [Habitacion] -> Day -> Day -> [Reservacion] -> [Habitacion]
habitacionesDisponibilidad habitaciones fechaEntrada fechaSalida reservas =
    filter disponible habitaciones
    where
        disponible habitacion = not (habitacionOcupada habitacion fechaEntrada fechaSalida reservas)
        
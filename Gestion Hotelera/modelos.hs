module Modelos where

-- Estados que puede tener una reservacion
-- Una reserva inicia Activa y luego puede pasar
-- a Facturada o Cancelada
data Estado = Activa | Facturada | Cancelada
    deriving (Show, Eq)


-- Informacion basica de una habitacion
-- Guarda el numero de habitacion y su tipo
data Habitacion = Habitacion
    { idHabitacion :: Int
    , tipoHabitacion :: String
    } deriving (Show, Eq)


-- Informacion que necesitamos de una reservacion
data Reserva = Reserva
    { idReserva :: Int              -- Codigo de la reserva
    , nombreCliente :: String       -- Persona que hizo la reserva
    , fechaEntrada :: String        -- Fecha de entrada al hotel
    , fechaSalida :: String         -- Fecha de salida del hotel
    , adultos :: Int                -- Cantidad de adultos
    , ninos :: Int                  -- Cantidad de niños
    , habitaciones :: [Habitacion]  -- Habitaciones reservadas
    , estadoReserva :: Estado       -- Activa, Facturada o Cancelada
    , subtotalReserva :: Double     -- Precio antes del impuesto
    } deriving (Show, Eq)


-- Informacion necesaria para una factura
data Factura = Factura
    { idFactura :: Int              -- Codigo de la factura
    , reservaFactura :: Int         -- Codigo de la reserva facturada
    , subtotalFactura :: Double     -- Monto antes del impuesto
    , impuestoFactura :: Double     -- Impuesto del 13%
    , totalFactura :: Double        -- Subtotal + impuesto
    } deriving (Show, Eq)
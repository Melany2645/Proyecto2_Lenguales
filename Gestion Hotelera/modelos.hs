module Modelos where

-- COMPONENTE 1: Modelos de datos del sistema hotelero 

-- Este módulo define los tipos de datos base del sistema hotelero.
-- Representa la información del hotel, las habitaciones, las tarifas,
-- las ocupaciones, las reservas, las facturas y el estado global del sistema.
--
-- La idea central es que todo el proyecto trabaje con un mismo modelo de datos,
-- evitando inconsistencias entre la lógica de reservas, facturación y estadísticas.

-- Estadías 

-- Estados que puede tener una reservación
data Estado = Activa | Facturada | Cancelada
    deriving (Show, Read, Eq)


-- Información del hotel

-- Datos de la empresa gestora del hotel
data HotelInfo = HotelInfo
    { nombreEmpresa  :: String
    , cedulaJuridica :: String
    , sitioWeb       :: String
    , telefono       :: String
    , pais           :: String
    , provincia      :: String
    } deriving (Show, Eq)


-- Tipos de habitaciones

-- Tipo de habitación
data TipoHabitacion = TipoHabitacion
    { nombreTH     :: String   -- Nombre único del tipo
    , descripcion  :: String   -- Descripción del tipo
    , maxHuespedes :: Int      -- Máximo de huéspedes permitidos
    } deriving (Show, Read, Eq)


-- Habitaciones

-- Habitación individual del hotel
data Habitacion = Habitacion
    { idHabitacion   :: String          -- Identificador (ej: "H001")
    , tipoHabitacion :: TipoHabitacion  -- Tipo al que pertenece
    } deriving (Show, Read, Eq)


-- Tarifas

-- Tarifa por persona según temporada y tipo de huésped
data Tarifa = Tarifa
    { codigoTarifa :: Int      -- Código 1 a 8
    , montoTarifa  :: Double   -- Monto en colones por persona por día
    } deriving (Show, Eq)


-- Ocupación de habitaciones

-- Detalle de ocupación de una habitación dentro de una reserva
data OcupacionHab = OcupacionHab
    { habitacionOcupada :: Habitacion  -- La habitación específica
    , adultosHab        :: Int         -- Adultos en esta habitación
    , ninosHab          :: Int         -- Niños en esta habitación
    } deriving (Show, Read, Eq)


-- Reservaciones

-- Información completa de una reservación
data Reserva = Reserva
    { idReserva        :: Int              -- Código de la reserva
    , nombreCliente    :: String           -- Persona que hizo la reserva
    , fechaReserva     :: String           -- Fecha y hora en que se hizo
    , fechaEntrada     :: String           -- Fecha de entrada al hotel
    , fechaSalida      :: String           -- Fecha de salida del hotel
    , totalAdultos     :: Int              -- Cantidad total de adultos
    , totalNinos       :: Int              -- Cantidad total de niños
    , ocupaciones      :: [OcupacionHab]   -- Detalle por habitación
    , estadoReserva    :: Estado           -- Activa, Facturada o Cancelada
    , subtotalReserva  :: Double           -- Precio antes del impuesto
    } deriving (Show, Read, Eq)


-- Facturas

-- Información necesaria para una factura
data Factura = Factura
    { idFactura       :: Int      -- Código de la factura
    , reservaFactura  :: Int      -- Código de la reserva facturada
    , subtotalFactura :: Double   -- Monto antes del impuesto
    , impuestoFactura :: Double   -- Impuesto del 13%
    , totalFactura    :: Double   -- Subtotal + impuesto
    } deriving (Show, Eq)


-- Estado del sistema

-- Estado completo del sistema
data EstadoSistema = EstadoSistema
    { estadoInfo     :: Maybe HotelInfo
    , estadoTipos    :: [TipoHabitacion]
    , estadoHab      :: [Habitacion]
    , estadoTarifas  :: [Tarifa]
    , estadoReservas :: [Reserva]
    , estadoFacturas :: [Factura]
    , nextIdReserva  :: Int
    , nextIdFactura  :: Int
    } deriving (Show)


-- Estado inicial del sistema
estadoInicial :: EstadoSistema
estadoInicial = EstadoSistema
    { estadoInfo     = Nothing
    , estadoTipos    = []
    , estadoHab      = []
    , estadoTarifas  = []
    , estadoReservas = []
    , estadoFacturas = []
    , nextIdReserva  = 1
    , nextIdFactura  = 1
    }
module Modelos where

-- ============================================
-- ESTADOS Y TIPOS BÁSICOS
-- ============================================

-- | Estados que puede tener una reservación
data Estado = Activa | Facturada | Cancelada
    deriving (Show, Eq)


-- ============================================
-- INFORMACIÓN DEL HOTEL 
-- ============================================

-- | Datos de la empresa gestora del hotel
data HotelInfo = HotelInfo
    { nombreEmpresa  :: String
    , cedulaJuridica :: String
    , sitioWeb       :: String
    , telefono       :: String
    , pais           :: String
    , provincia      :: String
    } deriving (Show, Eq)


-- ============================================
-- TIPOS DE HABITACIÓN 
-- ============================================

-- | Tipo de habitación (Estandar, Suite, etc.)
data TipoHabitacion = TipoHabitacion
    { nombreTH     :: String   -- Nombre único del tipo
    , descripcion  :: String   -- Descripción del tipo
    , maxHuespedes :: Int      -- Máximo de huéspedes permitidos
    } deriving (Show, Eq)


-- ============================================
-- HABITACIONES
-- ============================================

-- | Habitación individual del hotel
data Habitacion = Habitacion
    { idHabitacion   :: String          -- Identificador (ej: "H001")
    , tipoHabitacion :: TipoHabitacion  -- Tipo al que pertenece
    } deriving (Show, Eq)


-- ============================================
-- TARIFAS
-- ============================================

-- | Tarifa por persona según temporada y tipo de huésped
data Tarifa = Tarifa
    { codigoTarifa :: Int      -- Código 1 a 8
    , montoTarifa  :: Double   -- Monto en colones por persona por día
    } deriving (Show, Eq)


-- ============================================
-- OCUPACIÓN DE HABITACIÓN
-- ============================================

-- | Detalle de ocupación de una habitación dentro de una reserva
data OcupacionHab = OcupacionHab
    { habitacionOcupada :: Habitacion  -- La habitación específica
    , adultosHab        :: Int         -- Adultos en esta habitación
    , ninosHab          :: Int         -- Niños en esta habitación
    } deriving (Show, Eq)


-- ============================================
-- RESERVACIONES
-- ============================================

-- | Información completa de una reservación
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
    } deriving (Show, Eq)


-- ============================================
-- FACTURAS
-- ============================================

-- | Información necesaria para una factura
data Factura = Factura
    { idFactura       :: Int      -- Código de la factura
    , reservaFactura  :: Int      -- Código de la reserva facturada
    , subtotalFactura :: Double   -- Monto antes del impuesto
    , impuestoFactura :: Double   -- Impuesto del 13%
    , totalFactura    :: Double   -- Subtotal + impuesto
    } deriving (Show, Eq)


-- ============================================
-- ESTADO GLOBAL DEL SISTEMA 
-- ============================================

-- | Estado completo del sistema
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


-- | Estado inicial del sistema
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
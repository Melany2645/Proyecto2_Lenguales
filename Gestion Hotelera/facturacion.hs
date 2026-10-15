module Facturacion where

-- ============================================
-- COMPONENTE 3: FACTURACIÓN
-- ============================================
-- Este módulo se encarga de convertir una reserva activa en una factura válida.
-- Revisa el estado de la reserva, calcula impuestos y total, actualiza la reservación
-- como facturada y genera el comprobante económico para el cliente.
--
-- La facturación es la capa que conecta la operación hotelera con el cobro final.
-- Sin esta parte, el sistema solo registraría la reserva, pero no produciría la salida
-- financiera del proceso.

import Modelos
import qualified Reservaciones as R
import System.Directory (doesFileExist)
import System.IO (hFlush, stdout)
import Text.Read (readMaybe)


-- ============================================
-- FUNCIONES DE CALCULO
-- ============================================

-- F: calcularImpuesto
-- E: Subtotal
-- S: Impuesto del 13%
-- O: Calcular el impuesto de una reservacion

calcularImpuesto :: Double -> Double
calcularImpuesto subtotal =
    subtotal * 0.13


-- F: calcularTotal
-- E: Subtotal
-- S: Total con impuesto
-- O: Calcular el monto final de la factura

calcularTotal :: Double -> Double
calcularTotal subtotal =
    subtotal + calcularImpuesto subtotal


-- ============================================
-- VALIDACIONES
-- ============================================

-- F: validarCodigo
-- E: Texto ingresado
-- S: Numero valido o Nothing
-- O: Evitar letras, negativos y cero

validarCodigo :: String -> Maybe Int
validarCodigo texto =
    case reads texto :: [(Int, String)] of

        [(numero, "")] ->
            if numero > 0
                then Just numero
                else Nothing

        _ -> Nothing


-- F: validarSubtotal
-- E: Monto
-- S: True o False
-- O: Evitar montos negativos o invalidos

validarSubtotal :: Double -> Bool
validarSubtotal subtotal =
    subtotal >= 0
    && not (isNaN subtotal)
    && not (isInfinite subtotal)


-- ============================================
-- BUSCAR RESERVACIONES
-- ============================================

-- F: buscarReserva
-- E: Codigo y lista de reservaciones
-- S: Reservacion encontrada o Nothing
-- O: Buscar una reservacion por su codigo

buscarReserva :: Int -> [Reserva] -> Maybe Reserva
buscarReserva _ [] =
    Nothing

buscarReserva codigo (reserva:resto)
    | idReserva reserva == codigo =
        Just reserva

    | otherwise =
        buscarReserva codigo resto


-- ============================================
-- GENERAR FACTURA SIMPLE
-- ============================================

-- F: generarFactura
-- E: Codigo y lista de reservaciones
-- S: Factura o Nothing
-- O: Generar una factura de prueba

generarFactura :: Int -> [Reserva] -> Maybe Factura
generarFactura codigo reservas =

    case buscarReserva codigo reservas of

        Nothing ->
            Nothing

        Just reserva ->

            if codigo > 0
               && estadoReserva reserva == Activa
               && validarSubtotal (subtotalReserva reserva)

                then
                    let subtotal = subtotalReserva reserva
                        impuesto = calcularImpuesto subtotal
                        total = calcularTotal subtotal

                    in Just
                        (Factura codigo codigo subtotal impuesto total)

                else Nothing


-- ============================================
-- CAMBIAR ESTADO DE RESERVACION
-- ============================================

-- F: cambiarEstado
-- E: Codigo y lista de reservaciones
-- S: Lista actualizada
-- O: Cambiar una reservacion a Facturada

cambiarEstado :: Int -> [Reserva] -> [Reserva]
cambiarEstado _ [] =
    []

cambiarEstado codigo (reserva:resto)

    | idReserva reserva == codigo =
        reserva { estadoReserva = Facturada }
        : cambiarEstado codigo resto

    | otherwise =
        reserva : cambiarEstado codigo resto


-- ============================================
-- FACTURAR RESERVACION
-- ============================================

-- F: facturarReserva
-- E: Codigo y estado del sistema
-- S: Factura y sistema actualizado
-- O: Facturar una reservacion activa

facturarReserva :: Int -> EstadoSistema -> Maybe (Factura, EstadoSistema)
facturarReserva codigo sistema =

    case buscarReserva codigo (estadoReservas sistema) of

        Nothing ->
            Nothing

        Just reserva ->

            if codigo > 0
               && estadoReserva reserva == Activa
               && validarSubtotal (subtotalReserva reserva)
               && not (any
                    (\f -> reservaFactura f == codigo)
                    (estadoFacturas sistema))

                then
                    let subtotal = subtotalReserva reserva

                        impuesto = calcularImpuesto subtotal

                        total = calcularTotal subtotal

                        factura = Factura
                            (nextIdFactura sistema)
                            codigo
                            subtotal
                            impuesto
                            total

                        nuevasReservas =
                            cambiarEstado codigo (estadoReservas sistema)

                        nuevasFacturas =
                            estadoFacturas sistema ++ [factura]

                        nuevoSistema = sistema
                            { estadoReservas = nuevasReservas
                            , estadoFacturas = nuevasFacturas
                            , nextIdFactura = nextIdFactura sistema + 1
                            }

                    in Just (factura, nuevoSistema)

                else Nothing


-- ============================================
-- GUARDAR FACTURAS EN TXT
-- ============================================

-- F: convertirFactura
-- E: Factura
-- S: Texto separado por comas
-- O: Preparar una factura para guardarla

convertirFactura :: Factura -> String
convertirFactura factura =

    show (idFactura factura) ++ "," ++
    show (reservaFactura factura) ++ "," ++
    show (subtotalFactura factura) ++ "," ++
    show (impuestoFactura factura) ++ "," ++
    show (totalFactura factura)


-- F: guardarFacturas
-- E: Lista de facturas
-- S: Archivo TXT
-- O: Guardar las facturas del sistema

guardarFacturas :: [Factura] -> IO ()
guardarFacturas facturas = do

    writeFile
        "facturas_persistencia.txt"
        (unlines (map convertirFactura facturas))


-- ============================================
-- LEER FACTURAS DESDE TXT
-- ============================================

-- F: separarComas
-- E: Texto
-- S: Lista de textos
-- O: Separar los datos guardados

separarComas :: String -> [String]
separarComas texto =

    case break (== ',') texto of

        (parte, []) ->
            [parte]

        (parte, _:resto) ->
            parte : separarComas resto


-- F: leerFactura
-- E: Linea del TXT
-- S: Factura o Nothing
-- O: Convertir los datos guardados en una factura

leerFactura :: String -> Maybe Factura
leerFactura linea =

    case separarComas linea of

        [a,b,c,d,e] -> do

            numero <- readMaybe a
            reserva <- readMaybe b
            subtotal <- readMaybe c
            impuesto <- readMaybe d
            total <- readMaybe e

            if numero > 0
               && reserva > 0
               && validarSubtotal subtotal
               && validarSubtotal impuesto
               && validarSubtotal total

                then Just
                    (Factura numero reserva subtotal impuesto total)

                else Nothing

        _ ->
            Nothing


-- F: cargarFacturas
-- E: Archivo TXT
-- S: Lista de facturas
-- O: Recuperar las facturas al iniciar el programa

cargarFacturas :: IO [Factura]
cargarFacturas = do

    existe <- doesFileExist "facturas_persistencia.txt"

    if not existe

        then return []

        else do

            contenido <- readFile "facturas_persistencia.txt"

            let facturas =
                    [ factura
                    | linea <- lines contenido
                    , Just factura <- [leerFactura linea]
                    ]

            return facturas


-- ============================================
-- MOSTRAR FACTURA
-- ============================================

-- F: mostrarFactura
-- E: Factura, reservacion e informacion del hotel
-- S: Factura mostrada en pantalla
-- O: Mostrar los datos completos de una factura

mostrarFactura :: Factura -> Reserva -> HotelInfo -> IO ()
mostrarFactura factura reserva hotel = do

    putStrLn ""
    putStrLn "========================================"
    putStrLn "                FACTURA"
    putStrLn "========================================"

    putStrLn ("Empresa: " ++ nombreEmpresa hotel)
    putStrLn ("Cedula juridica: " ++ cedulaJuridica hotel)
    putStrLn ("Telefono: " ++ telefono hotel)
    putStrLn ("Sitio web: " ++ sitioWeb hotel)
    putStrLn ("Pais: " ++ pais hotel)
    putStrLn ("Provincia: " ++ provincia hotel)

    putStrLn "----------------------------------------"

    putStrLn ("Numero de factura: " ++ show (idFactura factura))
    putStrLn ("Numero de reservacion: " ++ show (reservaFactura factura))
    putStrLn ("Cliente: " ++ nombreCliente reserva)

    putStrLn ("Entrada: " ++ fechaEntrada reserva)
    putStrLn ("Salida: " ++ fechaSalida reserva)

    putStrLn ("Adultos: " ++ show (totalAdultos reserva))
    putStrLn ("Ninos: " ++ show (totalNinos reserva))

    putStrLn "----------------------------------------"

    putStrLn "Habitaciones:"

    mapM_ mostrarHabitacionFactura (ocupaciones reserva)

    putStrLn "----------------------------------------"

    putStrLn ("Subtotal: " ++ show (subtotalFactura factura))
    putStrLn ("Impuesto 13%: " ++ show (impuestoFactura factura))
    putStrLn ("Total: " ++ show (totalFactura factura))

    putStrLn "========================================"


-- F: mostrarHabitacionFactura
-- E: Ocupacion de una habitacion
-- S: Informacion mostrada
-- O: Mostrar las habitaciones de la reservacion

mostrarHabitacionFactura :: OcupacionHab -> IO ()
mostrarHabitacionFactura ocupacion = do

    let habitacion = habitacionOcupada ocupacion

    putStrLn
        ("Habitacion: " ++ idHabitacion habitacion
        ++ " - " ++ nombreTH (tipoHabitacion habitacion))

    putStrLn
        ("Adultos: " ++ show (adultosHab ocupacion)
        ++ " | Ninos: " ++ show (ninosHab ocupacion))


-- ============================================
-- MOSTRAR FACTURA DESDE EL SISTEMA
-- ============================================

-- F: mostrarFacturaSistema
-- E: Factura y estado del sistema
-- S: Informacion de factura en pantalla
-- O: Mostrar factura con los datos disponibles

mostrarFacturaSistema :: Factura -> EstadoSistema -> IO ()
mostrarFacturaSistema factura sistema = do

    case buscarReserva
        (reservaFactura factura)
        (estadoReservas sistema) of

        Nothing ->
            mostrarFacturaBasica factura

        Just reserva ->

            case estadoInfo sistema of

                Nothing -> do

                    putStrLn ""
                    putStrLn "Informacion del hotel no cargada."

                    mostrarFacturaBasica factura

                    putStrLn ("Cliente: " ++ nombreCliente reserva)
                    putStrLn ("Entrada: " ++ fechaEntrada reserva)
                    putStrLn ("Salida: " ++ fechaSalida reserva)

                    putStrLn "Habitaciones:"
                    mapM_ mostrarHabitacionFactura (ocupaciones reserva)

                Just hotel ->
                    mostrarFactura factura reserva hotel


-- F: mostrarFacturaBasica
-- E: Factura
-- S: Datos mostrados
-- O: Mostrar los montos de la factura

mostrarFacturaBasica :: Factura -> IO ()
mostrarFacturaBasica factura = do

    putStrLn ""
    putStrLn "========================================"
    putStrLn "                FACTURA"
    putStrLn "========================================"

    putStrLn ("Numero de factura: " ++ show (idFactura factura))

    putStrLn
        ("Numero de reservacion: " ++ show (reservaFactura factura))

    putStrLn ("Subtotal: " ++ show (subtotalFactura factura))
    putStrLn ("Impuesto 13%: " ++ show (impuestoFactura factura))
    putStrLn ("Total: " ++ show (totalFactura factura))

    putStrLn "========================================"


-- ============================================
-- PEDIR CODIGO
-- ============================================

-- F: pedirCodigo
-- E: Codigo ingresado por el usuario
-- S: Codigo valido
-- O: Solicitar el codigo de una reservacion

pedirCodigo :: IO Int
pedirCodigo = do

    putStr "Ingrese el codigo de la reservacion: "
    hFlush stdout

    texto <- getLine

    case validarCodigo texto of

        Just codigo ->
            return codigo

        Nothing -> do

            putStrLn "[ERROR] Ingrese un numero entero mayor que cero."

            pedirCodigo


-- ============================================
-- GENERAR Y GUARDAR FACTURA
-- ============================================

-- F: mensajeFacturacion
-- E: Codigo y estado del sistema
-- S: Estado actualizado
-- O: Facturar, guardar y mostrar el resultado

mensajeFacturacion :: Int -> EstadoSistema -> IO EstadoSistema
mensajeFacturacion codigo sistema = do

    case buscarReserva codigo (estadoReservas sistema) of

        Nothing -> do

            putStrLn "[ERROR] No existe una reservacion con ese codigo."

            return sistema

        Just reserva ->

            case estadoReserva reserva of

                Cancelada -> do

                    putStrLn "[ERROR] La reservacion esta cancelada."

                    return sistema

                Facturada -> do

                    putStrLn "[ERROR] Esta reservacion ya fue facturada."

                    return sistema

                Activa -> do

                    case facturarReserva codigo sistema of

                        Nothing -> do

                            putStrLn "[ERROR] No se pudo generar la factura."

                            return sistema

                        Just (factura, nuevoSistema) -> do

                            -- Guardar las facturas
                            guardarFacturas (estadoFacturas nuevoSistema)

                            -- Guardar las reservaciones actualizadas
                            R.guardarReservaciones
                                "reservaciones.txt"
                                (estadoReservas nuevoSistema)

                            putStrLn ""
                            putStrLn "[OK] Factura generada correctamente."
                            putStrLn "[OK] Factura guardada en TXT."

                            mostrarFacturaSistema factura nuevoSistema

                            return nuevoSistema


-- ============================================
-- CONSULTAR FACTURA
-- ============================================

-- F: consultarFactura
-- E: Estado del sistema
-- S: Factura mostrada
-- O: Buscar una factura por su numero

consultarFactura :: EstadoSistema -> IO ()
consultarFactura sistema = do

    putStr "Ingrese el numero de factura: "
    hFlush stdout

    texto <- getLine

    case validarCodigo texto of

        Nothing -> do

            putStrLn "[ERROR] Numero de factura invalido."

        Just numero -> do

            let encontradas =
                    filter
                        (\f -> idFactura f == numero)
                        (estadoFacturas sistema)

            case encontradas of

                [] -> do

                    putStrLn "[ERROR] No existe una factura con ese numero."

                (factura:_) -> do

                    mostrarFacturaSistema factura sistema


-- ============================================
-- MENU DE FACTURACION
-- ============================================

-- F: menuFacturacion
-- E: Estado del sistema
-- S: Estado actualizado
-- O: Generar y consultar facturas

menuFacturacion :: EstadoSistema -> IO EstadoSistema
menuFacturacion sistema = do

    putStrLn ""
    putStrLn "========================================"
    putStrLn "          MENU DE FACTURACION"
    putStrLn "========================================"
    putStrLn "1. Generar factura"
    putStrLn "2. Consultar factura"
    putStrLn "3. Volver al menu general"
    putStrLn "========================================"

    putStr "Seleccione una opcion: "
    hFlush stdout

    opcion <- getLine

    case opcion of

        "1" -> do

            codigo <- pedirCodigo

            nuevoSistema <- mensajeFacturacion codigo sistema

            menuFacturacion nuevoSistema

        "2" -> do

            consultarFactura sistema

            menuFacturacion sistema

        "3" -> do

            putStrLn "Regresando al menu general..."

            return sistema

        _ -> do

            putStrLn "[ERROR] Opcion no valida."

            menuFacturacion sistema
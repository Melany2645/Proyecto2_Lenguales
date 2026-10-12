module Facturacion where

-- Importamos los datos de las reservaciones y facturas
import Modelos


-- F: calcularImpuesto
-- E: subtotal de una reservacion
-- S: monto del impuesto
-- O: calcular el 13% de impuesto
calcularImpuesto :: Double -> Double
calcularImpuesto subtotal =
    subtotal * 0.13


-- F: calcularTotal
-- E: subtotal de una reservacion
-- S: monto total con impuesto
-- O: sumar el subtotal y el impuesto
calcularTotal :: Double -> Double
calcularTotal subtotal =
    subtotal + calcularImpuesto subtotal


-- F: buscarReserva
-- E: codigo de reserva y lista de reservas
-- S: reserva encontrada o Nothing
-- O: buscar una reserva utilizando su codigo
buscarReserva :: Int -> [Reserva] -> Maybe Reserva

-- Si la lista esta vacia, no existe la reserva
buscarReserva codigo [] =
    Nothing

-- Revisamos cada reserva de la lista
buscarReserva codigo (reserva:resto)

    -- Si encontramos el codigo, devolvemos la reserva
    | idReserva reserva == codigo =
        Just reserva

    -- Si no coincide, seguimos buscando
    | otherwise =
        buscarReserva codigo resto


-- F: generarFactura
-- E: codigo de reserva y lista de reservas
-- S: factura generada o Nothing
-- O: generar una factura si la reserva esta activa
generarFactura :: Int -> [Reserva] -> Maybe Factura
generarFactura codigo reservas =

    -- Buscamos la reserva por su codigo
    case buscarReserva codigo reservas of

        -- Si no existe, no se genera factura
        Nothing ->
            Nothing

        -- Si encontramos la reserva, revisamos su estado
        Just reserva ->

            -- Solo podemos facturar reservas activas
            if estadoReserva reserva == Activa
                then
                    let
                        -- Obtenemos el subtotal
                        subtotal = subtotalReserva reserva

                        -- Calculamos el impuesto
                        impuesto = calcularImpuesto subtotal

                        -- Calculamos el total
                        total = calcularTotal subtotal

                    -- Creamos la factura
                    in Just (Factura codigo codigo subtotal impuesto total)

                -- Si esta cancelada o facturada, no se factura
                else
                    Nothing

-- F: cambiarEstado
-- E: codigo y lista de reservas
-- S: lista de reservas actualizada
-- O: cambiar una reserva activa a facturada

cambiarEstado :: Int -> [Reserva] -> [Reserva]
cambiarEstado codigo [] = []

cambiarEstado codigo (reserva:resto)
    | idReserva reserva == codigo =
        reserva { estadoReserva = Facturada } : resto

    | otherwise =
        reserva : cambiarEstado codigo resto


-- F: facturarReserva
-- E: codigo y estado del sistema
-- S: factura y sistema actualizado, o Nothing
-- O: guardar la factura y actualizar la reserva

facturarReserva :: Int -> EstadoSistema -> Maybe (Factura, EstadoSistema)
facturarReserva codigo sistema =

    case buscarReserva codigo (estadoReservas sistema) of

        Nothing ->
            Nothing

        Just reserva ->
            if estadoReserva reserva == Activa
                then
                    let
                        -- Calculamos los montos
                        subtotal = subtotalReserva reserva
                        impuesto = calcularImpuesto subtotal
                        total = calcularTotal subtotal

                        -- Creamos la factura
                        factura = Factura
                            (nextIdFactura sistema)
                            codigo
                            subtotal
                            impuesto
                            total

                        -- Actualizamos las reservas
                        nuevasReservas =
                            cambiarEstado codigo (estadoReservas sistema)

                        -- Guardamos los cambios en el sistema
                        nuevoSistema = sistema
                            { estadoReservas = nuevasReservas
                            , estadoFacturas =
                                estadoFacturas sistema ++ [factura]
                            , nextIdFactura = nextIdFactura sistema + 1
                            }

                    in Just (factura, nuevoSistema)

                else
                    Nothing


-- F: mostrarFactura
-- E: factura, reserva e informacion del hotel
-- S: muestra la factura en pantalla
-- O: imprimir los datos de la factura

mostrarFactura :: Factura -> Reserva -> HotelInfo -> IO ()
mostrarFactura factura reserva hotel = do

    putStrLn ""
    putStrLn "================================"
    putStrLn "          FACTURA"
    putStrLn "================================"

    putStrLn ("Empresa: " ++ nombreEmpresa hotel)
    putStrLn ("Cedula juridica: " ++ cedulaJuridica hotel)
    putStrLn ("Telefono: " ++ telefono hotel)
    putStrLn ("Sitio web: " ++ sitioWeb hotel)
    putStrLn ("Pais: " ++ pais hotel)
    putStrLn ("Provincia: " ++ provincia hotel)

    putStrLn "--------------------------------"

    putStrLn ("Numero de factura: " ++ show (idFactura factura))
    putStrLn ("Numero de reserva: " ++ show (reservaFactura factura))
    putStrLn ("Cliente: " ++ nombreCliente reserva)

    putStrLn ("Fecha de entrada: " ++ fechaEntrada reserva)
    putStrLn ("Fecha de salida: " ++ fechaSalida reserva)

    putStrLn ("Adultos: " ++ show (totalAdultos reserva))
    putStrLn ("Ninos: " ++ show (totalNinos reserva))

    putStrLn "--------------------------------"

    putStrLn ("Subtotal: " ++ show (subtotalFactura factura))
    putStrLn ("Impuesto 13%: " ++ show (impuestoFactura factura))
    putStrLn ("Total: " ++ show (totalFactura factura))

    putStrLn "================================"
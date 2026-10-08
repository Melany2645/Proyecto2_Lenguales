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
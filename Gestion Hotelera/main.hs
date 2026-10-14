module Main where

-- Importamos nuestros archivos
import Modelos
import Facturacion
import Estadisticas


-- Creamos una habitacion de prueba
habitacion1 :: Habitacion
habitacion1 =
    Habitacion
        "101"
        (TipoHabitacion
            "Estandar"
            "Habitacion estandar con cama doble"
            2)


-- Creamos una reservacion de prueba
reserva1 :: Reserva
reserva1 =
    Reserva
        { idReserva = 1
        , nombreCliente = "Diego"
        , fechaReserva = "2026-10-09 12:00:00"
        , fechaEntrada = "2026-10-10"
        , fechaSalida = "2026-10-12"
        , totalAdultos = 2
        , totalNinos = 1
        , ocupaciones = [OcupacionHab habitacion1 2 1]
        , estadoReserva = Activa
        , subtotalReserva = 100000
        }


-- Lista que simula las reservaciones existentes
reservasPrueba :: [Reserva]
reservasPrueba =
    [reserva1]


-- Funcion principal para probar el programa
main :: IO ()
main = do

    putStrLn "============================="
    putStrLn "   PRUEBA DE FACTURACION"
    putStrLn "============================="

    -- Intentamos generar la factura de la reserva numero 1
    print (generarFactura 1 reservasPrueba)

    putStrLn ""
    putStrLn "Monto recaudado:"

    -- Mostramos el monto recaudado
    print (montoRecaudado reservasPrueba)
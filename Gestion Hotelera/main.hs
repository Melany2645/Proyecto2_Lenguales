module Main where

-- Importamos nuestros archivos
import Modelos
import Facturacion
import Estadisticas


-- Creamos una habitacion de prueba
habitacion1 :: Habitacion
habitacion1 =
    Habitacion
        101
        "Estandar"


-- Creamos una reservacion de prueba
reserva1 :: Reserva
reserva1 =
    Reserva
        1                   -- Codigo de reserva
        "Diego"             -- Nombre del cliente
        "10/10/2026"        -- Fecha de entrada
        "12/10/2026"        -- Fecha de salida
        2                   -- Adultos
        1                   -- Niños
        [habitacion1]       -- Habitaciones
        Activa              -- Estado
        100000              -- Subtotal


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
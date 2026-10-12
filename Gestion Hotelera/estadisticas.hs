module Estadisticas where

import Modelos
import Data.Time
import Data.Time.Calendar.WeekDate (toWeekDate)
import Data.List (sortBy)
import Data.Ord (comparing)


-- ============================================
-- 1. CALCULAR TOTAL CON IMPUESTO
-- ============================================

-- F: totalConImpuesto
-- E: una reservacion
-- S: monto de la reserva con impuesto
-- O: calcular el total incluyendo el 13%

totalConImpuesto :: Reserva -> Double
totalConImpuesto reserva =
    subtotalReserva reserva * 1.13


-- ============================================
-- 2. CALCULAR MONTO RECAUDADO
-- ============================================

-- F: montoRecaudado
-- E: lista de reservaciones
-- S: dinero total recaudado
-- O: sumar las reservas que no estan canceladas

montoRecaudado :: [Reserva] -> Double
montoRecaudado reservas =
    sum
        [ totalConImpuesto reserva
        | reserva <- reservas
        , estadoReserva reserva /= Cancelada
        ]


-- ============================================
-- 3. MENU DE ESTADISTICAS
-- ============================================

-- F: menuEstadisticas
-- E: ninguna
-- S: muestra las opciones
-- O: permitir seleccionar una estadistica

menuEstadisticas :: IO ()
menuEstadisticas = do

    putStrLn ""
    putStrLn "=============================="
    putStrLn "     MENU DE ESTADISTICAS"
    putStrLn "=============================="

    putStrLn "1. Total de huespedes por mes y año"
    putStrLn "2. Habitaciones ocupadas por dia"
    putStrLn "3. Habitaciones libres por dia"
    putStrLn "4. Monto recaudado"
    putStrLn "5. Top 3 de dias con mas ocupacion"
    putStrLn "6. Volver"

    putStrLn "Seleccione una opcion:"
    opcion <- getLine

    case opcion of

        "1" -> do
            putStrLn "Total de huespedes"
            menuEstadisticas

        "2" -> do
            putStrLn "Habitaciones ocupadas"
            menuEstadisticas

        "3" -> do
            putStrLn "Habitaciones libres"
            menuEstadisticas

        "4" -> do
            putStrLn "Monto recaudado"
            menuEstadisticas

        "5" -> do
            putStrLn "Top 3 de dias"
            menuEstadisticas

        "6" ->
            return ()

        _ -> do
            putStrLn "Opcion no valida"
            menuEstadisticas


-- ============================================
-- 4. TOTAL DE HUESPEDES POR MES Y AÑO
-- ============================================

-- F: totalHuespedes
-- E: mes, año y lista de reservas
-- S: cantidad total de huespedes
-- O: contar adultos y niños por mes y año

totalHuespedes :: String -> String -> [Reserva] -> Int
totalHuespedes mes anio reservas =

    sum
        [ totalAdultos reserva + totalNinos reserva
        | reserva <- reservas

        -- No contamos reservas canceladas
        , estadoReserva reserva /= Cancelada

        -- Revisamos el mes de entrada
        , take 2 (drop 5 (fechaEntrada reserva)) == mes

        -- Revisamos el año de entrada
        , take 4 (fechaEntrada reserva) == anio
        ]


-- ============================================
-- 5. CONVERTIR FECHAS
-- ============================================

-- F: convertirFecha
-- E: fecha como texto
-- S: fecha convertida o Nothing
-- O: convertir texto a fecha

convertirFecha :: String -> Maybe Day
convertirFecha fecha =
    parseTimeM True defaultTimeLocale "%Y-%m-%d" fecha


-- ============================================
-- 6. OBTENER DIAS DE UNA RESERVA
-- ============================================

-- F: diasReserva
-- E: una reservacion
-- S: lista de dias ocupados
-- O: obtener los dias entre entrada y salida

diasReserva :: Reserva -> [Day]
diasReserva reserva =

    case (convertirFecha (fechaEntrada reserva),
          convertirFecha (fechaSalida reserva)) of

        (Just entrada, Just salida) ->
            [entrada .. addDays (-1) salida]

        _ ->
            []


-- ============================================
-- 7. CONTAR OCUPACION POR DIA DE LA SEMANA
-- ============================================

-- F: contarOcupacion
-- E: numero del dia y lista de reservas
-- S: cantidad de habitaciones ocupadas
-- O: contar la ocupacion de un dia de la semana

contarOcupacion :: Int -> [Reserva] -> Int
contarOcupacion numero reservas =

    sum
        [ length (ocupaciones reserva)
        | reserva <- reservas

        -- No contamos reservas canceladas
        , estadoReserva reserva /= Cancelada

        -- Obtenemos cada dia de la reserva
        , dia <- diasReserva reserva

        -- Identificamos el dia de la semana
        , let (_, _, diaSemana) = toWeekDate dia

        -- Comparamos con el dia solicitado
        , diaSemana == numero
        ]


-- ============================================
-- 8. TOP 3 DE DIAS CON MAYOR OCUPACION
-- ============================================

-- F: topTresDias
-- E: lista de reservaciones
-- S: tres dias con mayor ocupacion
-- O: ordenar los dias de mayor a menor

topTresDias :: [Reserva] -> [(String, Int)]
topTresDias reservas =

    take 3 (sortBy (flip (comparing snd)) datos)

    where

        -- Nombres de los dias
        nombres =
            [ "Lunes"
            , "Martes"
            , "Miercoles"
            , "Jueves"
            , "Viernes"
            , "Sabado"
            , "Domingo"
            ]

        -- Calculamos la ocupacion de cada dia
        datos =
            [ (nombre, contarOcupacion numero reservas)
            | (numero, nombre) <- zip [1..7] nombres
            ]
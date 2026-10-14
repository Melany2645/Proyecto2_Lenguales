module HotelInformacion where

import Modelos
import System.IO (hFlush, stdout)
-- ============================================
-- FUNCIONES DEL MÓDULO
-- ============================================

-- | Carga la información del hotel desde la entrada del usuario
cargarInfoHotel :: Maybe HotelInfo -> IO (Maybe HotelInfo)
cargarInfoHotel _ = do
    putStrLn "--- Ingrese los datos del Hotel ---"
    putStr "Nombre de la empresa: "
    hFlush stdout
    nombre <- getLine

    putStr "Cedula juridica: "
    hFlush stdout
    cedula <- getLine

    putStr "Sitio web: "
    hFlush stdout
    web <- getLine

    putStr "Telefono: "
    hFlush stdout
    telefono <- getLine

    putStr "Pais: "
    hFlush stdout
    pais <- getLine

    putStr "Provincia: "
    hFlush stdout
    provincia <- getLine
    let info = HotelInfo nombre cedula web telefono pais provincia
    putStrLn "\n[OK] Informacion del hotel cargada."
    return (Just info)

-- | Muestra la información actual del hotel
mostrarInfoHotel :: Maybe HotelInfo -> IO ()
mostrarInfoHotel Nothing = 
    putStrLn "[AVISO] No hay informacion del hotel."
mostrarInfoHotel (Just info) = do
    putStrLn "--- Datos del Hotel ---"
    putStrLn ("Empresa: " ++ nombreEmpresa info)
    putStrLn ("Cedula: " ++ cedulaJuridica info)
    putStrLn ("Web: " ++ sitioWeb info)
    putStrLn ("Telefono: " ++ telefono info)
    putStrLn ("Ubicacion: " ++ pais info ++ ", " ++ provincia info)

-- | Guarda la información en un archivo de texto
guardarInfoHotel :: Maybe HotelInfo -> FilePath -> IO ()
guardarInfoHotel Nothing _ = 
    putStrLn "[ERROR] No hay informacion para guardar."
guardarInfoHotel (Just info) ruta = do
    let linea1 = nombreEmpresa info
    let linea2 = cedulaJuridica info
    let linea3 = sitioWeb info
    let linea4 = telefono info
    let linea5 = pais info
    let linea6 = provincia info
    let contenido = unlines [linea1, linea2, linea3, linea4, linea5, linea6]
    writeFile ruta contenido
    putStrLn ("[OK] Guardado en: " ++ ruta)

-- | Carga la información desde un archivo de texto
leerInfoHotel :: Maybe HotelInfo -> FilePath -> IO (Maybe HotelInfo)
leerInfoHotel _ ruta = do
    contenido <- readFile ruta
    let lineas = lines contenido
    if length lineas >= 6
        then do
            let info = HotelInfo 
                    (lineas !! 0) 
                    (lineas !! 1) 
                    (lineas !! 2)
                    (lineas !! 3) 
                    (lineas !! 4) 
                    (lineas !! 5)
            putStrLn "[OK] Leido desde archivo."
            return (Just info)
        else do
            putStrLn "[ERROR] Archivo incompleto."
            return Nothing

-- | Pausa el programa esperando un Enter
pausar :: IO ()
pausar = do
    putStrLn "\nPresione Enter para continuar..."
    getLine
    putStrLn ""
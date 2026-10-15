
module HotelInformacion where

import Modelos
import System.Directory (doesFileExist)
import System.IO (hFlush, stdout)

-- ============================================
-- ARCHIVO DEL HOTEL
-- ============================================

archivoHotel :: FilePath
archivoHotel = "hotel_info.txt"


-- ============================================
-- GUARDAR INFORMACION
-- ============================================

guardarHotel :: HotelInfo -> IO ()
guardarHotel info = do
    writeFile archivoHotel (unlines
        [ nombreEmpresa info
        , cedulaJuridica info
        , sitioWeb info
        , telefono info
        , pais info
        , provincia info
        ])
    putStrLn "[OK] Informacion del hotel guardada."


-- ============================================
-- CARGAR INFORMACION GUARDADA
-- ============================================

cargarHotel :: IO (Maybe HotelInfo)
cargarHotel = do
    existe <- doesFileExist archivoHotel

    if not existe
        then return Nothing
        else do
            contenido <- readFile archivoHotel

            case lines contenido of
                [nombre, cedula, web, tel, paisHotel, prov] ->
                    return (Just (HotelInfo
                        nombre cedula web tel paisHotel prov))

                _ -> do
                    putStrLn "[ERROR] Archivo del hotel incorrecto."
                    return Nothing


-- ============================================
-- PEDIR DATOS
-- ============================================

pedirDatoHotel :: String -> IO String
pedirDatoHotel mensaje = do
    putStr mensaje
    hFlush stdout
    getLine


-- ============================================
-- REGISTRAR HOTEL
-- ============================================

registrarHotel :: IO HotelInfo
registrarHotel = do
    putStrLn "\n=== REGISTRAR INFORMACION DEL HOTEL ==="

    nombre <- pedirDatoHotel "Nombre de la empresa: "
    cedula <- pedirDatoHotel "Cedula juridica: "
    web <- pedirDatoHotel "Sitio web: "
    tel <- pedirDatoHotel "Telefono: "
    paisHotel <- pedirDatoHotel "Pais: "
    prov <- pedirDatoHotel "Provincia: "

    let info = HotelInfo nombre cedula web tel paisHotel prov

    guardarHotel info
    return info


-- ============================================
-- MOSTRAR HOTEL
-- ============================================

mostrarHotel :: HotelInfo -> IO ()
mostrarHotel info = do
    putStrLn "\n=== INFORMACION DEL HOTEL ==="
    putStrLn ("Empresa: " ++ nombreEmpresa info)
    putStrLn ("Cedula juridica: " ++ cedulaJuridica info)
    putStrLn ("Sitio web: " ++ sitioWeb info)
    putStrLn ("Telefono: " ++ telefono info)
    putStrLn ("Pais: " ++ pais info)
    putStrLn ("Provincia: " ++ provincia info)


-- ============================================
-- FUNCIONES DEL MENU ADMINISTRATIVO
-- ============================================

-- OPCION 1: REGISTRAR O MODIFICAR

cargarInfoHotel :: Maybe HotelInfo -> IO (Maybe HotelInfo)
cargarInfoHotel _ = do
    info <- registrarHotel
    return (Just info)


-- OPCION 2: MOSTRAR INFORMACION

mostrarInfoHotel :: Maybe HotelInfo -> IO ()
mostrarInfoHotel Nothing =
    putStrLn "[ERROR] No hay informacion del hotel registrada."

mostrarInfoHotel (Just info) =
    mostrarHotel info


-- OPCION 3: GUARDAR INFORMACION

guardarInfoHotel :: Maybe HotelInfo -> FilePath -> IO ()
guardarInfoHotel Nothing _ =
    putStrLn "[ERROR] No hay informacion del hotel para guardar."

guardarInfoHotel (Just info) ruta = do
    writeFile ruta (unlines
        [ nombreEmpresa info
        , cedulaJuridica info
        , sitioWeb info
        , telefono info
        , pais info
        , provincia info
        ])
    putStrLn "[OK] Informacion del hotel guardada."


-- OPCION 4: LEER INFORMACION DEL ARCHIVO

leerInfoHotel :: Maybe HotelInfo -> FilePath -> IO (Maybe HotelInfo)
leerInfoHotel infoActual ruta = do
    existe <- doesFileExist ruta

    if not existe
        then do
            putStrLn "[ERROR] No existe el archivo del hotel."
            return infoActual
        else do
            contenido <- readFile ruta

            case lines contenido of
                [nombre, cedula, web, tel, paisHotel, prov] -> do
                    let info = HotelInfo
                            nombre cedula web tel paisHotel prov

                    putStrLn "[OK] Informacion del hotel cargada."
                    return (Just info)

                _ -> do
                    putStrLn "[ERROR] Formato del archivo incorrecto."
                    return infoActual

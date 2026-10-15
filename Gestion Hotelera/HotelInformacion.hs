module HotelInformacion where

import Modelos
import System.Directory (doesFileExist)
import System.IO (hFlush, stdout)
-- ============================================
-- FUNCIONES DEL MÓDULO
-- ============================================

-- | Carga la información del hotel desde la entrada del usuario
cargarInfoHotel :: Maybe HotelInfo -> IO (Maybe HotelInfo)
cargarInfoHotel _ = do
    info <- registrarHotel
    return (Just info)

-- | Muestra la información actual del hotel
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
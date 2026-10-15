# Proyecto2_Lenguales

## Documentación general del sistema hotelero

Este proyecto implementa una solución en Haskell para gestionar reservas, disponibilidad, facturación y estadísticas de un hotel. La lógica está organizada en cuatro componentes principales que permiten separar la responsabilidad del negocio:

1. Modelado
2. Gestión de reservas y disponibilidad
3. Facturación
4. Estadísticas y reportes

---

## 1. Componente de modelado

El módulo `Modelos` es la base del sistema. Define todos los tipos de datos necesarios para representar el hotel y su operación diaria:

- `Estado`: estado de una reserva (`Activa`, `Facturada`, `Cancelada`)
- `HotelInfo`: información institucional del hotel
- `TipoHabitacion`: nombre, descripción y capacidad máxima
- `Habitacion`: identificación y tipo asociado
- `Tarifa`: precio por persona según la tarifa aplicada
- `OcupacionHab`: detalle de cuántos adultos y niños ocupan una habitación
- `Reserva`: información completa de la reservación
- `Factura`: comprobante económico del alquiler
- `EstadoSistema`: estructura global del sistema con habitaciones, tarifas, reservas y facturas

---

## 2. Componente de gestión de reservas y disponibilidad

El componente de reservas incluye la lógica de validación, control de ocupación y cálculo de costos.

### Funciones principales

- `leerFecha`: valida una fecha en formato `AAAA-MM-DD`
- `textoValido`: verifica que un nombre o texto de entrada no sea vacío ni inválido
- `leerEntero`: convierte una cadena a entero cuando aplica
- `validarFechas`: asegura que la entrada sea válida y que la salida sea posterior
- `seCruzan`: determina si dos rangos de fechas se cruzan
- `validarHuespedes`: valida la capacidad de huéspedes por habitación
- `habitacionOcupadaEnFechas`: comprueba si una habitación ya está reservada en un rango
- `obtenerHabitacionesDisponibles`: filtra las habitaciones que sí pueden reservarse
- `seleccionarHabitaciones`: solicita al usuario la elección de habitaciones y su ocupación
- `buscarTarifa`: busca la tarifa según el código de temporada
- `codigoTarifaPara`: determina la tarifa según el día y el tipo de huésped
- `calcularCostoDia`: calcula el costo de una habitación en un día determinado
- `calcularSubtotal`: suma el costo total por cada día de la estancia
- `registrarReservacion`: flujo principal para registrar una nueva reserva

Este bloque representa la parte operativa del sistema: valida fechas, compara disponibilidad, selecciona habitaciones y calcula el costo base de los precios.

---

## 3. Componente de facturación

### Responsabilidades

- buscar la reserva por su identificador
- validar el estado de la reserva
- calcular el impuesto del 13%
- calcular el total final
- actualizar el estado de la reserva a `Facturada`
- guardar la factura en el estado del sistema
- mostrar la información de la factura con el detalle del cliente y del hotel

Funciones importantes:

- `calcularImpuesto`
- `calcularTotal`
- `buscarReserva`
- `generarFactura`
- `cambiarEstado`
- `facturarReserva`
- `mostrarFactura`

---

## 4. Componente de estadísticas y reportes

### Son funciones importantes

- `totalConImpuesto`: calcula el total con IVA incluido
- `montoRecaudado`: suma los ingresos generados por reservas facturadas
- `totalHuespedes`: cuenta huéspedes por mes y año
- `convertirFecha`: transforma texto en tipo `Day`
- `diasReserva`: obtiene los días de una reserva
- `contarOcupacion`: mide cuántas habitaciones están ocupadas en un día de la semana
- `topTresDias`: identifica los tres días con mayor ocupación
- `validarMes`, `validarAnio`, `validarFechaEst`: validan entradas del usuario
- `menuEstadisticas`: presenta el menú de consultas del hotel

---

## Flujo general del sistema

1. El sistema inicia con un estado vacío o con datos cargados.
2. El usuario registra una reserva con cliente, fechas y ocupación.
3. El sistema valida que las fechas sean correctas.
4. Se revisa la disponibilidad de habitaciones según las reservas existentes.
5. Se calcula el subtotal de la estancia según la tarifa vigente.
6. Si la reserva corresponde, se genera la factura y cambia a estado facturada.
7. Se pueden consultar reportes y estadísticas del rendimiento del hotel.

---

## Estructura del proyecto

- `Gestion Hotelera/modelos.hs`: tipos del dominio
- `Gestion Hotelera/ReservasUsuario.hs`: reserva, fechas y disponibilidad
- `Gestion Hotelera/reservaciones.hs`: persistencia y consulta de registros
- `Gestion Hotelera/facturacion.hs`: cálculo y emisión de facturas
- `Gestion Hotelera/estadisticas.hs`: reportes y métricas
- `Gestion Hotelera/main.hs`: ejemplo de ejecución y pruebas iniciales

---

## Conclusión

El proyecto usa una arquitectura modular en Haskell con separación de responsabilidades. Cada componente tiene un propósito específico, y la división permite un desarrollo más fácil, por así decirlo, de las responsabilidades, además de saber con más seguridad que componentes dependen de otros.

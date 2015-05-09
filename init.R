# init.R
# Elementos auxiliares que deben cargarse al inicio de todos los _scripts_.


# Constantes.
#
# * `DEVTOOLS_PACKAGE`: Nombre del paquete `devtools`, necesario para cargar el paquete `googlesheets`.
# * `GSHEETS_REPO`: Identificador del repositorio GitHub con el paquete `googlesheets`.
# * `GSHEETS_PACKAGE`: Nombre del paquete `googlesheets`.
# * `DPLYR_PACKAGE`: Nombre del paquete `dplyr`.
# * `EXPENSES_SHEET_NAME`: Nombre de la hoja de gastos e ingresos.
DEVTOOLS_PACKAGE <- "devtools"
GSHEETS_REPO <- "jennybc/googlesheets"
GSHEETS_PACKAGE <- "googlesheets"
DPLYR_PACKAGE <- "dplyr"
EXPENSES_SHEET_NAME <- "GastosIngresos"


# Carga una biblioteca de funciones en memoria.
# Si el paquete no está instalado, intenta instalarlo primero.
#
# @param package.name Nombre de la biblioteca a cargar.
load_package <- function (package.name) {
	if (package.name != "") {
		if (!package.name %in% installed.packages ( )) {
			install.packages (package.name)
		}

		library (package = package.name, character.only = TRUE)
	}
}


# Carga una biblioteca de funciones en memoria.
# La biblioteca de funciones está en GitHub (probablemente en desarrollo).
#
# @param repo.name Nombre del repositorio GitHub donde se encuentra la biblioteca.
# @param package.name Nombre de la biblioteca a cargar.
load_package_github <- function (repo.name, package.name) {
	if ((repo.name != "") && (package.name != "")) {
		if (!package.name %in% installed.packages ( )) {
			devtools::install_github (repo.name)
		}

		library (package = package.name, character.only = TRUE)
	}
}


# Carga de la hoja de cálculo con gastos e ingresos.
#
# @returns Hoja de cálculo de Google Spreadsheets de gastos e ingresos.
load_expenses_sheet <- function ( ) {
	return (register_ss (EXPENSES_SHEET_NAME, verbose = FALSE))
}


# Crea un conversor de elementos de tipo `character` a elementos de tipo `money`.
# En esencia:
#
# * Elimina el símbolo de euro (€) de la cadena de entrada.
# * Elimina el separador de miles (.) de la cadena de entrada.
# * Sustituye el separador decimal (,) por otro separador decimal (.).
create_money_converter <- function ( ) {
	setClass ("money")
	setAs (
		"character",
		"money",
		function (from) {
			return (as.numeric (gsub ("€", "", gsub (",", ".", gsub ("\\.", "", from)))))
		}
	)
}


# Crea un conversor de elementos de tipo carácter, con valores «Sí»/«No»/«» a elementos lógicos.
create_yesno_converter <- function ( ) {
	setClass ("yesno")
	setAs (
		"character",
		"yesno",
		function (from) {
			return (unlist (lapply (from, function (x) {switch (x, "Sí" = TRUE, "No" = FALSE, FALSE)})))
		}
	)
}


# Crea un conversor de elementos de tipo `character` a un formato específico de fecha `POSIXct`.
create_date_converter <- function ( ) {
	setClass ("pdate")
	setAs (
		"character",
		"pdate",
		function (from) {
			return (as.Date (from, tz = "", format = "%d/%m/%Y"))
		}
	)
}


# Determina si una fecha determinada se encuentra en el pasado.
# La fecha se considera en el pasado a partir del mes anterior.
#
# @param date Fecha para la que se desea averiguar si está en el pasado.
#
# @returns `TRUE` si la fecha es del mes pasado o anterior, o `FALSE` en caso contrario.
belongs_to_the_past <- function (date) {
	return (format (date, "%Y%m") < format (Sys.Date ( ), "%Y%m"))
}


# Determina si un gasto estimado se encuentra en el pasdado.
# Los gastos estimados en el pasado se consideran cerrados de forma automática.
#
# @param estimated Valor lógico de la columna `Estimado` que indica si el gasto es una estimación.
# @param date Fecha en la que se produjo el gasto.
#
# @returns `TRUE` si el gasto es estimado y se encuentra en el pasado, o `FALSE` en caso contrario.
estimation_is_in_the_past <- function (estimated, date) {
	return (belongs_to_the_past (date) & estimated)
}


# Determina si un gasto estimado del presente mes ha sido consumido.
# Los gastos estimados presentes consumidos se consideran cerrados de forma automática.
#
# @param estimated Valor lógico de la columna `Estimado` que indica si el gasto es una estimación.
# @param closed Valor lógico de la columna `Cerrado` que indica si el gasto estimado se ha cerrado de forma manual.
# @param date Fecha en la que se produjo el gasto.
# @param amount Importe de la estimación realizada.
# @param consumed Importe consumido sobre la estimación realizada.
#
# @returns `TRUE` si el gasto es estimado, no cerrado, en el presente y el consumo supera a la estimación; `FALSE` en caso contrario.
estimation_is_current_and_consumed <- function (estimated, closed, date, amount, consumed) {
	return (!belongs_to_the_past (date) & estimated & !is.na (consumed) & abs (amount) <= abs (consumed))
}


# Obtención del _data frame_ con los datos efectivos.
# El _data frame_ se empaqueta en un objeto de tipo `tbl_df` de `dplyr` para su manejo.
#
# @param expenses_sheet Hoja de cC!lculo de Google Spreadsheets de gastos e ingresos.
#
# @returns Objeto de tipo `tbl_df` con los datos de gastos e ingresos.
get_expenses_data <- function (expenses_sheet) {
	create_money_converter ( )
	create_date_converter ( )
	create_yesno_converter ( )

	expenses_data <- get_via_csv (
		expenses_sheet,
		header = TRUE,
		verbose = FALSE,
		colClasses = c (
			Id = "numeric",
			Fecha = "pdate",
			Estimado = "yesno",
			Cerrado = "yesno",
			Tipo = "factor",
			Importe = "money",
			Referencia = "numeric",
			Observaciones = "character"
		)
	)

	# Añadimos dos columnas adicionales para mes y año.
	expenses_data <- mutate (
		expenses_data,
		Mes = as.factor (as.numeric (format (Fecha, "%m"))),
		Año = as.factor (as.numeric (format (Fecha, "%Y")))
	)

	# Ajustamos la columna `Cerrado` para cerrar automáticamente las estimaciones en estos dos casos:
	#
	# * La estimación pertenece a un mes en el pasado.
	# * La estimación es del mes actual, pero el gasto real supera al gasto estimado.
	real_expenses_per_estimation <- expenses_data %>% group_by (Referencia) %>% summarise (Consumido = sum (Importe))
	expenses_data <- left_join (expenses_data, real_expenses_per_estimation, by = c ("Id" = "Referencia"))
	expenses_data <- mutate (
		expenses_data,
		Cerrado = ifelse (
			estimation_is_in_the_past (Estimado, Fecha) | estimation_is_current_and_consumed (Estimado, Cerrado, Fecha, Importe, Consumido),
			TRUE,
			FALSE
		)
	) %>%
		select (Id, Fecha, Mes, Año, Estimado, Cerrado, Tipo, Importe, Referencia, Observaciones)

	return (tbl_df (expenses_data))
}


# Protocolo de inicialización.
load_package (DEVTOOLS_PACKAGE)
load_package_github (GSHEETS_REPO, GSHEETS_PACKAGE)
load_package (DPLYR_PACKAGE)
expenses_data <- get_expenses_data (load_expenses_sheet ( ))

# Protocolo de limpieza.
rm (list = c (
	"create_date_converter",
	"create_yesno_converter",
	"create_money_converter",
	"get_expenses_data",
	"load_expenses_sheet",
	"load_package",
	"load_package_github",
	"belongs_to_the_past",
	"estimation_is_current_and_consumed",
	"estimation_is_in_the_past"
))
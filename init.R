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

	csv_expenses <- get_via_csv (
		expenses_sheet,
		header = TRUE,
		verbose = FALSE,
		colClasses = c (
			Id = "numeric",
			Fecha = "pdate",
			Estimado = "yesno",
			Tipo = "factor",
			Importe = "money",
			Referencia = "numeric",
			Observaciones = "character"
		)
	)

	csv_expenses <- mutate (
		csv_expenses,
		Mes = as.factor (as.numeric (format (Fecha, "%m"))),
		Año = as.factor (as.numeric (format (Fecha, "%Y")))
	) %>%
		select (Id, Fecha, Mes, Año, Estimado, Tipo, Importe, Referencia, Observaciones)

	return (tbl_df (csv_expenses))
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
	"load_package_github"
))
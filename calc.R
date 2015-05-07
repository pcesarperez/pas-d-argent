# calc.R
# Cálculos específicos sobre los datos de ingresos y gastos.

# Carga de datos de trabajo.
source.with.encoding ("init.R", encoding = "UTF-8")


# Obtiene una tabla resumen de estimaciones y gastos reales asociados.
#
# @param month Mes para el que se desea obtener el resumen (mes actual por defecto).
# @param year Año para el que se deea obtener el resumen (año actual por defecto).
#
# @returns Devuelve una tabla resumen de estimaciones con los siguientes campos:
#
# * `Id`: Identificador de la entrada estimada.
# * `Fecha`: Fecha de anotación de la estimación.
# * `Tipo`: Tipo de gasto.
# * `Importe.Estimado`: Importe de la estimación realizada.
# * `Importe.Real`: Importe real asociado a la estimación.
get_estimated_table <- function (expenses, month = NA, year = NA) {
	# Si no se especifica mes o año, tomamos el mes y año actual para el filtrado.
	if (is.na (month) || is.na (year)) {
		month <- as.numeric (format (Sys.Date ( ), "%m"))
		year <- as.numeric (format (Sys.Date ( ), "%Y"))
	}

	# En primer lugar obtenemos un _data frame_ con las estimaciones realizadas.
	estimated <- expenses %>%
		filter (Estimado == TRUE, Mes == month, Año == year) %>%
		select (Id, Fecha, Tipo, Importe.Estimado = Importe, Observaciones)

	# En segundo lugar obtenemos otro _data frame_ con los importes reales asociados a cada estimación.
	real <- expenses %>%
		filter (complete.cases (Referencia)) %>%
		group_by (Referencia) %>%
		summarize (Importe.Real = sum (Importe))

	# Finalmente, unimos los dos _data frames_ para mostrar los importes estimados y los reales.
	estimated_balance <- left_join (estimated, real, by = c ("Id" = "Referencia")) %>%
		mutate (Balance = Importe.Real - Importe.Estimado) %>%
		select (Id, Fecha, Tipo, Importe.Estimado, Importe.Real, Balance, Observaciones)

	return (estimated_balance)
}


# Obtiene el balance de cuentas estimadas para un mes y año determinados.
#
# @param estimated_table Tabla de estimaciones para un mes y año determinados.
#
# @returns Balance de cuentas (es decir, saldo final) de las estimaciones para un mes y año determinados.
get_estimated_balance <- function (estimated_table) {
	return (sum (estimated_table$Balance, na.rm = TRUE))
}


# Obtiene el gasto real derivado de las estimaciones realizadas para un mes y un año determinados.
#
# @param estimated_table Tabla de estimaciones para un mes y un año determinados.
#
# @returns Gasto real derivado de las estimaciones realizadas para un mes y un año determinados.
get_adjusted_expenses <- function (estimated_table) {
	return (sum (estimated_table$Importe.Real, na.rm = TRUE))
}


get_estimated_expenses <- function (estimated_table) {
	return (sum (estimated_table$Importe.Estimado, na.rm = TRUE))
}


# Obtiene el balance de cuentas reales para un mes y año determinados.
# En caso de no especificarse mes o año, se considerarán el mes y año actuales.
#
# @param expenses _Data frame_ con la lista de gastos obtenida de Google Spreadsheets.
# @param month Mes para el cual se desea obtener el balance (por defecto, el mes actual).
# @param year Año para el cual se desea obtener el balance (por defecto, el año actual).
#
# @returns Balance real de cuentas para el mes y año especificados.
get_actual_balance <- function (expenses, month = NA, year = NA) {
	# Si no se especifica mes o año, tomamos el mes y año actual para el filtrado.
	if (is.na (month) || is.na (year)) {
		month <- as.numeric (format (Sys.Date ( ), "%m"))
		year <- as.numeric (format (Sys.Date ( ), "%Y"))
	}

	estimated_table <- get_estimated_table (expenses, month, year)
	estimated_expenses <- get_estimated_expenses (estimated_table)
	print (estimated_expenses)
	adjusted_expenses <- get_adjusted_expenses (estimated_table)
	print (adjusted_expenses)

	actual_balance <- expenses %>%
		filter (Estimado == FALSE, is.na (Referencia), Mes == month, Año == year) %>%
		summarize (Balance = sum (Importe, na.rm = TRUE) + estimated_expenses - adjusted_expenses)

	return (actual_balance)
}
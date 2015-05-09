# init.R
# RHomeEconomy 0.8
#
# This is a personal project to manage my home economy using Google Spreadsheets and R scripts.
#
# Initial loading script.


# Constants.
#
# * `DEVTOOLS_PACKAGE`: Name of the package `devtools`, needed to load the package `googlesheets`.
# * `GSHEETS_REPO`: Github repo identifier for the package `googlesheets`.
# * `GSHEETS_PACKAGE`: Name of the package `googlesheets`.
# * `DPLYR_PACKAGE`: Name of the package `dplyr`.
# * `EXPENSES_SHEET_NAME`: Name of the base data sheet with incomes and expenses.
DEVTOOLS_PACKAGE <- "devtools"
GSHEETS_REPO <- "jennybc/googlesheets"
GSHEETS_PACKAGE <- "googlesheets"
DPLYR_PACKAGE <- "dplyr"
EXPENSES_SHEET_NAME <- "GastosIngresos"


# Loads a library into the workspace.
# If the library is not installed, it tries to install it first.
#
# @param package.name Name of the library to load (empty string by default).
load_package <- function (package.name = "") {
	if (package.name != "") {
		if (!package.name %in% installed.packages ( )) {
			install.packages (package.name)
		}

		library (package = package.name, character.only = TRUE)
	}
}


# Loads a library hosted in GitHub into the workspace.
#
# @param repo.name Name of the GitHub repo where the library is located (empty string by default).
# @param package.name Name of the library to load (empty string by default).
load_package_github <- function (repo.name = "", package.name = "") {
	if ((repo.name != "") && (package.name != "")) {
		if (!package.name %in% installed.packages ( )) {
			devtools::install_github (repo.name)
		}

		library (package = package.name, character.only = TRUE)
	}
}


# Registers the base data sheet with incomes and expenses.
# The sheet is registered as is, with no further modifications.
#
# @returns Reference to the base data sheet with incomes and expenses, loaded from Google Drive.
load_expenses_sheet <- function ( ) {
	return (register_ss (EXPENSES_SHEET_NAME, verbose = FALSE))
}


# Creates a handler to convert items of type `character` in items of type `money`.
# The handler does the following:
#
# * Removes the euro symbol (€) from the input string.
# * Removes the thousands separator (.) from the input string.
# * Replaces the comma decimal separator (,) by the dot decimal separator (.).
create_character_to_money_handler <- function ( ) {
	setClass ("money")
	setAs (
		"character",
		"money",
		function (from) {
			return (as.numeric (gsub ("€", "", gsub (",", ".", gsub ("\\.", "", from)))))
		}
	)
}


# Creates a handler to convert items of type `character` with "Sí"/"No" values ("Yes"/"No" in Spanish) to logical values.
create_character_to_yesno_handler <- function ( ) {
	setClass ("yesno")
	setAs (
		"character",
		"yesno",
		function (from) {
			return (unlist (lapply (from, function (x) {switch (x, "Sí" = TRUE, "No" = FALSE, FALSE)})))
		}
	)
}


# Creates a handler to convert items of type `character` to a `POSIXct` date with "%d/%m/%Y" format.
create_character_to_posixct_converter <- function ( ) {
	setClass ("pdate")
	setAs (
		"character",
		"pdate",
		function (from) {
			return (as.Date (from, tz = "", format = "%d/%m/%Y"))
		}
	)
}


# Defines if a given date is in the past.
# A date is considered to be in the past if it belongs to the previous month.
#
# @param date Date to check.
#
# @returns `TRUE` if the date belongs to the previous month or before; `FALSE` otherwise.
date_belongs_to_the_past <- function (date) {
	return (format (date, "%Y%m") < format (Sys.Date ( ), "%Y%m"))
}


# Defines if an estimated expense (budget) belongs to the past.
#
# @param is_budget Logical value of the `Is.Budget` column, stating if the expense is an estimation (budget).
# @param date Date of the expense.
#
# @returns `TRUE` if the expense is an estimation and belongs to the past; `FALSE` otherwise.
budget_belongs_to_the_past <- function (is_budget, date) {
	return (date_belongs_to_the_past (date) & is_budget)
}


# Defines if an estimated expense (budget) in the current month has been consumed.
#
# @param is_budget Logical value of the `Is.Budget` column, stating if the expense is an estimation (budget).
# @param date Date of the budget.
# @param amount Budget amount.
# @param budget_consumed Budget consumed.
#
# @returns `TRUE` if the expense is a budget in the current month, and the budget consumed exceeds the estimation; `FALSE` otherwise.
budget_is_current_and_consumed <- function (is_budget, date, amount, budget_consumed) {
	return (!date_belongs_to_the_past (date) & is_budget & !is.na (budget_consumed) & (abs (amount) <= abs (budget_consumed)))
}


# Defines if an estimated expense (budget) should be automatically closed.
# A budget should be closed in one of these cases:
#
# * The budget belongs to a month in the past.
# * The budget belongs to the current month, but has been consumed (the real expenses exceeds the budget amount).
#
# @param is_budget Logical value of the `Is.Budget` column, stating if the expense is an estimation (budget).
# @param date Date of the budget.
# @param amount Budget amount.
# @param budget_consumed Budget consumed.
#
# @returns `TRUE` if the expense is a budget which meets the conditions to be closed; `FALSE` otherwise.
budget_should_be_closed <- function (is_budget, date, amount, budget_consumed) {
	return (budget_belongs_to_the_past (is_budget, date) | budget_is_current_and_consumed (is_budget, date, amount, budget_consumed))
}


# Gets the month from a given date.
#
# @param date Date to get the month from.
#
# @returns Factor from the numeric representation of the month in the date.
get_month_from_date <- function (date) {
	return (as.factor (as.numeric (format (date, "%m"))))
}


# Gets the year from a given date.
#
# @param date Date to get the year from.
#
# @returns Factor from the numeric representation of the year in the date.
get_year_from_date <- function (date) {
	return (as.factor (as.numeric (format (date, "%Y"))))
}


# Creates the data frame with the incomes/expenses data.
# The data frame goes through several transformations:
#
# * The column names are translated into English (the original sheet is in Spanish).
# * A new `Month` column is added, to help in filtering the data frame.
# * A new `Year` column is added, to help in filtering the data frame.
# * Every estimated expense in the past is automatically closed.
# * Every estimated expense in the current month which has been consumed is automatically closed.
#
# @param expenses_reference Reference to the base data sheet with incomes and expenses.
#
# @returns A `tbl_df` data frame with the incomes/expenses data.
get_expenses_data <- function (expenses_reference) {
	# Creates the handlers to read the sheet with the correct data types.
	create_character_to_money_handler ( )
	create_character_to_yesno_handler ( )
	create_character_to_posixct_converter ( )

	# Loads the sheet from Google Spreadsheets.
	expenses_data <- get_via_csv (
		expenses_reference,
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
		),
		col.names = c (
			"Id",
			"Date",
			"Is.Budget",
			"Is.Closed",
			"Type",
			"Amount",
			"Reference",
			"Comments"
		)
	)

	# Adds the `Month` and `Year` columns.
	expenses_data <- mutate (
		expenses_data,
		Month = get_month_from_date (Date),
		Year = get_year_from_date (Date)
	)

	# Closes automatically the budgets if they fall into one of these cases:
	#
	# * The budget belongs to a month in the past.
	# * The budget belongs to the current month, but has been consumed (the real expenses exceeds the budget amount).
	real_expenses_per_budget <- expenses_data %>% group_by (Reference) %>% summarise (Budget.Consumed = sum (Amount))
	expenses_data <- left_join (expenses_data, real_expenses_per_budget, by = c ("Id" = "Reference"))
	expenses_data <- mutate (
		expenses_data,
		Is.Closed = ifelse (
			budget_should_be_closed (Is.Budget, Date, Amount, Budget.Consumed),
			TRUE,
			FALSE
		)
	) %>%
		select (Id, Date, Month, Year, Is.Budget, Is.Closed, Type, Amount, Reference, Comments)

	return (tbl_df (expenses_data))
}


# Initialization protocol.
load_package (DEVTOOLS_PACKAGE)
load_package_github (GSHEETS_REPO, GSHEETS_PACKAGE)
load_package (DPLYR_PACKAGE)
expenses_data <- get_expenses_data (load_expenses_sheet ( ))
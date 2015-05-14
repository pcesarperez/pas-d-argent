# init.R
# Pas d'argent 0.8
#
# This is a personal project to manage my home economy using Google Spreadsheets and R scripts.
#
# Initial loading script.


# Modules.
source.with.encoding ("properties.R", encoding = "UTF-8")
source.with.encoding ("packages.R", encoding = "UTF-8")

# Initialization.
load.package ("devtools")
load.package.github ("jennybc/googlesheets", "googlesheets")
load.package ("dplyr")
properties <- get.properties.from.file ("sheet.properties")

# Properties setup.
SHEET.NAME <- get.character.property (properties, "sheet.name")
COLUMN.ID <- get.character.property (properties, "column.id")
COLUMN.DATE <- get.character.property (properties, "column.date")
COLUMN.IS.BUDGET <- get.character.property (properties, "column.is.budget")
COLUMN.IS.CLOSED <- get.character.property (properties, "column.is.closed")
COLUMN.TYPE <- get.character.property (properties, "column.type")
COLUMN.AMOUNT <- get.character.property (properties, "column.amount")
COLUMN.REFERENCE <- get.character.property (properties, "column.reference")
COLUMN.COMMENTS <- get.character.property (properties, "column.comments")
VALUE.YES <- get.character.property (properties, "value.yes")
VALUE.NO <- get.character.property (properties, "value.no")
VALUE.CURRENCY <- get.character.property (properties, "value.currency")


# Registers the specified base data sheet with incomes and expenses.
# The sheet is registered as is, with no further modifications.
#
# @returns Reference to the specified base data sheet with incomes and expenses, loaded from Google Drive.
load.expenses.sheet <- function ( ) {
	return (gs_title (SHEET.NAME, verbose = FALSE))
}


# Creates a handler to convert items of type `character` in items of type `money`.
# The handler does the following:
#
# * Removes the currency symbol (VALUE.CURRENCY) from the input string.
# * Removes the thousands separator (.) from the input string.
# * Replaces the comma decimal separator (,) by the dot decimal separator (.).
create.character.to.money.handler <- function ( ) {
	setClass ("money")
	setAs (
		"character",
		"money",
		function (from) {
			return (as.numeric (gsub (VALUE.CURRENCY, "", gsub (",", ".", gsub ("\\.", "", from)))))
		}
	)
}


# Creates a handler to convert items of type `character` with VALUE.YES/VALUE.NO values to logical values.
create.character.to.yesno.handler <- function ( ) {
	setClass ("yesno")
	setAs (
		"character",
		"yesno",
		function (from) {
			transformation <- lapply (from, function (x) {
				if (is.na (x)) {
					return (FALSE)
				} else if (x == VALUE.YES) {
					return (TRUE)
				} else if (x == VALUE.NO) {
					return (FALSE)
				} else {
					return (FALSE)
				}
			})

			return (unlist (transformation))
		}
	)
}


# Creates a handler to convert items of type `character` to a `POSIXct` date with "%d/%m/%Y" format.
create.character.to.posixct.converter <- function ( ) {
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
date.belongs.to.the.past <- function (date) {
	return (format (date, "%Y%m") < format (Sys.Date ( ), "%Y%m"))
}


# Defines if an estimated expense (budget) belongs to the past.
#
# @param is.budget Logical value of the `Is.Budget` column, stating if the expense is an estimation (budget).
# @param date Date of the expense.
#
# @returns `TRUE` if the expense is an estimation and belongs to the past; `FALSE` otherwise.
budget.belongs.to.the.past <- function (is.budget, date) {
	return (date.belongs.to.the.past (date) & is.budget)
}


# Defines if an estimated expense (budget) in the current month has been consumed.
#
# @param is.budget Logical value of the `Is.Budget` column, stating if the expense is an estimation (budget).
# @param date Date of the budget.
# @param amount Budget amount.
# @param budget.consumed Budget consumed.
#
# @returns `TRUE` if the expense is a budget in the current month, and the budget consumed exceeds the estimation; `FALSE` otherwise.
budget.is.current.and.consumed <- function (is.budget, date, amount, budget.consumed) {
	return (!date.belongs.to.the.past (date) & is.budget & !is.na (budget.consumed) & (abs (amount) <= abs (budget.consumed)))
}


# Defines if an estimated expense (budget) should be automatically closed.
# A budget should be closed in one of these cases:
#
# * The budget belongs to a month in the past.
# * The budget belongs to the current month, but has been consumed (the real expenses exceeds the budget amount).
#
# @param is.budget Logical value of the `Is.Budget` column, stating if the expense is an estimation (budget).
# @param date Date of the budget.
# @param amount Budget amount.
# @param budget.consumed Budget consumed.
#
# @returns `TRUE` if the expense is a budget which meets the conditions to be closed; `FALSE` otherwise.
budget.should.be.closed <- function (is.budget, date, amount, budget.consumed) {
	return (budget.belongs.to.the.past (is.budget, date) | budget.is.current.and.consumed (is.budget, date, amount, budget.consumed))
}


# Gets the month from a given date.
#
# @param date Date to get the month from.
#
# @returns Factor from the numeric representation of the month in the date.
get.month.from.date <- function (date) {
	return (as.factor (as.numeric (format (date, "%m"))))
}


# Gets the year from a given date.
#
# @param date Date to get the year from.
#
# @returns Factor from the numeric representation of the year in the date.
get.year.from.date <- function (date) {
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
# @param expenses.reference Reference to the base data sheet with incomes and expenses.
#
# @returns A `tbl_df` data frame with the incomes/expenses data.
get.expenses.data <- function (expenses.reference) {
	# Creates the handlers to read the sheet with the correct data types.
	create.character.to.money.handler ( )
	create.character.to.yesno.handler ( )
	create.character.to.posixct.converter ( )

	# Loads the sheet from Google Spreadsheets.
	expenses.data <- get_via_csv (
		expenses.reference,
		header = TRUE,
		verbose = FALSE,
		colClasses = c (
			COLUMN.ID = "numeric",
			COLUMN.DATE = "pdate",
			COLUMN.IS.BUDGET = "yesno",
			COLUMN.IS.CLOSED = "yesno",
			COLUMN.TYPE = "factor",
			COLUMN.AMOUNT = "money",
			COLUMN.REFERENCE = "numeric",
			COLUMN.COMMENTS = "character"
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
	expenses.data <- mutate (
		expenses.data,
		Month = get.month.from.date (Date),
		Year = get.year.from.date (Date)
	)

	# Closes automatically the budgets if they fall into one of these cases:
	#
	# * The budget belongs to a month in the past.
	# * The budget belongs to the current month, but has been consumed (the real expenses exceeds the budget amount).
	real.expenses.per.budget <- expenses.data %>% group_by (Reference) %>% summarise (Budget.Consumed = sum (Amount))
	expenses.data <- left_join (expenses.data, real.expenses.per.budget, by = c ("Id" = "Reference"))
	expenses.data <- mutate (
		expenses.data,
		Is.Closed = ifelse (
			budget.should.be.closed (Is.Budget, Date, Amount, Budget.Consumed),
			TRUE,
			FALSE
		)
	) %>%
		select (Id, Date, Month, Year, Is.Budget, Is.Closed, Type, Amount, Reference, Comments)

	return (tbl_df (expenses.data))
}


expenses.data <- get.expenses.data (load.expenses.sheet ( ))
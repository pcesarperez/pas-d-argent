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
loadPackage ("devtools")
loadPackageGithub ("jennybc/googlesheets", "googlesheets")
loadPackage ("dplyr")
properties <- getPropertiesFromFile ("sheet.properties")

# Properties setup.
SHEET_NAME <- getCharacterProperty (properties, "sheet.name")
COLUMN_ID <- getCharacterProperty (properties, "column.id")
COLUMN_DATE <- getCharacterProperty (properties, "column.date")
COLUMN_IS_BUDGET <- getCharacterProperty (properties, "column.is.budget")
COLUMN_IS_CLOSED <- getCharacterProperty (properties, "column.is.closed")
COLUMN_TYPE <- getCharacterProperty (properties, "column.type")
COLUMN_AMOUNT <- getCharacterProperty (properties, "column.amount")
COLUMN_REFERENCE <- getCharacterProperty (properties, "column.reference")
COLUMN_COMMENTS <- getCharacterProperty (properties, "column.comments")
VALUE_YES <- getCharacterProperty (properties, "value.yes")
VALUE_NO <- getCharacterProperty (properties, "value.no")
VALUE_CURRENCY <- getCharacterProperty (properties, "value.currency")


# Registers the specified base data sheet with incomes and expenses.
# The sheet is registered as is, with no further modifications.
#
# @returns Reference to the specified base data sheet with incomes and expenses, loaded from Google Drive.
loadExpensesSheet <- function ( ) {
	return (gs_title (SHEET_NAME, verbose = FALSE))
}


# Creates a handler to convert items of type `character` in items of type `money`.
# The handler does the following:
#
# * Removes the currency symbol (`VALUE.CURRENCY`) from the input string.
# * Removes the thousands separator (.) from the input string.
# * Replaces the comma decimal separator (,) by the dot decimal separator (.).
createCharacterToMoneyHandler <- function ( ) {
	setClass ("money")
	setAs (
		"character",
		"money",
		function (from) {
			return (as.numeric (gsub (VALUE_CURRENCY, "", gsub (",", ".", gsub ("\\.", "", from)))))
		}
	)
}


# Creates a handler to convert items of type `character` with VALUE.YES/VALUE.NO values to logical values.
createCharacterToYesNoHandler <- function ( ) {
	setClass ("yesno")
	setAs (
		"character",
		"yesno",
		function (from) {
			transformation <- lapply (from, function (x) {
				if (is.na (x)) {
					return (FALSE)
				} else if (x == VALUE_YES) {
					return (TRUE)
				} else if (x == VALUE_NO) {
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
createCharacterToPosixCTHandler <- function ( ) {
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
dateBelongsToThePast <- function (date) {
	return (format (date, "%Y%m") < format (Sys.Date ( ), "%Y%m"))
}


# Determines if an estimated expense (budget) is closed yet.
#
# @param isBudget Logical value of the `Is.Budget` column, stating if the expense is an estimation (budget).
# @param isClosed Logical value of the `Is.Closed` column, stating if the expense has been manually closed.
#
# @returns `TRUE` if the expense is an estimation and has been closed; `FALSE` otherwise.
budgetHasBeenClosedYet <- function (isBudget, isClosed) {
	return (isBudget & isClosed)
}


# Defines if an estimated expense (budget) belongs to the past.
#
# @param isBudget Logical value of the `Is.Budget` column, stating if the expense is an estimation (budget).
# @param date Date of the expense.
#
# @returns `TRUE` if the expense is an estimation and belongs to the past; `FALSE` otherwise.
budgetBelongsToThePast <- function (isBudget, date) {
	return (dateBelongsToThePast (date) & isBudget)
}


# Defines if an estimated expense (budget) in the current month has been consumed.
#
# @param isBudget Logical value of the `Is.Budget` column, stating if the expense is an estimation (budget).
# @param date Date of the budget.
# @param amount Budget amount.
# @param budgetConsumed Budget consumed.
#
# @returns `TRUE` if the expense is a budget in the current month, and the budget consumed exceeds the estimation; `FALSE` otherwise.
budgetIsCurrentAndConsumed <- function (isBudget, date, amount, budgetConsumed) {
	return (!dateBelongsToThePast (date) & isBudget & !is.na (budgetConsumed) & (abs (amount) <= abs (budgetConsumed)))
}


# Defines if an estimated expense (budget) should be automatically closed.
# A budget should be closed in one of these cases:
#
# * The budget has been manually closed yet.
# * The budget belongs to a month in the past.
# * The budget belongs to the current month, but has been consumed (the real expenses exceeds the budget amount).
#
# @param isBudget Logical value of the `Is.Budget` column, stating if the expense is an estimation (budget).
# @param isClosed Logical value of the `Is.Closed` column, stating if the budget has been manually closed.
# @param date Date of the budget.
# @param amount Budget amount.
# @param budgetConsumed Budget consumed.
#
# @returns `TRUE` if the expense is a budget which meets the conditions to be closed; `FALSE` otherwise.
budgetShouldBeClosed <- function (isBudget, isClosed, date, amount, budgetConsumed) {
	return (
		budgetHasBeenClosedYet (isBudget, isClosed) |
		budgetBelongsToThePast (isBudget, date) |
		budgetIsCurrentAndConsumed (isBudget, date, amount, budgetConsumed)
	)
}


# Gets the month from a given date.
#
# @param date Date to get the month from.
#
# @returns Factor from the numeric representation of the month in the date.
getMonthFromDate <- function (date) {
	return (as.factor (as.numeric (format (date, "%m"))))
}


# Gets the year from a given date.
#
# @param date Date to get the year from.
#
# @returns Factor from the numeric representation of the year in the date.
getYearFromDate <- function (date) {
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
# @param expensesReference Reference to the base data sheet with incomes and expenses.
#
# @returns A `tbl_df` data frame with the incomes/expenses data.
getExpensesData <- function (expensesReference) {
	# Creates the handlers to read the sheet with the correct data types.
	createCharacterToMoneyHandler ( )
	createCharacterToYesNoHandler ( )
	createCharacterToPosixCTHandler ( )

	# Loads the sheet from Google Spreadsheets.
	expensesData <- get_via_csv (
		expensesReference,
		header = TRUE,
		verbose = FALSE,
		colClasses = c (
			COLUMN_ID = "numeric",
			COLUMN_DATE = "pdate",
			COLUMN_IS_BUDGET = "yesno",
			COLUMN_IS_CLOSED = "yesno",
			COLUMN_TYPE = "factor",
			COLUMN_AMOUNT = "money",
			COLUMN_REFERENCE = "numeric",
			COLUMN_COMMENTS = "character"
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
	expensesData <- mutate (
		expensesData,
		Month = getMonthFromDate (Date),
		Year = getYearFromDate (Date)
	)

	# Closes automatically the budgets if they fall into one of these cases:
	#
	# * The budget belongs to a month in the past.
	# * The budget belongs to the current month, but has been consumed (the real expenses exceeds the budget amount).
	realExpensesPerBudget <- expensesData %>% group_by (Reference) %>% summarise (Budget.Consumed = sum (Amount))
	expensesData <- left_join (expensesData, realExpensesPerBudget, by = c ("Id" = "Reference"))
	expensesData <- mutate (
		expensesData,
		Is.Closed = ifelse (
			budgetShouldBeClosed (Is.Budget, Is.Closed, Date, Amount, Budget.Consumed),
			TRUE,
			FALSE
		)
	) %>%
		select (Id, Date, Month, Year, Is.Budget, Is.Closed, Type, Amount, Reference, Comments)

	return (tbl_df (expensesData))
}


expensesData <- getExpensesData (loadExpensesSheet ( ))
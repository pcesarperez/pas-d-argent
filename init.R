# init.R
# Pas d'argent 0.8
#
# This is a personal project to manage my home economy using Google Spreadsheets and R scripts.
#
# Initial loading script.


# Modules.
source ("properties.R", encoding = "UTF-8")
source ("packages.R", encoding = "UTF-8")

# Initialization.
loadPackage ("methods")
loadPackage ("devtools")
loadPackage ("googlesheets")
loadPackage ("dplyr")
properties <- getPropertiesFromFile ("sheet.properties")

# Properties setup.
SHEET_NAME <- getCharacterProperty (properties, "sheet.name")
WORKSHEET_NAME <- getCharacterProperty (properties, "worksheet.name")
VALUE_YES <- getCharacterProperty (properties, "value.yes")
VALUE_NO <- getCharacterProperty (properties, "value.no")


# Registers the specified base data sheet with incomes and expenses.
# The sheet is registered as is, with no further modifications.
#
# @returns Reference to the specified base data sheet with incomes and expenses, loaded from Google Drive.
loadExpensesSheet <- function ( ) {
	return (gs_title (SHEET_NAME, verbose = FALSE))
}


# Converts a vector of items of type `character` with `VALUE.YES`/`VALUE.NO` values to logical values.
convertIntoLogicalValues <- function (character_vector) {
	transformation <- lapply (character_vector, function (x) {
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
	return (as.factor (as.numeric (format (as.Date (date, "%d/%m/%Y"), "%m"))))
}


# Gets the year from a given date.
#
# @param date Date to get the year from.
#
# @returns Factor from the numeric representation of the year in the date.
getYearFromDate <- function (date) {
	return (as.factor (as.numeric (format (as.Date (date, "%d/%m/%Y"), "%Y"))))
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
	# Specifies the decimal and grouping mark for currency values.
	spanishLocale <- locale (grouping_mark = ".", decimal_mark = ",")

	# Loads the sheet from Google Spreadsheets.
	expensesData <- gs_read (
		expensesReference,
		ws = WORKSHEET_NAME,
		verbose = FALSE,
		skip = 1,
		col_types = cols (
			Id = col_integer ( ),
			Date = col_date ("%d/%m/%Y"),
			Is.Shared = col_character ( ),
			Is.Budget = col_character ( ),
			Is.Closed = col_character ( ),
			Type = col_factor (c ("Niños", "Agua", "Coche", "Salud", "Gatuno", "Gasoil", "Gasto extra", "Gasto fijo", "Hogar", "Luz", "Nómina", "Ocio", "Restaurante", "Ropa", "Supermercado", "Teléfono", "Ingreso extra")),
			Amount = col_character ( ),
			Reference = col_integer ( ),
			Comments = col_character ( )
		),
		col_names = c (
			"Id",
			"Date",
			"Is.Shared",
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

	# Transforms the following columns:
	#
	# * `Is.Budget` should be converted to a logical value.
	# * `Is.Closed` should be converted to a logical value.
	# * `Is.Shared` should be converted to a logical value.
	# * `Amount` should be converted to a numeric value, taking into account it's actually a currency value.
	expensesData$Is.Shared <- convertIntoLogicalValues (expensesData$Is.Shared)
	expensesData$Is.Budget <- convertIntoLogicalValues (expensesData$Is.Budget)
	expensesData$Is.Closed <- convertIntoLogicalValues (expensesData$Is.Closed)
	expensesData$Amount <- parse_number(expensesData$Amount, locale = spanishLocale)

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
		select (Id, Date, Month, Year, Is.Shared, Is.Budget, Is.Closed, Type, Amount, Reference, Comments)

	return (tbl_df (expensesData))
}


expensesData <- getExpensesData (loadExpensesSheet ( ))
# calc.R
# Pas d'argent 0.8
#
# This is a personal project to manage my home economy using Google Spreadsheets and R scripts.
#
# Specific calculations over the data containing incomes and expenses.


# Modules.
source.with.encoding ("init.R", encoding = "UTF-8")


# Gets the current month.
#
# @returns The current month in numeric format.
get.current.month <- function ( ) {
	return (as.numeric (format (Sys.Date ( ), "%m")))
}


# Gets the current year.
#
# @returns The current year in numeric format.
get.current.year <- function ( ) {
	return (as.numeric (format (Sys.Date ( ), "%Y")))
}


# Gets a table with the budget projections filtered out from the expenses data.
#
# @param expenses Income/expenses data.
# @param month Month to filter the budget projections
# @param year Year to filter the budget projections.
#
# @returns A table with the budget projections for the given month and year.
get.budget.projections <- function (expenses, month, year) {
	budget.projections <- expenses %>%
		filter (Is.Budget == TRUE, Month == month, Year == year) %>%
		select (Id, Date, Type, Is.Closed, Projected.Amount = Amount, Comments)

	return (budget.projections)
}


# Gets a table with the budget consumption filtered out from the expenses data.
#
# @param expenses Income/expenses data.
# @param month Month to filter the budget consumption
# @param year Year to filter the budget consumption
#
# @returns A table with the budget consumption for the given month and year.
get.budget.consumption <- function (expenses, month, year) {
	budget.consumption <- expenses %>%
		filter (complete.cases (Reference), Month == month, Year == year) %>%
		group_by (Reference) %>%
		summarize (Actual.Amount = sum (Amount))

	return (budget.consumption)
}


# Gets a table with the budget summary for a given month and year.
#
# @param expenses Incomes/expenses data frame.
# @param month Month to summarize (current month by default).
# @param year Year to summarize (current year by default).
#
# @returns A budget summary table with the following columns:
#
# * `Id`: Id of the budget.
# * `Date`: Date of the budget.
# * `Type`: Type of expense.
# * `Is.Closed`: Logical value to state if the budget is closed.
# * `Projected.Amount`: Projected amount of the budget.
# * `Actual.Amount`: Actual amount consumed in the budget.
# * `Balance`: Difference between projected and actual amount of the budget.
# * `Comments`: Comments of the budget.
get.budget.summary <- function (expenses, month = NA, year = NA) {
	# If month and year are not specified, the current date is used.
	if (is.na (month) || is.na (year)) {
		month <- get.current.month ( )
		year <- get.current.year ( )
	}

	# Filters out a data frame with the budget expenses.
	budget.projections <- get.budget.projections (expenses, month, year)

	# Filters out a data frame with the expenses related to each declared budget.
	budget.consumption <- get.budget.consumption (expenses, month, year)

	# Joins the two data frames to obtain the budget summary.
	budget.summary <- left_join (budget.projections, budget.consumption, by = c ("Id" = "Reference")) %>%
		mutate (Balance = Actual.Amount - Projected.Amount) %>%
		select (Id, Date, Type, Is.Closed, Projected.Amount, Actual.Amount, Balance, Comments)

	return (budget.summary)
}


# Gets the total amount of projected and not closed budgets for a given budget summary.
#
# @param budget.summary Budget summary.
#
# @returns Final balance for the projected budgets in the budget summary.
get.projected.budgets.balance <- function (budget.summary) {
	projected.budgets.balance <- filter (budget.summary, Is.Closed == FALSE) %>%
		summarize (sum (Projected.Amount, na.rm = TRUE))

	return (projected.budgets.balance [[1]])
}


# Gets the total amount of actual expenses linked to budgets for a given bugdet summary.
#
# @param budget.summary Budget summary.
#
# @returns Final balance for the actual expenses linked to budgets in the budget summary.
get.actual.budgets.balance <- function (budget.summary) {
	actual.budgets.balance <- filter (budget.summary, Is.Closed == TRUE) %>%
		summarize (sum (Actual.Amount, na.rm = TRUE))

	return (actual.budgets.balance [[1]])
}


# Gets the account balance (incomes minus expenses) for a given month and year.
# The calculations take into account the projected budgets and the expenses linked to these budgets.
#
# @param expenses Incomes/expenses data.
# @param month Month to get the account balance (current month by default).
# @param year Year to get the account balance (current year by default).
#
# @returns Account balance for the given month and year.
get.actual.balance <- function (expenses, month = NA, year = NA) {
	# If month and year are not specified, the current date is used.
	if (is.na (month) || is.na (year)) {
		month <- get.current.month ( )
		year <- get.current.year ( )
	}

	# Gets the projected and actual budgets balance to adjust the account balance.
	budget.summary <- get.budget.summary (expenses, month, year)
	projected.budgets.balance <- get.projected.budgets.balance (budget.summary)
	actual.budgets.balance <- get.actual.budgets.balance (budget.summary)

	# Summarizes the expenses data using the budgets to adjust the account balance.
	actual.balance <- expenses %>%
		filter (Is.Budget == FALSE, is.na (Reference), Month == month, Year == year) %>%
		summarize (Balance = sum (Amount, na.rm = TRUE) + projected.budgets.balance + actual.budgets.balance)

	return (actual.balance [[1]])
}


# Automatic calculations for the current month and year.
current.budget.summary <- get.budget.summary (expenses.data)
current.balance <- get.actual.balance (expenses.data)
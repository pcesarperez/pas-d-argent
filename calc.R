# calc.R
# RHomeEconomy 0.8
#
# This is a personal project to manage my home economy using Google Spreadsheets and R scripts.
#
# Specific calculations over the data containing incomes and expenses.


# Initialization.
source.with.encoding ("init.R", encoding = "UTF-8")


# Gets the current month.
#
# @returns The current month in numeric format.
get_current_month <- function ( ) {
	return (as.numeric (format (Sys.Date ( ), "%m")))
}


# Gets the current year.
#
# @returns The current year in numeric format.
get_current_year <- function ( ) {
	return (as.numeric (format (Sys.Date ( ), "%Y")))
}


# Gets a table with the budget projections filtered out from the expenses data.
#
# @param expenses Income/expenses data.
# @param month Month to filter the budget projections
# @param year Year to filter the budget projections.
#
# @returns A table with the budget projections for the given month and year.
get_budget_projections <- function (expenses, month, year) {
	budget_projections <- expenses %>%
		filter (Is.Budget == TRUE, Month == month, Year == year) %>%
		select (Id, Date, Type, Is.Closed, Projected.Amount = Amount, Comments)

	return (budget_projections)
}


# Gets a table with the budget consumption filtered out from the expenses data.
#
# @param expenses Income/expenses data.
# @param month Month to filter the budget consumption
# @param year Year to filter the budget consumption
#
# @returns A table with the budget consumption for the given month and year.
get_budget_consumption <- function (expenses, month, year) {
	budget_consumption <- expenses %>%
		filter (complete.cases (Reference), Month == month, Year == year) %>%
		group_by (Reference) %>%
		summarize (Actual.Amount = sum (Amount))

	return (budget_consumption)
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
get_budget_summary <- function (expenses, month = NA, year = NA) {
	# If month and year are not specified, the current date is used.
	if (is.na (month) || is.na (year)) {
		month <- get_current_month ( )
		year <- get_current_year ( )
	}

	# Filters out a data frame with the budget expenses.
	budget_projections <- get_budget_projections (expenses, month, year)

	# Filters out a data frame with the expenses related to each declared budget.
	budget_consumption <- get_budget_consumption (expenses, month, year)

	# Joins the two data frames to obtain the budget summary.
	budget_summary <- left_join (budget_projections, budget_consumption, by = c ("Id" = "Reference")) %>%
		mutate (Balance = Actual.Amount - Projected.Amount) %>%
		select (Id, Date, Type, Is.Closed, Projected.Amount, Actual.Amount, Balance, Comments)

	return (budget_summary)
}


# Gets the total amount of projected and not closed budgets for a given budget summary.
#
# @param budget_summary Budget summary.
#
# @returns Final balance for the projected budgets in the budget summary.
get_projected_budgets_balance <- function (budget_summary) {
	projected_budgets_balance <- filter (budget_summary, Is.Closed == FALSE) %>%
		summarize (sum (Projected.Amount, na.rm = TRUE))

	return (projected_budgets_balance [[1]])
}


# Gets the total amount of actual expenses linked to budgets for a given bugdet summary.
#
# @param budget_summary Budget summary.
#
# @returns Final balance for the actual expenses linked to budgets in the budget summary.
get_actual_budgets_balance <- function (budget_summary) {
	actual_budgets_balance <- filter (budget_summary, Is.Closed == TRUE) %>%
		summarize (sum (Actual.Amount, na.rm = TRUE))

	return (actual_budgets_balance [[1]])
}


# Gets the account balance (incomes minus expenses) for a given month and year.
# The calculations take into account the projected budgets and the expenses linked to these budgets.
#
# @param expenses Incomes/expenses data.
# @param month Month to get the account balance (current month by default).
# @param year Year to get the account balance (current year by default).
#
# @returns Account balance for the given month and year.
get_actual_balance <- function (expenses, month = NA, year = NA) {
	# If month and year are not specified, the current date is used.
	if (is.na (month) || is.na (year)) {
		month <- get_current_month ( )
		year <- get_current_year ( )
	}

	# Gets the projected and actual budgets balance to adjust the account balance.
	budget_summary <- get_budget_summary (expenses, month, year)
	projected_budgets_balance <- get_projected_budgets_balance (budget_summary)
	actual_budgets_balance <- get_actual_budgets_balance (budget_summary)

	# Summarizes the expenses data using the budgets to adjust the account balance.
	actual_balance <- expenses %>%
		filter (Is.Budget == FALSE, is.na (Reference), Month == month, Year == year) %>%
		summarize (Balance = sum (Amount, na.rm = TRUE) + projected_budgets_balance + actual_budgets_balance)

	return (actual_balance [[1]])
}


# Automatic calculations for the current month and year.
budget_summary <- get_budget_summary (expenses_data)
actual_balance <- get_actual_balance (expenses_data)
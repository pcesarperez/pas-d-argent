# calc.R
# Pas d'argent 0.91
#
# This is a personal project to manage my home economy using Google Spreadsheets and R scripts.
#
# Specific calculations over the data containing incomes and expenses.


# Modules.
source ("init.R", encoding = "UTF-8")


# Gets the current month.
#
# @returns The current month in numeric format.
getCurrentMonth <- function ( ) {
	return (as.numeric (format (Sys.Date ( ), "%m")))
}


# Gets the current year.
#
# @returns The current year in numeric format.
getCurrentYear <- function ( ) {
	return (as.numeric (format (Sys.Date ( ), "%Y")))
}


# Gets a table with the budget projections filtered out from the expenses data.
#
# @param expenses Income/expenses data.
# @param month Month to filter the budget projections
# @param year Year to filter the budget projections.
#
# @returns A table with the budget projections for the given month and year.
getBudgetProjections <- function (expenses, month, year) {
	budgetProjections <- expenses %>%
		filter (Is.Budget == TRUE, Month == month, Year == year) %>%
		select (Id, Date, Type, Is.Closed, Projected.Amount = Amount, Comments)

	return (budgetProjections)
}


# Gets a table with the budget consumption filtered out from the expenses data.
#
# @param expenses Income/expenses data.
# @param month Month to filter the budget consumption
# @param year Year to filter the budget consumption
#
# @returns A table with the budget consumption for the given month and year.
getBudgetConsumption <- function (expenses, month, year) {
	budgetConsumption <- expenses %>%
		filter (complete.cases (Reference), Month == month, Year == year) %>%
		group_by (Reference) %>%
		summarize (Actual.Amount = sum (Amount))

	return (budgetConsumption)
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
getBudgetSummary <- function (expenses, month = NA, year = NA) {
	# If month and year are not specified, the current date is used.
	if (is.na (month) || is.na (year)) {
		month <- getCurrentMonth ( )
		year <- getCurrentYear ( )
	}

	# Filters out a data frame with the budget expenses.
	budgetProjections <- getBudgetProjections (expenses, month, year)

	# Filters out a data frame with the expenses related to each declared budget.
	budgetConsumption <- getBudgetConsumption (expenses, month, year)

	# Joins the two data frames to obtain the budget summary.
	budgetSummary <- left_join (budgetProjections, budgetConsumption, by = c ("Id" = "Reference")) %>%
		mutate (Balance = Actual.Amount - Projected.Amount) %>%
		select (Id, Date, Type, Is.Closed, Projected.Amount, Actual.Amount, Balance, Comments)

	return (budgetSummary)
}


# Gets the total amount of projected and not closed budgets for a given budget summary.
#
# @param budgetSummary Budget summary.
#
# @returns Final balance for the projected budgets in the budget summary.
getProjectedBudgetsBalance <- function (budgetSummary) {
	projectedBudgetsBalance <- filter (budgetSummary, Is.Closed == FALSE) %>%
		summarize (sum (Projected.Amount, na.rm = TRUE))

	return (projectedBudgetsBalance [[1]])
}


# Gets the total amount of actual expenses linked to budgets for a given budget summary.
#
# @param budgetSummary Budget summary.
#
# @returns Final balance for the actual expenses linked to budgets in the budget summary.
getActualBudgetsBalance <- function (budgetSummary) {
	actualBudgetsBalance <- filter (budgetSummary, Is.Closed == TRUE) %>%
		summarize (sum (Actual.Amount, na.rm = TRUE))

	return (actualBudgetsBalance [[1]])
}


# Gets the account balance (incomes minus expenses) for a given month and year.
# The calculations take into account the projected budgets and the expenses linked to these budgets.
#
# @param expenses Incomes/expenses data.
# @param month Month to get the account balance (current month by default).
# @param year Year to get the account balance (current year by default).
#
# @returns Account balance for the given month and year.
getActualBalance <- function (expenses, month = NA, year = NA) {
	# If month and year are not specified, the current date is used.
	if (is.na (month) || is.na (year)) {
		month <- getCurrentMonth ( )
		year <- getCurrentYear ( )
	}

	# Gets the projected and actual budgets balance to adjust the account balance.
	budgetSummary <- getBudgetSummary (expenses, month, year)
	projectedBudgetsBalance <- getProjectedBudgetsBalance (budgetSummary)
	actualBudgetsBalance <- getActualBudgetsBalance (budgetSummary)

	# Summarizes the expenses data using the budgets to adjust the account balance.
	actualBalance <- expenses %>%
		filter (Is.Budget == FALSE, is.na (Reference), Month == month, Year == year) %>%
		summarize (Balance = sum (Amount, na.rm = TRUE) + projectedBudgetsBalance + actualBudgetsBalance)

	return (actualBalance [[1]])
}


# Automatic calculations for the current month and year.
currentBudgetSummary <- getBudgetSummary (expensesData)
currentBalance <- getActualBalance (expensesData)

print (currentBalance)
#Pas d'argent

##Introduction

This is a personal project to create a home economy managent software in R language.

There's not much to say for the moment, as this is a very first-stage work in progress.

By the way, "pas d'argent" means "no money" in French `:P`

##Files

There are two files right now in the repo:

* `init.R`: Initialization functions. This loads a Google Spreadsheet containing the income/expenses data into the `expenses_data` variable.
* `calc.R`: Statistical calculations. This contanins several functions to work with the data loaded in the first script (for example, getting the estimated monthly budget or getting a summary of the monthly budgets).

##Additional notes

* The spreadsheet I am using is private, of course, but I intend to create a playground, public, read-only sheet to illustrate how the system works.
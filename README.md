#Pas d'argent

##Introduction

This is a personal project to create a home economy managent software in R language.

There's not much to say for the moment, as this is a very first-stage work in progress.

By the way, "pas d'argent" means "not much money" in French `:P`

##Files

There are two files right now in the repo:

* `init.R`: Initialization functions. This loads a Google Spreadsheet containing the income/expenses data into the `expenses_data` variable.
* `calc.R`: Statistical calculations. This contanins several functions to work with the data loaded in the first script (for example, get the estimated monthly budget or get a summary of the monthly expenses).

##Additional notes

* The comments are in Spanish right now. I'm not economist, so I need to hone my vocabulary regarding this subject. I am planning to translate the comments in the future, although the function and variable names are in english.
* The spreadsheet I am using is private, of course, but I intend to create a playground, public, read-only sheet to illustrate how the system works.

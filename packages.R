# packages.R
# Pas d'argent 0.8
#
# This is a personal project to manage my home economy using Google Spreadsheets and R scripts.
#
# Package loading functions.


# Loads a library into the workspace.
# If the library is not installed, it tries to install it first.
#
# @param package.name Name of the library to load (empty string by default).
load.package <- function (package.name = "") {
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
load.package.github <- function (repo.name = "", package.name = "") {
	if ((repo.name != "") && (package.name != "")) {
		if (!package.name %in% installed.packages ( )) {
			devtools::install_github (repo.name)
		}

		library (package = package.name, character.only = TRUE)
	}
}
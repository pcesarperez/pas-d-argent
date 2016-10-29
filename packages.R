# packages.R
# Pas d'argent 0.9
#
# This is a personal project to manage my home economy using Google Spreadsheets and R scripts.
#
# Package loading functions.


# Loads a library into the workspace.
# If the library is not installed, it tries to install it first.
#
# @param packageName Name of the library to load (empty string by default).
loadPackage <- function (packageName = "") {
	if (packageName != "") {
		if (!packageName %in% installed.packages ( )) {
			install.packages (packageName)
		}

		library (package = packageName, character.only = TRUE)
	}
}


# Loads a library hosted in GitHub into the workspace.
#
# @param repoName Name of the GitHub repo where the library is located (empty string by default).
# @param packageName Name of the library to load (empty string by default).
loadPackageGithub <- function (repoName = "", packageName = "") {
	if ((repoName != "") && (packageName != "")) {
		if (!packageName %in% installed.packages ( )) {
			devtools::install_github (repoName)
		}

		library (package = packageName, character.only = TRUE)
	}
}
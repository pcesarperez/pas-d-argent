# properties.R
# RHomeEconomy 0.8
#
# This is a personal project to manage my home economy using Google Spreadsheets and R scripts.
#
# Functions to read properties from a file.
# Source of wisdom: http://stackoverflow.com/questions/13681310/reading-configuration-from-text-file


# Gets a set of properties (key/value pairs) from a properties file.
# The properties have a `{key}={value}` format.
#
# @param path.to.file Path of the properties file.
#
# @returns A named vector with the key/value pairs in the properties file.
get_properties_from_file <- function (path.to.file) {
	properties <- read.table (
		path.to.file,
		header = FALSE,
		sep = "=",
		row.names = 1,
		strip.white = TRUE,
		na.strings = "NA",
		stringsAsFactors = FALSE,
	)

	properties <- setNames (properties [, 1], row.names (properties))

	return (properties)
}
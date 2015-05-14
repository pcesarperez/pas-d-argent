# properties.R
# Pas d'argent 0.8
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
get.properties.from.file <- function (path.to.file) {
	properties <- read.table (
		path.to.file,
		header = FALSE,
		sep = "=",
		row.names = 1,
		strip.white = TRUE,
		na.strings = "NA",
		stringsAsFactors = FALSE,
		encoding = "UTF-8"
	)

	properties <- setNames (properties [, 1], row.names (properties))

	return (properties)
}


# Gets a property value with `character` format.
#
# @param properties Named vector with key/value property pairs.
# @param key Property key.
#
# @returns Property value with `character` format.
get.character.property <- function (properties, key) {
	return (as.character (properties [key]))
}


# Gets a property value with `numeric` format.
#
# @param properties Named vector with key/value property pairs.
# @param key Property key.
#
# @returns Property value with `numeric` format.
get.numeric.property <- function (properties, key) {
	return (as.numeric (properties [key]))
}


# Gets a property value as a character vector.
# The property value should be comma separated values.
#
# @param properties Named vector with key/value property pairs.
# @param key Property key.
#
# @returns Property value as a character vector.
get.multipart.property <- function (properties, key) {
	return (unlist (strsplit (properties [key], ",[ ]*"), use.names = FALSE))
}
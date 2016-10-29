# print.R
# Pas d'argent 0.9
#
# This is a personal project to manage my home economy using Google Spreadsheets and R scripts.
#
# Fancy printing functions.


# Gets the given table in a PDF-printable format.
#
# @param dataTable The table to be printed.
#
# @returns A triplet `(table, width, height)`.
getPrintableTable <- function (dataTable) {
	table <- tableGrob (dataTable, show.rownames = FALSE)
	width <- grobWidth (table)
	height <- grobHeight (table)

	result <- list (
		"table" = table,
		"width" = width,
		"height" = height
	)

	return (result)
}


# Gets the title of the printable table, according to its height.
#
# @param tableHeight The height of the printable table.
# @param titleText The text of the title.
#
# @returns The title of the printable table, according to its height.
getPrintableTableTitle <- function (tableHeight, titleText) {
	title <- textGrob (
		titleText,
		y = unit (0.5, "npc") + 0.5 * tableHeight,
		vjust = 0,
		gp = gpar (fontsize = 20)
	)

	return (title)
}


# Gets a composite object with the printable table and its title.
#
# @param printableTable The table to be printed.
# @param printableTableTitle The title of the printable table.
#
# @returns A composite object with the printable table and its title.
getPrintableTableWithTitle <- function (printableTable, printableTableTitle) {
	return (gTree (children = gList (printableTable, printableTableTitle)))
}


# Dumps a printable table plus its title to the specified PDF file.
#
# @param pdfName The name of the PDF file to dump the printable table into.
# @param width The paper width of the PDF.
# @param height The paper height of the PDF.
# @param printableTableWithTitle A composite object with a printable table and its title.
printTableToPDF <- function (pdfName, width, height, printableTableWithTitle) {
	pdf (file = pdfName, width = width, height = height)
	grid.draw (printableTableWithTitle)
	dev.off ( )
}


# Prints out to a PDF file a budget summary in a fancy format.
#
# @param budgetSummary The Budget summary to be printed.
# @param pdfName The name of the PDF file to dump the summary into.
printBudgetSummary <- function (budgetSummary, pdfName = "budget_summary.pdf") {
	printableTable <- getPrintableTable (budgetSummary)
	printableTableTitle <- getPrintableTableTitle (printableTable$height, "Resumen de estimaciones")

	printableTableWithTitle <- getPrintableTableWithTitle (printableTable$table, printableTableTitle)

	printTableToPDF (pdfName, width = 16.53, height = 11.69, printableTableWithTitle)
}
# mail.R
# Pas d'argent 0.8
#
# This is a personal project to manage my home economy using Google Spreadsheets and R scripts.
#
# Mailing protocol to send information about the current acount balance via mail.


# Modules.
source.with.encoding ("packages.R", encoding = "UTF-8")
source.with.encoding ("properties.R", encoding = "UTF-8")
source.with.encoding ("calc.R", encoding = "UTF-8")

# Initialization.
loadPackage ("mailR")
properties <- getPropertiesFromFile ("mail.properties")


# Gets the current date in "%d/%m/%Y" format.
#
# @returns The current date in "%d/%m/%Y" format.
getCurrentDate <- function ( ) {
	return (format (Sys.Date ( ), "%d/%m/%Y"))
}


# Gets the message subject on a daily basis.
#
# @param msgSubject Fixed part of the message subject.
#
# @returns The message subject with the current date as a prefix.
getDailyMessageSubject <- function (msgSubject) {
	return (paste (paste ("[", getCurrentDate ( ), "]", sep = ""), msgSubject))
}


# Gets the message body on a daily basis.
#
# @param msgBody Message body with a placeholder to put the account balance to date.
# @param currentBalance Account balance to date.
#
# @returns The message body with the current account balance.
getDailyMessageBody <- function (msgBody, currentBalance) {
	return (gsub (pattern = "%", msgBody, replacement = round (currentBalance, digits = 2)))
}


# Properties setup.
SENDER <- getCharacterProperty (properties, "sender")
RECIPIENTS <- getMultipartProperty (properties, "recipients")
MSG_SUBJECT <- getDailyMessageSubject (getCharacterProperty (properties, "msg.subject"))
MSG_BODY <- getDailyMessageBody (getCharacterProperty (properties, "msg.body"), currentBalance)
MSG_ENCODING <- getCharacterProperty (properties, "msg.encoding")
SMTP_SERVER <- getCharacterProperty (properties, "smtp.server")
SMTP_PORT <- getNumericProperty (properties, "smtp.port")
USER_NAME <- getCharacterProperty (properties, "user.name")
USER_PASSWORD <- getCharacterProperty (properties, "user.password")


# Sends an email with the current account balance.
sendCurrentBalanceEmail <- function ( ) {
	send.mail (
		from = SENDER,
		to = RECIPIENTS,
		subject = MSG_SUBJECT,
		body = MSG_BODY,
		encoding = MSG_ENCODING,
		html = TRUE,
		inline = FALSE,
		smtp = list (
			host.name = SMTP_SERVER,
			port = SMTP_PORT,
			ssl = TRUE,
			user.name = USER_NAME,
			passwd = USER_PASSWORD
		),
		authenticate = TRUE,
		send = TRUE,
		attach.files = NULL,
		debug = FALSE
	)
}
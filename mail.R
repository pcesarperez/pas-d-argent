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
load.package ("mailR")
properties <- get.properties.from.file ("mail.properties")


# Gets the current date in "%d/%m/%Y" format.
#
# @returns The current date in "%d/%m/%Y" format.
get.current.date <- function ( ) {
	return (format (Sys.Date ( ), "%d/%m/%Y"))
}


# Gets the message subject on a daily basis.
#
# @param msg.subject Fixed part of the message subject.
#
# @returns The message subject with the current date as a prefix.
get.daily.message.subject <- function (msg.subject) {
	return (paste (paste ("[", get.current.date ( ), "]", sep = ""), msg.subject))
}


# Gets the message body on a daily basis.
#
# @param msg.body Message body with a placeholder to put the account balance to date.
# @param current.balance Account balance to date.
#
# @returns The message body with the current account balance.
get.daily.message.body <- function (msg.body, current.balance) {
	return (gsub (pattern = "%", msg.body, replacement = round (current.balance, digits = 2)))
}


# Properties setup.
SENDER <- get.character.property (properties, "sender")
RECIPIENTS <- get.multipart.property (properties, "recipients")
MSG.SUBJECT <- get.daily.message.subject (get.character.property (properties, "msg.subject"))
MSG.BODY <- get.daily.message.body (get.character.property (properties, "msg.body"), current.balance)
MSG.ENCODING <- get.character.property (properties, "msg.encoding")
SMTP.SERVER <- get.character.property (properties, "smtp.server")
SMTP.PORT <- get.numeric.property (properties, "smtp.port")
USER.NAME <- get.character.property (properties, "user.name")
USER.PASSWORD <- get.character.property (properties, "user.password")


# Sends an email with the current account balance.
send.current.balance.email <- function ( ) {
	send.mail (
		from = SENDER,
		to = RECIPIENTS,
		subject = MSG.SUBJECT,
		body = MSG.BODY,
		encoding = MSG.ENCODING,
		html = TRUE,
		inline = FALSE,
		smtp = list (
			host.name = SMTP.SERVER,
			port = SMTP.PORT,
			ssl = TRUE,
			user.name = USER.NAME,
			passwd = USER.PASSWORD
		),
		authenticate = TRUE,
		send = TRUE,
		attach.files = NULL,
		debug = FALSE
	)
}
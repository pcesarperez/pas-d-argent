# mail.R
# RHomeEconomy 0.8
#
# This is a personal project to manage my home economy using Google Spreadsheets and R scripts.
#
# Mailing protocol to send information about the current acount balance via mail.


# Initialization.
source.with.encoding ("calc.R", encoding = "UTF-8")
source.with.encoding ("properties.R", encoding = "UTF-8")
load_package ("mailR")
properties <- get_properties_from_file ("mail.properties")


# Gets the current date in "%d/%m/%Y" format.
#
# @returns The current date in "%d/%m/%Y" format.
get_current_date <- function ( ) {
	return (format (Sys.Date ( ), "%d/%m/%Y"))
}


# Gets the message subject on a daily basis.
#
# @param msg_subject Fixed part of the message subject.
#
# @returns The message subject with the current date as a prefix.
get_daily_message_subject <- function (msg_subject) {
	return (paste (paste ("[", get_current_date ( ), "]", sep = ""), msg_subject))
}


# Gets the message body on a daily basis.
#
# @param msg_body Message body with a placeholder to put the account balance to date.
# @param current_balance Account balance to date.
#
# @returns The message body with the current account balance.
get_daily_message_body <- function (msg_body, current_balance) {
	return (gsub (pattern = "%", msg_body, replacement = current_balance))
}


# Properties setup.
SENDER <- as.character (properties ["sender"])
RECIPIENTS <- unlist (strsplit (properties ["recipients"], ",[ ]*"), use.names = FALSE)
MSG_SUBJECT <- get_daily_message_subject (as.character (properties ["msg.subject"]))
MSG_BODY <- get_daily_message_body (as.character (properties ["msg.body"]), current_balance)
MSG_ENCODING <- as.character (properties ["msg.encoding"])
SMTP_SERVER <- as.character (properties ["smtp.server"])
SMTP_PORT <- as.numeric (properties ["smtp.port"])
USER_NAME <- as.character (properties ["user.name"])
USER_PASSWORD <- as.character (properties ["user.password"])


# Sends an email with the current account balance.
#
# @param current_balance Account balance to date.
send_current_balance_email <- function (current_balance) {
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
			port = as.numeric (SMTP_PORT),
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
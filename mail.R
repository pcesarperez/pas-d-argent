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
sender <- get_character_property (properties, "sender")
recipients <- get_multipart_property (properties, "recipients")
msg_subject <- get_daily_message_subject (get_character_property (properties, "msg.subject"))
msg_body <- get_daily_message_body (get_character_property (properties, "msg.body"), current_balance)
msg_encoding <- get_character_property (properties, "msg.encoding")
smtp_server <- get_character_property (properties, "smtp.server")
smtp_port <- get_numeric_property (properties, "smtp.port")
user_name <- get_character_property (properties, "user.name")
user_password <- get_character_property (properties, "user.password")


# Sends an email with the specified parameters.
#
# @param sender Sender email address.
# @param recipients List of comma separated email addresses of the report recipients.
# @param msg_subject Email subject.
# @param msg_body Email body..
# @param msg_encoding Email encoding.
# @param smtp_server SMTP server used to send the email.
# @param smtp_port Port of the SMTP server used to send the email.
# @param user_name Username used for authentication in the SMTP server.
# @param user_password Password used for authentication in the SMTP server.
send_balance_email <- function (sender, recipients, msg_subject, msg_body, msg_encoding, smtp_server, smtp_port, user_name, user_password) {
	send.mail (
		from = sender,
		to = recipients,
		subject = msg_subject,
		body = msg_body,
		encoding = msg_encoding,
		html = TRUE,
		inline = FALSE,
		smtp = list (
			host.name = smtp_server,
			port = smtp_port,
			ssl = TRUE,
			user.name = user_name,
			passwd = user_password
		),
		authenticate = TRUE,
		send = TRUE,
		attach.files = NULL,
		debug = FALSE
	)
}


# Sends and email with the current balance in the message body.
send_current_balance_email <- function ( ) {
	send_balance_email (
		sender,
		recipients,
		msg_subject,
		msg_body,
		msg_encoding,
		smtp_server,
		smtp_port,
		user_name,
		user_password
	)
}
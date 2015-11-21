#' Authenticate Operator
#'
#' Retrieves your SORACOM's API key.
#'
#' @param email
#'    Your email address.
#' @param password
#'    Your password.
#' @param timeout
#'    The timeout duration of the request token. See details.
#' @return
#'    Your API token.
#'
#' @details
#' When \code{timeout} is given in numeric format, it is interpret as integer.
#' If another format is given, the function tries to convert it into seconds.
#' For example, if the value is "2 hours" in character, it will be 7200 (secs).
#'
#' @export
get_token <- function(email, password, timeout) {
   body <- list(email = email, password = password)
   if (!missing(timeout)) {
      body <- c(body, tokenTimeoutSeconds = duration(timeout))
   }

   response <- POST(get_endpoint("/auth"), body = body, encode = "json")
   status_code <- status_code(response)
   content <- content(response, "text", encoding = "UTF-8")

   switch(
      as.character(status_code),
      "200" = {
         from_content(content, c("soracom_token", "soracom_operator"), force_data_frame = FALSE)
      },
      "401" = {
         stop("Wrong email or password.")
      },
      {
         stop(content)
      }
   )
}

#' Print
#'
#' Prints SORACOM API token.
#'
#' @param x
#'    The token.
#' @param ...
#'    Not used.
#'
#' @rdname print
#' @export
print.soracom_token <- function(x, ...) {
   cat("SORACOM API token for", sQuote(x$operatorId), fill = TRUE)
}

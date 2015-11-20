#' Password Reset
#'
#' Password reset operations.
#'
#' @details
#' If \code{issue_password_reset_token} succeed,
#' then an email is send to the requested email address.
#' The mail cotains a token to be used in \code{reset_password}.
#'
#' @param email
#'    Email address to send a password reset token.
#' @param token
#'    Token sent via the reset mail.
#' @param password
#'    New password. See details.
#'
#' @details
#' \code{password} must satisfy some criteria.
#' See https://console.soracom.io/#/signup for details.
#'
#' @rdname password_reset
#' @export
issue_password_reset_token <- function(email) {
   body <- list("email" = email)

   response <- POST(get_endpoint("/auth/password_reset_token/issue"), body = body, encode = "json")
   status_code <- status_code(response)
   content <- content(response, "text", encoding = "UTF-8")

   switch(
      as.character(status_code),
      "200" = {
         message("Sent a mail to ", sQuote(email), ".")
         invisible()
      },
      "400" = {
         stop("Invalid email address.")
      },
      {
         stop(content)
      }
   )
}

#' @rdname password_reset
#' @export
reset_password <- function(token, password) {
   body <- list("password" = password, "token" = token)

   response <- POST(get_endpoint("/auth/password_reset_token/verify"), body = body, encode = "json")
   status_code <- status_code(response)
   content <- content(response, "text", encoding = "UTF-8")

   switch(
      as.character(status_code),
      "200" = {
         message("Password is successfully reset.")
         invisible()
      },
      "400" = {
         stop("Invalid token or password.")
      },
      "404" = {
         stop("Token timeout.")
      },
      {
         stop(content)
      }
   )
}

#' Operator Registration
#'
#' Creates a new operator.
#'
#' @param email
#'    Email address.
#' @param password
#'    Password. See details.
#' @param registration_token
#'    API token sent via a verification email.
#'
#' @details
#' \code{password} must satisfy some criteria.
#' See https://console.soracom.io/#/signup for details.
#'
#' @rdname operators_registration
#' @export
create_operator <- function(email, password) {
   body <- list("email" = email, "password" = password)

   ## authorization is not required
   response <- POST(get_endpoint("/operators"), body = body, encode = "json")
   status_code <- status_code(response)
   content <- content(response, "text", encoding = "UTF-8")

   switch(
      as.character(status_code),
      "201" = {
         message("Operator is successfully created. Sent a verification mail to ", sQuote(email), ".")
         invisible()
      },
      "400" = {
         stop("Invalid email address or password.")
      },
      {
         stop(content)
      }
   )
}

#' @rdname operators_registration
#' @export
verify_operator <- function(registration_token) {
   body <- list("token" = registration_token)

   ## authorization is not required
   response <- POST(get_endpoint("/operators/verify"), body = body, encode = "json")
   status_code <- status_code(response)
   content <- content(response, "text", encoding = "UTF-8")

   switch(
      as.character(status_code),
      "200" = {
         message("Operator is successfully registered.")
         invisible()
      },
      "400" = {
         stop("Invalid token.")
      },
      "404" = {
         stop("Token timeout.")
      },
      {
         stop(content)
      }
   )
}

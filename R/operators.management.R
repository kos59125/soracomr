#' Manage Operator
#'
#' Manages operators.
#'
#' @param token
#'    Your API token.
#' @param timeout
#'    The timeout duration of the request token. See details.
#' @param current
#'    Current password.
#' @param new
#'    New password.
#'
#' @details
#' \code{new} must satisfy some criteria.
#' See https://console.soracom.io/#/signup for details.
#'
#' When \code{timeout} is given in numeric format, it is interpret as integer.
#' If another format is given, the function tries to convert it into seconds.
#' For example, if the value is "2 hours" in character, it will be 7200 (secs).
#'
#' @rdname operators_management
#' @export
get_new_token <- function(token, timeout) {
   path <- sprintf("/operators/%s/token", get_segment(token))

   body <- list()
   if (!missing(timeout)) {
      body <- c(body, "tokenTimeoutSeconds" = duration(timeout))
   }

   response <- POST(get_endpoint(path), add_headers(.headers = to_headers(token)), body = body, encode = "json")
   status_code <- status_code(response)
   content <- content(response, "text", encoding = "UTF-8")

   switch(
      as.character(status_code),
      "200" = {
         new_token <- from_content(content, force_data_frame = FALSE)
         modifyList(token, new_token)
      },
      "400" = {
         stop("Invalid request or token timeout.")
      },
      {
         stop(content)
      }
   )
}

#' @rdname operators_management
#' @export
change_password <- function(token, current, new) {
   path <- sprintf("/operators/%s/password", get_segment(token))

   body <- list("currentPassword" = current, "newPassword" = new)

   response <- POST(get_endpoint(path), add_headers(.headers = to_headers(token)), body = body, encode = "json")
   status_code <- status_code(response)
   content <- content(response, "text", encoding = "UTF-8")

   switch(
      as.character(status_code),
      "200" = {
         message("Password updated.")
         invisible()
      },
      "400" = {
         stop("Invalid password.")
      },
      {
         stop(content)
      }
   )
}

#' @rdname operators_management
#' @export
get_support_token <- function(token) {
   path <- sprintf("/operators/%s/support/token", get_segment(token))

   response <- POST(get_endpoint(path), add_headers(.headers = to_headers(token)))
   status_code <- status_code(response)
   content <- content(response, "text", encoding = "UTF-8")

   switch(
      as.character(status_code),
      "200" = {
         support_token <- from_content(content, force_data_frame = FALSE)
         support_token$token
      },
      "400" = {
         stop("Invalid operator ID.")
      },
      "403" = {
         stop("Invalid request or token timeout.")
      },
      {
         stop(content)
      }
   )
}

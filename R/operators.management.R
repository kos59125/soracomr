#' Manage Operator
#'
#' Manages operators.
#'
#' @param token
#'    Your API token.
#' @param timeout
#'    The timeout seconds of the request token.
#'
#' @details
#' \code{password} must satisfy some criteria.
#' See https://console.soracom.io/#/signup for details.
#'
#' @rdname operators_management
#' @export
get_new_token <- function(token, timeout) {
   path <- sprintf("/operators/%s/token", get_segment(token))

   body <- list()
   if (!missing(timeout)) {
      body <- c(body, "timeout_seconds" = timeout)
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

#' Get User Data
#'
#' Get user data combined to the current subscriber.
#'
#' @param ...
#'    Handles response.
#'    See \code{httr::content} for details.
#'
#' @export
get_user_data <- function(...) {
   response <- GET(get_metadata_endpoint("/userdata"))
   status_code <- status_code(response)

   switch(
      as.character(status_code),
      "200" = {
         content(response, ...)
      },
      {
         message <- content(response, "text", encoding = "UTF-8")
         stop(message)
      }
   )
}

#' Get Operator
#'
#' Gets an operator.
#'
#' @param token
#'    Your API token.
#'
#' @rdname operators
#' @export
get_operator <- function(token) {
   path <- sprintf("/operators/%s", get_segment(token))

   response <- GET(get_endpoint(path), add_headers(.headers = to_headers(token)))
   status_code <- status_code(response)
   content <- content(response, "text", encoding = "UTF-8")

   switch(
      as.character(status_code),
      "200" = {
         from_content(content, "soracom_operator")
      },
      "400" = {
         stop("Invalid operator ID.")
      },
      {
         stop(content)
      }
   )
}

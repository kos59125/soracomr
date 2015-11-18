#' Subscribers Operation
#'
#' Gets/Lists ubscribers.
#'
#' @param token
#'    Your API token.
#' @param imsi
#'    Subscriber's IMSI.
#'
#' @rdname subscribers
#'
#' @export
list_subscribers <- function(token) {
   # TODO build a query.

   response <- GET(get_endpoint("/subscribers"), add_headers(.headers = to_headers(token)))
   status_code <- status_code(response)
   content <- content(response, "text", encoding = "UTF-8")

   switch(
      as.character(status_code),
      "200" = {
         from_content(content, "soracom_subscriber")
      },
      {
         stop(content)
      }
   )
}

#' @rdname subscribers
#' @export
get_subscriber <- function(token, imsi) {
   path <- sprintf("/subscribers/%s", get_segment(imsi))

   response <- GET(get_endpoint(path), add_headers(.headers = to_headers(token)))
   status_code <- status_code(response)
   content <- content(response, "text", encoding = "UTF-8")

   switch(
      as.character(status_code),
      "200" = {
         from_content(content, "soracom_subscriber")
      },
      "404" = {
         stop("Subscriber ", sQuote(imsi), " was not found.")
      },
      {
         stop(content)
      }
   )
}


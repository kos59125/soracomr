#' Event Handlers
#'
#' Get event handlers.
#'
#' @param token
#'    Your API token.
#' @param imsi
#'    Subscriber's IMSI.
#' @param filter
#'    Filter condition. If both \code{imsi} and \code{filter} are present, \code{filter} is ignored.
#'
#' @seealso query_filter_event_handler
#'
#' @rdname event_handlers
#' @export
list_event_handlers <- function(token, imsi, filter) {
   query <- list()
   if (!missing(filter)) {
      query <- c(query, filter)
   }

   path <- "/event_handlers"
   if (!missing(imsi)) {
      path <- sprintf("%s/subscribers/%s", path, get_segment(imsi))
   }

   response <- GET(get_endpoint(path), add_headers(.headers = to_headers(token)), query = query)
   status_code <- status_code(response)
   content <- content(response, "text", encoding = "UTF-8")

   switch(
      as.character(status_code),
      "200" = {
         from_content(content, "soracom_event_handler", force_data_frame = FALSE)
      },
      {
         stop(content)
      }
   )
}

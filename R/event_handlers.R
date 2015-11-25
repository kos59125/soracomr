#' Event Handlers
#'
#' Get event handlers.
#'
#' @param token
#'    Your API token.
#' @param handler_id
#'    Handler ID.
#' @param handler
#'    Event handler object.
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

#' @rdname event_handlers
#' @export
get_event_handler <- function(token, handler_id) {
   path <- sprintf("/event_handlers/%s", get_segment(handler_id))

   response <- GET(get_endpoint(path), add_headers(.headers = to_headers(token)))
   status_code <- status_code(response)
   content <- content(response, "text", encoding = "UTF-8")

   switch(
      as.character(status_code),
      "200" = {
         ## this is a hack to enforce the result of jsonlite::fromJSON as a data frame
         content <- sprintf("[%s]", content)
         from_content(content, "soracom_event_handler", force_data_frame = FALSE)
      },
      "404" = {
         stop("Event handler ", sQuote(handler_id), " was not found.")
      },
      {
         stop(content)
      }
   )
}

#' @rdname event_handlers
#' @export
create_event_handler <- function(token, handler) {
   response <- POST(get_endpoint("/event_handlers"), add_headers(.headers = to_headers(token)), body = reshape_for_post(handler), encode = "json")
   status_code <- status_code(response)
   content <- content(response, "text", encoding = "UTF-8")

   switch(
      as.character(status_code),
      "201" = {
         ## documentation says this API returns an object, actually not.
         invisible()
      },
      {
         stop(content)
      }
   )
}

#' @rdname event_handlers
#' @export
put_event_handler <- function(token, handler) {
   path <- sprintf("/event_handlers/%s", get_segment(handler))

   response <- PUT(get_endpoint(path), add_headers(.headers = to_headers(token)), body = reshape_for_post(handler), encode = "json")
   status_code <- status_code(response)
   content <- content(response, "text", encoding = "UTF-8")

   switch(
      as.character(status_code),
      "200" = {
         invisible()
      },
      "400" = {
         stop("Invalid handler ID.")
      },
      {
         stop(content)
      }
   )
}

#' @rdname event_handlers
#' @export
delete_event_handler <- function(token, handler_id) {
   path <- sprintf("/event_handlers/%s", get_segment(handler_id))

   response <- DELETE(get_endpoint(path), add_headers(.headers = to_headers(token)))
   status_code <- status_code(response)
   content <- content(response, "text", encoding = "UTF-8")

   switch(
      as.character(status_code),
      "200" = {  # as-is, I prefer 204
         invisible()
      },
      "404" = {
         warning("Event handler ", sQuote(handler_id), " was not found.")
      },
      {
         stop(content)
      }
   )
}

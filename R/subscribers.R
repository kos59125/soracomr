#' Subscribers Operation
#'
#' Gets/Lists ubscribers.
#'
#' @param token
#'    Your API token.
#' @param imsi
#'    Subscriber's IMSI.
#'    It is mandatory when \code{token} is given.
#' @param group_id
#'    Group's ID.
#' @param limit
#'    Maximum number of response records.
#' @param last_evaluated_key
#'    The last subscriber's IMSI obtained last time.
#'    If given, the API would return subscribers from its next.
#' @param filter
#'    Filter condition. If both \code{group_id} and \code{filter} are present, \code{filter} is ignored.
#'
#' @seealso query_filter_subscriber
#'
#' @rdname subscribers
#' @export
list_subscribers <- function(token, group_id, limit, last_evaluated_key, filter) {
   query <- list()
   if (!missing(limit)) {
      query <- c(query, "limit" = limit)
   }
   if (!missing(last_evaluated_key)) {
      query <- c(query, "last_evaluated_key" = last_evaluated_key)
   }
   if (!missing(filter)) {
      query <- c(query, filter)
   }

   path <- "/subscribers"
   if (!missing(group_id)) {
      path <- sprintf("/groups/%s%s", get_segment(group_id), path)
   }

   # /groups/{group_id}/subscribers doesn't accept filter parameters.
   if (!missing(filter) && !missing(group_id)) {
      warning("Both group_id and filter are given. filter is ignored.")
   }

   response <- GET(get_endpoint(path), add_headers(.headers = to_headers(token)), query = query)
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
   url <- if (missing(token)) {
      if (!missing(imsi)) {
         warning("imsi is ignored since token is missing.")
      }
      token <- NULL
      get_metadata_endpoint("/subscriber")
   } else {
      path <- sprintf("/subscribers/%s", get_segment(imsi))
      get_endpoint(path)
   }
   response <- GET(url, add_headers(.headers = to_headers(token)))
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

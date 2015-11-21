#' Groups Operation
#'
#' Gets, lists or creates groups operation.
#'
#' @param token
#'    Your API token.
#' @param limit
#'    Maximum number of response records.
#' @param last_evaluated_key
#'    The last subscriber's IMSI obtained last time.
#'    If given, the API would return subscribers from its next.
#' @param group_id
#'    The group id.
#' @param group_name
#'    The group name. For display on the user console.
#'
#' @rdname groups
#'
#' @export
list_groups <- function(token, limit, last_evaluated_key) {
   query <- list()
   if (!missing(limit)) {
      query <- c(query, "limit" = limit)
   }
   if (!missing(last_evaluated_key)) {
      query <- c(query, "last_evaluated_key" = last_evaluated_key)
   }

   response <- GET(get_endpoint("/groups"), add_headers(.headers = to_headers(token)), query = query)
   status_code <- status_code(response)
   content <- content(response, "text", encoding = "UTF-8")

   switch(
      as.character(status_code),
      "200" = {
         from_content(content, "soracom_group")
      },
      {
         stop(content)
      }
   )
}

#' @rdname groups
#' @export
get_group <- function(token, group_id) {
   path <- sprintf("/groups/%s", get_segment(group_id))

   response <- GET(get_endpoint(path), add_headers(.headers = to_headers(token)))
   status_code <- status_code(response)
   content <- content(response, "text", encoding = "UTF-8")

   switch(
      as.character(status_code),
      "200" = {
         from_content(content, "soracom_group")
      },
      "404" = {
         stop("Group ", sQuote(group_id), " was not found.")
      },
      {
         stop(content)
      }
   )
}

#' @rdname groups
#' @export
create_group <- function(token, group_name) {
   tags <- if (missing(group_name)) {
      list()
   } else {
      structure(list(as.character(group_name)), names = "name")
   }

   response <- POST(get_endpoint("/groups"), add_headers(.headers = to_headers(token)), body = list("tags" = tags), encode = "json")
   status_code <- status_code(response)
   content <- content(response, "text", encoding = "UTF-8")

   switch(
      as.character(status_code),
      "201" = {
         from_content(content, "soracom_group")
      },
      {
         stop(content)
      }
   )
}

#' @rdname groups
#' @export
delete_group <- function(token, group_id) {
   path <- sprintf("/groups/%s", get_segment(group_id))

   response <- DELETE(get_endpoint(path), add_headers(.headers = to_headers(token)))
   status_code <- status_code(response)
   content <- content(response, "text", encoding = "UTF-8")

   switch(
      as.character(status_code),
      "204" = {
         invisible()
      },
      "404" = {
         warning("Group ", sQuote(group_id), " was not found.")
      },
      {
         stop(content)
      }
   )
}

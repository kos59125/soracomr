#' Groups Operation
#'
#' Gets, lists or creates groups operation.
#'
#' @param token
#'    Your API token.
#' @param group_id
#'    The group id.
#' @param group_name
#'    The group name. For display on the user console.
#'
#' @rdname groups
#'
#' @export
list_groups <- function(token) {
   # TODO build a query.

   response <- GET(get_endpoint("/groups"), add_headers(.headers = to_headers(token)))
   status_code <- status_code(response)
   content <- content(response, "text", encoding = "UTF-8")

   switch(
      as.character(status_code),
      "200" = {
         fromJSON(content)
      },
      {
         stop(content)
      }
   )
}

#' @rdname groups
#' @export
get_group <- function(token, group_id) {
   path <- sprintf("/groups/%s", group_id)

   response <- GET(get_endpoint(path), add_headers(.headers = to_headers(token)))
   status_code <- status_code(response)
   content <- content(response, "text", encoding = "UTF-8")

   switch(
      as.character(status_code),
      "200" = {
         fromJSON(content)
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
get_group <- function(token, group_id) {
   path <- sprintf("/groups/%s", group_id)

   response <- GET(get_endpoint(path), add_headers(.headers = to_headers(token)))
   status_code <- status_code(response)
   content <- content(response, "text", encoding = "UTF-8")

   switch(
      as.character(status_code),
      "200" = {
         fromJSON(content)
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
      structure(list(group_name), names = "name")
   } else {
      list()
   }

   response <- POST(get_endpoint("/groups"), add_headers(.headers = to_headers(token)), body = list("tags" = tags), encode = "json", verbose())
   status_code <- status_code(response)
   content <- content(response, "text", encoding = "UTF-8")

   switch(
      as.character(status_code),
      "201" = {
         fromJSON(content)
      },
      {
         stop(content)
      }
   )
}

#' @rdname groups
#' @export
delete_group <- function(token, group_id) {
   path <- sprintf("/groups/%s", group_id)

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

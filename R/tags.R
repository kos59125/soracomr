#' Tags
#'
#' @param token
#'    Your API token.
#' @param imsi
#'    Subscriber's IMSI.
#' @param group_id
#'    Group ID.
#' @param tags
#'    Tags to set. See details for valid input format.
#' @param tag_name
#'    Tag name to delete.
#' @param name
#'    Name for subscribers or groups.
#' @param x
#'    Something that may have one or more tags.
#'
#' @details
#' \code{tags} should be one of a character vector with names, a list with names, or a data frame with column tagName and tagValue.
#'
#' @rdname tags
#' @export
add_subscribers_tags <- function(token, imsi, tags) {
   path <- sprintf("/subscribers/%s/tags", get_segment(imsi))

   response <- PUT(get_endpoint(path), add_headers(.headers = to_headers(token)), body = build_tags(tags), encode = "json")
   status_code <- status_code(response)
   content <- content(response, "text", encoding = "UTF-8")

   switch(
      as.character(status_code),
      "200" = {
         invisible(from_content(content, "soracom_subscriber"))
      },
      "404" = {
         stop("Subscriber ", sQuote(imsi), " was not found.")
      },
      {
         stop(content)
      }
   )
}

#' @rdname tags
#' @export
delete_subscribers_tag <- function(token, imsi, tag_name) {
   path <- sprintf("/subscribers/%s/tags/%s", get_segment(imsi), tag_name)

   response <- DELETE(get_endpoint(path), add_headers(.headers = to_headers(token)))
   status_code <- status_code(response)

   switch(
      as.character(status_code),
      "204" = {
         invisible()
      },
      "404" = {
         warning("Subscriber ", sQuote(imsi), " did not have a tag ", sQuote(tag_name), ".")
      },
      {
         stop(content)
      }
   )
}

#' @rdname tags
#' @export
add_groups_tags <- function(token, group_id, tags) {
   path <- sprintf("/groups/%s/tags", get_segment(group_id))

   response <- PUT(get_endpoint(path), add_headers(.headers = to_headers(token)), body = build_tags(tags), encode = "json")
   status_code <- status_code(response)
   content <- content(response, "text", encoding = "UTF-8")

   switch(
      as.character(status_code),
      "200" = {
         invisible(from_content(content, "soracom_group"))
      },
      "404" = {
         stop("Group ", sQuote(group_id), " was not found.")
      },
      {
         stop(content)
      }
   )
}

#' @rdname tags
#' @export
delete_groups_tag <- function(token, group_id, tag_name) {
   path <- sprintf("/groups/%s/tags/%s", get_segment(group_id), tag_name)

   response <- DELETE(get_endpoint(path), add_headers(.headers = to_headers(token)))
   status_code <- status_code(response)

   switch(
      as.character(status_code),
      "204" = {
         invisible()
      },
      "404" = {
         warning("Group ", sQuote(group_id), " did not have a tag ", sQuote(tag_name), ".")
      },
      {
         stop(content)
      }
   )
}

#' @rdname tags
#' @export
set_subscribers_name <- function(token, imsi, name) {
   if (is.null(name) || is.na(name)) {
      delete_subscribers_tag("name")
   } else {
      add_subscribers_tags(token, imsi, c("name" = name))
   }
}

#' @rdname tags
#' @export
set_groups_name <- function(token, group_id, name) {
   if (is.null(name) || is.na(name)) {
      delete_groups_tag("name")
   } else {
      add_groups_tags(token, group_id, c("name" = name))
   }
}

#' @rdname tags
#' @export
tags <- function(x) {
   UseMethod("tags")
}

#' @export
tags.soracom_subscriber <- function(x) {
   tags_(x)
}

#' @export
tags.soracom_group <- function(x) {
   tags_(x)
}

tags_ <- function(x) {
   extract_property(x, "tags", "soracom_tags")
}

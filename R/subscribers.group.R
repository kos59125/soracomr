#' Subscriber's Group
#'
#' @param token
#'    Your API token.
#' @param imsi
#'    Subscriber's IMSI.
#' @param group_id
#'    The group name the subscriber to belong to.
#'
#' @rdname subscribers_group
#' @export
attach_group <- function(token, imsi, group_id) {
   path <- sprintf("/subscribers/%s/set_group", get_segment(imsi))
   body <- list("groupId" = group_id)

   response <- POST(get_endpoint(path), add_headers(.headers = to_headers(token)), body = body, encode = "json")
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

#' @rdname subscribers_group
#' @export
detach_group <- function(token, imsi) {
   path <- sprintf("/subscribers/%s/unset_group", get_segment(imsi))

   response <- POST(get_endpoint(path), add_headers(.headers = to_headers(token)))
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

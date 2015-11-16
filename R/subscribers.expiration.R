#' Expiration
#'
#' Sets or Unsets the expiry time of subscribers.
#'
#' @param token
#'    Your API token.
#' @param imsi
#'    Subscriber's IMSI.
#' @param expiration
#'    The expiry time.
#'
#' @rdname subscribers_expiration
#' @export
set_expiry_time <- function(token, imsi, expiration) {
   path <- sprintf("/subscribers/%s/set_expiry_time", imsi)
   body <- list(
      "expiryTime" = get_unixtime(expiration, type = "milliseconds")
   )

   response <- POST(get_endpoint(path), add_headers(.headers = to_headers(token)), body = body, encode = "json")
   status_code <- status_code(response)
   content <- content(response, "text", encoding = "UTF-8")

   switch(
      as.character(status_code),
      "200" = {
         fromJSON(content)
      },
      "404" = {
         stop("Subscriber ", sQuote(imsi), " was not found.")
      },
      {
         stop(content)
      }
   )
}

#' @rdname subscribers_expiration
#' @export
unset_expiry_time <- function(token, imsi) {
   path <- sprintf("/subscribers/%s/unset_expiry_time", imsi)

   response <- POST(get_endpoint(path), add_headers(.headers = to_headers(token)))
   status_code <- status_code(response)
   content <- content(response, "text", encoding = "UTF-8")

   switch(
      as.character(status_code),
      "200" = {
         fromJSON(content)
      },
      "404" = {
         stop("Subscriber ", sQuote(imsi), " was not found.")
      },
      {
         stop(content)
      }
   )
}

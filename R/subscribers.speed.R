#' Speed
#'
#' Sets or Unsets the speed class of subscribers.
#'
#' @param token
#'    Your API token.
#' @param imsi
#'    Subscriber's IMSI.
#' @param class
#'    The speed class.
#'
#' @rdname subscribers_speed
#' @export
update_speed_class <- function(token, imsi, class) {
   path <- sprintf("/subscribers/%s/update_speed_class", get_segment(imsi))
   body <- list("speedClass" = as.character(class))

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

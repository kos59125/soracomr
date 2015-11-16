#' Statistics
#'
#' Get report for subscribers or operators.
#'
#' @param token
#'    Your API token.
#' @param imsi
#'    Subscriber's IMSI.
#' @param service
#'    SORACOM service.
#' @param period
#'    Period for the output.
#'    For get_stats, one of \code{"month"}, \code{"day"}, or \code{"minutes"} is to be chosen.
#'    For export_stats, only \code{"month"} is supported now.
#' @param from
#'    Initial time of the output.
#' @param to
#'    Last time of the output.
#' @param download_file
#'    Whether the exporeted file will be downloaded or not.
#'
#' @rdname statictics
#' @export
get_stats <- function(token, imsi, service = c("air", "beam"), period = c("month", "day", "minutes"), from = 0, to = Sys.time()) {
   service <- match.arg(service)
   period <- match.arg(period)

   path <- sprintf("/stats/%s/subscribers/%s", service, imsi)
   query <- list(
      # Make unixtimes into characters to prevent from formatting into exponent notation.
      "from" = as.character(get_unixtime(from, type = "seconds")),
      "to" = as.character(get_unixtime(to, type = "seconds")),
      "period" = period
   )

   response <- GET(get_endpoint(path), add_headers(.headers = to_headers(token)), query = query)
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

#' @rdname statictics
#' @export
export_stats <- function(token, service = c("air", "beam"), period = c("month"), from = 0, to = Sys.time(), download_file = TRUE) {
   service <- match.arg(service)
   period <- match.arg(period)

   path <- sprintf("/stats/%s/operators/%s/export", service, token$operatorId)
   body <- list(
      "from" = as.integer(as.POSIXct(from, origin = "1970-01-01", tz = "UTC")),
      "to" = as.integer(as.POSIXct(to, origin = "1970-01-01", tz = "UTC")),
      "period" = period
   )

   response <- POST(get_endpoint(path), add_headers(.headers = to_headers(token)), body = body, verbose(), encode = "json")
   status_code <- status_code(response)
   content <- content(response, "text", encoding = "UTF-8")

   switch(
      as.character(status_code),
      "200" = {
         result <- fromJSON(content)
         if (isTRUE(download_file)) {
            url <- result$url
            tryCatch(
               {
                  structure(readr::read_csv(url), "return" = result)
               },
               error = function(e) {
                  warning("Export succeeded, but download failed: ", e)
                  result
               }
            )
         } else {
            result
         }
      },
      {
         stop(content)
      }
   )
}

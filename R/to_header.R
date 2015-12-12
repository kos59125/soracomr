#' HTTP Request Headers
#'
#' Constructs additional headers from the specified token.
#'
#' @param token
#'    API Token.
#' @param language
#'    Two-letter language.
to_headers <- function(token = list(), language = getOption("soracomr.language")) {
   c(
      "X-Soracom-API-Key" = token$apiKey,
      "X-Soracom-Token" = token$token,
      "X-Soracom-Lang" = language
   )
}

#' HTTP Request Headers
#'
#' Constructs additional headers from the specified token.
#'
#' @param token
#'    API Token.
#' @param lang
#'    Two-letter language.
to_headers <- function(token, language = getOption("soracomr.language")) {
   c(
      "X-Soracom-API-Key" = token$apiKey,
      "X-Soracom-Token" = token$token,
      "X-Soracom-Lang" = language
   )
}

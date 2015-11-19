#' Character Vectors
#'
#' @rdname as.character
#' @export
as.character.soracom_operator <- function(x) {
   x$operatorId
}

#' @rdname as.character
#' @export
as.character.soracom_subscriber <- function(x) {
   x$imsi
}

#' @rdname as.character
#' @export
as.character.soracom_group <- function(x) {
   x$groupId
}
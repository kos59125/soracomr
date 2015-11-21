#' Character Vectors
#'
#' @param x
#'    Response by SORACOM API.
#' @param ...
#'    Not used.
#'
#' @rdname as.character
#' @export
as.character.soracom_operator <- function(x, ...) {
   x$operatorId
}

#' @rdname as.character
#' @export
as.character.soracom_subscriber <- function(x, ...) {
   x$imsi
}

#' @rdname as.character
#' @export
as.character.soracom_group <- function(x, ...) {
   x$groupId
}

#' @rdname as.character
#' @export
as.character.soracom_event_handler <- function(x, ...) {
   x$handlerId
}

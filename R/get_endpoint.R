get_endpoint <- function(path) {
   sprintf("https://api.soracom.io/v1%s", path)
}

get_segment <- function(x) {
   UseMethod("get_segment")
}

get_segment.default <- function(x) {
   x
}

get_segment.soracom_operator <- function(x) {
   x$operatorId
}

get_segment.soracom_subscriber <- function(x) {
   x$imsi
}

get_segment.soracom_group <- function(x) {
   x$groupId
}

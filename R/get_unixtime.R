get_unixtime <- function(x, type = c("seconds", "milliseconds")) {
   type <- match.arg(type)
   scale <- switch(type, "seconds" = 1, "milliseconds" = 1000)
   unixtime <- as.POSIXct(x, origin = "1970-01-01", tz = "UTC")
   bit64::as.integer64(as.numeric(unixtime) * scale)
}

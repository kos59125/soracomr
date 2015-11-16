get_unixtime <- function(x, type = c("seconds", "milliseconds")) {
   type <- match.arg(type)
   scale <- switch(type, "seconds" = 1, "milliseconds" = 1000)
   as.numeric(as.POSIXct(x, origin = "1970-01-01", tz = "UTC")) * scale
}

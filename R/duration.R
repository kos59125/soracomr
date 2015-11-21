duration <- function(x) {
   UseMethod("duration")
}

duration.numeric <- function(x) {
   as.integer(x)
}

duration.character <- function(x) {
   from <- as.POSIXct("1970-01-01", tz = "UTC")  # arbitrary POSIXct value is appropriate
   range <- seq(from, by = x, length.out = 2)
   as.integer(difftime(range[2], range[1], units = "secs"))
}

duration.difftime <- function(x) {
   units(x) <- "secs"
   as.integer(x)
}

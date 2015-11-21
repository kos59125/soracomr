#' Query Filter
#'
#' Constructs a query list for request filtering.
#'
#' @param tag_name
#'    Tag name for filtering.
#' @param tag_value
#'    Tag value for filtering.
#' @param tag_value_match_mode
#'    Tag match mode.
#'
#' @export
query_filter <- function(tag_name, tag_value, tag_value_match_mode = c("exact", "prefix"), status_filter, speed_class_filter) {
   query <- list()

   # tag filter
   if (!missing(tag_name)) {
      if (missing(tag_value)) {
         stop("tag_value is required when tag_name is given.")
      }
      query <- c(
         query,
         "tag_name" = tag_name,
         "tag_value" = tag_value,
         "tag_value_match_mode" = match.arg(tag_value_match_mode)
      )
   }

   # status filter
   if (!missing(speed_class_filter)) {
      if (!all(speed_class_filter %in% c("s1.minimum", "s1.slow", "s1.standard", "s1.fast"))) {
         stop("Invalid speed class.")
      }
      speed_class_filter <- paste(unique(speed_class_filter), collapse = "|")
      query <- c(query, "speed_class_filter" = speed_class_filter)
   }

   # speed class filter
   if (!missing(status_filter)) {
      if (!all(status_filter %in% c("active", "inactive", "ready", "instock", "shipped", "suspended", "terminated"))) {
         stop("Invalid status.")
      }
      status_filter <- paste(unique(status_filter), collapse = "|")
      query <- c(query, "status_filter" = status_filter)
   }

   query
}

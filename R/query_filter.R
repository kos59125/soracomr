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
#' @param status_filter
#'    Status filter.
#' @param speed_class_filter
#'    Speed class filter.
#' @param target
#'    A value indicating to which event handlers to get is bound to.
#'
#' @details
#' \code{tag_value_match_mode} must be either \code{"exact"} or \code{"prefix"}.
#'
#' \code{status_filter} must be a character vector that contains one or more in \code{"s1.minimum"}, \code{"s1.slow"}, \code{"s1.standard"}, or \code{"s1.fast"}
#'
#' \code{speed_class_filter} must be a character vector that contains one or more in \code{"active"}, \code{"inactive"}, \code{"ready"}, \code{"instock"}, \code{"shipped"}, \code{"suspended"}, or \code{"terminated"}.
#'
#' @rdname query_filter
#' @export
query_filter_subscriber <- function(tag_name, tag_value, tag_value_match_mode = c("exact", "prefix"), status_filter, speed_class_filter) {
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

#' @rdname query_filter
#' @export
query_filter_group <- function(tag_name, tag_value, tag_value_match_mode = c("exact", "prefix")) {
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

   query
}

#' @rdname query_filter
#' @export
query_filter_event_handler <- function(target = c("operator", "group")) {
   query <- list()

   # target filter
   if (!missing(target)) {
      target <- match.arg(target)
      query <- c(query, list("target" = target))
   }

   query
}

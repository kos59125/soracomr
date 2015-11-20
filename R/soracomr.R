#' **EXPERIMENTAL** Management of SORACOM SIM cards
#'
#' soracomr helps you to access management api of SORACOM.
#'
#' @name soracomr
#' @docType package
NULL

#' Table of Rates
#'
#' @format
#' \itemize{
#' \item{speed_class} Speed class.
#' \item{time} Time.
#' \item{upload_rate} Charge for uploads (JPY/MiB).
#' \item{download_rate} Charge for downloads (JPY/MiB).
#' }
#'
#' @docType data
#' @export
rates_table <- data.frame(
   speed_class = factor(
      rep(c("s1.minimum", "s1.slow", "s1.standard", "s1.fast"), 2),
      levels = c("s1.minimum", "s1.slow", "s1.standard", "s1.fast"),
      ordered = TRUE
   ),
   time = factor(
      rep(c("daytime", "nighttime"), each = 4)
   ),
   upload_rate = c(0.2, 0.22, 0.24, 0.3, rep(0.2, 4)),
   download_rate = c(0.6, 0.7, 0.8, 1, rep(0.2, 4)),
   stringsAsFactors = FALSE
)

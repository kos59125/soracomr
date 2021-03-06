% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/stats.R
\name{get_stats}
\alias{export_stats}
\alias{get_stats}
\title{Statistics}
\usage{
get_stats(token, imsi, service = c("air", "beam"), period = c("month",
  "day", "minutes"), from = 0, to = Sys.time())

export_stats(token, service = c("air", "beam"), period = c("month"),
  from = 0, to = Sys.time(), download_file = TRUE)
}
\arguments{
\item{token}{Your API token.}

\item{imsi}{Subscriber's IMSI.}

\item{service}{SORACOM service.}

\item{period}{Period for the output.
For get_stats, one of \code{"month"}, \code{"day"}, or \code{"minutes"} is to be chosen.
For export_stats, only \code{"month"} is supported now.}

\item{from}{Initial time of the output.}

\item{to}{Last time of the output.}

\item{download_file}{Whether the exporeted file will be downloaded or not.}
}
\description{
Get report for subscribers or operators.
}


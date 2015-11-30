[![Build Status](https://travis-ci.org/kos59125/soracomr.svg)](https://travis-ci.org/kos59125/soracomr)

soracomr
========

Touches SORACOM API (v1) from R.

Installation
------------

```r
# install.packages("devtools")
devtools::install_github("kos59125/soracomr")
```

Examples
--------

```r
library(soracomr)

# Get a 30-minute-lifetime token for API use
token <- get_token("Your Email Address", "Your Password", timeout = "30 mins")
# Get subscribers
subscribers <- list_subscribers(token)
print(subscribers)
# Get report for the first subscriber
first_subscriber <- head(subscribers, 1)
stats <- get_stats(token, first_subscriber, period = "minutes")
print(head(stats))
```

Support API List
----------------

<iframe src="https://docs.com/d/embed/D25195341-1641-6979-3700-001893352615%7eMdd0ea073-4484-a5f1-3f39-cbf3a807aca8" frameborder="0" scrolling="no" width="752px" height="500px" style="max-width:100%" allowfullscreen="False"></iframe>


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

# Get a token for API use
token <- get_token("Your Email Address", "Your Password")
# Get subscribers
subscribers <- list_subscribers(token)
print(subscribers)
# Get report for the first subscriber
first_subscriber <- head(subscribers, 1)
stats <- get_stats(token, first_subscriber, period = "minutes")
print(head(stats))
```

TODO
----

* Implements EventHandler APIs

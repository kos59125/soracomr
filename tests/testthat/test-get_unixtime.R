context("get_unixtime")

test_that("timestamp starts at 1970-01-01 UTC", {
   time <- as.POSIXct(0, tz = "UTC", origin = "1970-01-01")
   actual <- get_unixtime(time)
   expect_equal(actual, 0)
})

test_that("type = 'seconds' returns unixtime in milliseconds", {
   time <- as.POSIXct(1, tz = "UTC", origin = "1970-01-01")
   actual <- get_unixtime(time, type = "seconds")
   expect_equal(actual, 1)
})

test_that("type = 'milliseconds' returns unixtime in milliseconds", {
   time <- as.POSIXct(1, tz = "UTC", origin = "1970-01-01")
   actual <- get_unixtime(time, type = "milliseconds")
   expect_equal(actual, 1000)
})

test_that("type = 'unknown' raises an error", {
   time <- as.POSIXct(1, tz = "UTC", origin = "1970-01-01")
   expect_error(get_unixtime(time, type = "unknown"))
})

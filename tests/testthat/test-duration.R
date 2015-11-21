context("duration")

test_that("integer returns itself", {
   expect_equal(duration(10L), 10L)
})

test_that("numer returns itself in integer type", {
   expect_equal(duration(10.5), 10L)
})

test_that("character is interpreted appropriately", {
   expect_equal(duration("2 hours"), 7200L)
})

test_that("difftime results in seconds", {
   x <- difftime(as.Date("2015-10-12"), as.Date("2015-10-10"))
   expect_equal(duration(x), 172800L)
})

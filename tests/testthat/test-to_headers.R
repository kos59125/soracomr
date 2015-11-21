context("to_headers")

test_that("Check headers", {
   token <- list("token" = "tokenstring", "apiKey" = "myapikey")
   actual <- to_headers(token)
   expect_equal(actual["X-Soracom-API-Key"], c("X-Soracom-API-Key" = "myapikey"))
   expect_equal(actual["X-Soracom-Token"], c("X-Soracom-Token" = "tokenstring"))
})

test_that("Check lang", {
   token <- list("token" = "tokenstring", "apiKey" = "myapikey")
   actual <- to_headers(token, language = "ja")
   expect_equal(actual["X-Soracom-Lang"], c("X-Soracom-Lang" = "ja"))
})

test_that("Check lang in option", {
   token <- list("token" = "tokenstring", "apiKey" = "myapikey")
   actual <- to_headers(token)
   expect_false("X-Soracom-Lang" %in% names(actual))

   options("soracomr.language" = "ja")
   actual <- to_headers(token)
   expect_equal(actual["X-Soracom-Lang"], c("X-Soracom-Lang" = "ja"))
})

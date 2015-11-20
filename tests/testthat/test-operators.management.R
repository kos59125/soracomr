context("operators management")

with_mock(
   "httr::status_code" = function(r) {
      r$status_code
   },
   "httr::content" = function(r, ...) {
      r$content
   },

   with_mock(
      "httr::POST" = function(..., body) {
         list(..., status_code = 200, content = '{"token":"newtoken"}')
      },
      test_that("get_new_token shows a message when succeeded", {
         token <- list("token" = "foo", "bar" = "baz")
         class(token) <- "soracom_token"

         new_token <- get_new_token(token)

         expect_equal(class(new_token), "soracom_token")
         expect_equal(new_token$token, "newtoken")  # updated
         expect_equal(new_token$bar, "baz")  # other attributes should not be changed
      })
   ),

   with_mock(
      "httr::POST" = function(..., body) {
         list(..., body = body, status_code = 400, content = '')
      },
      test_that("When HTTP status code is 400, get_new_token raises an error", {
         token <- list()
         expect_error(get_new_token(token), "Invalid request or token timeout.")
      })
   ),

   with_mock(
      "httr::POST" = function(..., body) {
         list(..., body = body, status_code = 999, content = 'message')
      },
      test_that("With an unknown error, get_new_token raises an error with response content", {
         token <- list()
         expect_error(get_new_token(token), "message")
      })
   ),

   with_mock(
      "httr::POST" = function(..., body) {
         expect_equal(length(body), 0)  # empty
         list(..., body = body, status_code = 200, content = '{"token":"newtoken"}')
      },
      test_that("Check query without timeout", {
         token <- list("token" = "foo")
         get_new_token(token)
      })
   ),

   with_mock(
      "httr::POST" = function(..., body) {
         expect_equal(body$timeout_seconds, 10)
         list(..., body = body, status_code = 200, content = '{"token":"newtoken"}')
      },
      test_that("Check timeout", {
         token <- list("token" = "foo")
         get_new_token(token, 10)
      })
   ),

   with_mock(
      "httr::POST" = function(..., body) {
         list(..., body = body, status_code = 200, content = '')
      },
      test_that("change_password shows a message when succeeded", {
         token <- list()
         expect_message(change_password(token, "old", "new"), "Password updated.")
      })
   ),

   with_mock(
      "httr::POST" = function(..., body) {
         list(..., body = body, status_code = 400, content = '')
      },
      test_that("When HTTP status code is 400, change_password raises an error", {
         token <- list()
         expect_error(change_password(token, "old", "new"), "Invalid password.")
      })
   ),

   with_mock(
      "httr::POST" = function(..., body) {
         list(..., body = body, status_code = 999, content = 'message')
      },
      test_that("With an unknown error, change_password raises an error with response content", {
         token <- list()
         expect_error(change_password(token, "old", "new"), "message")
      })
   ),

   with_mock(
      "httr::POST" = function(...) {
         list(..., status_code = 200, content = '{"token":"supporttoken"}')
      },
      test_that("get_support_token shows a message when succeeded", {
         token <- list()
         support_token <- get_support_token(token)

         expect_equal(support_token, "supporttoken")
      })
   ),

   with_mock(
      "httr::POST" = function(..., body) {
         list(..., status_code = 400, content = '')
      },
      test_that("When HTTP status code is 400, get_support_token raises an error", {
         token <- list()
         expect_error(get_support_token(token), "Invalid operator ID.")
      })
   ),

   with_mock(
      "httr::POST" = function(..., body) {
         list(..., status_code = 403, content = '')
      },
      test_that("When HTTP status code is 403, get_support_token raises an error", {
         token <- list()
         expect_error(get_support_token(token), "Invalid request or token timeout.")
      })
   ),

   with_mock(
      "httr::POST" = function(...) {
         list(..., status_code = 999, content = 'message')
      },
      test_that("With an unknown error, get_support_token raises an error with response content", {
         token <- list()
         expect_error(get_support_token(token), "message")
      })
   )

)

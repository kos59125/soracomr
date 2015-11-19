context("auth")

with_mock(
   "httr::status_code" = function(r) {
      r$status_code
   },
   "httr::content" = function(r, ...) {
      r$content
   },

   with_mock(
      "httr::POST" = function(..., body) {
         list(..., body = body, status_code = 200, content = '{"apiKey":"aaa","operatorId":"bbb","token":"ccc"}')
      },
      test_that("get_token returns soracom_token object", {
         actual <- get_token("email", "password")

         expect_true(inherits(actual, "soracom_token"))

         expect_equal(actual$apiKey, "aaa")
         expect_equal(actual$operatorId, "bbb")
         expect_equal(actual$token, "ccc")
      })
   ),

   with_mock(
      "httr::POST" = function(..., body) {
         list(..., body = body, status_code = 401, content = '')
      },
      test_that("When HTTP status code is 401, get_token raises an error", {
         expect_error(get_token("email", "password"), "Wrong email or password.")
      })
   ),

   with_mock(
      "httr::POST" = function(..., body) {
         list(..., body = body, status_code = 999, content = 'message')
      },
      test_that("With an unknown error, get_token raises an error with response content", {
         expect_error(get_token("email", "password"), "message")
      })
   )
)

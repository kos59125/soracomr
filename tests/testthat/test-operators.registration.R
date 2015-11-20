context("operators registration")

with_mock(
   "httr::status_code" = function(r) {
      r$status_code
   },
   "httr::content" = function(r, ...) {
      r$content
   },

   with_mock(
      "httr::POST" = function(..., body) {
         list(..., status_code = 201, content = '')
      },
      test_that("create_operator shows a message when succeeded", {
         expect_message(create_operator("email", "password"), sprintf(sprintf("Operator is successfully created. Sent a verification mail to %s.", sQuote("email"))))
      })
   ),

   with_mock(
      "httr::POST" = function(..., body) {
         list(..., body = body, status_code = 400, content = '')
      },
      test_that("When HTTP status code is 400, create_operator raises an error", {
         expect_error(create_operator("email", "password"), "Invalid email address or password.")
      })
   ),

   with_mock(
      "httr::POST" = function(..., body) {
         list(..., body = body, status_code = 999, content = 'message')
      },
      test_that("With an unknown error, create_operator raises an error with response content", {
         expect_error(create_operator("email", "password"), "message")
      })
   ),

   with_mock(
      "httr::POST" = function(..., body) {
         list(..., status_code = 200, content = '')
      },
      test_that("verify_operator shows a message when succeeded", {
         expect_message(verify_operator("token"), "Operator is successfully registered.")
      })
   ),

   with_mock(
      "httr::POST" = function(..., body) {
         list(..., body = body, status_code = 400, content = '')
      },
      test_that("When HTTP status code is 400, verify_operator raises an error", {
         expect_error(verify_operator("token"), "Invalid token.")
      })
   ),

   with_mock(
      "httr::POST" = function(..., body) {
         list(..., body = body, status_code = 404, content = '')
      },
      test_that("When HTTP status code is 404, verify_operator raises an error", {
         expect_error(verify_operator("token"), "Token timeout.")
      })
   ),

   with_mock(
      "httr::POST" = function(..., body) {
         list(..., body = body, status_code = 999, content = 'message')
      },
      test_that("With an unknown error, verify_operator raises an error with response content", {
         expect_error(verify_operator("token"), "message")
      })
   )
)

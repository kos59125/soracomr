context("password reset")

with_mock(
   "httr::status_code" = function(r) {
      r$status_code
   },
   "httr::content" = function(r, ...) {
      r$content
   },

   with_mock(
      "httr::POST" = function(..., body) {
         list(..., status_code = 200, content = '')
      },
      test_that("issue_password_reset_token shows a message when succeeded", {
         expect_message(issue_password_reset_token("email"), sprintf(sprintf("Sent a mail to %s.", sQuote("email"))))
      })
   ),

   with_mock(
      "httr::POST" = function(..., body) {
         list(..., body = body, status_code = 400, content = '')
      },
      test_that("When HTTP status code is 400, issue_password_reset_token raises an error", {
         expect_error(issue_password_reset_token("email"), "Invalid email address.")
      })
   ),

   with_mock(
      "httr::POST" = function(..., body) {
         list(..., body = body, status_code = 999, content = 'message')
      },
      test_that("With an unknown error, issue_password_reset_token raises an error with response content", {
         expect_error(issue_password_reset_token("email"), "message")
      })
   ),

   with_mock(
      "httr::POST" = function(..., body) {
         list(..., status_code = 200, content = '{"apiKey":"aaa","operatorId":"bbb","token":"ccc"}')
      },
      test_that("reset_password shows a message when succeeded", {
         expect_message(reset_password("password", "token"), "Password is successfully reset.")
      })
   ),

   with_mock(
      "httr::POST" = function(..., body) {
         list(..., body = body, status_code = 400, content = '')
      },
      test_that("When HTTP status code is 400, reset_password raises an error", {
         expect_error(reset_password("password", "token"), "Invalid token or password.")
      })
   ),

   with_mock(
      "httr::POST" = function(..., body) {
         list(..., body = body, status_code = 404, content = '')
      },
      test_that("When HTTP status code is 404, reset_password raises an error", {
         expect_error(reset_password("password", "token"), "Token timeout.")
      })
   ),

   with_mock(
      "httr::POST" = function(..., body) {
         list(..., body = body, status_code = 999, content = 'message')
      },
      test_that("With an unknown error, reset_password raises an error with response content", {
         expect_error(reset_password("password", "token"), "message")
      })
   )
)

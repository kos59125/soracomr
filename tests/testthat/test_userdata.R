context("userdata")

with_mock(
   "httr::status_code" = function(r) {
      r$status_code
   },

   with_mock(
      "httr::GET" = function(...) {
         list(..., status_code = 200, content = 'my data')
      },
      "httr::content" = function(r, as, ...) {
         expect_equal(as, "text")
         r$content
      },
      test_that("get_user_data", {
         expect_equal(get_user_data(as = "text"), "my data")
      })
   ),

   with_mock(
      "httr::GET" = function(...) {
         list(..., status_code = 999, content = 'message')
      },
      "httr::content" = function(r, ...) {
         r$content
      },
      test_that("With an unknown status code, get_user_data raises an error with response content", {
         expect_error(get_user_data(), "message")
      })
   )

)

context("operators")

operator_schema <- '{
   "createDate": "string",
   "description": "string",
   "email": "string",
   "operatorId": "string",
   "rootOperatorId": "string",
   "updateDate": "string"
}'
list_operator_schema <- sprintf("[%s]", operator_schema)

with_mock(
   "httr::status_code" = function(r) {
      r$status_code
   },
   "httr::content" = function(r, ...) {
      r$content
   },

   with_mock(
      "httr::GET" = function(...) {
         list(..., status_code = 200, content = list_operator_schema)
      },
      test_that("get_operator returns soracom_operator object", {
         token <- list("operatorId" = "operator_id")
         class(token) <- "soracom_token"
         operators <- get_operator(token)
         expect_true(inherits(operators, "soracom_operator"))

         expected <- from_content(list_operator_schema, "soracom_operator")
         expect_equal(operators, expected)
      })
   ),

   with_mock(
      "httr::GET" = function(...) {
         list(..., status_code = 400, content = '')
      },
      test_that("get_operator raises an error when the operator is not found", {
         token <- list("operatorId" = "operator_id")
         class(token) <- "soracom_token"
         expect_error(get_operator(token), "Invalid operator ID.")
      })
   ),

   with_mock(
      "httr::GET" = function(...) {
         list(..., status_code = 999, content = 'message')
      },
      test_that("With an unknown status code, get_operator raises an error with response content", {
         token <- list("operatorId" = "operator_id")
         class(token) <- "soracom_token"
         expect_error(get_operator(token), "message")
      })
   )
)

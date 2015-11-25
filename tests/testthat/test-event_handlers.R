context("event handlers")

# dummy token
token <- list()

event_handler_schema <- '{
"actionConfigList": [
   {
   "properties": {},
   "type": "string"
   }
],
"description": "string",
"handlerId": "string",
"name": "string",
"ruleConfig": {
   "properties": {},
   "type": "string"
},
"status": "string",
"targetGroupId": "string",
"targetImsi": "string",
"targetOperatorId": "string"
}'
list_event_handler_schema <- sprintf('[%s]', event_handler_schema)


with_mock(
   "httr::status_code" = function(r) {
      r$status_code
   },
   "httr::content" = function(r, ...) {
      r$content
   },

   with_mock(
      "httr::GET" = function(...) {
         list(..., status_code = 200, content = list_event_handler_schema)
      },
      test_that("list_event_handlers returns soracom_event_handler object", {
         handlers <- list_event_handlers(token)
         expect_true(inherits(handlers, "soracom_event_handler"))

         expected <- from_content(list_event_handler_schema, "soracom_event_handler", force_data_frame = FALSE)
         expect_equal(handlers, expected)
      })
   ),

   with_mock(
      "httr::GET" = function(...) {
         list(..., status_code = 999, content = 'message')
      },
      test_that("With an unknown status code, list_event_handlers raises an error with response content", {
         expect_error(list_event_handlers(token), "message")
      })
   ),

   with_mock(
      "httr::GET" = function(url, ...) {
         expect_false(grepl("/subscribers/", url))
         list(url = url, ..., status_code = 200, content = list_event_handler_schema)
      },
      test_that("Check path without imsi", {
         list_event_handlers(token)
      })
   ),

   with_mock(
      "httr::GET" = function(url, ...) {
         expect_true(grepl("/event_handlers/subscribers/imsi$", url))
         list(url = url, ..., status_code = 200, content = list_event_handler_schema)
      },
      test_that("Check path when imsi is given", {
         list_event_handlers(token, "imsi")
      })
   ),

   with_mock(
      "httr::GET" = function(...) {
         list(..., status_code = 200, content = event_handler_schema)
      },
      test_that("get_event_handler returns soracom_event_handler object", {
         handlers <- get_event_handler(token, "handler_id")
         expect_true(inherits(handlers, "soracom_event_handler"))

         expected <- from_content(list_event_handler_schema, "soracom_event_handler", force_data_frame = FALSE)
         expect_equal(handlers, expected)
      })
   ),

   with_mock(
      "httr::GET" = function(...) {
         list(..., status_code = 404, content = '')
      },
      test_that("get_event_handler raises an error when the event_handler is not found", {
         expected_message <- sprintf("Event handler %s was not found.", sQuote("handler_id"))
         expect_error(get_event_handler(token, "handler_id"), expected_message)
      })
   ),

   with_mock(
      "httr::GET" = function(...) {
         list(..., status_code = 999, content = 'message')
      },
      test_that("With an unknown status code, get_event_handler raises an error with response content", {
         expect_error(get_event_handler(token, "handler_id"), "message")
      })
   ),

   with_mock(
      "httr::POST" = function(...) {
         list(..., status_code = 201, content = '')
      },
      test_that("create_event_handler returns nothing when it succeeds", {
         handler <- list()
         result <- create_event_handler(token, handler)
         expect_null(result)
      })
   ),

   with_mock(
      "httr::POST" = function(...) {
         list(..., status_code = 999, content = 'message')
      },
      test_that("With an unknown status code, create_event_handler raises an error with response content", {
         handler <- list()
         expect_error(create_event_handler(token, handler), "message")
      })
   ),

   with_mock(
      "httr::PUT" = function(...) {
         list(..., status_code = 200, content = '')
      },
      test_that("put_event_handler returns nothing when it succeeds", {
         handler <- list()
         result <- put_event_handler(token, handler)
         expect_null(result)
      })
   ),

   with_mock(
      "httr::PUT" = function(...) {
         list(..., status_code = 400, content = '')
      },
      test_that("put_event_handler warns with a specified message when the group is not found", {
         handler <- list()
         expect_warning(put_event_handler(token, handler), "Invalid handler ID.")
      })
   ),

   with_mock(
      "httr::PUT" = function(...) {
         list(..., status_code = 999, content = 'message')
      },
      test_that("With an unknown status code, put_event_handler raises an error with response content", {
         handler <- list()
         expect_error(put_event_handler(token, handler), "message")
      })
   ),

   with_mock(
      "httr::DELETE" = function(...) {
         list(..., status_code = 200, content = '')
      },
      test_that("delete_event_handler returns nothing when it succeeds", {
         result <- delete_event_handler(token, "handler_id")
         expect_null(result)
      })
   ),

   with_mock(
      "httr::DELETE" = function(...) {
         list(..., status_code = 404, content = '')
      },
      test_that("delete_event_handler warns with a specified message when the group is not found", {
         expected_message <- sprintf("Event handler %s was not found.", sQuote("handler_id"))
         expect_warning(delete_event_handler(token, "handler_id"), expected_message)
      })
   ),

   with_mock(
      "httr::DELETE" = function(...) {
         list(..., status_code = 999, content = 'message')
      },
      test_that("With an unknown status code, delete_event_handler raises an error with response content", {
         expect_error(delete_event_handler(token, "handler_id"), "message")
      })
   )

)

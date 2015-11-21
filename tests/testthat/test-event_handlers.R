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
   )

)

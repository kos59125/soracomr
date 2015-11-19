context("subscribers")

# dummy token
token <- list()

subscriber_schema <- '{
   "imsi": "string",
   "msisdn": "string",
   "ipAddress": "string",
   "apn": "string",
   "speed_class": "string",
   "createdAt": 0,
   "lastModifiedAt": 0,
   "expiryTime": 0,
   "status": "string",
   "tags": {},
   "operatorId": "string"
}'
list_subscriber_schema <- sprintf('[%s]', subscriber_schema)


with_mock(
   "httr::status_code" = function(r) {
      r$status_code
   },
   "httr::content" = function(r, ...) {
      r$content
   },

   with_mock(
      "httr::GET" = function(...) {
         list(..., status_code = 200, content = list_subscriber_schema)
      },
      test_that("list_subscribers returns soracom_token object", {
         subscribers <- list_subscribers(token)

         expected <- from_content(list_subscriber_schema, "soracom_subscriber")
         expect_equal(subscribers, expected)
      })
   ),

   with_mock(
      "httr::GET" = function(...) {
         list(..., status_code = 999, content = 'message')
      },
      test_that("With an unknown status code, list_subscribers raises an error with response content", {
         expect_error(list_subscribers(token), "message")
      })
   ),

   with_mock(
      "httr::GET" = function(...) {
         list(..., status_code = 200, content = list_subscriber_schema)
      },
      test_that("get_subscriber returns soracom_token object", {
         subscribers <- get_subscriber(token, "imsi")

         expected <- from_content(list_subscriber_schema, "soracom_subscriber")
         expect_equal(subscribers, expected)
      })
   ),

   with_mock(
      "httr::GET" = function(...) {
         list(..., status_code = 404, content = '')
      },
      test_that("get_subscriber raises an error when the subscriber is not found", {
         message <- sprintf("Subscriber %s was not found.", sQuote("imsi"))
         expect_error(get_subscriber(token, "imsi"), message)
      })
   ),

   with_mock(
      "httr::GET" = function(...) {
         list(..., status_code = 999, content = 'message')
      },
      test_that("With an unknown status code, get_subscriber raises an error with response content", {
         expect_error(get_subscriber(token, "imsi"), "message")
      })
   )

)

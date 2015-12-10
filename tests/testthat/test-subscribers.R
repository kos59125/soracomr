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
      test_that("list_subscribers returns soracom_subscriber object", {
         subscribers <- list_subscribers(token)
         expect_true(inherits(subscribers, "soracom_subscriber"))

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
   ),

   with_mock(
      "httr::GET" = function(url, ...) {
         expect_true(grepl("/subscribers$", url))
         expect_false(grepl("/groups/", url))
         list(..., status_code = 200, content = list_subscriber_schema)
      },
      test_that("Checks the endpoint whether group_id parameter is not passed", {
         list_subscribers(token)
      })
   ),

   with_mock(
      "httr::GET" = function(url, ...) {
         expect_true(grepl("/groups/group_id/subscribers$", url))
         list(..., status_code = 200, content = list_subscriber_schema)
      },
      test_that("Checks the endpoint whether group_id parameter is passed", {
         list_subscribers(token, group_id = "group_id")
      })
   ),

   with_mock(
      "httr::GET" = function(query, ...) {
         expect_equal(query, list("limit" = 20))
         list(..., status_code = 200, content = list_subscriber_schema)
      },
      test_that("Checks limit is in query", {
         list_subscribers(token, limit = 20)
      })
   ),

   with_mock(
      "httr::GET" = function(query, ...) {
         expect_equal(query, list("last_evaluated_key" = "imsi"))
         list(..., status_code = 200, content = list_subscriber_schema)
      },
      test_that("Checks limit is in query", {
         list_subscribers(token, last_evaluated_key = "imsi")
      })
   ),

   with_mock(
      "httr::GET" = function(url, ...) {
         expect_equal(url, get_metadata_endpoint("/subscriber"))
         expect_equal(length(list(...)), 0)
         list(..., status_code = 200, content = subscriber_schema)
      },
      test_that("Checks metadata subscriber", {
         get_subscriber()
      })
   ),

   with_mock(
      "httr::GET" = function(url, ...) {
         expect_equal(url, get_endpoint("/subscribers/imsi"))
         expect_equal(length(list(...)), 1)
         list(..., status_code = 200, content = subscriber_schema)
      },
      test_that("Checks usual API subscriber", {
         token <- list()
         get_subscriber(token, "imsi")
      })
   )

)

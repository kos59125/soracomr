context("groups")

# dummy token
token <- list()

group_schema <- '{
   "configuration": {},
   "createdTime": 0,
   "groupId": "string",
   "lastModifiedTime": 0,
   "operatorId": "string",
   "tags": {
      "location": "tokyo"
   }
}'
list_group_schema <- sprintf('[%s]', group_schema)


with_mock(
   "httr::status_code" = function(r) {
      r$status_code
   },
   "httr::content" = function(r, ...) {
      r$content
   },

   with_mock(
      "httr::GET" = function(...) {
         list(..., status_code = 200, content = list_group_schema)
      },
      test_that("list_groups returns soracom_group object", {
         groups <- list_groups(token)
         expect_true(inherits(groups, "soracom_group"))

         expected <- from_content(list_group_schema, "soracom_group")
         expect_equal(groups, expected)
      })
   ),

   with_mock(
      "httr::GET" = function(...) {
         list(..., status_code = 999, content = 'message')
      },
      test_that("With an unknown status code, list_groups raises an error with response content", {
         expect_error(list_groups(token), "message")
      })
   ),

   with_mock(
      "httr::POST" = function(..., body) {
         expect_equal(body, list("tags" = list()))
         list(..., body = body, status_code = 201, content = group_schema)
      },
      test_that("create_group without name", {
         group <- create_group(token)

         expected <- from_content(group_schema, "soracom_group")
         expect_equal(group, expected)
      })
   ),

   with_mock(
      "httr::POST" = function(..., body) {
         expect_equal(body, list("tags" = list("name" = "group name")))
         list(..., body = body, status_code = 201, content = group_schema)
      },
      test_that("create_group with name", {
         group <- create_group(token, "group name")

         expected <- from_content(group_schema, "soracom_group")
         expect_equal(group, expected)
      })
   ),

   with_mock(
      "httr::POST" = function(...) {
         list(..., status_code = 999, content = 'message')
      },
      test_that("With an unknown status code, create_group raises an error with response content", {
         expect_error(create_group(token), "message")
      })
   ),

   with_mock(
      "httr::DELETE" = function(...) {
         list(..., status_code = 204, content = '')
      },
      test_that("delete_group returns nothing when it succeeds", {
         result <- delete_group(token, "group id")
         expect_null(result)
      })
   ),

   with_mock(
      "httr::DELETE" = function(...) {
         list(..., status_code = 404, content = '')
      },
      test_that("delete_group warns with a specified message when the group is not found", {
         expected_message <- sprintf("Group %s was not found.", sQuote("group_id"))
         expect_warning(delete_group(token, "group_id"), expected_message)
      })
   ),

   with_mock(
      "httr::DELETE" = function(...) {
         list(..., status_code = 999, content = 'message')
      },
      test_that("With an unknown status code, delete_group raises an error with response content", {
         expect_error(delete_group(token, "group_id"), "message")
      })
   ),

   with_mock(
      "httr::GET" = function(...) {
         list(..., status_code = 200, content = group_schema)
      },
      test_that("get_group without name", {
         group <- get_group(token, "group_id")

         expected <- from_content(group_schema, "soracom_group")
         expect_equal(group, expected)
      })
   ),

   with_mock(
      "httr::GET" = function(...) {
         list(..., status_code = 404, content = '')
      },
      test_that("get_group raises an error when the group is not found", {
         expected_message <- sprintf("Group %s was not found.", sQuote("group_id"))
         expect_error(get_group(token, "group_id"), expected_message)
      })
   ),

   with_mock(
      "httr::GET" = function(...) {
         list(..., status_code = 999, content = 'message')
      },
      test_that("With an unknown status code, get_group raises an error with response content", {
         expect_error(get_group(token, "group_id"), "message")
      })
   ),

   with_mock(
      "httr::GET" = function(query, ...) {
         expect_equal(query, list("limit" = 20))
         list(..., status_code = 200, content = list_group_schema)
      },
      test_that("Checks limit is in query", {
         list_groups(token, limit = 20)
      })
   ),

   with_mock(
      "httr::GET" = function(query, ...) {
         expect_equal(query, list("last_evaluated_key" = "imsi"))
         list(..., status_code = 200, content = list_group_schema)
      },
      test_that("Checks limit is in query", {
         list_groups(token, last_evaluated_key = "imsi")
      })
   )

)

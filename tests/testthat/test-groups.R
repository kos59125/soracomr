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
      test_that("list_groups returns soracom_token object", {
         groups <- list_groups(token)

         expect_equal(groups, fromJSON(list_group_schema))
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

         expect_equal(group, fromJSON(group_schema))
      })
   ),

   with_mock(
      "httr::POST" = function(..., body) {
         expect_equal(body, list("tags" = list("name" = "group name")))
         list(..., body = body, status_code = 201, content = group_schema)
      },
      test_that("create_group with name", {
         group <- create_group(token, "group name")

         expect_equal(group, fromJSON(group_schema))
      })
   ),

   with_mock(
      "httr::POST" = function(...) {
         list(..., status_code = 999, content = 'message')
      },
      test_that("With an unknown status code, create_group raises an error with response content", {
         expect_error(create_group(token), "message")
      })
   )
)

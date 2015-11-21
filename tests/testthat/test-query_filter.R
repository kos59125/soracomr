context("query filter")

##############################
## query_filter_subscriber
##############################

##
## tag
##
test_that("Check tag filter query_filter_subscriber", {
   filter <- query_filter_subscriber(tag_name = "foo", tag_value = "bar", tag_value_match_mode = "prefix")
   expect_equal(filter$tag_name, "foo")
   expect_equal(filter$tag_value, "bar")
   expect_equal(filter$tag_value_match_mode, "prefix")
})

test_that("When tag_name is given, tag_value is required", {
   expect_error(query_filter_subscriber(tag_name = "foo"), "tag_value is required when tag_name is given.")
})

##
## speed class
##
test_that("Check speed class filter", {
   filter <- query_filter_subscriber(speed_class_filter = c("s1.slow", "s1.fast"))
   expect_equal(filter$speed_class_filter, "s1.slow|s1.fast")
})

test_that("Error when unknown speed class is given", {
   expect_error(query_filter_subscriber(speed_class_filter = "s1.unknown"), "Invalid speed class.")
})

##
## status
##
test_that("Check speed class filter", {
   filter <- query_filter_subscriber(status_filter = c("active", "inactive", "ready"))
   expect_equal(filter$status_filter, "active|inactive|ready")
})

test_that("Error when unknown speed class is given", {
   expect_error(query_filter_subscriber(status_filter = "unknown"), "Invalid status.")
})


##############################
## query_filter_group
##############################

test_that("Check tag filter query_filter_group", {
   filter <- query_filter_subscriber(tag_name = "foo", tag_value = "bar", tag_value_match_mode = "prefix")
   expect_equal(filter$tag_name, "foo")
   expect_equal(filter$tag_value, "bar")
   expect_equal(filter$tag_value_match_mode, "prefix")
})

test_that("When tag_name is given, tag_value is required", {
   expect_error(query_filter_subscriber(tag_name = "foo"), "tag_value is required when tag_name is given.")
})

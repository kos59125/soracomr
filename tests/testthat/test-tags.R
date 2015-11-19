context("tags")

test_that("soracom_subscriber", {
   tag1 <- c("tag1_1 value", "tag1_2 value")
   tag2 <- c("tag2_1 value", "tag2_2 value")
   subscribers <- data.frame(
      imsi = c("1", "2"),
      tags.tag1 = tag1,
      tags.tag2 = tag2,
      stringsAsFactors = FALSE
   )
   class(subscribers) <- c("soracom_subscriber", "data.frame")

   expected <- data.frame(
      tag1 = tag1,
      tag2 = tag2,
      stringsAsFactors = FALSE
   )
   class(expected) <- c("soracom_tags", "data.frame")

   actual <- tags(subscribers)
   expect_equal(actual, expected)
})

test_that("soracom_subscriber with no tags", {
   subscribers <- data.frame(
      imsi = c("1", "2"),
      stringsAsFactors = FALSE
   )
   class(subscribers) <- c("soracom_subscriber", "data.frame")

   expected <- data.frame(matrix(nrow = 2, ncol = 0))
   class(expected) <- c("soracom_tags", "data.frame")

   actual <- tags(subscribers)
   expect_equal(actual, expected)
})

test_that("soracom_group", {
   tag1 <- c("tag1_1 value", "tag1_2 value")
   tag2 <- c("tag2_1 value", "tag2_2 value")
   groups <- data.frame(
      groupId = c("1", "2"),
      tags.tag1 = tag1,
      tags.tag2 = tag2,
      stringsAsFactors = FALSE
   )
   class(groups) <- c("soracom_group", "data.frame")

   expected <- data.frame(
      tag1 = tag1,
      tag2 = tag2,
      stringsAsFactors = FALSE
   )
   class(expected) <- c("soracom_tags", "data.frame")

   actual <- tags(groups)
   expect_equal(actual, expected)
})

test_that("soracom_group with no tags", {
   groups <- data.frame(
      groupId = c("1", "2"),
      stringsAsFactors = FALSE
   )
   class(groups) <- c("soracom_group", "data.frame")

   expected <- data.frame(matrix(nrow = 2, ncol = 0))
   class(expected) <- c("soracom_tags", "data.frame")

   actual <- tags(groups)
   expect_equal(actual, expected)
})

test_that("unknown class", {
   tag1 <- c("tag1_1 value", "tag1_2 value")
   tag2 <- c("tag2_1 value", "tag2_2 value")
   unknown <- data.frame(
      groupId = c("1", "2"),
      tags.tag1 = tag1,
      tags.tag2 = tag2,
      stringsAsFactors = FALSE
   )

   expect_error(tags(unknown))
})

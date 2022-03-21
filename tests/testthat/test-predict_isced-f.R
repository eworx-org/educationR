test_that("output length is the same as input length", {
  x <- c("Law degree", "PhD in Linguistics")
  exp <- length(x)
  res <- length(predict_isced_f(x))
  expect_equal(res, exp)
})

test_that("class of output is a list of top_docs is not NULL", {
  x <- "Biology"
  exp <- "list"
  res <- class(predict_isced_f(x, top_docs = 10L))
  expect_equal(res, exp)
})

test_that("predictions for some explicitly typed qualifications are correct", {
  x <- c("Biology", "Mathematics", "Law", "gibberish123asdf")
  exp <- c("biological and related sciences", "mathematics and statistics", "law", NA)
  res <- predict_isced_f(x, "en", "isced_2_label")
  expect_equal(res, exp)
})

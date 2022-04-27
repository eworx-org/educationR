test_that("output length is the same as input length", {
  x <- c("Law degree", "PhD in Linguistics")
  exp <- length(x)
  res <- length(isced_field(x))
  expect_equal(res, exp)
})

test_that("class of output is a list of top_docs is not NULL", {
  x <- "Biology"
  exp <- "list"
  res <- class(predict_isced_f(x, top_docs = 10L))
  expect_equal(res, exp)
})

test_that("labels for some explicitly typed qualifications are correct", {
  x <- c("Biology", "Mathematics", "Law", "gibberish123asdf")
  exp <- c("biological and related sciences", "mathematics and statistics", "law", NA)
  res <- isced_field(x, "en", 3)
  expect_equal(res, exp)
})

test_that("codes for some explicitly typed qualifications are correct", {
  x <- c("Biology", "Mathematics", "Law", "gibberish123asdf")
  exp <- c("05", "05", "04", NA)
  res <- isced_field(x, "en", 2, code = TRUE)
  expect_equal(res, exp)
})

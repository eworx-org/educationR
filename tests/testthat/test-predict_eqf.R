test_that("output length is the same as input length", {
  x <- c("Master of Science", "PhD in Linguistics")
  exp <- length(x)
  res <- length(predict_eqf(x))
  expect_equal(res, exp)
})

test_that("predictions for some typical qualifications are correct", {
  x <- c("Bachelor's degree", "Master's degree", "PhD", "gibberish123asdf")
  exp <- factor(c("6", "7", "8", NA), level = as.character(1:8))
  res <- predict_eqf(x)
  expect_equal(res, exp)
})

test_that("output is a matrix if na_threshold is NULL", {
  x <- c("Master of Science", "PhD in Linguistics")
  exp <- c("matrix", "array")
  res <- class(predict_eqf(x, na_threshold = NULL))
  expect_equal(res, exp)
})

test_that("output length is 8 * input length if na_threshold is NULL", {
  x <- c("Master of Science", "PhD in Linguistics")
  exp <- 8 * length(x)
  res <- length(predict_eqf(x, na_threshold = NULL))
  expect_equal(res, exp)
})

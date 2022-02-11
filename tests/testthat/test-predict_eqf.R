test_that("output length is the same as input length", {
  x <- c("Master of Science", "PhD in Linguistics")
  exp <- length(x)
  res <- length(predict_eqf(x))
  expect_equal(res, exp)
})

test_that("predictions for some typical qualifications are correct", {
  x <- c("Bachelor's degree", "Master's degree", "PhD")
  exp <- factor(c("6", "7", "8"), level = as.character(1:8))
  res <- predict_eqf(x)
  expect_equal(res, exp)
})

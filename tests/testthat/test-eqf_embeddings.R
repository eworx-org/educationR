test_that("dimensions of embeddings are consistent", {
  x <- c("Master of Science", "PhD in Linguistics")
  exp <- c(length(x), 10)
  res <- dim(eqf_embeddings(x, "en"))
  expect_equal(res, exp)
})

test_that("dimensions of embeddings are consistent", {
  x <- c("Master of Science", "PhD in Linguistics")
  exp <- c(length(x), 10)
  res <- dim(eqf_embeddings(x, "en"))
  expect_equal(res, exp)
})

test_that("embeddings are identical for texts that become identical through preprocessing", {
  x <- c("High School", "High School!", "high  school\t", "High\t\tschool ...")
  emb <- eqf_embeddings(x)
  expect_true(all(emb[1, ] == t(emb)))
})

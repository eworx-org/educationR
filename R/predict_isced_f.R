#' Match qualification title with ISCED fields
#' 
#' @param x A character vector of qualification titles.
#' @param locale The ISO 639-1 code of the language used.
#' @param target Target level and formatting of output ISCED-F level.
#' @param top_docs Maximum number of ISCED-F fields returned per qualification title.
#' 
#' @return A list of data frames with the top matched documents sorted by similarity.
#' @import text2vec
#' @export
#' 
#' @examples
#' predict_isced_f("MSc in Biology", top_docs = 10L)
#' predict_isced_f(c("Law degree", "PhD in Linguistics"), "en", "isced_2_key")
predict_isced_f <- function(x, locale = "en", target = "isced_4_label", top_docs = 5) {
  # Apply transformations
  y <- itoken(x, preprocessor = prep_fun, progressbar = FALSE)
  y <- create_dtm(y, models$isced$docs[[locale]][["model"]][["vec"]])
  y <- transform(y, models$isced$docs[[locale]][["model"]][["tfidf"]])
  # Calculate similarity with documents
  docs <- models$isced$docs[[locale]][["tfidf"]]
  sims <- sim2(docs$stats, y, method = "cosine", norm = "none")
  # Return list of data.frames otherwise
  res <- apply(sims, 2, function(sim) {
    sim <- sim[sim > 0]
    sim <- head(sort(sim, decreasing = TRUE), top_docs)
    sim <- data.frame(code = docs$class[as.integer(names(sim))], sim)
    # Match top N documents with their associated ISCED-F field
    keys_match <- match(sim[["code"]], models$isced$class[["isced_4_key"]])
    sim[[target]] <- models$isced$class[keys_match,][[target]]
    sim[, c(target, "sim")]
  })
  names(res) <- x
  res
}

#' Get the top matched ISCED field for a qualification title
#' 
#' @param x A character vector of qualification titles.
#' @param locale The ISO 639-1 code of the language used.
#' @param target Target level and formatting of output ISCED-F level.
#' @param top_docs Maximum number of ISCED-F fields returned per qualification title.
#' 
#' @return A character vector of equal length to x with the top matched ISCED fields.
#' @export
#' 
#' @examples
#' isced_field("MSc in Biology", top_docs = 10L)
#' isced_field(c("Law degree", "PhD in Linguistics"), "en", "isced_2_key")
isced_field <- function(x, locale = "en", target = "isced_4_label", top_docs = 5) {
  preds <- predict_isced_f(x, locale, target, top_docs)
  fields <- lapply(preds, function(pred) {
    pred[1, target]
  })
  unname(unlist(fields))
}

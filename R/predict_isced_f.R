#' Predict the ISCED-F field of a qualification title
#' 
#' @param x A character vector of qualification titles.
#' @param locale The ISO 639-1 code of the language used.
#' @param target Target level and formatting of output ISCED-F level.
#' 
#' @return A character vector of equal length to x with the predicted ISCED-F fields.
#' @import text2vec
#' @export
#' 
#' @examples
#' predict_isced_f("MSc in Biology")
#' predict_isced_f(c("Law degree", "PhD in Linguistics"), "en", "isced_1_key")
predict_isced_f <- function(x, locale = "en", target = "isced_3_label") {
  # Apply transformations
  x <- itoken(x, preprocessor = prep_fun, progressbar = FALSE)
  x <- create_dtm(x, models$isced$docs[[locale]][["model"]][["vec"]])
  x <- transform(x, models$isced$docs[[locale]][["model"]][["tfidf"]])
  # Find closest document
  docs <- models$isced$docs[[locale]][["tfidf"]]
  sim <- sim2(docs$stats, x, method = "cosine", norm = "none")
  keys <- apply(sim, 2, function(y) docs$class[which(y == max(y))[1]])
  keys_match <- match(keys, models$isced$class[["isced_3_key"]])
  models$isced$class[keys_match,][[target]]
}

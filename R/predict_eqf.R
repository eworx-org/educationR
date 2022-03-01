#' Predict the EQF level of a qualification title
#' 
#' @param x A character vector of qualification titles.
#' @param locale The ISO 639-1 code of the language used.
#' 
#' @return A factor of equal length to x with the predicted EQF levels.
#' @import text2vec
#' @import glmnet
#' @export
#' 
#' @examples
#' predict_eqf("Bachelor of Science")
#' predict_eqf(c("Master of Science", "PhD in Linguistics"), "en")
predict_eqf <- function(x, locale = "en") {
  # Extract features
  x <- itoken(x, preprocessor = prep_fun, progressbar = FALSE)
  x <- create_dtm(x, models$eqf[[locale]][["vec"]])
  x <- transform(x, models$eqf[[locale]][["tfidf"]])
  x <- transform(x, models$eqf[[locale]][["lsa"]])
  # Make predictions
  pred <- predict(models$eqf[[locale]][["model"]], x, type = "response")
  eqf_levels <- colnames(pred)
  if(nrow(pred) > 1) {
    eqf_pred <- apply(pred[, , 1], 1, function(y) names(sort(-y))[1])
  } else {
    eqf_pred <- names(sort(-pred[, , 1]))[1]
  }
  factor(unname(eqf_pred), levels = eqf_levels)
}

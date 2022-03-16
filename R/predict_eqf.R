#' Predict the EQF level of a qualification title
#' 
#' @param x A character vector of qualification titles.
#' @param locale The ISO 639-1 code of the language used.
#' @param na_threshold Threshold for EQF level prediction score.
#' @return If `na_threshold` is not `NULL`, a factor of equal length to x with the predicted EQF levels. Otherwise, a matrix with as many rows as length of x and scores for each EQF level.
#' @import text2vec
#' @import glmnet
#' @export
#' 
#' @examples
#' predict_eqf("Bachelor of Science")
#' predict_eqf(c("Master of Science", "PhD in Linguistics"), "en")
predict_eqf <- function(x, locale = NULL, na_threshold = 0.25) {
  # Calculate prediction for every supported language if locale is NULL
  if(is.null(locale)) {
    pred <- 0
    for(loc in get_languages_eqf()) {
      pred <- pred + predict_eqf(x, loc, na_threshold = NULL)
    }
    pred <- pred / length(get_languages_eqf())
  }
  else {
    # Extract features
    x <- itoken(x, preprocessor = prep_fun, progressbar = FALSE)
    x <- create_dtm(x, models$eqf[[locale]][["vec"]])
    x <- transform(x, models$eqf[[locale]][["tfidf"]])
    x <- transform(x, models$eqf[[locale]][["lsa"]])
    # Calculate score per EQF level and return if scores is TRUE
    pred <- predict(models$eqf[[locale]][["model"]], x, type = "response")[, , 1]
  }
  if(is.null(na_threshold)) {
    return(pred)
  }
  # Return the EQF level with the maximum score per qualification title.
  if(is.matrix(pred)) {
    eqf_pred <- apply(pred, 1, function(y) names(sort(-y[y > na_threshold]))[1])
    eqf_levels <- colnames(pred)
  } else {
    eqf_pred <- names(sort(-pred[pred > na_threshold]))[1]
    eqf_levels <- names(pred)
  }
  factor(unname(eqf_pred), levels = eqf_levels)
}

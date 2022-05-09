#' Classify qualification titles with respect to EQF level
#' 
#' @param x A character vector of qualification titles.
#' @param locale The ISO 639-1 code of the language used. If a character vector with multiple codes is provided, the final prediction will be the average of predictions across those languages.
#' @return A matrix with as many rows as length of x and scores for each EQF level.
#' @import text2vec
#' @import glmnet
#' @export
#' 
#' @examples
#' predict_eqf("Bachelor of Science")
#' predict_eqf(c("Master of Science", "PhD in Linguistics"), "en")
predict_eqf <- function(x, locale = get_languages_eqf()) {
  # Calculate prediction for every supported language if locale is NULL
  if(length(locale) > 1) {
    pred <- lapply(locale, function(loc) {
      predict_eqf(x, loc)
    })
    pred <- apply(simplify2array(pred), c(1,2), sum, na.rm = TRUE)
    pred[apply(pred, 1, sum) == 0] <- NA
    pred <- pred / apply(pred, 1, sum)
  }
  else {
    # Extract features
    y <- itoken(x, preprocessor = prep_fun, progressbar = FALSE)
    y <- create_dtm(y, models$eqf[[locale]][["vec"]])
    no_match <- apply(y == 0, 1, all)
    y <- transform(y, models$eqf[[locale]][["tfidf"]])
    y <- transform(y, models$eqf[[locale]][["lsa"]])
    # Calculate score per EQF level
    pred <- predict(models$eqf[[locale]][["model"]], y, type = "response")
    pred <- matrix(pred, nrow = length(x))
    pred[no_match] <- NA
  }
  pred
}

#' Predict the EQF level of a qualification title
#' 
#' @param x A character vector of qualification titles.
#' @param locale The ISO 639-1 code of the language used. If a character vector with multiple codes is provided, the final prediction will be based on the average of predictions across those languages.
#' @param na_threshold Threshold for EQF level prediction score.
#' @return An integer vector equal in length to x.
#' @export
#' 
#' @examples
#' eqf_level("Bachelor of Science")
#' eqf_level(c("Master of Science", "PhD in Linguistics"), "en")
eqf_level <- function(x, locale = get_languages_eqf(), na_threshold = 0.25) {
  # Return the EQF level with the maximum score per qualification title.
  pred <- predict_eqf(x, locale)
  above_threshold <- apply(pred, 1, max) >= na_threshold
  pred <- max.col(pred)
  ifelse(above_threshold, pred, NA)
}

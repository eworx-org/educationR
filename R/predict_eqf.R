#' Predict the EQF level of a qualification title
#' 
#' @param x A character vector of qualification titles.
#' @param locale The ISO 639-1 code of the language used.
#' 
#' @return A factor of equal length to x with the predicted EQF levels.
#' @import glmnet
#' @export
#' 
#' @examples
#' predict_eqf("Bachelor of Science")
#' predict_eqf(c("Master of Science", "PhD in Linguistics"), "en")
predict_eqf <- function(x, locale = "en") {
  model <- models$eqf[[locale]][["model"]]
  emb <- eqf_embeddings(x, locale)
  pred <- predict(model, emb, type = "response")[, , 1]
  eqf_levels <- names(head(sort(-pred), 1))
  if(nrow(emb) > 1) {
    eqf_levels <- apply(pred, 1, function(y) {
      names(head(sort(-y), 1))
    })
  }
  factor(unname(eqf_levels), levels = names(coef(model)))
}

#' Get word embeddings of a qualification title with respect to EQF
#' 
#' @param x A character vector of qualification titles.
#' @param locale The ISO 639-1 code of the language used.
#' 
#' @return Matrix with each row representing a qualification title.
#' @import text2vec
#' @export
#' 
#' @examples
#' eqf_embeddings("Bachelor of Science")
#' eqf_embeddings(c("Master of Science", "PhD in Linguistics"), "en")
eqf_embeddings <- function(x, locale = "en") {
  x <- itoken(x, preprocessor = eqf_model[[locale]][["prep"]],
              progressbar = FALSE)
  x <- create_dtm(x, eqf_model[[locale]][["vec"]])
  x <- transform(x, eqf_model[[locale]][["tfidf"]])
  transform(x, eqf_model[[locale]][["lsa"]])
}

#' Preprocess character vectors
#' 
#' @param x A character vector.
#' 
#' @return Character vector with lower case alphanumeric text and trimmed white space.
prep_fun <- function(x) {
  x <- tolower(x)
  x <- gsub("\t", " ", x)
  x <- gsub("[^[:alnum:][:space:]]", " ", x)
  x <- trimws(x)
}

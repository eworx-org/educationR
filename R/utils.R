#' Preprocess character vectors
#' 
#' @param x A character vector.
#' 
#' @return Character vector with lower case alphanumeric text and trimmed white space.
prep_fun <- function(x) {
  x <- tolower(x)
  x <- gsub("\t", " ", x)
  x <- gsub("[^[:alnum:][:space:]]", " ", x)
  trimws(x)
}

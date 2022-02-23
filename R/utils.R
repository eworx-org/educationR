#' Preprocess character vectors
#' 
#' @param x A character vector.
#' @param num_max_char An integer equal to the maximum number of characters per word desired, unless set to `NULL`.
#' 
#' @return Character vector with lower case alphanumeric text, trimmed white space, and maximum number of characters per word.
prep_fun <- function(x, num_max_char = 10L) {
  x <- tolower(x)
  x <- gsub("[^[:alnum:] ]", " ", x)
  x <- gsub("[[:space:]]+", " ", x)
  if(!is.null(num_max_char)) {
    pattern <- paste0("(^|\\W)\\w{1,", num_max_char, "}")
    x <- sapply(regmatches(x, gregexpr(pattern, x)), paste, collapse = "")
  } else {
    x <- trimws(x)
  }
  x
}

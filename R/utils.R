#' Preprocess character vectors
#' 
#' @param x A character vector.
#' @param num_max_char An integer equal to the maximum number of characters per word desired, unless set to `NULL`.
#' @param keep Boolean that determines whether the original character vector is kept along with the trimmed one.
#'
#' @return Character vector with lower case alphanumeric text, trimmed white space, and maximum number of characters per word.
prep_fun <- function(x, num_max_char = 5L, keep = TRUE) {
  x <- tolower(x)
  x <- gsub("[^[:alnum:] ]", " ", x)
  x <- gsub("[[:space:]]+", " ", x)
  if(!is.null(num_max_char)) {
    pattern <- paste0("(^|\\W)\\w{1,", num_max_char, "}")
    res <- sapply(regmatches(x, gregexpr(pattern, x)), paste, collapse = "")
    if (keep == TRUE) {
      res <- paste(res, x)
    }
  } else {
    res <- trimws(x)
  }
  res
}

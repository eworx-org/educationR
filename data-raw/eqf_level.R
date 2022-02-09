library(data.table)
library(magrittr)
library(text2vec)
library(stopwords)

# Settings
set.seed(10)
locales <- c("en", "it")
tr_split <- 0.8
n_dims <- 10

# eqf_dat is a data.frame with labeled data and schema: locale, text, eqf
eqf_dat <- readRDS("eqf_labeled_data.rds")
eqf_dat <- lapply(locales, function(loc) {
  eqf_dat[locale == loc, .(text, eqf)]
}) %>% set_names(locales)

# Split train-test
train_test <- lapply(eqf_dat, function(dat) {
  train <- sample(nrow(dat), split*nrow(dat))
  test <- setdiff(1:nrow(dat), train)
  list("train" = train, "test" = test)
})

# Extract features
prep_fun <- function(x) {
  x <- tolower(x)
  x <- gsub("\t", " ", x)
  x <- gsub("[^[:alnum:][:space:]]", " ", x)
  x <- trimws(x)
}

train_dat <- lapply(locales, function(loc) {
  dat <- eqf_dat[[loc]][train_test[[loc]][["train"]]]
  it <- itoken(dat[, text], preprocessor = prep_fun, progressbar = FALSE)
  
  vectorizer <- create_vocabulary(it, stopwords = stopwords(loc)) %>%
    prune_vocabulary(doc_proportion_max = 0.5, term_count_min = 50) %>%
    vocab_vectorizer()
  m_tfidf <- TfIdf$new()
  m_lsa <- LSA$new(n_topics = n_dims)
  embeddings <- create_dtm(it, vectorizer) %>%
    fit_transform(m_tfidf) %>%
    fit_transform(m_lsa)
  
  list(
    "emb" = embeddings,
    "model" = list(
      "prep" = prep_fun,
      "vec" = vectorizer,
      "tfidf" = m_tfidf,
      "lsa" = m_lsa
    )
  )
}) %>% set_names(locales)

eqf_feat_extr <- lapply(train_dat, function(dat)dat[["model"]])

usethis::use_data(eqf_feat_extr, internal = TRUE, overwrite = TRUE, compress = "xz")

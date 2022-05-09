# Required packages
require(data.table)
require(magrittr)
require(text2vec)
require(stopwords)
require(caret)
require(glmnet)

# Source package functions and data
source("R/utils.R")
source("R/predict_eqf.R")
load("R/sysdata.rda")

# Settings
locales <- c("en", "it")
prop_min <- 0.1
lsa_dims <- 16
tr_split <- 0.8
set.seed(10)

# Load labeled data (data.frame with mandatory columns: locale, text, eqf)
eqf_dat <- readRDS("data-raw/eqf_labeled_data.rds")
eqf_levels <- eqf_dat[, sort(unique(eqf))]

# Split train-test
train <- sample(nrow(eqf_dat), tr_split * nrow(eqf_dat))
test <- setdiff(1:nrow(eqf_dat), train)

# Prepare model
eqf_model <- function(loc, prop_min, lsa_dims) {
  dat <- eqf_dat[train][locale == loc, .(text, eqf)]
  dat[, eqf := factor(eqf, levels = eqf_levels)]
  dat[, batch := sample(as.integer(.N * 0.01), size = .N, replace = TRUE)]
  corpus <- dat[, .(text = paste(text, collapse = " ")), by = c("eqf", "batch")]
  
  # Construct vocabulary and vectorizer
  vocab <- lapply(eqf_levels, function(level) {
    itoken(corpus[eqf == level, text], preprocessor = prep_fun, progressbar = FALSE) %>%
      create_vocabulary(stopwords = stopwords(loc), ngram = c(1L, 2L)) %>%
      prune_vocabulary(doc_proportion_min = prop_min)
  })
  vocab <- do.call(combine_vocabularies, vocab)
  vectorizer <- vocab_vectorizer(vocab)
  
  # Create LSA model
  it <- itoken(corpus[, text], preprocessor = prep_fun, progressbar = FALSE)
  m_tfidf <- TfIdf$new()
  m_lsa <- LSA$new(n_topics = lsa_dims)
  create_dtm(it, vectorizer) %>%
    fit_transform(m_tfidf) %>%
    fit_transform(m_lsa)
  
  # Get embeddings
  it <- itoken(dat[, text], preprocessor = prep_fun, progressbar = FALSE)
  embeddings <- create_dtm(it, vectorizer) %>%
    transform(m_tfidf) %>%
    transform(m_lsa)
  
  # Balance data
  # train_dat <- data.table(as.data.table(embeddings), Class = dat[, eqf])
  train_dat <- downSample(x = embeddings, y = dat[, eqf]) %>% setDT
  
  # Train classifier
  m_glm <- cv.glmnet(x = train_dat[, -"Class"] %>% as.matrix,
                     y = train_dat[, Class],
                     alpha = 1,
                     family = "multinomial",
                     type.measure = "auc",
                     parallel = TRUE,
                     nfolds = 4)
  
  # Test classifier
  list("vec" = vectorizer, "tfidf" = m_tfidf, "lsa" = m_lsa, "model" = m_glm)
}

# Train classifier
eqf <- lapply(locales, eqf_model, prop_min, lsa_dims) %>% set_names(locales)

# Save internal data
models["eqf"] <- list("eqf" = eqf)
usethis::use_data(models, internal = TRUE, overwrite = TRUE, compress = "xz")

# Test classifier
confusion_mat <- lapply(locales, function(loc) {
  dat <- eqf_dat[test][locale == loc, .(text, eqf)]
  dat[, eqf := factor(eqf, levels = eqf_levels)]
  dat[, pred := eqf_level(text, loc, na_threshold = 0)]
  confusionMatrix(table(dat[["eqf"]], dat[["pred"]]))
}) %>% set_names(locales)

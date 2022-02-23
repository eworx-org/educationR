library(data.table)
library(magrittr)
library(text2vec)
library(stopwords)
library(caret)
library(glmnet)
source("R/utils.R")
source("R/predict_eqf.R")
load("R/sysdata.rda")

# Settings
set.seed(10)
locales <- c("en", "it")
tr_split <- 0.8
n_dims <- 25
doc_min <- 0.0005
doc_max <- 0.5

# eqf_dat is a data.frame with labeled data and schema: locale, text, eqf
eqf_dat <- readRDS("eqf_labeled_data.rds")
eqf_dat <- lapply(locales, function(loc) {
  eqf_dat[locale == loc, .(text, eqf)]
}) %>% set_names(locales)

# Split train-test
train_test <- lapply(eqf_dat, function(dat) {
  train <- sample(nrow(dat), tr_split*nrow(dat))
  test <- setdiff(1:nrow(dat), train)
  list("train" = train, "test" = test)
})

# Extract features
train_dat <- lapply(locales, function(loc) {
  dat <- eqf_dat[[loc]][train_test[[loc]][["train"]]]
  dat[, eqf := factor(eqf, levels = dat[, sort(unique(eqf))])]
  
  # Create embeddings
  it <- itoken(dat[, text], preprocessor = prep_fun, progressbar = FALSE)
  vectorizer <- create_vocabulary(it, stopwords = stopwords(loc)) %>%
    prune_vocabulary(doc_proportion_max = doc_max, doc_proportion_min = doc_min) %>%
    vocab_vectorizer()
  m_tfidf <- TfIdf$new()
  m_lsa <- LSA$new(n_topics = n_dims)
  embeddings <- create_dtm(it, vectorizer) %>%
    fit_transform(m_tfidf) %>%
    fit_transform(m_lsa)
  
  # Balance data
  train <- downSample(x = embeddings, y = dat[, eqf]) %>% setDT
  
  list("data" = train,
       "model" = list("vec" = vectorizer, "tfidf" = m_tfidf, "lsa" = m_lsa))
}) %>% set_names(locales)

# Train classifier
m_glm <- lapply(train_dat, function(dat) {
  train <- dat$data
  train <- train[sample(nrow(train), 0.1*nrow(train))]
  cv.glmnet(x = train[, -"Class"] %>% as.matrix,
            y = train[, Class],
            alpha = 1,
            family = "multinomial",
            type.measure = "auc",
            parallel = TRUE,
            nfolds = 4)
})

# Save internal data
eqf <- lapply(locales, function(loc) {
  append(train_dat[[loc]][["model"]], list("model" = m_glm[[loc]]))
}) %>% set_names(locales)
models["eqf"] <- list(eqf)

usethis::use_data(models, internal = TRUE, overwrite = TRUE, compress = "xz")

# Test classifier
test_dat <- lapply(locales, function(loc) {
  test <- eqf_dat[[loc]][train_test[[loc]][["test"]]]
  test[, eqf := factor(eqf, levels = test[, sort(unique(eqf))])]
  test[, pred := predict_eqf(text, loc)]
  # Produce confusion matrix
  test[, .N, by = c("eqf", "pred")][order(eqf, pred)]
}) %>% set_names(locales)

for(loc in locales) {
  test <- test_dat[[loc]][, .(count = sum(N)), by = .(pred == eqf)]
  acc <- test[pred == TRUE, count / test[, sum(count)]]
  print(paste0("Accuracy for locale '", loc, "': ", 100 * round(acc, 4), "%"))
}

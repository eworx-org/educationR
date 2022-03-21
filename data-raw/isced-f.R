library(data.table)
library(magrittr)
library(text2vec)
library(stopwords)
library(glue)
library(caret)
source("R/utils.R")
source("R/predict_isced_f.R")
load("R/sysdata.rda")

# Settings
locales <- c("en", "it")
esco_v <- "1.0.8"
doc_max <- 0.1

# Tidyfy multilingual ISCED-F data
isced_f <- lapply(locales, function(loc) {
  esco <- fread(glue("data-raw/esco-bundle/v{esco_v}/skillGroups_{loc}.csv"))
  esco <- esco[grep("isced-f", conceptUri)]
  esco <- esco[, .(code = gsub(".*isced-f/", "", conceptUri), 
                   label = preferredLabel)]
  isced <- esco[nchar(code) == 4]
  isced <- isced[, .(isced_3_key = code, isced_2_key = substr(code, 1, 3), 
                     isced_1_key = substr(code, 1, 2), isced_3_label = label)]
  isced <- merge(isced, esco[, .(isced_2_key = code, isced_2_label = label)], 
                 by = "isced_2_key", all.x = TRUE)
  isced <- merge(isced, esco[, .(isced_1_key = code, isced_1_label = label)], 
                 by = "isced_1_key", all.x = TRUE)
  isced[, sort(names(isced), decreasing = TRUE), with = FALSE]
}) %>% set_names(locales)

# Enhance corpus with associated ESCO skills
esco_corpus <- lapply(locales, function(loc) {
  broad <- fread(glue("data-raw/esco-bundle/v{esco_v}/broaderRelationsSkillPillar.csv"))
  skills <- fread(glue("data-raw/esco-bundle/v{esco_v}/skills_{loc}.csv"))
  
  broad <- broad[grep("isced-f", broaderUri), 
                 .(code = gsub(".*isced-f/", "", broaderUri), conceptUri)]
  broad <- broad[nchar(code) == 4]
  corpus <- merge(broad, skills[, .(conceptUri, text = preferredLabel)], 
                  by = "conceptUri")
  corpus[order(code), .(code, text)]
}) %>% set_names(locales)

# Prepare model for document similarity comparison
doc_tfidf <- lapply(locales, function(loc) {
  dat <- rbind(esco_corpus[[loc]], 
               isced_f[[loc]][, .(code = isced_3_key, text = isced_3_label)])
  dat[, code := factor(code, levels = dat[, sort(unique(code))])]
  
  # Create tf-idf model
  it <- itoken(dat[, text], preprocessor = prep_fun, progressbar = FALSE)
  vectorizer <- create_vocabulary(it, stopwords = stopwords(loc), ngram = c(1L, 2L)) %>%
    prune_vocabulary(doc_proportion_max = doc_max, term_count_min = 1) %>%
    vocab_vectorizer()
  m_tfidf <- TfIdf$new()
  tfidf <- create_dtm(it, vectorizer) %>% 
    fit_transform(m_tfidf)
  
  list("tfidf" = list("stats" = tfidf, "class" = dat[, code]),
       "model" = list("vec" = vectorizer, "tfidf" = m_tfidf))
}) %>% set_names(locales)

# Save internal data
models["isced"] <- list(list("class" = isced_f$en, "docs" = doc_tfidf))
usethis::use_data(models, internal = TRUE, overwrite = TRUE, compress = "xz")

# Test
test_dat <- readRDS("data-raw/foet_labeled_data.rds")
isced_cor <- fread("data-raw/isced_2013_2011.csv", colClasses = "character")
confusion_mat <- lapply(locales, function(loc) {
  test <- test_dat[locale == loc]
  test[, foet := substr(foet, 1, 1)]
  test[, key_2013 := predict_isced_f(text, loc, "isced_1_key")]
  test <- merge(test, isced_cor[, .(key_2013, pred = key_2011)], 
                by = "key_2013")
  confusionMatrix(table(test[["pred"]], test[["foet"]]))
}) %>% set_names(locales)

# Required packages
require(data.table)
require(magrittr)
require(text2vec)
require(stopwords)
require(glue)
require(caret)

# Source package functions and data
source("R/utils.R")
source("R/predict_isced_f.R")
load("R/sysdata.rda")

# Settings
locales <- c("en", "it")
esco_v <- "1.0.8"
doc_max <- 0.1

# Prepare ISCED-F data
isced_fields <- function(loc, digits = 4, extra = FALSE, descr_cols = FALSE) {
  isced <- fread(glue("data-raw/esco-bundle/v{esco_v}/skillGroups_{loc}.csv"))
  isced <- isced[grep("isced-f", conceptUri)]
  isced <- isced[, .(code = conceptUri, label = preferredLabel)]
  isced[, code := gsub(".*isced-f/", "", code)]
  isced <- isced[nchar(code) == digits]
  if(extra == FALSE) {
    isced <- isced[(grepl("[^089]$", code) | digits == 2) & code != "99"]
  }
  if(descr_cols == TRUE) {
    cols <- c(glue("isced_{digits}_key"), glue("isced_{digits}_label"))
    setnames(isced, c("code", "label"), cols)
  }
  isced
}

# Prepare ESCO skills associated with ISCED fields
esco_skills <- function(loc) {
  skills <- fread(glue("data-raw/esco-bundle/v{esco_v}/skills_{loc}.csv"))
  isced <- fread(glue("data-raw/esco-bundle/v{esco_v}/broaderRelationsSkillPillar.csv"))
  isced <- isced[grep("isced-f", broaderUri)]
  isced <- isced[, .(code = gsub(".*isced-f/", "", broaderUri), conceptUri)]
  isced <- isced[nchar(code) == 4]
  merge(skills, isced, by = "conceptUri")[, .(code, label = preferredLabel)]
}

# Prepare model for document similarity comparison
tfidf_prep <- function(loc) {
  corpus <- rbind(isced_fields(loc), esco_skills(loc))
  corpus[, code := factor(code, levels = corpus[, sort(unique(code))])]
  
  it <- itoken(corpus[, label], preprocessor = prep_fun, progressbar = FALSE)
  vectorizer <- create_vocabulary(it, stopwords = stopwords(loc), ngram = c(1L, 2L)) %>%
    prune_vocabulary(doc_proportion_max = doc_max, term_count_min = 1) %>%
    vocab_vectorizer()
  m_tfidf <- TfIdf$new()
  tfidf <- create_dtm(it, vectorizer) %>%
    fit_transform(m_tfidf)
  
  list("tfidf" = list("stats" = tfidf, "class" = corpus[, code]),
       "model" = list("vec" = vectorizer, "tfidf" = m_tfidf))
}

# ISCED-F definitions
isced <- isced_fields("en", digits = 4, extra = TRUE, descr_cols = TRUE)
isced[, isced_3_key := substr(isced_4_key, 1, 3)]
isced[isced_fields("en", digits = 3, extra = TRUE, descr_cols = TRUE), 
      isced_3_label := i.isced_3_label, on = "isced_3_key"]
isced[, isced_2_key := substr(isced_4_key, 1, 2)]
isced[isced_fields("en", digits = 2, extra = TRUE, descr_cols = TRUE),
      isced_2_label := i.isced_2_label, on = "isced_2_key"]

# Models
doc_tfidf <- lapply(locales, tfidf_prep) %>% set_names(locales)

# Save internal data
models["isced"] <- list(list("class" = isced, "docs" = doc_tfidf))
usethis::use_data(models, internal = TRUE, overwrite = TRUE, compress = "xz")

# Test
test_dat <- readRDS("data-raw/foet_labeled_data.rds")
isced_cor <- fread("data-raw/isced_2013_2011.csv", colClasses = "character")
confusion_mat <- lapply(locales, function(loc) {
  test <- test_dat[locale == loc]
  test[, foet := substr(foet, 1, 1)]
  test[, key_2013 := predict_isced_f(text, loc, "isced_2_key")]
  test <- merge(test, isced_cor[, .(key_2013, pred = key_2011)], 
                by = "key_2013")
  confusionMatrix(table(test[["pred"]], test[["foet"]]))
}) %>% set_names(locales)

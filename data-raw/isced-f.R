library(data.table)
library(magrittr)
library(glue)
load("R/sysdata.rda")

# Settings
locales <- c("en", "it")
esco_v <- "1.0.8"

# Tidyfy multilingual ISCED-F data
isced_f <- lapply(locales, function(loc) {
  esco <- fread(glue("esco-bundle/v{esco_v}/skillGroups_{loc}.csv"))
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
  broad <- fread(glue("esco-bundle/v{esco_v}/broaderRelationsSkillPillar.csv"))
  skills <- fread(glue("esco-bundle/v{esco_v}/skills_{loc}.csv"))
  
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
    prune_vocabulary(doc_proportion_max = 0.5, term_count_min = 1) %>%
    vocab_vectorizer()
  m_tfidf <- TfIdf$new()
  tfidf <- create_dtm(it, vectorizer) %>% 
    fit_transform(m_tfidf)
  
  list(
    "tfidf" = list("stats" = tfidf, 
                   "class" = dat[, code]),
    "model" = list("prep" = prep_fun,
                   "vec" = vectorizer,
                   "tfidf" = m_tfidf)
  )
}) %>% set_names(locales)

# Save internal data
models["isced"] <- list(list("class" = isced_f$en, "docs" = doc_tfidf))
usethis::use_data(models, internal = TRUE, overwrite = TRUE, compress = "xz")

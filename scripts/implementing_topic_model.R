# Preparing and running the topic model
# Loading packages and data------------------------
source("scripts/paths_and_packages.R")
source("scripts/helper_functions.R")
p_load(tokenizers,
       tidytext,
       tidyfast,
       stringi,
       stm)

# Load data
documents <- read_rds(file.path(data_path, "documents.rds")) %>% 
  .[level_1 == TRUE]
documents[, year := as.numeric(year)]
text <- documents[level_1 == TRUE, .(LLS_id, abstract)] %>% # keeping only articles that cite Livestock report
  unique()
text <- text[, token := tokenize_words(abstract, 
                                             lowercase = TRUE,
                                             strip_punct = TRUE, # delete punctuation 
                                             strip_numeric = TRUE # delete numbers
                                        )] 

# Preparing the list of terms-------------------------------
text <- text[, abstract := NULL]
text <- dt_unnest(text, token, keep = FALSE)
text <- text[, token_id := 1:.N, by = LLS_id]
setnames(text, "V1", "token")

## Prepare stop_words------------------------------
stop_words <- bind_rows(get_stopwords(language = "en", source = "stopwords-iso"),
                        get_stopwords(language = "en", source = "snowball")) %>% 
  pull(word)
  
text <- text[!token %in% stop_words]
text[, token := stri_trans_general(token, id = "Latin-ASCII")] # remove accents
text[, token := str_remove_all(token, "'s")]
text <- text[!(str_detect(token, "^\\d") & !str_detect(token, "th$|nd$|\\ds$")) & !str_detect(token, "^[^\\p{Latin}]+$") ]  # removing most pattern with digits and latin characters
text[, token := textstem::lemmatize_words(token)]
       
## Creating a list of bigrams and trigrams--------------------------
# Create bigrams and tigrams
text <- text[order(LLS_id, token_id)]  # Ensure correct order
text[, bigram := ifelse(token_id + 1 == shift(token_id, type = "lead"), 
                             paste(token, shift(token, type = "lead"), sep = "_"), 
                             NA),
          by = .(LLS_id)]
text[, trigram := ifelse(token_id + 1 == shift(token_id, type = "lead") & token_id + 2 == shift(token_id, type = "lead", n = 2),
                          paste(token, shift(token, type = "lead"), shift(token, type = "lead", n = 2), sep = "_"),
                          NA),
     by = .(LLS_id)]

### Calculate PMI values for bigrams---------------------
# filter na and count bigrams, keep only bigrams that appear more than 10 times
bigram_counts <- text[!is.na(bigram), .N, by = .(bigram)]
bigram_counts <- bigram_counts[N > 4]  

# Split bigram into word_1 and word_2
bigram_counts[, c("word_1", "word_2") := tstrsplit(bigram, "_", fixed = TRUE)]

# Assign a unique window ID to each bigram (acts like `window_id`)
bigram_counts[, window_id := .I]  # .I is the row number (unique ID), the context window is thus only the bigram itself

#Count occurrences of each word
word_prob <- text[, .N, by = token]
word_prob[, prob := N / length(text$token)]

#Count occurrences of each bigram
bigram_prob <- text[!is.na(bigram), .N, by = bigram]
bigram_prob[, prob_bigram := N /(length(text$token))]

# Merge word_1 and word_2 probabilities into bigram table and rename
bigram_counts <- merge(bigram_counts, word_prob[, .(token, prob_word_1 = prob)], by.x = "word_1", by.y = "token", all.x = TRUE)
bigram_counts <- merge(bigram_counts, word_prob[, .(token, prob_word_2 = prob)], by.x = "word_2", by.y = "token", all.x = TRUE)

# merge bigram probabilities into bigram table and rename
bigram_counts <- merge(bigram_counts, bigram_prob[, .(bigram, prob_bigram)], by = "bigram")

# compute pmi 
bigram_counts[, pmi := log2(prob_bigram / (prob_word_1 * prob_word_2))]

# keep only bigrams with pmi > 0
bigram_counts[pmi > median(pmi), keep_bigram := TRUE] 

### Calculate PMI values for trigrams---------------------
# Filter and count trigrams, keep only trigrams appearing more than 10 times
trigram_counts <- text[!is.na(trigram), .N, by = .(trigram)]
trigram_counts <- trigram_counts[N > 4]

# Split trigram into word_1, word_2, and word_3
trigram_counts[, c("word_1", "word_2", "word_3") := tstrsplit(trigram, "_", fixed = TRUE)]

# Count occurrences of each trigram
trigram_prob <- text[!is.na(trigram), .N, by = trigram]
trigram_prob[, prob_trigram := N / (length(text$token))]

# Merge word probabilities into trigram table
trigram_counts <- merge(trigram_counts, word_prob[, .(token, prob_word_1 = prob)], by.x = "word_1", by.y = "token", all.x = TRUE)
trigram_counts <- merge(trigram_counts, word_prob[, .(token, prob_word_2 = prob)], by.x = "word_2", by.y = "token", all.x = TRUE)
trigram_counts <- merge(trigram_counts, word_prob[, .(token, prob_word_3 = prob)], by.x = "word_3", by.y = "token", all.x = TRUE)

# Merge trigram probabilities into trigram table
trigram_counts <- merge(trigram_counts, trigram_prob[, .(trigram, prob_trigram)], by = "trigram")

# Compute PMI for trigrams
trigram_counts[, pmi := log2(prob_trigram / (prob_word_1 * prob_word_2 * prob_word_3))]

# Keep only trigrams with PMI above the median
trigram_counts[pmi > median(pmi), keep_trigram := TRUE]

### Adding bigrams and trigrams to the token list---------------

text <- merge(text, bigram_counts[, .(bigram, keep_bigram)], by = "bigram", all.x = TRUE)
text <- merge(text, trigram_counts[, .(trigram, keep_trigram)], by = "trigram", all.x = TRUE)
setorder(text, LLS_id, token_id)

text[, keep_bigram := ifelse(is.na(keep_bigram), FALSE, keep_bigram)]
text[, keep_trigram := ifelse(is.na(keep_trigram), FALSE, keep_trigram)]
text[, token := ifelse(keep_bigram, bigram, token)]
text[, token := ifelse(keep_trigram, trigram, token)] # trigram in second as we want to keep trigrams over bigrams
text[, new_token := ifelse((shift(keep_trigram, type = "lag") == TRUE | shift(keep_trigram, n = 2, type = "lag") == TRUE | shift(keep_bigram, type = "lag") == TRUE), "", token)]
text[, token := ifelse(!is.na(new_token), new_token, token)] # We do that only to take back the problems with the lag of two above (missing first two words)
text <- text[token != "", .(LLS_id, token)]
text[, n_word := .N, by = .(LLS_id, token)]

# Running the topic model-------------------------------

## Preparing the data for the topic model
# Later: preparing different thresholds for the number of words

thresholds <- c(5, 10, 20)
corpora_in_dfm <- vector(mode = "list", length = length(thresholds))
names(corpora_in_dfm) <- as.character(thresholds)
for(threshold in thresholds){
  corpus_in_dfm <- text %>% 
    .[token %in% text[, .N, by = token][N > threshold]$token,] %>% 
    cast_dfm(LLS_id, token, n_word)
  metadata <- documents[LLS_id %in% unique(text$LLS_id), .(LLS_id, year, title, author_first, journal, abstract)] %>%
    unique()
  
  corpora_in_dfm[[paste(threshold)]] <- quanteda::convert(corpus_in_dfm, to = "stm",  docvars = metadata)
}
write_rds(corpora_in_dfm, here::here(data_path, "topic_modelling", "corpora.rds"), compress = "gz")

rstudioapi::jobRunScript("background_topic_model.R",
                         importEnv = TRUE,
                         exportEnv = "R_GlobalEnv")

# Evaluating the different topic models-------------------
many_stm <- read_rds(here::here(data_path, "topic_modelling", "many_stm.rds"))

# estimate exclusivity and coherence 
setDT(many_stm)
# unnest corpus_in_stm by K 
many_stm[, corpus_in_stm := corpora_in_dfm, by = K]

# many_stm[, heldout := future_map(corpus_in_stm, ~ make.heldout(.x$documents, .x$vocab))]
many_stm[, exclusivity := map(models, exclusivity)]
many_stm[, exclusivity := map_dbl(exclusivity, mean)]
many_stm[, semantic_coherence := map2(models, corpus_in_stm, ~ semanticCoherence(.x, .y$documents))]
many_stm[, semantic_coherence := map_dbl(semantic_coherence, mean)]
many_stm[, heldout := map(corpus_in_stm, ~ make.heldout(.x$documents, .x$vocab))]
many_stm[, heldout_likelihood := map2_dbl(models, heldout, ~ eval.heldout(.x, .y$missing)$expected.heldout)]
many_stm[, residual := map2(models, corpus_in_stm, ~checkResiduals(.x, .y$documents))]
many_stm[, residual := map_dbl(residual, "dispersion")]
many_stm[, lbound := map_dbl(models, function(x) max(x$convergence$bound) + lfactorial(x$settings$dim$K))]

plots <- plot_topicmodels_stat(many_stm)

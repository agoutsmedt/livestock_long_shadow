#: Tutorial ---------
# Script goals:
# - Prepare text data (tokens, bigrams, trigrams) from `documents.rds`.
# - Compute PMI to keep meaningful multi-word expressions.
# - Build document-feature matrices at several frequency thresholds and
#   convert them to objects suitable for the `stm` package.
# - Kick off a background job that fits topic models and evaluate them.
#
# Why this is useful:
# - Topic models require careful token and phrase handling. This script
#   demonstrates a reproducible pipeline to prepare data and compare models.
#
# Setup instructions:
# - Ensure `scripts/paths_and_packages.R` and `scripts/helper_functions.R`
#   are available and set `data_path` variable.
# - Required packages: tokenizers, tidytext, tidyfast, stringi, stm,
#   quanteda, data.table, textstem, here, rstudioapi, and their dependencies.
# - This script is intended to be run interactively or sourced from a
#   controlling script. It dispatches a background job to run heavy model fits.
#
# How it works (high level):
# 1. Load metadata and select documents of interest (level_1 == TRUE).
# 2. Tokenise abstracts, remove stopwords, lemmatize, and remove noisy tokens.
# 3. Create candidate bigrams/trigrams and compute PMI to select meaningful phrases.
# 4. Replace tokens with selected multi-word expressions and build dfm at
#    multiple frequency thresholds.
# 5. Convert dfm objects to `stm`-compatible inputs, save them and launch
#    a background topic-modelling script. Finally, read results and compute
#    evaluation statistics (exclusivity, coherence, heldout likelihood).

#: Loading packages and data ---------
## Keep package loading and path helper scripts external for reproducibility.
source("scripts/paths_and_packages.R")
source("scripts/helper_functions.R")

# Use pacman::p_load (via p_load wrapper) to ensure packages are installed/loaded.
p_load(
  tokenizers,
  tidytext,
  tidyfast,
  stringi,
  stm
)

# Load documents and keep only level-1 items.
# Note: `data_path` is expected to be defined in `paths_and_packages.R`.
documents <- read_rds(file.path(data_path, "documents.rds")) %>%
  .[level_1 == TRUE]
documents[, year := as.numeric(year)]


#: Tokenisation and basic cleaning ---------
# We will produce a data.table `tokens_dt` with one row per token and its id
# for later phrase construction.
tokens_dt <- documents[level_1 == TRUE, .(LLS_id, abstract)] %>%
  unique()

# Tokenize abstracts into word vectors; tokenizers::tokenize_words returns a list
# column so we store it as `tokens_list` first for clarity.
tokens_dt[,
  tokens_list := tokenize_words(
    abstract,
    lowercase = TRUE,
    strip_punct = TRUE, # remove punctuation
    strip_numeric = TRUE # remove numbers
  )
]

# dt_unnest expands the list-column into one token per row.
tokens_dt[, abstract := NULL]
tokens_dt <- dt_unnest(tokens_dt, tokens_list, keep = FALSE)

# Rename the unnested column to `token` and add `token_id` per document.
setnames(tokens_dt, "V1", "token")
tokens_dt[, token_id := seq_len(.N), by = LLS_id]


#: Stop words, accents, lemmatisation ---------
# Collect common English stopwords from two sources and remove them.
stop_words <- bind_rows(
  get_stopwords(language = "en", source = "stopwords-iso"),
  get_stopwords(language = "en", source = "snowball")
) %>%
  pull(word)

tokens_dt <- tokens_dt[!token %in% stop_words]

# Normalize accents/diacritics and remove possessive 's.
tokens_dt[, token := stri_trans_general(token, id = "Latin-ASCII")]
tokens_dt[, token := str_remove_all(token, "'s")]

# Remove tokens that are primarily non-Latin or problematic numeric patterns.
# ? This filters tokens that start with a digit except ordinal forms (th/nd)
tokens_dt <- tokens_dt[
  !(str_detect(token, "^\\d") & !str_detect(token, "th$|nd$|\\ds$")) &
    !str_detect(token, "^[^\\p{Latin}]+$")
]

# Lemmatize to collapse inflected forms to their base forms - helps topic coherence.
tokens_dt[, token := textstem::lemmatize_words(token)]


#: Candidate bigrams and trigrams ---------
# Create bigram and trigram candidates by looking at contiguous tokens within
# each document. We compute candidate expressions but will later keep only
# those with strong PMI values.
tokens_dt <- tokens_dt[order(LLS_id, token_id)]

# bigram: token + next token when adjacent
tokens_dt[,
  bigram := ifelse(
    token_id + 1L == shift(token_id, type = "lead"),
    paste(token, shift(token, type = "lead"), sep = "_"),
    NA_character_
  ),
  by = .(LLS_id)
]

# trigram: token + next + next-next when contiguous
tokens_dt[,
  trigram := ifelse(
    token_id + 1L == shift(token_id, type = "lead") &
      token_id + 2L == shift(token_id, type = "lead", n = 2),
    paste(
      token,
      shift(token, type = "lead"),
      shift(token, type = "lead", n = 2),
      sep = "_"
    ),
    NA_character_
  ),
  by = .(LLS_id)
]


#: PMI calculation for bigrams ---------
# We compute pointwise mutual information to select bigrams that are more
# informative than expected by chance.
# Keep only phrases that occur more than a small threshold to avoid noisy PMI.
bigram_counts <- tokens_dt[!is.na(bigram), .N, by = .(bigram)]
bigram_counts <- bigram_counts[N > 4]

# Split the bigram into component words for probability computations.
bigram_counts[, c("word_1", "word_2") := tstrsplit(bigram, "_", fixed = TRUE)]

# Row id - unique identifier for each bigram entry (not used further here but kept
# to mirror original logic and for clarity).
bigram_counts[, window_id := .I]

# Word marginal probabilities (by counting tokens)
word_prob <- tokens_dt[, .N, by = token]
word_prob[, prob := N / length(tokens_dt$token)]

# Bigram marginal probabilities
bigram_prob <- tokens_dt[!is.na(bigram), .N, by = bigram]
bigram_prob[, prob_bigram := N / length(tokens_dt$token)]

# Merge component word probabilities into the bigram table.
bigram_counts <- merge(
  bigram_counts,
  word_prob[, .(token, prob_word_1 = prob)],
  by.x = "word_1",
  by.y = "token",
  all.x = TRUE
)
bigram_counts <- merge(
  bigram_counts,
  word_prob[, .(token, prob_word_2 = prob)],
  by.x = "word_2",
  by.y = "token",
  all.x = TRUE
)

# Attach bigram probability and compute PMI
bigram_counts <- merge(
  bigram_counts,
  bigram_prob[, .(bigram, prob_bigram)],
  by = "bigram"
)
bigram_counts[, pmi := log2(prob_bigram / (prob_word_1 * prob_word_2))]

# Mark bigrams whose PMI is above the median (keeps roughly the top half).
bigram_counts[pmi > median(pmi), keep_bigram := TRUE]


#: PMI calculation for trigrams ---------
trigram_counts <- tokens_dt[!is.na(trigram), .N, by = .(trigram)]
trigram_counts <- trigram_counts[N > 4]
trigram_counts[,
  c("word_1", "word_2", "word_3") := tstrsplit(trigram, "_", fixed = TRUE)
]

trigram_prob <- tokens_dt[!is.na(trigram), .N, by = trigram]
trigram_prob[, prob_trigram := N / length(tokens_dt$token)]

# Merge component word probabilities into trigram table
trigram_counts <- merge(
  trigram_counts,
  word_prob[, .(token, prob_word_1 = prob)],
  by.x = "word_1",
  by.y = "token",
  all.x = TRUE
)
trigram_counts <- merge(
  trigram_counts,
  word_prob[, .(token, prob_word_2 = prob)],
  by.x = "word_2",
  by.y = "token",
  all.x = TRUE
)
trigram_counts <- merge(
  trigram_counts,
  word_prob[, .(token, prob_word_3 = prob)],
  by.x = "word_3",
  by.y = "token",
  all.x = TRUE
)

trigram_counts <- merge(
  trigram_counts,
  trigram_prob[, .(trigram, prob_trigram)],
  by = "trigram"
)
trigram_counts[,
  pmi := log2(prob_trigram / (prob_word_1 * prob_word_2 * prob_word_3))
]
trigram_counts[pmi > median(pmi), keep_trigram := TRUE]


#: Incorporate selected phrases into token list ---------
# Merge flags back to the token table so we can replace tokens by chosen n-grams.
tokens_dt <- merge(
  tokens_dt,
  bigram_counts[, .(bigram, keep_bigram)],
  by = "bigram",
  all.x = TRUE
)
tokens_dt <- merge(
  tokens_dt,
  trigram_counts[, .(trigram, keep_trigram)],
  by = "trigram",
  all.x = TRUE
)
setorder(tokens_dt, LLS_id, token_id)

# Replace NA flags with FALSE and prefer trigrams over bigrams when both apply.
tokens_dt[, keep_bigram := fifelse(is.na(keep_bigram), FALSE, keep_bigram)]
tokens_dt[, keep_trigram := fifelse(is.na(keep_trigram), FALSE, keep_trigram)]
tokens_dt[, token := ifelse(keep_bigram, bigram, token)]
tokens_dt[, token := ifelse(keep_trigram, trigram, token)]

# `new_token` attempts to avoid duplicated tokens when a phrase covers following tokens.
# This mirrors the original logic which blanks tokens that are subsumed by a multi-word phrase.
tokens_dt[,
  new_token := ifelse(
    (shift(keep_trigram, type = "lag") == TRUE |
      shift(keep_trigram, n = 2, type = "lag") == TRUE |
      shift(keep_bigram, type = "lag") == TRUE),
    "",
    token
  )
]
tokens_dt[, token := ifelse(!is.na(new_token), new_token, token)]

# Remove empty tokens and compute token counts per document
tokens_dt <- tokens_dt[token != "", .(LLS_id, token)]
tokens_dt[, n_word := .N, by = .(LLS_id, token)]


#: Prepare document-feature matrices for STM ---------
# We create dfm objects at several frequency thresholds so we can compare models.
thresholds <- c(5, 10, 20)
corpora_in_dfm <- vector(mode = "list", length = length(thresholds))
names(corpora_in_dfm) <- as.character(thresholds)

for (threshold in thresholds) {
  corpus_in_dfm <- tokens_dt %>%
    .[token %in% tokens_dt[, .N, by = token][N > threshold]$token, ] %>%
    cast_dfm(LLS_id, token, n_word)

  metadata <- documents[
    LLS_id %in% unique(tokens_dt$LLS_id),
    .(
      LLS_id,
      year,
      title,
      author_first,
      journal,
      abstract
    )
  ] %>%
    unique()

  # Convert quanteda dfm to stm input (documents, vocab, meta)
  corpora_in_dfm[[as.character(threshold)]] <- quanteda::convert(
    corpus_in_dfm,
    to = "stm",
    docvars = metadata
  )
}

# Save prepared corpora for the background modelling script.
write_rds(
  corpora_in_dfm,
  here::here(data_path, "topic_modelling", "corpora.rds"),
  compress = "gz"
)


#: Launch background modelling job ---------
# The heavy `stm` fits are dispatched to `background_topic_model.R` to avoid
# blocking the interactive session. This script should read the `corpora.rds`
# file and save results to `many_stm.rds` (convention used below).
rstudioapi::jobRunScript(
  "background_topic_model.R",
  importEnv = TRUE,
  exportEnv = "R_GlobalEnv"
)


#: Evaluate fitted topic models ---------
# Read the result produced by the background job. The background script is
# expected to write `many_stm.rds` to the same `topic_modelling` folder.
many_stm <- read_rds(here::here(data_path, "topic_modelling", "many_stm.rds"))

# Convert to data.table for fast manipulations and attach corpora list by K
setDT(many_stm)
many_stm[, corpus_in_stm := corpora_in_dfm, by = K]

# Compute evaluation metrics for each fitted model
# - exclusivity: how unique are the top words per topic
# - semantic_coherence: typical coherence measure for STM (Mimno)
# - heldout likelihood: using a held-out set for predictive fit
# - residual dispersion and lower bound to assess fit quality
many_stm[, exclusivity := map(models, exclusivity)]
many_stm[, exclusivity := map_dbl(exclusivity, mean)]
many_stm[,
  semantic_coherence := map2(
    models,
    corpus_in_stm,
    ~ semanticCoherence(.x, .y$documents)
  )
]
many_stm[, semantic_coherence := map_dbl(semantic_coherence, mean)]
many_stm[,
  heldout := map(corpus_in_stm, ~ make.heldout(.x$documents, .x$vocab))
]
many_stm[,
  heldout_likelihood := map2_dbl(
    models,
    heldout,
    ~ eval.heldout(.x, .y$missing)$expected.heldout
  )
]
many_stm[,
  residual := map2(models, corpus_in_stm, ~ checkResiduals(.x, .y$documents))
]
many_stm[, residual := map_dbl(residual, "dispersion")]
many_stm[,
  lbound := map_dbl(models, function(x) {
    max(x$convergence$bound) + lfactorial(x$settings$dim$K)
  })
]

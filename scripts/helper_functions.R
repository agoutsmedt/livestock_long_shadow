#' ---
#' title: "List of the functions for text preprocessing and topic modelling"
#' author: "Aurélien Goutsmedt"
#' date: "/ Last compiled on `r format(Sys.Date())`"
#' output: 
#'   github_document:
#'     toc: true
#'     number_sections: true
#' ---

#+ r setup, include = FALSE
knitr::opts_chunk$set(eval = FALSE)

#' # What is this script for?
#' 
#' This script lists all the functions built for network analysis for the project. The functions are documenting in a way to be
#' later implemented in packages or at least for generating a help page (see below).
#' 
#' We first load the [docstring](https://cran.r-project.org/web/packages/docstring/vignettes/docstring_intro.html) package
#' to be able to write documentation in a roxygen2 style. You can then run docstring(name_of_function) to get the standard help page.

if ("docstring" %in% installed.packages() == FALSE) {
  install.packages("docstring", dependencies = TRUE)
}
library(docstring)

#' # Transforming text as data
#' 
#' ## Function for filtering the features selected
#' 
filter_terms <- function(data, upper_share = 1,
                         lower_share = 0, 
                         min_word = 0, 
                         max_word = Inf,
                         prop_word = 1,
                         document_name = "document",
                         term_name = "term") {
  
  data.table::setDT(data)
  data_dt <- copy(data)
  data.table::setnames(data_dt, 
                       c(document_name, term_name),
                       c("document", "term"))
  setkey(data_dt, document)
  nb_doc <- length(unique(data_dt$document))
  
  count_unigram <- data_dt %>% 
    filter(ngram == "unigram") %>% 
    group_by(document) %>% 
    add_count(name = "nb_words") %>% 
    distinct(document, nb_words) 
  data_dt <- merge(data_dt, count_unigram, by = "document")
  
  data_filtered <- data_dt %>% 
    .[, count := .N, by = term] %>%
    .[, count_per_doc := .N, by = c("term", "document")] %>% 
    unique() %>% 
    .[, nb_apparition := .N/nb_doc, by = term] %>% 
    .[between(nb_apparition, lower_share, upper_share) & between(nb_words, min_word, max_word)] %>% 
    {if(prop_word != 1) slice_max(., order_by = count, prop = prop_word) else . }
  
  return(data_filtered)
}

#' # Functions for creating the data table for different pre-processing criteria
#' 
create_topicmodels_dataset <- function(tuning_parameters, 
                                       data, 
                                       document_name = "document", 
                                       term_name = "term",
                                       verbose = TRUE) {
  data_list <- list()
  for(i in 1:nrow(hyper_grid)) {
    data_filtered <- filter_terms(data,
                                  upper_share = hyper_grid$upper_share[i],
                                  lower_share = hyper_grid$lower_share[i],
                                  min_word = hyper_grid$min_word[i],
                                  max_word = hyper_grid$max_word[i],
                                  prop_word = hyper_grid$prop_word[i],
                                  document_name = document_name,
                                  term_name = term_name)
    data_filtered <- as_tibble(data_filtered) %>% 
      mutate(upper_share = hyper_grid$upper_share[i],
             lower_share = hyper_grid$lower_share[i],
             min_word = hyper_grid$min_word[i],
             max_word = hyper_grid$max_word[i],
             prop_word = hyper_grid$prop_word[i])
    
    data_list[[i]] <- data_filtered
    
    if(verbose == TRUE) {
      message(paste0("Hyper grid ", i, 
                     " completed. The resulting data frame has ", 
                     nrow(data_filtered),
                     "rows."))
    }
    
  }
  
  data_set <- data_list %>% 
    bind_rows() %>%
    mutate(document = as.character(document)) %>% 
    nest(data = 1:(which(colnames(data_filtered) == "upper_share") - 1))
}

#' # Functions for Topic-Modelling
#' 
#' ## Creating stm object from a set of data with different preprocessing criteria
#' 
create_stm <- function(data, min_word_number = 200){ 
  data_set <- data %>% 
    mutate(dfm = furrr::future_map(data, 
                                   ~tidytext::cast_dfm(data = ., document, term, count_per_doc))) %>% 
    mutate(stm = furrr::future_map(dfm, 
                                   ~quanteda::convert(x = ., to = "stm")),
           preprocessing_id = row_number())
  
  list_document <- list()
  list_vocab <- list()
  for(i in 1:nrow(data_set)) {
    doc <- data_set$stm[[i]][[1]]
    vocab <- data_set$stm[[i]][[2]]
    
    list_document[[i]] <- doc
    list_vocab[[i]] <- vocab
    
  }
  
  data_set <- data.table::data.table(data_set, list_document, list_vocab) %>% 
    rename(document = list_document,
           vocab = list_vocab)
  
  # removing data sets with two few vocabularies (if vocab inferior to the number
  # of topics, the algorithm through an error)
  too_few_vocab <- which(map_dbl(data_set$vocab, length) < min_word_number)
  if(length(too_few_vocab) > 0){
    data_set <- data_set[-which(map_dbl(data_set$vocab, length) < min_word_number)]
  }
  return(data_set)
}

#' ## Creating several topic models for different preprocessing criteria and different number of topics
#' 
create_many_models <- function(data, nb_topics, max.em.its, seed, verbose = TRUE) {
  list_models <- list()
  for(i in 1:nrow(data_set)) {
    topic_model <- tibble(K = nb_topics) %>%
      mutate(topic_model = furrr::future_map(K, 
                                             ~stm(data$stm[[i]][[1]],
                                                  data$stm[[i]][[2]],
                                                  init.type = "Spectral",
                                                  K = .,
                                                  max.em.its = max.em.its,
                                                  seed = seed),
                                             .progress = TRUE,
                                             .options = furrr_options(seed = seed)))
    list_models[[i]] <- topic_model 
    if(verbose == TRUE) {
      message(paste0("Topic Models for Preprocessing number", 
                     i, 
                     " completed."))
    }
  }
  
  many_models <- data.table(data_set, list_models) %>% 
    unnest(cols = list_models)
  
}

#' ## Producing a list of statistics for each topic model
#' 
stm_results <- function(data){
  
  if(exists("tuning_results")){
    rm(tuning_results, envir = .GlobalEnv)
    message("The object `tuning_results` has been overwritten")
  }
  try(
    tuning_results <- data %>% 
      mutate(exclusivity = map(topic_model, exclusivity),
             semantic_coherence = map(topic_model, semanticCoherence(model = ., documents = document)),
             heldout = map2(document, vocab, make.heldout),
             residual = map2(topic_model, document, checkResiduals),
             bound =  map_dbl(topic_model, function(x) max(x$convergence$bound)),
             lfact = map_dbl(topic_model, function(x) lfactorial(x$settings$dim$K)),
             lbound = bound + lfact) %>% 
      select(-bound, -lfact) %>% 
      mutate(residual = map_dbl(residual, "dispersion"),
             semantic_coherence = map_dbl(semantic_coherence, mean),
             exclusivity = map_dbl(exclusivity, mean)),
    silent = TRUE)
  
  
  if(! exists("tuning_results")){
    tuning_results <- data %>% 
      mutate(exclusivity = map(topic_model, exclusivity),
             heldout = map2(document, vocab, make.heldout),
             bound =  map_dbl(topic_model, function(x) max(x$convergence$bound)),
             lfact = map_dbl(topic_model, function(x) lfactorial(x$settings$dim$K)),
             lbound = bound + lfact) %>% 
      select(-bound, -lfact)
    
    list_coherence <- list()
    list_residual <- list()
    
    for(i in 1:nrow(many_models)) {
      coherence <- semanticCoherence(model = data$topic_model[[i]], documents = data$document[[i]])
      residual <- checkResiduals(stmobj = data$topic_model[[i]], documents = data$document[[i]])
      list_coherence[[i]] <- coherence
      list_residual[[i]] <- residual
    }
    
    tuning_results <- data.table(tuning_results, list_coherence, list_residual)
    tuning_results <- tuning_results %>% 
      mutate(residual = map_dbl(list_residual, "dispersion"),
             semantic_coherence_mean = map_dbl(list_coherence, mean),
             exclusivity_mean = map_dbl(exclusivity, mean)) %>% 
      select(-list_coherence, -list_residual)
  }
  
  
  list_heldout <- list()
  for(i in 1:nrow(tuning_results)) {
    topic_model <- tuning_results$topic_model[[i]]
    missing_heldout <- tuning_results$heldout[[i]]$missing 
    eval_heldout <- eval.heldout(topic_model, missing_heldout)
    list_heldout[[i]] <- eval_heldout$expected.heldout
  }
  
  tuning_results <- cbind(tuning_results, list_heldout) %>% 
    rename(heldout_likelihood = list_heldout) %>% 
    mutate(heldout_likelihood = as.double(heldout_likelihood))
}

#' ## Harmonic mean of coherence and exclusivity
#' 
#' This will be used in the next function to build the harmonic mean between coherence and exclusivity
harmonic_mean_exclusivity_coherence <- function(weight, semantic_coherence_mean, exclusivity_mean) {
  measure <- (weight/semantic_coherence_mean + (1 - weight)/exclusivity_mean)^(-1)
}

#' ## Building multiple plots for summing up the topic models statistics
#' 
plot_topicmodels_stat <- function(data, size = 1, weight_1 = 0.5, weight_2 = 0.3, nb_terms = 10){
  
  results_summary <- data %>%
    transmute(K,
              word_threshold,
              residual,
              semantic_coherence,
              heldout_likelihood,
              exclusivity,
              lbound) %>%
    gather(Metric, Value, -c(word_threshold, K))
  
  plot_summary <- ggplot(results_summary, aes(K, Value, color = as.factor(word_threshold))) +
    geom_point(size = size) + 
    geom_line(linewidth = size, alpha = 0.8) +
    facet_wrap(~Metric, scales = "free_y", ncol = 3) +
    theme_bw() +
    labs(x = "K (number of topics)",
         y = NULL,
         title = "Model diagnostics by number of topics and preprocessing type") +
    theme(legend.position = "bottom")
  
  plot_exclusivity_coherence <- data %>% 
    mutate(id = paste0(word_threshold, "-", K)) %>% 
    ggplot(aes(semantic_coherence, exclusivity, group = id, color = as.factor(K))) +
    geom_label(aes(label = as.factor(word_threshold))) +
    theme_bw() +
    labs(x = "Semantic Coherence",
         y = "Exclusivity",
         title = "Exclusivity versus Coherence by number of topics and preprocessing type")
  
  data[, frex_data_1 := map(models, average_frex, w = weight_1, nb_terms = nb_terms)]
  data[, frex_data_2 := map(models, average_frex, w = weight_2, nb_terms = nb_terms)]
  mix_measure <- data[, .(word_threshold, K, frex_data_1, frex_data_2)]
  setnames(mix_measure, c("frex_data_1","frex_data_2"), c(paste0("frex_mean_",weight_1), paste0("frex_mean_",weight_2)))
  
  plot_mix_measure <- mix_measure %>% 
    pivot_longer(cols = starts_with("frex"), names_to = "measure", values_to = "measure_value") %>% 
    mutate(measure_value = unlist(measure_value)) %>% 
    ggplot(aes(K, measure_value, color = as.factor(word_threshold), group = as.factor(word_threshold))) +
    geom_point(size = size, alpha = 0.7) +
    geom_line() +
    facet_wrap(~measure, scales = "free_y") +
    theme_bw() +
    labs(x = "Number of topics",
         y = "Frex mean",
         title = "Frex mean value for different number of topics and preprocessing steps")
  
  plot_list <- list("summary" = plot_summary, 
                    "exclusivity_coherence" = plot_exclusivity_coherence, 
                    "exclusivity_coherence_mean" = plot_mix_measure)
}

#' ## Calculate FREX measure
#' 
#' Inspired by the STM package but a bit revisited to fit with our goals

calculate_frex <- function(model, nb_terms = nb_terms, w = w) {
  logbeta <- model$beta$logbeta[[1]]
  
  col.lse <- function(mat) {
    matrixStats::colLogSumExps(mat)
  }
  
  excl <- t(t(logbeta) - col.lse(logbeta))
  freqscore <- apply(logbeta,1,data.table::frank)/ncol(logbeta)
  exclscore <- apply(excl,1,data.table::frank)/ncol(logbeta)
  frex <- 1/(w/freqscore + (1-w)/exclscore)
  frex <- data.table("term" = model$vocab,
                     as.data.table(frex)) %>% 
    pivot_longer(cols = starts_with("V"), 
                 names_to = "topic", 
                 values_to = "frex") %>%
    mutate(topic = as.integer(str_remove(topic, "V"))) %>% 
    group_by(topic) %>% 
    slice_max(frex, n = nb_terms) %>% 
    select(topic, term, frex) %>% 
    mutate(rank = row_number(),
           mean = mean(frex))
}

#' Calculate Frex mean
#' 
average_frex <- function(model, nb_terms = nb_terms, w = w) {
  frex_mean <- calculate_frex(model, nb_terms = nb_terms, w = w) %>% 
    select(topic, mean) %>% 
    unique()
  frex_mean <- mean(frex_mean$mean)
} 

#' ## Calculate Beta (Highest probability of a word per topic):
#' 
calculate_beta <- function(model, nb_terms = 10) {
  beta_value <- tidytext::tidy(model, matrix = "beta") %>% 
    filter(! is.na(topic) & ! is.na(term)) %>% 
    group_by(topic) %>% 
    slice_max(beta, n = nb_terms) %>% 
    mutate(rank = row_number())
}

#' ## Calculate Score measure (cf. stm package):
#' 
calculate_score <- function(model, nb_terms = 10) {
  logbeta <- model$beta$logbeta[[1]]
  
  ldascore <- exp(logbeta)*(logbeta - rep(colMeans(logbeta), each=nrow(logbeta)))
  ldascore <- t(ldascore)
  ldascore <- data.table("term" = model$vocab,
                         as.data.table(ldascore)) %>%
    pivot_longer(cols = starts_with("V"), 
                 names_to = "topic", 
                 values_to = "score") %>% 
    mutate(topic = as.integer(str_remove(topic, "V"))) %>% 
    group_by(topic) %>% 
    slice_max(score, n = nb_terms) %>% 
    select(topic, term, score) %>% 
    mutate(rank = row_number())
}

#' ## Calculate Lift measure (cf. stm package):
#' 
calculate_lift <- function(model, list_terms, nb_terms = 10) {
  wordcounts <- list_terms %>% select(term, count) %>% unique() %>% arrange(term)
  logbeta <- model$beta$logbeta[[1]]
  emp.prob <- log(wordcounts$count) - log(sum(wordcounts$count))
  lift <- logbeta - rep(emp.prob, each=nrow(logbeta)) 
  lift <- t(lift)
  lift <- data.table("term" = model$vocab,
                     as.data.table(lift)) %>% 
    pivot_longer(cols = starts_with("V"), 
                 names_to = "topic", 
                 values_to = "lift") %>%
    mutate(topic = as.integer(str_remove(topic, "V"))) %>% 
    group_by(topic) %>% 
    slice_max(lift, n = nb_terms) %>% 
    select(topic, term, lift) %>% 
    mutate(rank = row_number())
}

#' ## Extract top terms for 4 different measures (beta, FREX, score, lift)
#' 
extract_top_terms <- function(model, list_terms, nb_terms = 10, frexweight = 0.5) {
  frex_value <- calculate_frex(model, nb_terms = nb_terms, w = frexweight) %>% 
    mutate(measure = "frex") %>% 
    rename(value = frex) %>% 
    select(-mean)
  beta_value <- calculate_beta(model, nb_terms = nb_terms) %>% 
    mutate(measure = "beta") %>% 
    rename(value = beta)
  score_value <- calculate_score(model, nb_terms = nb_terms) %>% 
    mutate(measure = "score") %>% 
    rename(value = score)
  lift_value <- calculate_lift(model, list_terms = list_terms, nb_terms = nb_terms)%>% 
    mutate(measure = "lift") %>% 
    rename(value = lift)
  top_terms <- rbind(frex_value, beta_value, score_value, lift_value)
}


#' # Studying a topic model
#' 
#' Many functions that follow are used to transform the data produce by the 
#' `stm` package in data.frame/data.table to be used in `ggplot2`/`ggraph`.
#' 
#' ## Plot top beta words
#' 
#' > Deprecated

plot_beta_value <- function(model, nb_terms = 10){
  beta_top_terms <- tidytext::tidy(model, matrix = "beta") %>% 
    group_by(topic) %>%
    slice_max(beta, n = nb_terms) %>% 
    ungroup() %>%
    arrange(topic, -beta)
  
  beta_top_terms %>%
    mutate(term = reorder_within(term, beta, topic)) %>%
    ggplot(aes(beta, term, fill = factor(topic))) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~ topic, scales = "free") +
    scale_y_reordered() 
}

#' ## Extract top terms for different measures
#' 
#' Here we extract the top terms per topic for the four 
#' measures calculated in the `stm` package. It serves to 
#' better describe the topics and to used other measures for
#' labelling the topic if we don't want to use beta (the "prob" measure).
#' 
#' > Deprecated

extract_top_terms_old <- function(model, 
                                  nb_words = 20, 
                                  weight_frex = 0.4){
  terms <- stm::labelTopics(model, n = nb_words, frexweight = weight_frex)
  top_terms <- data.table("topic" = rep(1:model$settings$dim$K, each = nb_words),
                          "rank" = 1:nb_words)
  
  if(length(terms$topics) == 0)
    for(i in names(terms)[-5]){
      terms_per_stat <- as.data.frame(terms[[i]]) %>% 
        mutate(topic = row_number()) %>% 
        pivot_longer(cols = starts_with("V"), names_to = "rank", values_to = i) %>% 
        mutate(rank = as.integer(str_remove(rank, "V"))) 
      
      top_terms <- merge(top_terms, terms_per_stat, by = c("topic", "rank"))
    } else {
      top_terms <- as.data.frame(terms$topics) %>% 
        mutate(topic = row_number()) %>% 
        pivot_longer(cols = starts_with("V"), names_to = "rank", values_to = "frex") %>% 
        mutate(rank = as.integer(str_remove(rank, "V"))) %>% 
        filter(! frex == "") %>% 
        as.data.table()
    }
  return(top_terms)
}

#' ## Naming topics
#' 
name_topics <- function(data, 
                        method = c("beta", "frex", "score", "lift"), 
                        nb_word = 3) {
  labels <- data %>% 
    filter(rank <= nb_word & 
             measure == method) %>%
    select(topic, term, value) %>% 
    group_by(topic) %>%
    mutate(term_label = paste0(term, collapse = " / ")) %>% 
    ungroup() %>% 
    select(-value, -term) %>% 
    rename(id = topic) %>% 
    mutate(topic = paste0("Topic ", id),
           topic_name = paste0(topic, "\n", term_label)) %>% 
    unique()
}
#' ## Topic Correlation graph with ggraph
#' 

ggraph_topic_correlation <- function(model,
                                     nodes = NULL,
                                     id_column = "id",
                                     top_terms = NULL, 
                                     method = c("simple", "huge"), 
                                     size_label = 4,
                                     nb_topics = NULL,
                                     resolution = 1){
  correlation <- topicCorr(model, method = method)
  
  if(is.null(nodes)){
    nodes <- name_topics(top_terms, "frex", nb_word = 4)
  }
  
  edges <- as.data.frame(as.matrix(correlation$poscor)) %>% 
    mutate(from = row_number()) %>% 
    pivot_longer(cols = 1:nb_topics, names_to = "to", values_to = "weight") %>% 
    mutate(to = str_remove(to, "V"),
           from = as.character(from)) %>% 
    filter(weight != 0) %>% 
    unique()
  
  nodes <- nodes %>% 
    mutate(id = as.character(id))
  
  graph_corr <- tbl_graph(nodes = nodes, edges = edges, directed = FALSE)
  graph_corr <- leiden_workflow(graph_corr, res_1 = resolution, niter = 3000)
  graph_corr <- community_colors(graph_corr, palette = scico(n = length(unique((V(graph_corr)$Com_ID))), palette = "roma", begin = 0.1))
  graph_corr <- vite::complete_forceatlas2(graph_corr, first.iter = 5000)
  graph_plot <- ggraph(graph_corr, layout = "manual", x = x, y = y) +
    geom_edge_arc0(aes(color = color_edges, width = weight), strength = 0.3, alpha = 0.8, show.legend = FALSE) +
    scale_edge_width_continuous(range = c(0.5,12)) +
    scale_edge_colour_identity() +
    scale_fill_identity() +
    geom_node_label(aes(label = topic_name, fill = color), size = size_label, alpha = 0.7)
  
  list <- list("graph" = graph_corr,
               "plot" = graph_plot)
  return(list)
}

#' ## Plot topic frequency
#' 
plot_frequency <- function(topics_data, model, palette = color){
  topics_data <- topics_data %>% 
    arrange(id) %>% 
    mutate(frequency = colMeans(model$theta)) %>% 
    as.data.table()
  topics_data$topic <- factor(topics_data$topic, levels = topics_data[order(frequency)]$topic)
  
  ggplot(topics_data, aes(x = frequency, y = topic,
                          group = factor(topic_name),
                          color = {{palette}})) +
    geom_segment(aes(x = 0, xend = frequency, yend = topic), size = 4, show.legend = FALSE) +
    theme(legend.position = "none") +
    geom_label(aes(x = frequency, color = {{palette}}, label = term_label), size = 4, alpha = 1, hjust = -0.01) +
    scale_color_identity() +
    expand_limits(x = c(0, max(topics_data$frequency) + 0.015)) +
    labs(title = "Top Topics",
         x = "Frequency",
         y = "") +
    theme_bw()
}

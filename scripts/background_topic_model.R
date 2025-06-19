#run multiple topic models 
library(magrittr)
library(stm)

seed <- 123
many_stm <- tibble::tibble(
  K = seq(30, 100, by = 10),
  word_threshold = list(names(corpora_in_dfm))) %>% 
  tidyr::unnest(cols = c(K, word_threshold)) %>% 
  dplyr::mutate(models = purrr::map2(
    K,
    word_threshold,
    ~ stm::stm(
        documents = corpora_in_dfm[[.y]]$documents,
        vocab = corpora_in_dfm[[.y]]$vocab,
        data = corpora_in_dfm[[.y]]$meta,
        prevalence = as.formula("~s(year)"),
        K = .x,
        init.type = "Spectral",
        max.em.its = 800,
        verbose = TRUE,
        seed = seed
      ),
    .progress = TRUE,
  ))


readr::write_rds(many_stm, here::here(data_path, "topic_modelling", "many_stm.rds"), compress = "gz")

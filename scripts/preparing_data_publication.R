source(here::here("scripts", "paths_and_packages.R"))
source(here::here("scripts", "helper_functions.R"))
pacman::p_load(tidytext)

documents <- read_rds(file.path(data_path, "documents.rds"))
alluvial <- read_rds(file.path(data_path, glue("alluvial_round2_coupling_similarity_2_5_0.6.rds")))
community_names <- readxl::read_xlsx(file.path(data_path, "Communities_2_0.6_5_Summary.xlsx"), sheet = "Feuil1") %>%
  janitor::clean_names() %>% 
  mutate(dynamic_cluster_leiden = tolower(id)) %>% 
  rename(thematic_grouping = first_order_thematic_grouping) %>%
  left_join(readxl::read_xlsx(file.path(data_path, "Communities_2_0.6_5_Summary.xlsx"), sheet = "Labels") %>% 
              janitor::clean_names()) %>% 
  mutate(extended_name = str_remove(extended_name, "( related)? communities$"),
         extended_name = if_else(str_detect(extended_name, "Ethics"), NA_character_, extended_name)) %>% 
  select(dynamic_cluster_leiden, thematic_group_name = extended_name) %>% 
  unique()

# Loading topic modelling data data
topic_model <- read_rds(here::here(data_path, "topic_modelling", "many_stm.rds")) %>%
  filter(K == 40 & word_threshold == "20") %>% 
  pull(models) %>% 
  pluck(1)

metadata <- corpus$meta %>% 
  as_tibble() %>%
  mutate(document = row_number())

kmeans_cluster <- readRDS(here::here(data_path, "topic_modelling", "kmeans_4_clusters.rds")) %>% 
  as_tibble() %>% 
  mutate(topic = as.character(topic) %>% as.integer)

# Preparing topic modelling data
gamma_tm <- tidy(topic_model, matrix = "gamma") %>%
  left_join(select(metadata, citing_id = LLS_id, document), by = "document") %>% 
  left_join(select(documents, citing_id, doi), by = "citing_id") %>% 
  left_join(kmeans_cluster, by = "topic") %>% 
  arrange(topic, desc(gamma)) %>% 
  select(doi, topic_name = label_beta, kmeans_cluster = cluster, gamma) %>% 
  unique()
write_csv(gamma_tm, file.path(data_path, "topic_modelling", "topic_modelling_results.csv"), na = "")

# Preparing Alluvial Data
alluvial[, citing_id := as.integer(citing_id)]

biblio_coupling_data <- alluvial %>%
  left_join(community_names) %>% 
  left_join(select(documents, citing_id, doi), by = "citing_id") %>% 
  select(doi, window, bibliographic_community = dynamic_cluster_leiden, thematic_group_name, 
         share_of_nodes = share_cluster_alluv, eigenvector_centrality, participation_coefficient) %>% 
  arrange(thematic_group_name, desc(share_of_nodes), desc(eigenvector_centrality))
write_csv(biblio_coupling_data, file.path(data_path, "bibliographic_coupling_results.csv"), na = "")

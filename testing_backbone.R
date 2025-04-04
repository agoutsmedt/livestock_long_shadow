# Testing backbone
library(unix)
rlimit_as(cur = 14000000000, max = 12737418240)
networks <- build_dynamic_networks2(nodes,
                                    direct_citations,
                                    "citing_id",
                                    "cited_id",
                                    backbone_method = "statistical",
                                    statistical_method = "fdsm",
                                    alpha = 0.05,
                                    time_variable = "year",
                                    time_window = 5,
                                    overlapping_window = TRUE,
                                    filter_components = TRUE,
                                    compute_size = TRUE) 

networks %>% 
  add_clusters(clustering_method = "leiden",
               objective_function = "modularity",
               resolution = resolution,
               seed = 1234
  ) %>%
  merge_dynamic_clusters(cluster_id = "cluster_leiden",
                         node_id = "citing_id",
                         threshold_similarity = 0.55,
                         similarity_type = "partial") %>% 
  name_clusters(method = "tf-idf",
                cluster_id = "dynamic_cluster_leiden",
                name_merged_clusters = TRUE,
                text_columns = c("title", "abstract"),
                clean_word_method = "lemmatise",
                n_gram = 3,
                nb_terms_label = 5
  )

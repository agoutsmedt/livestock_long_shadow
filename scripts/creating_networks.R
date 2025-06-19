# Creating the list of networks
networks <- build_dynamic_networks(nodes,
                          direct_citations,
                          "citing_id",
                          "cited_id",
                          cooccurrence_method = method,
                          time_variable = "year",
                          time_window = window,
                          overlapping_window = TRUE,
                          filter_components = TRUE,
                          compute_size = TRUE,
                          edges_threshold = threshold) %>% 
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

# Creating the alluvial
cli::cli_alert_info("Creating alluvial")
alluvial <- networks %>% 
  networks_to_alluv(intertemporal_cluster_column = "dynamic_cluster_leiden",
                    node_id = "citing_id")

colors <- alluvial %>% 
  select(dynamic_cluster_leiden, cluster_label) %>% 
  arrange(desc(cluster_label)) %>% 
  distinct(dynamic_cluster_leiden) %>% 
 # mutate(color = scico::scico(palette = "roma", n = 79) %>% sample()) 
  mutate(color = scico::scico(palette = "roma", n = n())) 

alluvial <- alluvial %>%
  left_join(colors, by = "dynamic_cluster_leiden")

# Preparing the plot

alluvial_prepared <- alluvial %>% 
  mutate(cluster_label = str_c(dynamic_cluster_leiden, " - ", cluster_label)) %>% 
  prepare_label_alluvial(window_column = "window",
                         cluster_label_column = "cluster_label") %>% 
  networkflow::minimize_crossing_alluvial(intertemporal_cluster_column = "dynamic_cluster_leiden",
                                          node_id = "citing_id") 

# Plot
cli::cli_alert_info("Plotting alluvial")
alluvial_prepared <- alluvial_prepared %>%
  mutate(label_x = if_else(share_cluster_max < 4, NA, label_x),
         color = if_else(share_cluster_max < 4, "gray", color),
         label_x = str_wrap(label_x, 20)) 

plot_alluvial <- alluvial_prepared %>% 
  plot_alluvial(intertemporal_cluster_column = "dynamic_cluster_leiden",
                node_id = "citing_id",
                window_column = "window",
                color_column = "color",
                color_alluvial = FALSE,
                color = NULL,
                minimize_crossing = FALSE,
                prepare_label = FALSE,
                cluster_label_column = "cluster_label",
                print_plot_code = TRUE) +
  theme_minimal(base_size = 25) +
  labs(y = "Proportion of the network",
       x = NULL)

ggsave(glue("pictures/alluvial_round2_{method}_{threshold}_{window}_{resolution}.png"), plot_alluvial, width = 30, height = 20)  

# Adding data on nodes centrality
cli::cli_alert_info("Adding data on nodes centrality and participation coefficient")
centrality_data <- networks %>% 
  map(~ .x %N>% 
        mutate(eigenvector_centrality = centrality_eigen()) %>% 
        select(citing_id, eigenvector_centrality) %>% 
        as_tibble()) %>% 
  bind_rows(.id = "window")

# Calculate participation coefficient
participation_coefficient <- networks %>% 
  map(~ .x %N>%
        mutate(total_degree = centrality_degree(mode = "all")) %>%  # Total degree of the node
        mutate(participation_coefficient = sapply(1:n(), function(i) {
          neighbors <- neighbors(.x, i, mode = "all") # Get neighbors
          if(length(neighbors) > 3) { # filter by minimum number of neighbors
            neighbor_communities <- V(.x)[neighbors]$cluster_leiden # Get communities of neighbors
            community_counts <- table(neighbor_communities) # Count connections per community
            1 - sum((community_counts / total_degree[i])^2) # Participation coefficient formula 
          } else {
            return(NA)
          }
        })
        ) %>% 
  as_tibble() %>% 
  select(citing_id, participation_coefficient)) %>% 
  bind_rows(.id = "window")

alluvial_prepared <- alluvial_prepared %>% 
  left_join(centrality_data, by = c("citing_id", "window")) %>% 
  left_join(participation_coefficient, by = c("citing_id", "window"))

saveRDS(alluvial_prepared, file.path(data_path, glue("alluvial_round2_{method}_{threshold}_{window}_{resolution}.rds")))


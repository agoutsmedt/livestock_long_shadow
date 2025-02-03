source("paths_and_packages.R")

# Load data
documents <- read_rds(file.path(data_path, "documents.rds"))
direct_citations <- read_rds(file.path(data_path, "direct_citations.rds"))

# Creating nodes and direct_citations files

# Id of livestock report : "46698"
nodes <- documents[level_1 == TRUE,]
setnames(nodes, "LLS_id", "citing_id")
nodes[, cited_id := citing_id]
nodes[, year := as.integer(year)]

direct_citations <- direct_citations[citing_id %in% nodes$citing_id & cited_id != "46698",]

# Creating the list of networks
networks <- build_dynamic_networks(nodes,
                          direct_citations,
                          "citing_id",
                          "cited_id",
                          cooccurrence_method = "coupling_angle",
                          time_variable = "year",
                          time_window = 5,
                          overlapping_window = TRUE,
                          filter_components = TRUE,
                          compute_size = TRUE,
                          edges_threshold = 2) %>% 
  add_clusters(clustering_method = "leiden",
               objective_function = "modularity",
               resolution = 1,
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
alluvial <- networks %>% 
  networks_to_alluv(intertemporal_cluster_column = "dynamic_cluster_leiden",
                    node_id = "citing_id")

colors <- alluvial %>% 
  select(dynamic_cluster_leiden, share_cluster_max) %>% 
  arrange(desc(share_cluster_max)) %>% 
  distinct(dynamic_cluster_leiden) %>% 
  mutate(color = scico::scico(palette = "roma", n = 79) %>% sample()) 

alluvial <- alluvial %>%
  left_join(colors, by = "dynamic_cluster_leiden")

# Preparing the plot

alluvial_prepared <- alluvial %>% 
  prepare_label_alluvial(window_column = "window",
                         cluster_label_column = "cluster_label") %>% 
  networkflow::minimize_crossing_alluvial(intertemporal_cluster_column = "dynamic_cluster_leiden",
                                          node_id = "citing_id")

plot_alluvial <- alluvial_prepared %>%
  mutate(label_x = if_else(share_cluster_max < 5, NA, label_x),
         color = if_else(share_cluster_max < 5, "gray", color)) %>% 
  plot_alluvial(intertemporal_cluster_column = "dynamic_cluster_leiden",
                node_id = "citing_id",
                window_column = "window",
                color_column = "color",
                color_alluvial = FALSE,
                color = NULL,
                minimize_crossing = FALSE,
                prepare_label = FALSE,
                cluster_label_column = "cluster_label",
                print_plot_code = TRUE)

ggsave("pictures/alluvial.png", plot_alluvial, width = 30, height = 20)  

# Finding clusters

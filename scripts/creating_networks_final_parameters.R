########### Running networks and reports #############
source("scripts/paths_and_packages.R")

# Load data
nodes <- read_rds(file.path(data_path, "documents.rds"))
direct_citations <- read_rds(file.path(data_path, "direct_citations.rds"))
community_names <- read_csv(file.path(data_path, "naming_communities_2_0.6_5.csv")) %>%
  mutate(dynamic_cluster_leiden = tolower(id))

community_names <- readxl::read_xlsx(file.path(data_path, "Communities_2_0.6_5_Summary.xlsx"), sheet = "Feuil1") %>%
  janitor::clean_names() %>% 
  mutate(dynamic_cluster_leiden = tolower(id)) %>% 
  rename(thematic_grouping = first_order_thematic_grouping) %>%
  left_join(readxl::read_xlsx(file.path(data_path, "Communities_2_0.6_5_Summary.xlsx"), sheet = "Labels") %>% 
              janitor::clean_names()) %>% 
  mutate(extended_name = str_remove(extended_name, "( related)? communities$"),
         extended_name = if_else(str_detect(extended_name, "Ethics"), NA_character_, extended_name)) %>% 
  mutate(rank_extended_name = 1:n(), .by = extended_name)

# Creating direct_citations files
direct_citations <- direct_citations[citing_id %in% nodes$citing_id & cited_id != "46698",]


# Setting the final parameters for analysis
method <- "coupling_similarity"
threshold <- 2
resolution <- 0.6
window <- 5

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
                    node_id = "citing_id") %>% 
  left_join(community_names)
# 
# colors <- alluvial %>% 
#   select(dynamic_cluster_leiden, label) %>% 
#   arrange(desc(label)) %>% 
#   filter(!is.na(label)) %>% 
#   distinct(dynamic_cluster_leiden) %>% 
#   # mutate(color = scico::scico(palette = "roma", n = 79) %>% sample()) 
#   mutate(color = scico::scico(palette = "roma", n = n(), begin = 0.1, end = 0.9)) 

alluvial <- alluvial %>%
 # left_join(colors, by = "dynamic_cluster_leiden") %>% 
  mutate(cluster_label = str_c(dynamic_cluster_leiden, " - ", label)) 

# Preparing the plot

# Put the label at the beginning of the new community
label <- data.table::copy(alluvial)
label <- label[, .N, .(cluster_label, window)] %>%
  .[, label_alluvial := window == first(window), .(cluster_label)] %>%
  .[label_alluvial == TRUE] %>%
  distinct(cluster_label, window) %>%
  mutate(label_x = cluster_label)

alluvial_prepared <- alluvial %>% 
  left_join(label) %>% 
  networkflow::minimize_crossing_alluvial(intertemporal_cluster_column = "dynamic_cluster_leiden",
                                          node_id = "citing_id")

# Plot general alluvial
cli::cli_alert_info("Plotting alluvial")
# alluvial_prepared <- alluvial_prepared %>%
#   mutate(label_x = if_else(share_cluster_max < 5, NA, label_x),
#          color = if_else(share_cluster_max < 5, "gray", color),
#          label_x = str_wrap(label_x, 25)) 

color_narrative <- tibble(extended_name = (unique(community_names$extended_name))) %>% 
  filter(!is.na(extended_name)) %>%
  mutate(color = see::oi_colors()[1:7])

alluvial_prepared_narrative <- alluvial_prepared %>% 
  # select(-color) %>% 
  left_join(color_narrative)

alluvial_prepared_narrative <- alluvial_prepared_narrative %>%
  mutate(label_x = if_else(color == "gray", NA, label_x),
         label_x = str_remove(label_x, "cl_"),
         label_x = str_wrap(label_x, 25)) 

alluvial_prepared_narrative[["dynamic_cluster_leiden"]] <- forcats::fct_reorder(alluvial_prepared_narrative[["dynamic_cluster_leiden"]],
                                                                                alluvial_prepared_narrative[["minimize_crossing_order"]],
                                                                                min,
                                                                                .desc = TRUE)

plot_alluvial_narrative <- alluvial_prepared_narrative %>% 
  ggplot2::ggplot(ggplot2::aes(x = window, y = y_alluv, 
                               stratum = dynamic_cluster_leiden, alluvium = citing_id, 
                               fill = color, alpha = rank_extended_name)) + 
  ggalluvial::geom_stratum(show.legend = TRUE) + 
  ggalluvial::geom_flow(show.legend = FALSE) + 
  ggplot2::scale_fill_identity("Main Research\nStrands",
                               guide = "legend", 
                               labels = str_wrap(color_narrative$extended_name, 32),
                               breaks = color_narrative$color) +
  ggrepel::geom_label_repel(ggplot2::aes(label = label_x), 
                            stat = ggalluvial::StatStratum,
                            size = 4
  ) + 
  scale_alpha_continuous(range = c(1, 0.7), 
                     guide = "none") +
  ggplot2::theme_classic(base_size = 20) +
  theme(legend.position = "bottom") +
  labs(y = "Proportion of the network",
       x = NULL)

ggsave(glue("pictures/alluvial_{method}_{threshold}_{window}_{resolution}_final_narrative.png"), 
       plot_alluvial_narrative, 
       width = 30, height = 20,
       dpi = 300)

ggsave(glue("pictures/figure-2.png"), 
       plot_alluvial_narrative, 
       width = 18, height = 14,
       dpi = 300)

# Plot with focus on protein
plot_alluvial_focused <- alluvial_prepared_narrative %>% 
  mutate(label_x = if_else(str_detect(extended_name, "protein|Socio-|Sustainable"), label_x, NA),
         color = if_else(str_detect(extended_name, "protein|Socio-|Sustainable"), color, "gray")) %>% 
  ggplot2::ggplot(ggplot2::aes(x = window, y = y_alluv, 
                               stratum = dynamic_cluster_leiden, alluvium = citing_id, fill = color, alpha = rank_extended_name)) + 
  ggalluvial::geom_stratum(show.legend = TRUE) + 
  ggalluvial::geom_flow(show.legend = FALSE) + 
    ggplot2::scale_fill_identity("Main Research Strands",
                               guide = "legend", 
                               labels = str_wrap(color_narrative$extended_name, 50),
                               breaks = color_narrative$color) +
  ggrepel::geom_label_repel(ggplot2::aes(label = label_x), 
                            stat = ggalluvial::StatStratum,
                            size = 5
  ) + 
  scale_alpha_continuous(range = c(1, 0.7), 
                         guide = "none") +
  ggplot2::theme_classic(base_size = 20) +
  theme(legend.position = "bottom") +
  labs(y = "Proportion of the network",
       x = NULL)

ggsave(glue("pictures/alluvial_{method}_{threshold}_{window}_{resolution}_focused_narrative.png"), 
       plot_alluvial_focused, 
       width = 30, height = 20,
       dpi = 300)

ggsave(glue("pictures/figure-3.png"), 
       plot_alluvial_focused, 
       width = 18, height = 14,
       dpi = 300)

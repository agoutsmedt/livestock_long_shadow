# Preparing and running the topic model
# Loading packages and data------------------------
source("scripts/paths_and_packages.R")
source("scripts/helper_functions.R")
p_load(ggraph,
       ggiraph,
       stm,
       tidytext)

# Loading data
topic_model <- read_rds(here::here(data_path, "topic_modelling", "many_stm.rds")) %>% 
  filter(K == 40 & word_threshold == "20") %>% 
  pull(models) %>% 
  pluck(1)

corpus <- read_rds(here::here(data_path, "topic_modelling", "corpora.rds")) %>% 
  .[["20"]]

metadata <- corpus$meta %>% 
  as_tibble() %>%
  mutate(document = row_number())

# Working with the chosen topic model: basic description-----------
## Extracting statistics---------------------------
beta <- tidy(topic_model, matrix = "beta")
frex <- tidy(topic_model, matrix = "frex")
label_topic_beta <- beta %>% 
  slice_max(beta, n = 5, with_ties = FALSE, by = topic) %>% 
  mutate(label_beta = paste0("Topic ", topic, ": ", paste0(term, collapse = ", ")), .by = topic) %>% 
  distinct(topic, label_beta)
label_topic_frex <- frex %>% 
  slice(1:5, .by = topic) %>% 
  mutate(label_frex = paste0("Topic ", topic, ": ", paste0(term, collapse = ", ")), .by = topic) %>% 
  distinct(topic, label_frex)

gamma <- tidy(topic_model,
              matrix = "gamma") %>% 
  left_join(metadata, by = "document") %>% 
  left_join(label_topic_beta, by = "topic") %>% 
  left_join(label_topic_frex, by = "topic")

## Looking at prevalence-----------------------------
gamma_mean <- gamma %>%
  group_by(topic, label_beta, label_frex) %>%
  summarise(gamma = mean(gamma)) %>%
  ungroup %>% 
  mutate(topic = reorder(topic, gamma)) 

prevalence_beta <- gamma_mean %>%
  ggplot() +
  geom_segment(aes(x = 0, xend = gamma, y = topic, yend = topic), color = "black", linewidth = 0.5) +
  geom_text(aes(x = gamma, y = topic, label = label_beta), size = 6, hjust = -.01, nudge_y = 0.0005) +
  scale_x_continuous(expand = c(0, 0), limits = c(0, max(gamma_mean$gamma) + 0.05)) +
  theme_light() +
  theme(text = element_text(size = 20), 
        axis.text.y = element_blank(),  # Removes y-axis text
        axis.ticks.y = element_blank()) + 
  labs(x = "Prévalences moyennes des thématiques",
       y = NULL,
       caption = "\n\n Note: chaque thématique est associée à ses mots les plus probables selon la distribution beta")

ggsave(
  "stm_prevalence_beta.png",
  device = "png",
  plot = prevalence_beta,
  path = figures_path,
  width = 15,
  height = 15,
  dpi = 300
)

prevalence_frex <- gamma_mean %>%
  ggplot() +
  geom_segment(aes(x = 0, xend = gamma, y = topic, yend = topic), color = "black", linewidth = 0.5) +
  geom_text(aes(x = gamma, y = topic, label = label_frex), size = 6, hjust = -.01, nudge_y = 0.0005) +
  scale_x_continuous(expand = c(0, 0), limits = c(0, max(gamma_mean$gamma) + 0.05)) +
  theme_light() +
  theme(text = element_text(size = 20), 
        axis.text.y = element_blank(),  # Removes y-axis text
        axis.ticks.y = element_blank()) + 
  labs(x = "Prévalences moyennes des thématiques",
       y = NULL,
       caption = "\n\n Note: chaque thématique est associée à ses mots les plus fréquents selon la distribution frex")

ggsave(
  "stm_prevalence_frex.png",
  device = "png",
  plot = prevalence_frex,
  path = figures_path,
  width = 15,
  height = 15,
  dpi = 300
)

## Looking at topics description----------------
plot_beta <- beta %>% 
  slice_max(beta, n = 10, with_ties = FALSE, by = topic) %>% 
  mutate(topic_name = paste0("topic ", topic),
         topic = as.integer(topic),
         term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(beta, term)) +
  scale_fill_identity() +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ fct_reorder(topic_name, topic), scales = "free", ncol = 7) +
  scale_y_reordered() 

ggsave(
  "beta_topics.png",
  device = "png",
  plot = plot_beta,
  path = figures_path,
  width = 18,
  height = 14,
  dpi = 300
)

frex <- calculate_frex(topic_model, nb_terms = 10, w = 0.5)
plot_frex <- frex %>% 
  mutate(topic_name = paste0("topic ", topic),
         topic = as.integer(topic),
         term = reorder_within(term, frex, topic)) %>%
  ggplot(aes(frex, term)) +
  scale_fill_identity() +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ fct_reorder(topic_name, topic), scales = "free", ncol = 7) +
  scale_y_reordered() 

ggsave(
  "frex_topics.png",
  device = "png",
  plot = plot_beta,
  path = figures_path,
  width = 18,
  height = 14,
  dpi = 300
)

## Looking at correlations between topics----------------

### Network methods--------------
corr <- stm::topicCorr(topic_model, cutoff = 0.001)

# matrix to table
corr_table <- reshape2::melt(corr$cor) 
label_topic <- labelTopics(topic_model, n = 10)

nodes <- label_topic %>% 
  .[[1]] %>%
  as_tibble() %>%
  reframe(topic_label_prob = pmap_chr(., ~ paste(c(...), collapse = ", "))) %>%
  mutate(source_id = row_number() %>% as.factor()) %>% 
  left_join(gamma_mean %>% select(source_id = topic, gamma), by = "source_id")

edges <- corr_table %>% 
  dplyr::filter(Var1 != Var2) %>% 
  rename(source_id = Var1,
         target_id = Var2,
         weight = value) 

graph <- tidygraph::tbl_graph(nodes = nodes, edges = edges, directed = FALSE)

# fa 2
graph_layout <- vite::complete_forceatlas2(graph, first.iter = 50000, kgrav = 1)
write_rds(graph_layout, here::here(data_path, "topic_modelling", "graph_layout.rds"))

## PCA------
topic_vectors <- gamma %>%
  select(label_beta, document, gamma) %>%
  pivot_wider(names_from = document, values_from = gamma) %>%
  column_to_rownames("label_beta") %>%
  as.matrix()

pca_result <- prcomp(topic_vectors, 
                     scale. = TRUE)

# Squared standard deviations of the components
pca_var <- pca_result$sdev^2

# Proportion of variance for each PC
pca_var_explained <- pca_var / sum(pca_var)

plot(pca_var_explained, type = "b",
     xlab = "Principal Component", ylab = "Proportion of Variance Explained",
     main = "Scree Plot")

# Get PCA coordinates for topics
topic_coords <- as.data.frame(pca_result$x[, 1:40])
topic_coords$label_beta <- rownames(topic_coords)

topic_coords <- topic_coords %>% 
  left_join(gamma_mean)

#### K-means clustering on PCA scores----------------
set.seed(123)
library(cluster)

avg_silhouette <- function(k, data, runs = 100) {
  scores <- numeric(runs)
  for (i in 1:runs) {
    km <- kmeans(data, centers = k, nstart = 100, iter.max = 1000)
    sil <- silhouette(km$cluster, dist(data))
    scores[i] <- mean(sil[, 3])
  }
  mean(scores)
}

# Run this for k = 2 to 10
k_range <- 2:15
avg_sil_scores <- sapply(k_range, avg_silhouette, data = topic_coords[, 1:40], runs = 10)

tibble(k = k_range, avg_silhouette = avg_sil_scores) %>%
  ggplot(aes(k, avg_silhouette)) +
  geom_line(linewidth = 2) +
  geom_point(size = 3) +
  scale_x_continuous(breaks = k_range) +
  labs(title = NULL,
       x = "Number of Clusters (K)",
       y = "Average Silhouette Score") +
  theme_minimal(base_size = 22)

ggsave(
  "silhouette_scores.png",
  device = "png",
  path = figures_path,
  width = 18,
  height = 12,
  dpi = 300
)

ggsave(
  "figure-6.png",
  device = "png",
  path = figures_path,
  width = 18,
  height = 12,
  dpi = 300
)

for(k in c(2,4)){
set.seed(123)
kmeans_result <- kmeans(topic_coords[, 1:40], centers = k, nstart = 100, iter.max = 1000)
# Add cluster info to data
topic_coords$cluster <- as.factor(kmeans_result$cluster)

if(k == 2){
  color_order <- c(5, 6)
} else {
  color_order <- c(5, 7, 3, 6)
}

pca_plot <- topic_coords %>% 
  mutate(label = if_else(gamma > quantile(gamma, 0.4), str_wrap(label_beta, 20), topic)) %>% 
  ggplot(aes(PC1, PC2, label = label, color = cluster)) +
  geom_point(aes(size = gamma)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray") +
  scale_x_continuous(breaks = 0) +
  scale_y_continuous(breaks = 0) +
  ggrepel::geom_label_repel(vjust = -0.5, alpha = 0.9, size = 6) +
  labs(size = "Topic Prevalence",
       color = "K-means Clusters",
       x = glue("Principal Component 1 (variance = {round(pca_var_explained[1] * 100, 2)}%)"),
       y = glue("Principal Component 2 (variance = {round(pca_var_explained[2] * 100, 2)}%)")) + 
  see::scale_color_oi(order = color_order) +
  scale_size_continuous(range = c(1, 12)) +
  theme_classic(base_size = 22) +
  theme(legend.position = "bottom")

ggsave(
  paste0("topic_pca_", k, "_clusters.png"),
  device = "png",
  plot = pca_plot,
  path = figures_path,
  width = 18,
  height = 12,
  dpi = 300
)

if(k == 2){
  ggsave(
    "figure-7.png",
    device = "png",
    plot = pca_plot,
    path = figures_path,
    width = 18,
    height = 12,
    dpi = 300
  ) 
} else {
  ggsave(
    "figure-4.png",
    device = "png",
    plot = pca_plot,
    path = figures_path,
    width = 18,
    height = 12,
    dpi = 300
  )
}
}

### Association heatmap between STM clusters and bibliographic clusters----------------
alluvial <- read_rds(file.path(data_path, glue("alluvial_round2_coupling_similarity_2_5_0.6.rds")))
alluvial[, citing_id := as.integer(citing_id)]

community_names <- readxl::read_xlsx(file.path(data_path, "Communities_2_0.6_5_Summary.xlsx"), sheet = "Feuil1") %>%
  janitor::clean_names() %>% 
  mutate(dynamic_cluster_leiden = tolower(id)) %>% 
  rename(thematic_grouping = first_order_thematic_grouping) %>%
  left_join(readxl::read_xlsx(file.path(data_path, "Communities_2_0.6_5_Summary.xlsx"), sheet = "Labels") %>% 
              janitor::clean_names()) %>% 
  mutate(extended_name = str_remove(extended_name, "( related)? communities$"),
         extended_name = if_else(str_detect(extended_name, "Ethics"), NA_character_, extended_name)) %>% 
  mutate(rank_extended_name = 1:n(), .by = extended_name)

bibliographic_themes <- alluvial %>% 
  left_join(community_names, by = "dynamic_cluster_leiden") %>% 
  distinct(citing_id, extended_name)
  
df_counts <- gamma %>% 
  filter(gamma > 0.1) %>% 
  left_join(select(topic_coords, label_beta, cluster), by = "label_beta") %>%
  left_join(select(metadata, document, citing_id = LLS_id), by = "document") %>%
  left_join(bibliographic_themes, by = "citing_id") %>% 
  filter(! is.na(extended_name)) %>% 
  count(cluster, extended_name)

# Total documents
total_docs <- df_counts %>% summarise(total = sum(n)) %>% pull(total)

# Marginal totals
row_totals <- df_counts %>% group_by(extended_name) %>% summarise(row_sum = sum(n))
col_totals <- df_counts %>% group_by(cluster) %>% summarise(col_sum = sum(n))

# Merge and compute expected counts
df_expected <- df_counts %>%
  left_join(row_totals, by = "extended_name") %>%
  left_join(col_totals, by = "cluster") %>%
  mutate(expected = (row_sum * col_sum) / total_docs,
         ratio = n / expected)

# Log ratio for interpretability (positive: over-represented; negative: under)
df_expected <- df_expected %>%
  mutate(log_ratio = log2(ratio))

# Extended_name order (y-axis)
extended_order <- c("Emergence of novel protein and food innovation",
                    "Socio-technological",
                    "Sustainable consumption practices",
                    "GHG emissions and climate change mitigation",
                    "Emissions modeling and nutrient pollution",
                    "Land use, biodiversity and ecosystem services",
                    "Livestock nutrition, microbiome and emission reduction strategies"
                    ) %>% 
  str_wrap(., width = 25)

# Set factor levels accordingly
df_expected <- df_expected %>%
  mutate(extended_name = str_wrap(extended_name, width = 25),
         extended_name = factor(extended_name, levels = extended_order))

### Heatmap
ggplot(df_expected, aes(x = cluster, y = extended_name, fill = log_ratio)) +
  geom_tile(color = "white") +
  scico::scale_fill_scico(
    palette = "vik",
    midpoint = 0,
    name = expression(log[2](Observed / Expected)),  # This renders "log₂(Observed / Expected)"
    guide = guide_colorbar(barwidth = 20, barheight = 1.5)
  ) +
  labs(
    x = "K-Means Clusters from Topic Modelling", 
    y = "Thematic groupings from Bibliographic Communities",
    title = NULL
  ) +
  theme_minimal(base_size = 22) +
  theme(legend.position = "bottom")

ggsave(
  "association_heatmap.png",
  device = "png",
  path = figures_path,
  width = 18,
  height = 12,
  dpi = 300
)

ggsave(
  "figure-5.png",
  device = "png",
  path = figures_path,
  width = 18,
  height = 12,
  dpi = 300
)
  
# Running regressions---------------

formula_obj <- as.formula("~s(year)")

estimate_effect <- estimateEffect(~s(year),
                                  topic_model,
                                  metadata = metadata,
                                  documents = corpus$documents,
                                  uncertainty = "Global",
                                  nsims = 25)
write_rds(estimate_effect, here::here(data_path, "topic_modelling", "estimate_effect.rds"))

tidyprep_year <- tidystm::extract.estimateEffect(estimate_effect, 
                                                 "year", 
                                                 topic_model, 
                                                 method = "continuous") %>% 
  left_join(label_topic_beta, by = "topic")

topic_per_year <- ggplot(tidyprep_year, aes(x = covariate.value, y = estimate,
                                            ymin = ci.lower, ymax = ci.upper,
                                            group = as.factor(topic),
                                            fill = as.factor(topic))) +
  geom_ribbon(alpha = .5, show.legend = FALSE) +
  geom_line() +
  facet_wrap(~ fct_reorder(str_wrap(label_beta, 25), topic), nrow = 6, scales = "free_y") +
  theme(strip.text = element_text(size = 10)) +
  scico::scale_fill_scico_d(palette = "roma")

ggsave(
  "topic_per_year.png",
  device = "png",
  plot = topic_per_year,
  path = figures_path,
  width = 20,
  height = 15,
  dpi = 300
)

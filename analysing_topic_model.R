# Preparing and running the topic model
# Loading packages and data------------------------
source("paths_and_packages.R")
source("helper_functions.R")
p_load(ggraph,
       ggiraph)

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

gg <- ggraph(graph_layout, 
             "manual", 
             x = x, 
             y = y) +
  geom_edge_arc0(aes(
    # color = cluster_leiden,
    width = weight), 
    alpha = 0.1, strength = 0.2, show.legend = FALSE) +
  scale_edge_width_continuous(range = c(0.1,0.3)) +
  # scale_edge_colour_identity() +
  geom_point(aes(x = x, y = y), show.legend = FALSE) +
  geom_label_repel_interactive(aes(x = x, 
                                   y = y, 
                                   size = gamma,
                                   label = source_id,
                                   tooltip = topic_label_prob,
                                   data_id = source_id),
                               show.legend = FALSE) +
  scale_size_continuous(range = c(0.5,5)) +
  # scale_fill_identity() +
  theme_void()

girafe(ggobj = gg,
       width_svg  = 8,
       height_svg = 4.5)
  
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

#: Creating networks and alluvials ---------

# Script goals:
# This script builds dynamic co-occurrence/coupling networks from citation
# data, computes clusters over time, prepares an alluvial (stream) dataset
# to visualise cluster evolution, and saves publication-ready plots and data.
# * Useful for reproducible network analysis and visual storytelling.

# Setup Instructions:
# - Ensure `scripts/paths_and_packages.R` defines `data_path` and loads
#   required packages (tidyverse, glue, networkflow, ggalluvial, etc.).
# - Place input files in `data_path`:
#   - documents.rds, direct_citations.rds
#   - Communities_2_0.6_5_Summary.xlsx and naming_communities_2_0.6_5.csv
# - This script assumes the workspace root is the project root.

# How it works (high level):
# 1. Read cleaned node and citation data.
# 2. Set final parameters for network construction (method, window, etc.).
# 3. Build dynamic networks and compute temporal clusters (Leiden algorithm).
# 4. Post-process clusters: merge similar clusters, name clusters with tf-idf.
# 5. Convert networks into an alluvial-friendly table, prepare labels, and
#    minimize crossings to improve readability.
# 6. Create and save plots (general narrative and focused views) and save
#    the processed alluvial data for downstream use.

#: Setup ---------
source("scripts/paths_and_packages.R")

#: Load data ---------
# Read node (documents) and citation tables saved as RDS files.
# These objects are expected to contain columns referenced later, e.g.
# `citing_id`, `cited_id`, and `year` in `direct_citations` and
# `citing_id` in `nodes`.
nodes <- read_rds(file.path(data_path, "documents.rds"))
direct_citations <- read_rds(file.path(data_path, "direct_citations.rds"))

# Read community names: there is a CSV with naming suggestions and a
# richer Excel summary with labels. We keep both sources but the
# final `community_names` object below is the Excel-derived table.
community_names_csv <- read_csv(file.path(
  data_path,
  "naming_communities_2_0.6_5.csv"
)) %>%
  mutate(dynamic_cluster_leiden = tolower(id))

# Read the community summary (Excel) and its labels sheet. We clean
# column names and create a lower-case cluster id to match network output.
community_names <- readxl::read_xlsx(
  file.path(data_path, "Communities_2_0.6_5_Summary.xlsx"),
  sheet = "Feuil1"
) %>%
  janitor::clean_names() %>%
  mutate(dynamic_cluster_leiden = tolower(id)) %>%
  rename(thematic_grouping = first_order_thematic_grouping) %>%
  left_join(
    readxl::read_xlsx(
      file.path(data_path, "Communities_2_0.6_5_Summary.xlsx"),
      sheet = "Labels"
    ) %>%
      janitor::clean_names()
  ) %>%
  #: Clean and normalize some label text
  mutate(
    extended_name = str_remove(extended_name, "( related)? communities$"),
    extended_name = if_else(
      str_detect(extended_name, "Ethics"),
      NA_character_,
      extended_name
    )
  ) %>%
  # Rank extended names for plotting priority later on.
  mutate(rank_extended_name = 1:n(), .by = extended_name)

#: Pre-process citations ---------
# Keep only citations that connect documents we have in `nodes`, and
# remove a specific cited id (legacy filtering rule from original script).
direct_citations <- direct_citations[
  citing_id %in% nodes$citing_id & cited_id != "46698",
]

#: Final analysis parameters ---------
# These are the chosen parameter values for the final runs. Keep them
# together at the top so it's easy to tweak for repeatable experiments.
method <- "coupling_similarity"
threshold <- 2
resolution <- 0.6
window <- 5

#: Build dynamic networks and compute clusters ---------
# We use the project helper `build_dynamic_networks()` (assumed to be
# available from sourced scripts or a package) and then post-process
# clusters: add leiden clustering, merge similar clusters, and name them.
networks <- build_dynamic_networks(
  nodes,
  direct_citations,
  "citing_id",
  "cited_id",
  cooccurrence_method = method,
  time_variable = "year",
  time_window = window,
  overlapping_window = TRUE,
  filter_components = TRUE,
  compute_size = TRUE,
  edges_threshold = threshold
) %>%
  add_clusters(
    clustering_method = "leiden",
    objective_function = "modularity",
    resolution = resolution,
    seed = 1234
  ) %>%
  merge_dynamic_clusters(
    cluster_id = "cluster_leiden",
    node_id = "citing_id",
    threshold_similarity = 0.55,
    similarity_type = "partial"
  ) %>%
  name_clusters(
    method = "tf-idf",
    cluster_id = "dynamic_cluster_leiden",
    name_merged_clusters = TRUE,
    text_columns = c("title", "abstract"),
    clean_word_method = "lemmatise",
    n_gram = 3,
    nb_terms_label = 5
  )

# Save the networks object so downstream scripts can reuse it. The file
# name encodes the parameters so results remain traceable.
saveRDS(
  networks,
  file.path(
    data_path,
    glue("networks_{method}_{threshold}_{window}_{resolution}_final.rds")
  )
)

#: Prepare alluvial (stream) data ---------
cli::cli_alert_info("Creating alluvial")

# Convert networks into an alluvial-friendly table and join community
# labels. This table has one row per node-window-cluster combination.
alluvial <- networks %>%
  networks_to_alluv(
    intertemporal_cluster_column = "dynamic_cluster_leiden",
    node_id = "citing_id"
  ) %>%
  left_join(community_names)

# Drop redundant columns and build a human-readable cluster label used in
# some plot labels.
alluvial <- alluvial %>%
  select(-c(thematic_grouping, second_order_thematic_grouping, x5)) %>%
  mutate(cluster_label = str_c(dynamic_cluster_leiden, " - ", label))

#: Prepare labels to place at the first window of a cluster ---------
# We copy and aggregate the alluvial table using data.table for a concise
# grouping operation: find the first window when each cluster appears and
# use that to create a label (label_x) that will be plotted at the start.
label_df <- data.table::copy(alluvial)
label_df <- label_df[, .N, .(cluster_label, window)] %>%
  .[, label_alluvial := window == first(window), .(cluster_label)] %>%
  .[label_alluvial == TRUE] %>%
  distinct(cluster_label, window) %>%
  mutate(label_x = cluster_label)

# Minimize crossing order to get a visually cleaner alluvial plot.
alluvial_prepared <- alluvial %>%
  left_join(label_df) %>%
  networkflow::minimize_crossing_alluvial(
    intertemporal_cluster_column = "dynamic_cluster_leiden",
    node_id = "citing_id"
  )

#: Plotting - general narrative ---------
cli::cli_alert_info("Plotting alluvial")

# Build a color palette for the narrative using the main extended names.
# We filter NA extended_name values to avoid assigning colors to missing
# categories.
color_narrative <- tibble(
  extended_name = (unique(community_names$extended_name))
) %>%
  filter(!is.na(extended_name)) %>%
  mutate(color = see::oi_colors()[1:7])

# Join color information and prepare small cosmetic transformations
alluvial_prepared_narrative <- alluvial_prepared %>%
  left_join(color_narrative) %>%
  mutate(
    label_x = if_else(color == "gray", NA, label_x),
    label_x = str_remove(label_x, "cl_"),
    label_x = str_wrap(label_x, 25)
  )

# Reorder factor levels so plotting respects the minimized crossing order.
alluvial_prepared_narrative[["dynamic_cluster_leiden"]] <- forcats::fct_reorder(
  alluvial_prepared_narrative[["dynamic_cluster_leiden"]],
  alluvial_prepared_narrative[["minimize_crossing_order"]],
  min,
  .desc = TRUE
)

plot_alluvial_narrative <- alluvial_prepared_narrative %>%
  ggplot2::ggplot(ggplot2::aes(
    x = window,
    y = y_alluv,
    stratum = dynamic_cluster_leiden,
    alluvium = citing_id,
    fill = color,
    alpha = rank_extended_name
  )) +
  ggalluvial::geom_stratum(show.legend = TRUE) +
  ggalluvial::geom_flow(show.legend = FALSE) +
  ggplot2::scale_fill_identity(
    "Main Research\nStrands",
    guide = "legend",
    labels = str_wrap(color_narrative$extended_name, 32),
    breaks = color_narrative$color
  ) +
  ggrepel::geom_label_repel(
    ggplot2::aes(label = label_x),
    stat = ggalluvial::StatStratum,
    size = 4
  ) +
  scale_alpha_continuous(range = c(1, 0.7), guide = "none") +
  ggplot2::theme_classic(base_size = 20) +
  theme(legend.position = "bottom") +
  labs(y = "Proportion of the network", x = NULL)

ggsave(
  glue(
    "pictures/alluvial_{method}_{threshold}_{window}_{resolution}_final_narrative.png"
  ),
  plot_alluvial_narrative,
  width = 30,
  height = 20,
  dpi = 300
)

ggsave(
  glue("pictures/figure-2.png"),
  plot_alluvial_narrative,
  width = 18,
  height = 14,
  dpi = 300
)

#: Plotting - focused view (protein / selected strands) ---------
# We selectively keep labels and color for strands matching keywords to
# draw attention to them while greying out the rest for context.
plot_alluvial_focused <- alluvial_prepared_narrative %>%
  mutate(
    label_x = if_else(
      str_detect(extended_name, "protein|Socio-|Sustainable"),
      label_x,
      NA_character_
    ),
    color = if_else(
      str_detect(extended_name, "protein|Socio-|Sustainable"),
      color,
      "gray"
    )
  ) %>%
  ggplot2::ggplot(ggplot2::aes(
    x = window,
    y = y_alluv,
    stratum = dynamic_cluster_leiden,
    alluvium = citing_id,
    fill = color,
    alpha = rank_extended_name
  )) +
  ggalluvial::geom_stratum(show.legend = TRUE) +
  ggalluvial::geom_flow(show.legend = FALSE) +
  ggplot2::scale_fill_identity(
    "Main Research Strands",
    guide = "legend",
    labels = str_wrap(color_narrative$extended_name, 50),
    breaks = color_narrative$color
  ) +
  ggrepel::geom_label_repel(
    ggplot2::aes(label = label_x),
    stat = ggalluvial::StatStratum,
    size = 5
  ) +
  scale_alpha_continuous(range = c(1, 0.7), guide = "none") +
  ggplot2::theme_classic(base_size = 20) +
  theme(legend.position = "bottom") +
  labs(y = "Proportion of the network", x = NULL)

ggsave(
  glue(
    "pictures/alluvial_{method}_{threshold}_{window}_{resolution}_focused_narrative.png"
  ),
  plot_alluvial_focused,
  width = 30,
  height = 20,
  dpi = 300
)

ggsave(
  glue("pictures/figure-3.png"),
  plot_alluvial_focused,
  width = 18,
  height = 14,
  dpi = 300
)

#: Save processed alluvial for reuse ---------
alluvial_to_save <- alluvial_prepared_narrative %>%
  select(
    citing_id,
    window,
    dynamic_cluster_leiden,
    label,
    extended_name,
    color,
    y_alluv,
    share_cluster_max
  )

write_rds(
  alluvial_to_save,
  file.path(
    data_path,
    glue("alluvial_{method}_{threshold}_{window}_{resolution}_final.rds")
  )
)

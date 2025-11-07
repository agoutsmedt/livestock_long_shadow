# language: r
#' Create dynamic networks, name clusters, build an alluvial and compute node metrics
#'
#' Purpose:
#' - Build time-sliced (dynamic) networks from citation/co-occurrence data
#' - Cluster each network (Leiden), merge clusters across time, and name merged clusters
#' - Create an alluvial dataset for plotting and produce a labeled alluvial plot
#' - Compute node-level eigenvector centrality and participation coefficient
#'
#' Inputs (expected in environment)
#' - nodes: tibble/data.frame of node metadata
#' - direct_citations: tibble of edges (must include citing_id, cited_id, year)
#' - method: character; cooccurrence method used by build_dynamic_networks
#' - window: numeric/integer; time window length used by build_dynamic_networks
#' - threshold: numeric; edges threshold used by build_dynamic_networks
#' - resolution: numeric; resolution parameter passed to Leiden clustering
#' - data_path, figures_path: strings for saving results/figures
#'
#' Outputs
#' - Saves an alluvial RDS and a PNG figure; returns the path to the saved RDS invisibly
#'
#' Dependencies: purrr, dplyr, tibble, igraph, networkflow, scico, glue, cli, stringr, ggsave/ggplot2
#' Note: This is a script-style workflow; adapt into functions for reuse if desired.

# 1. Build dynamic networks and compute clusters / intertemporal clusters / names
cli::cli_alert_info("Building dynamic networks and naming clusters")
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

# 2. Convert networks to an alluvial dataset suitable for plotting
cli::cli_alert_info("Creating alluvial data")
alluvial <- networks %>%
  networks_to_alluv(
    intertemporal_cluster_column = "dynamic_cluster_leiden",
    node_id = "citing_id"
  )

# 3. Assign consistent colors to clusters
colors <- alluvial %>%
  dplyr::select(dynamic_cluster_leiden, cluster_label) %>%
  dplyr::arrange(desc(cluster_label)) %>%
  dplyr::distinct(dynamic_cluster_leiden) %>%
  dplyr::mutate(color = scico::scico(palette = "roma", n = dplyr::n()))

alluvial <- dplyr::left_join(alluvial, colors, by = "dynamic_cluster_leiden")

# 4. Prepare labels and minimize crossing for plotting (minimization step optional)
alluvial_prepared <- alluvial %>%
  dplyr::mutate(
    cluster_label = stringr::str_c(dynamic_cluster_leiden, " - ", cluster_label)
  ) %>%
  prepare_label_alluvial(
    window_column = "window",
    cluster_label_column = "cluster_label"
  ) %>%
  networkflow::minimize_crossing_alluvial(
    intertemporal_cluster_column = "dynamic_cluster_leiden",
    node_id = "citing_id"
  )

# 5. Post-process labels for readability and small clusters
cli::cli_alert_info("Preparing and plotting alluvial")
alluvial_prepared <- alluvial_prepared %>%
  dplyr::mutate(
    label_x = ifelse(share_cluster_max < 4, NA, label_x),
    color = ifelse(share_cluster_max < 4, "gray", color),
    label_x = stringr::str_wrap(label_x, 20)
  )

plot_alluvial <- alluvial_prepared %>%
  plot_alluvial(
    intertemporal_cluster_column = "dynamic_cluster_leiden",
    node_id = "citing_id",
    window_column = "window",
    color_column = "color",
    color_alluvial = FALSE,
    color = NULL,
    minimize_crossing = FALSE,
    prepare_label = FALSE,
    cluster_label_column = "cluster_label",
    print_plot_code = TRUE
  ) +
  ggplot2::theme_minimal(base_size = 25) +
  ggplot2::labs(y = "Proportion of the network", x = NULL)

# Save figure (filename uses runtime variables)
ggplot2::ggsave(
  glue::glue(
    "pictures/alluvial_round2_{method}_{threshold}_{window}_{resolution}.png"
  ),
  plot_alluvial,
  width = 30,
  height = 20
)

# 6. Compute node-level centrality and participation coefficient and join back to alluvial
cli::cli_alert_info("Adding node centrality and participation coefficient")

centrality_data <- compute_eigenvector_centrality(networks)
participation_coefficient <- compute_participation_coefficient(
  networks,
  min_neighbors = 4
)

# Join metrics into the prepared alluvial dataset (safe left joins)
alluvial_prepared <- alluvial_prepared %>%
  dplyr::left_join(centrality_data, by = c("citing_id", "window")) %>%
  dplyr::left_join(participation_coefficient, by = c("citing_id", "window"))

# 7. Save the final dataset for reuse
out_path <- file.path(
  data_path,
  "parameter_comparison_data",
  glue::glue("alluvial_round2_{method}_{threshold}_{window}_{resolution}.rds")
)
saveRDS(alluvial_prepared, out_path)

# Return the saved path invisibly so the script can be source()-ed in pipelines
invisible(out_path)

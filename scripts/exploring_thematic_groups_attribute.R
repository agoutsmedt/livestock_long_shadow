#: Tutorial & Setup --------------------------------------------------------
#: Script goals
# - Compute and visualise how thematic groups (communities) distribute across
#   countries, regions, subject areas and journals.
# - Measure group "siloing" (diversity metrics) and inter-group connectivity
#   using bibliographic coupling networks.
#
#: Setup instructions
# - Run this script from the project root. It expects `scripts/paths_and_packages.R`
#   to define `data_path`, `figures_path`, and to load the libraries used below
#   (tidyverse, tidygraph, tidytext, scico, ggtext, cli, janitor, etc.).
# - Files read here are precomputed (networks, alluvial mapping, metadata).
#
#: How it works (high level)
# 1. Load mapping document -> thematic group (alluvial coupling file) and
#    supporting metadata (documents, affiliations, subject areas).
# 2. Compute per-group shares by geography, subject and journal; visualise
#    top contributors and compare to global shares.
# 3. Use precomputed bibliographic-coupling networks to compute group-level
#    connectivity (mixing matrices, cosine similarities), aggregate results
#    across networks and visualise heatmaps and silo/rank indicators.
#
#: Notes for beginners
# - This is an analysis script: it reads precomputed RDS objects and produces
#   figures. If you change paths prefer the variables in
#   `scripts/paths_and_packages.R` so outputs remain consistent.
# - Comments explain why blocks exist; follow them to adapt safely.

source("scripts/paths_and_packages.R")

#: Data loading & initial cleaning ---------
# Load mapping of documents -> thematic groups (alluvial coupling file).
# This object maps each document (citing_id) to a cluster (extended_name).
alluvial_df <- read_rds(file.path(
  data_path,
  "alluvial_coupling_similarity_2_5_0.6_final.rds"
))

# Ensure the join key has a stable type. We coerce `citing_id` to integer so
# joins with other data.tables or tibbles are consistent. We also drop rows
# missing essential labels to avoid propagation of NAs in later joins.
alluvial_df[, citing_id := as.integer(citing_id)]
alluvial_df <- alluvial_df |>
  filter(!is.na(label), !is.na(extended_name))

#: Labels & ordering (presentation helpers) ---------
# Define a consistent ordering of thematic groups so facets and legends
# remain stable between plots. Update `extended_order` if you rename groups.
extended_order <- c(
  "Emissions modeling and nutrient pollution",
  "Land use, biodiversity and ecosystem services",
  "Livestock nutrition, microbiome and emission reduction strategies",
  "GHG emissions and climate change mitigation",
  "Emergence of novel protein and food innovation",
  "Socio-technological",
  "Sustainable consumption practices"
)

# Apply the factor ordering so ggplot facets keep the requested sequence.
alluvial_df <- alluvial_df |>
  mutate(extended_name = factor(extended_name, levels = extended_order))

# Build a small named vector for prettier facet labels. We deliberately bold
# some group names (rendered via `ggtext::element_markdown()`) and wrap long
# labels for readability in plots.
labels_map <- alluvial_df |>
  distinct(extended_name) |>
  mutate(
    extended_label = if_else(
      stringr::str_detect(
        extended_name,
        stringr::regex("protein|Socio-|Sustainable", ignore_case = TRUE)
      ),
      paste0("**", extended_name, "**"),
      extended_name
    ) |>
      stringr::str_wrap(45),
    extended_label = stringr::str_replace_all(extended_label, "\n", "<br/>")
  )

labels_vec <- setNames(labels_map$extended_label, labels_map$extended_name)

#: Networks (bibliographic coupling) ---------
# Load a precomputed list of bibliographic-coupling graphs. Each list element
# should be a `tbl_graph` (from tidygraph) where node metadata contains
# `citing_id` and `dynamic_cluster_leiden` so we can annotate nodes with the
# thematic group (extended_name).
networks_list <- read_rds(file.path(
  data_path,
  "networks_coupling_similarity_2_5_0.6_final.rds"
))

# Annotate node tables with the group assignment. We convert the join key to
# character because node metadata often stores IDs as strings; this avoids
# accidental mismatches when joining.
networks_list <- purrr::map(
  networks_list,
  ~ .x %>%
    activate("nodes") %>%
    left_join(
      alluvial_df |>
        distinct(citing_id, dynamic_cluster_leiden, extended_name) |>
        mutate(citing_id = as.character(citing_id)),
      by = c("citing_id", "dynamic_cluster_leiden"),
      relationship = "many-to-one"
    )
)

# Loading citing documents to get EIDs
#: Document metadata (EIDs, subjects, affiliations) ---------
# Load citing documents metadata. This contains Scopus EIDs, subject areas,
# journal names and other fields we use to compute group-level distributions.
citing_documents <- read_rds(file.path(data_path, "documents.rds")) |>
  unique()

# Expand combined subject area strings so each row has one subject area. We
# also strip quotes and trailing parenthetical notes (source labels) so we
# get a clean subject label per row.
subject_areas <- citing_documents |>
  separate_rows(subject_areas, sep = "\", \"") |>
  mutate(
    subject_areas = stringr::str_remove_all(
      subject_areas,
      "^\"|\"$|\\s\\(.+"
    ) |>
      stringr::str_trim()
  ) |>
  distinct(citing_id, subject_areas)

# Extract Scopus EIDs to link documents to affiliation records. We make each
# EID -> citing_id association explicit by expanding rows and trimming quotes.
eids <- citing_documents |>
  separate_rows(scopus_eid, sep = "\", \"") |>
  mutate(
    scopus_eid = stringr::str_remove_all(scopus_eid, "^\"|\"$") |>
      stringr::str_trim()
  ) |>
  distinct(eid = scopus_eid, citing_id)

# Quick data quality check: documents that list multiple eids (informational)
multiple_eids <- citing_documents |>
  filter(stringr::str_detect(scopus_eid, "\", \"")) |>
  pull(citing_id)

# Load affiliations and keep only relevant, cleaned columns. We use
# `janitor::clean_names()` to normalise column names coming from external files.
affiliations <- readRDS(file.path(data_path, "affiliations.rds")) |>
  janitor::clean_names() |>
  distinct(eid, dc_creator, country = affiliation_country)

cli::cli_alert_info(
  "Number of missing affiliations: {nrow(eids |> filter(!eid %in% affiliations$eid))}"
)
cli::cli_alert_info(
  "Number of eids in affiliations that are not in citing documents: {nrow(affiliations |> filter(!eid %in% citing_documents$scopus_eid))}"
)

# Drop empty country rows before aggregation to avoid introducing spurious
# NA buckets in the geographic statistics.
affiliations <- affiliations |>
  filter(!is.na(country) & country != "")

# Attach `citing_id` to affiliation records so each affiliation row refers to
# a document; this is needed to aggregate by group later. We perform an inner
# merge so only affiliations with known citing documents are kept.
affiliations <- merge.data.table(
  affiliations,
  eids,
  by = "eid",
  all = FALSE
)

# If a document has multiple affiliations we assume each contributes equally
# to the document's country share. We compute `share_country` as 1 / n_affils
# per citing_id so we can weight counts correctly later.
affiliations <- affiliations |>
  mutate(share_country = 1 / n(), .by = citing_id)

# merge with alluvial groups data and create region
#: Geographic aggregation (country / region shares) ---------
# Build a table of document -> group -> affiliation and aggregate to compute
# per-group shares by country and region.
communities_affiliation <- alluvial_df |>
  distinct(citing_id, extended_name) |>
  left_join(affiliations, by = "citing_id", relationship = "many-to-many") |>
  mutate(
    # Simplify some country names into regions for easier visualisation.
    region = case_when(
      country %in% c("United States", "Canada") ~ "North America",
      country %in%
        c(
          "Germany",
          "France",
          "Italy",
          "Spain",
          "Netherlands",
          "Sweden",
          "Norway",
          "Denmark",
          "Finland",
          "Belgium",
          "Switzerland",
          "Austria",
          "Ireland",
          "Portugal",
          "Greece",
          "Poland",
          "Czech Republic",
          "Hungary",
          "Romania"
        ) ~ "Europe",
      TRUE ~ country
    )
  )

# Compute stats separately for country and region then combine into `geo_stats`.
# `compute_group_stats()` is a project helper defined elsewhere that returns
# per-level shares and global shares suitable for plotting.
country_stats <- compute_group_stats(
  communities_affiliation,
  level_col = "country"
)
region_stats <- compute_group_stats(
  communities_affiliation,
  level_col = "region"
)

geo_stats <- bind_rows(
  country_stats |> mutate(geo_type = "country"),
  region_stats |> mutate(geo_type = "region")
) |>
  select(
    extended_name,
    geo_type,
    everything(),
    -group_share,
    -group_weight,
    -group_total,
    -global_weight,
    -global_share
  ) |>
  # Re-attach the original computed columns to preserve expected column order
  left_join(
    bind_rows(
      country_stats |> mutate(geo_type = "country"),
      region_stats |> mutate(geo_type = "region")
    ),
    by = c("extended_name", "geo_type", "level")
  )

# Create and save the region and country plots. These helper functions are
# defined elsewhere in the project. They return ggplot objects and also save
# PNG files to `figures_path` as a side-effect.
region_plot <- plot_group_shares(
  filter(geo_stats, geo_type == "region"),
  top_n = 6,
  wrap_width = 30,
  filename = "region_share_thematic_groups.png",
  figures_path = figures_path,
  base_size = 20,
  plot_height = 12
)
country_plot <- plot_group_shares(
  filter(geo_stats, geo_type == "country"),
  top_n = 6,
  wrap_width = 30,
  filename = "country_share_thematic_groups.png",
  figures_path = figures_path,
  base_size = 18,
  plot_height = 12
)

# ------------------------------------------------------------
# SUBJECTS
# ------------------------------------------------------------
communities_subject <- alluvial_df |>
  distinct(citing_id, extended_name) |>
  left_join(subject_areas, by = "citing_id", relationship = "many-to-many") |>
  rename(subject_area = subject_areas)

subject_stats <- compute_group_stats(
  communities_subject,
  level_col = "subject_area"
)
subject_plot <- plot_group_shares(
  subject_stats,
  top_n = 5,
  wrap_width = 30,
  filename = "subject_share_thematic_groups.png",
  figures_path = figures_path,
  base_size = 17,
  plot_height = 12
)

# ------------------------------------------------------------
# JOURNALS
# ------------------------------------------------------------
communities_journal <- alluvial_df |>
  distinct(citing_id, extended_name) |>
  left_join(
    citing_documents |> distinct(citing_id, journal),
    by = "citing_id",
    relationship = "many-to-many"
  )

journal_stats <- compute_group_stats(communities_journal, level_col = "journal")
journal_plot <- plot_group_shares(
  journal_stats,
  top_n = 5,
  wrap_width = 35,
  filename = "journal_share_thematic_groups.png",
  figures_path = figures_path,
  base_size = 16,
  plot_height = 12
)

#: Exploring thematic group diversity ------------------

# 1) country metrics: compute diversity / siloing indicators per group.
country_metrics <- geo_stats |>
  filter(geo_type == "country") |>
  select(extended_name, level, group_share) |>
  rename(country = level) |>
  diversity_metrics(
    group_col = "extended_name",
    share_col = "group_share",
    level_col = "country"
  ) |>
  rename_with(~ paste0("country_", .), -extended_name)

# Compute subject and journal metrics (same pipeline used for country, but concise)

subject_metrics <- subject_stats |>
  select(extended_name, level, group_share) |>
  rename(subject = level) |>
  diversity_metrics(
    group_col = "extended_name",
    share_col = "group_share",
    level_col = "subject"
  ) |>
  rename_with(~ paste0("subject_", .), -extended_name)


journal_metrics <- journal_stats |>
  select(extended_name, level, group_share) |>
  rename(journal = level) |>
  diversity_metrics(
    group_col = "extended_name",
    share_col = "group_share",
    level_col = "journal"
  ) |>
  rename_with(~ paste0("journal_", .), -extended_name)

# Create the three rank plots and save them
# Create rank plots for each domain. `make_rank_plot()` returns ggplot
# objects and saves files to `figures_path` as a side-effect.
p_rank_country <- make_rank_plot(
  metrics_df = country_metrics,
  prefix = "country_",
  alluvial_df = alluvial_df,
  figures_path = figures_path
)

p_rank_subject <- make_rank_plot(
  metrics_df = subject_metrics,
  prefix = "subject_",
  alluvial_df = alluvial_df,
  figures_path = figures_path
)

p_rank_journal <- make_rank_plot(
  metrics_df = journal_metrics,
  prefix = "journal_",
  alluvial_df = alluvial_df,
  figures_path = figures_path
)
# Combine metrics and compute silo indices per indicator (Shannon, normalized Shannon, HHI)
group_metrics <- country_metrics |>
  left_join(subject_metrics, by = "extended_name") |>
  left_join(journal_metrics, by = "extended_name") |>
  left_join(
    distinct(alluvial_df, extended_name, color),
    by = "extended_name"
  ) |>
  # Scale components so higher values correspond to *more* siloing. We invert
  # Shannon where necessary and rescale HHI into [0,1] so indicators are
  # comparable across domains.
  mutate(
    # country
    country_shannon_s = 1 - scales::rescale(country_shannon, to = c(0, 1)),
    country_hhi_s = scales::rescale(country_hhi, to = c(0, 1)),
    # subject
    subject_shannon_s = 1 - scales::rescale(subject_shannon, to = c(0, 1)),
    subject_hhi_s = scales::rescale(subject_hhi, to = c(0, 1)),
    # journal
    journal_shannon_s = 1 - scales::rescale(journal_shannon, to = c(0, 1)),
    journal_hhi_s = scales::rescale(journal_hhi, to = c(0, 1))
  ) |>
  # Composite silo indices: simple average across the three domains. We keep
  # NA handling conservative (na.rm = TRUE) in case one domain is missing.
  mutate(
    silo_shannon = rowMeans(across(ends_with("_shannon_s")), na.rm = TRUE),
    silo_hhi = rowMeans(across(ends_with("_hhi_s")), na.rm = TRUE)
  )

# Prepare long table for plotting
# Prepare long table for plotting the silo indices. We reorder names within
# each facet so bars are drawn in descending order per indicator.
# Use labels_map for pretty (wrapped + bold) group labels, keep reorder_within for facet ordering
silo_long <- group_metrics |>
  select(extended_name, color, silo_shannon, silo_hhi) |>
  pivot_longer(
    cols = starts_with("silo_"),
    names_to = "indicator",
    values_to = "silo_index"
  ) |>
  mutate(
    indicator = recode(
      indicator,
      silo_shannon = "Shannon (inverted)",
      silo_hhi = "Herfindahl-Hirschman Index"
    )
  ) |>
  left_join(
    labels_map |> select(extended_name, extended_label),
    by = "extended_name"
  ) |>
  # Use the prepared extended_label (contains bold + <br/>) for plotting
  mutate(
    extended_name_ord = reorder_within(extended_label, silo_index, indicator)
  )

# Create a minimal-like theme that supports ggtext::element_markdown and base_size = 18
theme_minimal_like <- function(base_size = 18, base_family = "") {
  theme(
    text = element_text(
      size = base_size,
      family = base_family,
      colour = "black"
    ),
    plot.title = element_text(size = rel(1.2), face = "bold"),
    plot.subtitle = element_text(size = rel(1), margin = margin(b = 6)),
    plot.caption = element_text(size = rel(0.8), hjust = 0),
    panel.background = element_rect(fill = "white", colour = NA),
    panel.grid.major = element_line(colour = "#e9e9e9", linewidth = 0.4),
    panel.grid.minor = element_line(colour = "#f5f5f5", linewidth = 0.25),
    panel.border = element_blank(),
    axis.title = element_text(size = rel(1)),
    axis.text = element_text(size = rel(0.95), colour = "black"),
    axis.text.y = ggtext::element_markdown(size = rel(0.95), colour = "black"),
    axis.ticks = element_blank(),
    legend.position = "none",
    strip.background = element_blank(),
    strip.text = element_text(face = "bold", size = rel(0.95)),
    plot.margin = margin(8, 40, 8, 8)
  )
}

p_silo_facet <- silo_long |>
  ggplot(aes(x = silo_index, y = extended_name_ord, fill = color)) +
  geom_col() +
  scale_fill_identity() +
  facet_wrap(~indicator, nrow = 3, scales = "free_y") +
  scale_x_continuous(
    labels = scales::percent_format(accuracy = 1),
    expand = c(0.01, 0)
  ) +
  scale_y_reordered() +
  coord_cartesian(clip = "off") +
  labs(
    title = "Silo indices by indicator and thematic group",
    x = "Silo index (0 = open -> 1 = siloed)",
    y = NULL,
    caption = "Shannon metrics inverted so higher values indicate more siloing"
  ) +
  theme_minimal_like(base_size = 18)

ggsave(
  file.path(figures_path, "silo_indices_facet_by_indicator.png"),
  plot = p_silo_facet,
  width = 16,
  height = 12,
  dpi = 300
)

#: Testing silo effects based on bibliographic coupling ---------

# The project expects a `network_stats` list where each entry contains
# precomputed group-level statistics, typically produced by `compute_group_connectivity()`.
# Example shape: `network_stats[[i]]$mixing_matrix`, `network_stats[[i]]$cosine`.

# Aggregate network-level statistics across time windows / thresholds.
aggregated_stats <- aggregate_networks(
  network_stats,
  weight_by = "none"
)

# Mixing matrix heatmap: visualise normalized mixing between groups.
p_mixing <- plot_heatmap(
  mat = aggregated_stats$mixing_norm,
  title = "Aggregated Mixing Matrix (Normalized)",
  fill_label = "Normalized Weight",
  figures_path = figures_path,
  filename = "aggregated_mixing_matrix_heatmap.png",
  wrap_width = 18
)

# Save mixing heatmap (helpers return ggplot objects; we explicitly save to
# ensure files exist even if helper also saved them).
file_path <- file.path(figures_path, "aggregated_mixing_matrix_heatmap.png")
ggplot2::ggsave(
  filename = file_path,
  plot = p_mixing,
  width = 16,
  height = 12,
  dpi = 300
)

# Cosine similarity heatmap: visualise aggregated cosine similarity between groups.
p_cosine <- plot_heatmap(
  mat = aggregated_stats$cosine,
  title = "Aggregated Cosine Similarity Matrix",
  fill_label = "Cosine Similarity",
  figures_path = figures_path,
  filename = "aggregated_cosine_similarity_heatmap.png",
  wrap_width = 18
)

file_path <- file.path(figures_path, "aggregated_cosine_similarity_heatmap.png")
ggplot2::ggsave(
  filename = file_path,
  plot = p_cosine,
  width = 16,
  height = 12,
  dpi = 300
)

# Displayed data from the pooled connectivity stats
plot_fraction_within <- aggregated_stats$pooled_group_summary |>
  left_join(
    alluvial_df |> distinct(extended_name, color),
    by = c("group" = "extended_name")
  ) |>
  mutate(group = stringr::str_wrap(group, 30)) |>
  ggplot(aes(
    x = reorder(group, fraction_within),
    y = fraction_within,
    fill = color
  )) +
  geom_col() +
  scale_fill_identity() +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1),
    expand = c(0, 0.01),
    n.breaks = 10
  ) +
  coord_flip() +
  labs(
    title = "Fraction of Within-Group Connections by Thematic Group",
    x = "Thematic Group",
    y = "Fraction of Within-Group Connections"
  ) +
  theme_minimal(base_size = 18)

ggsave(
  file.path(figures_path, "fraction_within_group_connections.png"),
  plot = plot_fraction_within,
  width = 14,
  height = 10,
  dpi = 300
)

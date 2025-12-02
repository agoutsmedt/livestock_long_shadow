########### Running networks and reports #############
#: Script goals ---------
# This script renders a series of Quarto reports that explore community
# detection results for a range of parameter settings. It is intended as a
# lightweight driver that:
# * Loads project paths and helper functions
# * Loads the precomputed data used by the reports
# * Iterates a grid of parameters and renders one HTML report per row

#: Setup Instructions ---------
# Prerequisites:
# - This project expects `scripts/paths_and_packages.R` to set `data_path`
#   and to load required packages (tidyverse, glue, quarto, etc.).
# - The Quarto document `exploring_communities.qmd` must accept the
#   parameters `method`, `threshold`, `resolution`, and `window`.
# How to run:
# - From the project root, run this script with R or source it from an R
#   session. Example: `source("scripts/running_networks_and_report.R")`.

#: How It Works (high level) ---------
# 1. Source shared project setup (paths, packages, helpers).
# 2. Read the required data files into memory (these are used by the
#    Quarto document during execution).
# 3. Build a parameter grid with combinations of method / threshold /
#    resolution / window values.
# 4. Loop over each row of the grid and call `quarto::quarto_render()` to
#    generate an HTML report whose filename encodes the parameters.

#: Setup ---------
source("scripts/paths_and_packages.R")
source("scripts/helper_functions.R")

#: Data loading ---------
# Read data from the project `data_path`. We keep the original object names
# but append `_df` to make it clear these are data frames / tibbles.
# @param data_path From `scripts/paths_and_packages.R`.
nodes_df <- read_rds(file.path(data_path, "documents.rds"))
direct_citations_df <- read_rds(file.path(data_path, "direct_citations.rds"))

#: Parameter grid ---------
# Create a tidy tibble with all combinations of the parameters we want to
# sweep over. This is clearer than hard-coded nested loops and makes it easy
# to extend later.
param_grid <- expand_grid(
  thresholds = c(1, 2),
  resolutions = c(0.6, 1),
  methods = c("coupling_angle", "coupling_similarity"),
  windows = c(5, 8)
)

#: Render reports ---------
# Iterate over rows of `param_grid` and render a Quarto report for each set
# of parameters. We keep the loop simple and explicit so beginners can follow
# the control flow easily.
# build output filenames, render with purrr::pwalk, and return the grid with output_file
param_grid <- param_grid |>
  mutate(
    output_file = as.character(glue::glue(
      "exploring_communities_round2_{methods}_{thresholds}_{resolutions}_{windows}.html"
    ))
  )

purrr::pwalk(
  list(
    method = param_grid$methods,
    threshold = param_grid$thresholds,
    resolution = param_grid$resolutions,
    window = param_grid$windows,
    output_file = param_grid$output_file
  ),
  function(method, threshold, resolution, window, output_file) {
    quarto::quarto_render(
      input = "exploring_communities.qmd",
      output_file = output_file,
      execute_params = list(
        method = method,
        threshold = threshold,
        resolution = resolution,
        window = window
      )
    )
  }
)

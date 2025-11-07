########### Running networks and reports #############

source("scripts/paths_and_packages.R")
source("scripts/helper_functions.R")

# Load data
nodes <- read_rds(file.path(data_path, "documents.rds"))
direct_citations <- read_rds(file.path(data_path, "direct_citations.rds"))

# Creating nodes and direct_citations files
grid <- expand_grid(
  thresholds = c(1, 2),
  resolutions = c(0.6, 1),
  methods = c("coupling_angle", "coupling_similarity"),
  windows = c(5, 8)
)

for (i in 1:nrow(grid)) {
  method <- grid$methods[i]
  threshold <- grid$thresholds[i]
  resolution <- grid$resolutions[i]
  window <- grid$windows[i]
  #source("scripts/creating_networks.R")
  quarto::quarto_render(
    "exploring_communities.qmd",
    output_file = glue(
      "exploring_communities_round2_{method}_{threshold}_{resolution}_{window}.html"
    ),
    execute_params = list(
      method = grid$methods[i],
      threshold = grid$thresholds[i],
      resolution = grid$resolutions[i],
      window = grid$windows[i]
    )
  )
}

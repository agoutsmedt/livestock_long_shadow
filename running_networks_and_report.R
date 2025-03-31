########### Running networks and reports #############

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
saveRDS(nodes, file.path(data_path, "nodes.rds"))

direct_citations <- direct_citations[citing_id %in% nodes$citing_id & cited_id != "46698",]

grid <- expand_grid(thresholds = c(1, 2),
            resolutions = c(0.6, 1),
            methods = c("coupling_angle", "coupling_similarity"),
            windows = c(5, 8))

for (i in 1:nrow(grid)) {
  method <- grid$methods[i]
  threshold <- grid$thresholds[i]
  resolution <- grid$resolutions[i]
  window <- grid$windows[i]
  source("creating_networks.R")
  quarto::quarto_render("exploring_communities.qmd",
                        output_file = glue("exploring_communities_{method}_{threshold}_{resolution}_{window}.html"),
                        execute_params = list(method = grid$methods[i],
                                              threshold = grid$thresholds[i],
                                              resolution = grid$resolutions[i],
                                              window = grid$windows[i]))
}

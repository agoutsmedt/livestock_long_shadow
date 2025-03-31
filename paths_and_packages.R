data_path <- "~/Nextcloud/Research/R/projets/data/lls_data"
library(pacman)
p_load(tidyverse,
       data.table,
       tidygraph,
       glue)

remotes::install_github("agoutsmedt/networkflow")
pacman::p_load(networkflow)

# Number of threads used with data.table in percentage of total CPU
setDTthreads(percent = 70)

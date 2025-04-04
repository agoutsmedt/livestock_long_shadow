data_path <- "~/Nextcloud/Research/data/lls_data"
library(pacman)
p_load(tidyverse,
       data.table,
       tidygraph,
       glue)

if("networkflow" %in% rownames(installed.packages()) == FALSE) {
  remotes::install_github("agoutsmedt/networkflow")
}
if ("vite" %in% rownames(installed.packages()) == FALSE) {
  devtools::install_github("ParkerICI/vite")
}
pacman::p_load(networkflow)
#      


# Number of threads used with data.table in percentage of total CPU
setDTthreads(percent = 70)

figures_path <- here::here("pictures")

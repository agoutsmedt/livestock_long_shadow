data_path <- "~/Nextcloud/Research/R/projets/data/lls_data"
pacman::p_load(tidyverse,
               data.table)

# Number of threads used with data.table in percentage of total CPU
setDTthreads(percent = 70)

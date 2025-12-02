#: Tutorial ---------
# Script goals:
# - Load the consolidated LLS database and produce two cleaned, ready-to-use
#   datasets:
#     1) `documents.rds`: level-1 documents with basic metadata
#     2) `direct_citations.rds`: pairs of citing and cited document IDs
#
# Why this script is useful:
# - Downstream analyses rely on consistent identifiers and filtered citation
#   edges. This script prepares those datasets and keeps the original
#   behaviour (no changes to filtering logic).
#
# Setup Instructions:
# - This script expects `scripts/paths_and_packages.R` to set `data_path`
#   and to load required packages. Typical required packages are:
#     - data.table, readr, purrr, dplyr (used via pluck/select)
# - Run the project from the repository root so relative paths work.
#
# How it works (high level):
# 1. Read the consolidated RDS file produced by upstream ingestion.
# 2. Extract the documents table and keep only documents with an LLS id.
# 3. Extract the citations table and restrict to document DOIs present in
#    the documents table.
# 4. Extract article-level flags, coerce them to logical values, and merge
#    them into the documents table.
# 5. Keep only level-1 documents as citing documents and save outputs.
#
#: Setup ---------
source("scripts/paths_and_packages.R")

#: Load data ---------
# Load the consolidated LLS database. This object is a list containing
# several tables; we access elements by position below (1 = documents,
# 2 = citations, 4 = article levels). `read_rds()` preserves R objects.
lls_data <- read_rds(file.path(data_path, "LLS_database_consolidated_2.rds"))

#: Documents (articles) ---------
# Extract the first element (documents table) and coerce to data.table.
# Keep only rows where we have an `LLS_id` (we drop records that only have DOI).
documents_dt <- pluck(lls_data, 1) %>%
  as.data.table(key = "doi") %>%
  .[!is.na(LLS_id)] %>% # ! Important: drop entries lacking an internal ID
  unique()

#: Direct citations (DOI pairs) ---------
# Extract the citations table and restrict it to DOIs present in
# `documents_dt`. We only keep the doi pairs (doi_citing, doi_cited).
citations_dt <- pluck(lls_data, 2) %>%
  as.data.table(key = c("doi_citing", "doi_cited")) %>%
  .[doi_citing %in% documents_dt$doi & doi_cited %in% documents_dt$doi] %>%
  .[, .(doi_citing, doi_cited)] %>%
  unique()

#: Article levels (flags) ---------
# The source stores level flags as values (e.g. 1 or NA). We convert those
# to logical columns for easier filtering later.
article_levels_dt <- pluck(lls_data, 4) %>%
  select(LLS_id, level_1 = level1, level_2 = level2) %>%
  as.data.table(key = "LLS_id")

## Convert level values to logical (TRUE / FALSE).
## ? Why: logical flags are easier and less error-prone in filters.
article_levels_dt[, level_1 := !is.na(level_1)]
article_levels_dt[, level_2 := !is.na(level_2)]

#: Add numeric IDs to citation table ---------
# For downstream joins we want citing/cited internal IDs (LLS_id). We
# merge the DOI->LLS_id mapping from `documents_dt` into the citations table.
# Note: we keep `all.x = TRUE` to preserve citation rows even if mapping
# is unexpectedly missing; these could be inspected later.
citations_dt <- merge.data.table(
  citations_dt,
  documents_dt[, .(doi, citing_id = LLS_id)],
  by.x = "doi_citing",
  by.y = "doi",
  all.x = TRUE
)

citations_dt <- merge.data.table(
  citations_dt,
  documents_dt[, .(doi, cited_id = LLS_id)],
  by.x = "doi_cited",
  by.y = "doi",
  all.x = TRUE
)

setkey(citations_dt, citing_id, cited_id)

#: Merge levels into documents and filter ---------
# We join the article-level flags into the documents table so we can
# filter to only level-1 documents (as the set of citing documents).
documents_dt <- merge(
  documents_dt,
  article_levels_dt[!is.na(LLS_id)],
  by = "LLS_id",
  all.x = TRUE
) %>%
  .[level_1 == TRUE]

# Rename the internal ID column for clarity: these rows represent
# citing documents (we'll keep a `cited_id` column later for counting).
setnames(documents_dt, "LLS_id", "citing_id")

#: Prepare document metadata fields ---------
# Create a `cited_id` column equal to the citing_id for ease of later
# aggregation (this mirrors previous behaviour). Ensure `year` is integer.
documents_dt[, cited_id := citing_id] # used later for counting citations
documents_dt[, year := as.integer(year)]

#: Final filtering of citations ---------
# Keep only citations where the citing DOI belongs to our filtered
# `documents_dt`. Also exclude a specific `cited_id` (keeps original logic).
# ! Note: the hard-coded id "46698" is the LLS id
citations_dt <- citations_dt[
  doi_citing %in% documents_dt$doi & cited_id != "46698",
]

#: Save outputs ---------
# Persist the prepared datasets for downstream analysis. We compress the
# RDS files to save disk space; this does not change R object structure.
write_rds(documents_dt, file.path(data_path, "documents.rds"), compress = "gz")
write_rds(
  unique(citations_dt[, .(citing_id, cited_id)]),
  file.path(data_path, "direct_citations.rds"),
  compress = "gz"
)

#: Notes & small suggestions ---------
# * Suggestion: consider replacing positional `pluck()` calls with named
#   components (if the RDS structure is stable) for clarity. Example:
#   `lls_data$documents` instead of `pluck(lls_data, 1)`.
# * todo: validate that `documents_dt$doi` has unique entries before joins.

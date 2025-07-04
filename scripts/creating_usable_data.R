source("paths_and_packages.R")

# Loading the list of data
data <- read_rds(file.path(data_path, "LLS_database_consolidated_2.rds"))

# Creating the table with all the articles (citing and cited)
documents <- pluck(data, 1) %>% 
  as.data.table(key = "doi") %>% 
  .[! is.na(LLS_id)] %>% # we remove all the documents for which we have only the doi.
  unique()

# Cleaning the table of direct citations
direct_citations <- pluck(data, 2) %>% 
  as.data.table(key = c("doi_citing", "doi_cited")) %>% 
  .[doi_citing %in% documents$doi & doi_cited %in% documents$doi] %>% 
  .[, .(doi_citing, doi_cited)] %>% 
  unique()

# Articles level
article_levels <- pluck(data, 4) %>% 
  select(LLS_id, level_1 = level1, level_2 = level2) %>% 
  as.data.table(key = "LLS_id")
# We change the value of levels, 1 becomes TRUE, and NA becomes FALSE, for both level_1 and level_2 columns
article_levels[, level_1 := !is.na(level_1)]
article_levels[, level_2 := !is.na(level_2)]

# Joining id for easy of use in direct_citations
direct_citations <- merge.data.table(direct_citations, documents[, .(doi, citing_id = LLS_id)], by.x = "doi_citing", by.y = "doi", all.x = TRUE)
direct_citations <- merge.data.table(direct_citations, documents[, .(doi, cited_id = LLS_id)], by.x = "doi_cited", by.y = "doi", all.x = TRUE)
setkey(direct_citations, citing_id, cited_id)

# We merge the levels with the documents
documents <- merge(documents, article_levels[! is.na(LLS_id)], by = "LLS_id", all.x = TRUE) %>% 
  .[level_1 == TRUE]
setnames(documents, "LLS_id", "citing_id")
documents[, cited_id := citing_id] # needed to count number of citations later
documents[, year := as.integer(year)]
# We filter direct citations to keep only level_1 documents as citing documents
direct_citations <- direct_citations[doi_citing %in% documents$doi & cited_id != "46698",] # keeping only citations by level 1 articles
# Save the data
write_rds(documents, file.path(data_path, "documents.rds"), compress = "gz")
write_rds(unique(direct_citations[,.(citing_id, cited_id)]), file.path(data_path, "direct_citations.rds"), compress = "gz")

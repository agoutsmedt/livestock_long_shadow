if ("docstring" %in% installed.packages() == FALSE) {
  install.packages("docstring", dependencies = TRUE)
}
library(docstring)

#' # Transforming text as data
#'
#' ## Function for filtering the features selected
#'
filter_terms <- function(
  data,
  upper_share = 1,
  lower_share = 0,
  min_word = 0,
  max_word = Inf,
  prop_word = 1,
  document_name = "document",
  term_name = "term"
) {
  data.table::setDT(data)
  data_dt <- copy(data)
  data.table::setnames(
    data_dt,
    c(document_name, term_name),
    c("document", "term")
  )
  setkey(data_dt, document)
  nb_doc <- length(unique(data_dt$document))

  count_unigram <- data_dt %>%
    filter(ngram == "unigram") %>%
    group_by(document) %>%
    add_count(name = "nb_words") %>%
    distinct(document, nb_words)
  data_dt <- merge(data_dt, count_unigram, by = "document")

  data_filtered <- data_dt %>%
    .[, count := .N, by = term] %>%
    .[, count_per_doc := .N, by = c("term", "document")] %>%
    unique() %>%
    .[, nb_apparition := .N / nb_doc, by = term] %>%
    .[
      between(nb_apparition, lower_share, upper_share) &
        between(nb_words, min_word, max_word)
    ] %>%
    {
      if (prop_word != 1) {
        slice_max(., order_by = count, prop = prop_word)
      } else {
        .
      }
    }

  return(data_filtered)
}

#' # Functions for creating the data table for different pre-processing criteria
#'
create_topicmodels_dataset <- function(
  tuning_parameters,
  data,
  document_name = "document",
  term_name = "term",
  verbose = TRUE
) {
  data_list <- list()
  for (i in 1:nrow(hyper_grid)) {
    data_filtered <- filter_terms(
      data,
      upper_share = hyper_grid$upper_share[i],
      lower_share = hyper_grid$lower_share[i],
      min_word = hyper_grid$min_word[i],
      max_word = hyper_grid$max_word[i],
      prop_word = hyper_grid$prop_word[i],
      document_name = document_name,
      term_name = term_name
    )
    data_filtered <- as_tibble(data_filtered) %>%
      mutate(
        upper_share = hyper_grid$upper_share[i],
        lower_share = hyper_grid$lower_share[i],
        min_word = hyper_grid$min_word[i],
        max_word = hyper_grid$max_word[i],
        prop_word = hyper_grid$prop_word[i]
      )

    data_list[[i]] <- data_filtered

    if (verbose == TRUE) {
      message(paste0(
        "Hyper grid ",
        i,
        " completed. The resulting data frame has ",
        nrow(data_filtered),
        "rows."
      ))
    }
  }

  data_set <- data_list %>%
    bind_rows() %>%
    mutate(document = as.character(document)) %>%
    nest(data = 1:(which(colnames(data_filtered) == "upper_share") - 1))
}

#' ## Calculate FREX measure
#'
#' Inspired by the STM package but a bit revisited to fit with our goals

calculate_frex <- function(model, nb_terms = nb_terms, w = w) {
  logbeta <- model$beta$logbeta[[1]]

  col.lse <- function(mat) {
    matrixStats::colLogSumExps(mat)
  }

  excl <- t(t(logbeta) - col.lse(logbeta))
  freqscore <- apply(logbeta, 1, data.table::frank) / ncol(logbeta)
  exclscore <- apply(excl, 1, data.table::frank) / ncol(logbeta)
  frex <- 1 / (w / freqscore + (1 - w) / exclscore)
  frex <- data.table("term" = model$vocab, as.data.table(frex)) %>%
    pivot_longer(
      cols = starts_with("V"),
      names_to = "topic",
      values_to = "frex"
    ) %>%
    mutate(topic = as.integer(str_remove(topic, "V"))) %>%
    group_by(topic) %>%
    slice_max(frex, n = nb_terms) %>%
    select(topic, term, frex) %>%
    mutate(rank = row_number(), mean = mean(frex))
}

#' Calculate Frex mean
#'
average_frex <- function(model, nb_terms = nb_terms, w = w) {
  frex_mean <- calculate_frex(model, nb_terms = nb_terms, w = w) %>%
    select(topic, mean) %>%
    unique()
  frex_mean <- mean(frex_mean$mean)
}

#' ## Calculate Beta (Highest probability of a word per topic):
#'
calculate_beta <- function(model, nb_terms = 10) {
  beta_value <- tidytext::tidy(model, matrix = "beta") %>%
    filter(!is.na(topic) & !is.na(term)) %>%
    group_by(topic) %>%
    slice_max(beta, n = nb_terms) %>%
    mutate(rank = row_number())
}

#' # Studying a topic model
#'
#' Many functions that follow are used to transform the data produce by the
#' `stm` package in data.frame/data.table to be used in `ggplot2`/`ggraph`.
#'
#' ## Plot top beta words
#'
#' > Deprecated

plot_beta_value <- function(model, nb_terms = 10) {
  beta_top_terms <- tidytext::tidy(model, matrix = "beta") %>%
    group_by(topic) %>%
    slice_max(beta, n = nb_terms) %>%
    ungroup() %>%
    arrange(topic, -beta)

  beta_top_terms %>%
    mutate(term = reorder_within(term, beta, topic)) %>%
    ggplot(aes(beta, term, fill = factor(topic))) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~topic, scales = "free") +
    scale_y_reordered()
}

#' ## Naming topics
#'
name_topics <- function(
  data,
  method = c("beta", "frex", "score", "lift"),
  nb_word = 3
) {
  labels <- data %>%
    filter(
      rank <= nb_word &
        measure == method
    ) %>%
    select(topic, term, value) %>%
    group_by(topic) %>%
    mutate(term_label = paste0(term, collapse = " / ")) %>%
    ungroup() %>%
    select(-value, -term) %>%
    rename(id = topic) %>%
    mutate(
      topic = paste0("Topic ", id),
      topic_name = paste0(topic, "\n", term_label)
    ) %>%
    unique()
}

#' Compute eigenvector centrality for each node in a list of network/igraph objects
#'
#' @description
#' Compute the eigenvector centrality for every node in each network contained in
#' the input list. The function returns a tidy tibble where each row is a node
#' observation tied to the originating window (list id).
#'
#' @details
#' Eigenvector centrality measures the influence of a node in a network by
#' assigning relative scores based on the principle that connections to highly
#' scored nodes contribute more to a node's score than connections to low-scored
#' nodes. Formally, it is obtained as the entries of the principal eigenvector
#' v of the adjacency matrix A (A v = λ v), where larger entries indicate more
#' central/influential nodes. Typical properties and caveats:
#' - Values are non-negative for standard adjacency matrices and are often
#'   normalized (implementation-dependent); higher values mean greater influence.
#' - For directed networks the interpretation depends on whether incoming or
#'   outgoing ties are emphasized by the underlying centrality implementation.
#' - Weighted edges are accounted for if the network object encodes weights.
#' - In disconnected graphs, scores for nodes outside the largest component may
#'   be very small or effectively zero; interpretation should consider network
#'   connectivity.
#' - Signed networks or cases with non-unique leading eigenvalues require
#'   additional care; results may be unstable in those settings.
#'
#' @param networks list-like of network or igraph objects (for example, the
#'   output of build_dynamic_networks). Each element should be a network object
#'   for which node-wise attributes can be computed (the function uses the
#'   object's node context and centrality_eigen()).
#'
#' @return A tibble with the following columns:
#'   - window: list id / window identifier (copied from the input list names or index)
#'   - citing_id: node identifier within the network
#'   - eigenvector_centrality: numeric centrality score for the node
#'
#' @examples
#' # networks <- build_dynamic_networks(...)
#' # compute_eigenvector_centrality(networks)
#'
#' @export
#' Compute participation coefficient per-node across a list of networks
#'
#' Participation coefficient measures how distributed a node's links are across communities.
#' We return NA for nodes with fewer than min_neighbors neighbors (insufficient data).
#'
#' @param networks list-like of network objects
#' @param min_neighbors integer minimum neighbor count to compute coefficient (default 4)
#' @return tibble with columns: window, citing_id, participation_coefficient
compute_participation_coefficient <- function(networks, min_neighbors = 4) {
  purrr::imap_dfr(networks, function(net, window_id) {
    # Work with igraph semantics via tibble-networks (.N and V/.x interchangeable)
    # Create vectors once for speed
    total_degree <- net %N>%
      dplyr::mutate(total_degree = centrality_degree(mode = "all")) %>%
      dplyr::pull(total_degree)
    # If total_degree is length zero, return empty tibble
    if (length(total_degree) == 0) {
      return(tibble::tibble(
        window = window_id,
        citing_id = character(0),
        participation_coefficient = numeric(0)
      ))
    }
    # Compute participation coefficient robustly
    pc_vec <- sapply(seq_len(length(total_degree)), function(i) {
      nbrs <- igraph::neighbors(net, i, mode = "all")
      if (length(nbrs) >= min_neighbors && total_degree[i] > 0) {
        # neighbor communities (assumes node attribute cluster_leiden exists)
        neighbor_communities <- igraph::V(net)[nbrs]$cluster_leiden
        # handle case where neighbor_communities are NULL or NA
        if (is.null(neighbor_communities) || all(is.na(neighbor_communities))) {
          return(NA_real_)
        }
        community_counts <- table(neighbor_communities, useNA = "no")
        # numeric safety: coerce to numeric and divide by degree
        p <- sum((as.numeric(community_counts) / total_degree[i])^2)
        1 - p
      } else {
        NA_real_
      }
    })
    out <- net %N>%
      dplyr::mutate(participation_coefficient = pc_vec) %>%
      dplyr::select(citing_id, participation_coefficient) %>%
      tibble::as_tibble()
    dplyr::bind_cols(tibble::tibble(window = window_id), out)
  })
}

# Exploring cluster attributes functions------------------

#' Compute per-group and global level shares from a joined table
#'
#' @title Compute group-level and global shares
#' @description
#' Compute weighted shares for each thematic group (`extended_name`) across a
#' specified categorical level (e.g. `country`, `region`, `subject_area`).
#' Shares split a document's contribution equally across multiple rows for the
#' same `citing_id` (documents with multiple affiliations or subject areas).
#'
#' @param joined_df tibble or data.frame. Must contain `citing_id` (identifier),
#'   `extended_name` (thematic group name), and a column named by `level_col`
#'   (character scalar or factor). Rows with missing or empty level values are
#'   ignored.
#' @param level_col character scalar. Name of the column in `joined_df` to
#'   aggregate over (e.g. `"country"`, `"region"`, `"subject_area"`). Must be a
#'   valid column name present in `joined_df`.
#' @param level_name NULL or character scalar. Optional human-readable name for
#'   the level; if `NULL` the `level_col` value is used. Not used in calculations.
#'
#' @return A tibble with columns:
#'   - `extended_name` (character): thematic group name.
#'   - `level` (character): value of the `level_col` (renamed).
#'   - `group_weight` (numeric): summed share of documents for group & level.
#'   - `group_total` (numeric): total summed share for the group across all levels.
#'   - `group_share` (numeric): proportion of the group's weight accounted by `level` (group_weight / group_total).
#'   - `global_weight` (numeric): total weighted occurrences of `level` across all groups.
#'   - `global_share` (numeric): `global_weight` normalized to sum to 1 across levels.
#'
#'   Returns an empty tibble with these columns if no valid (non-missing,
#'   non-empty) `level_col` rows are present.
#'
#' @details
#' This function is a small aggregation utility used to compute both group-level
#' distributions and global baseline shares for plotting and diversity
#' calculations. It treats each `citing_id` as contributing equally across all
#' associated `level_col` rows (documents with multiple affiliations or subject
#' area tags are split).
#'
#' Implementation notes:
#' - Algorithm:
#'   1. Filter out rows where the `level_col` is missing or empty.
#'   2. For each `citing_id`, assign `share = 1 / n_rows_for_citing_id` so the
#'      document's influence is split evenly across its rows.
#'   3. Sum shares by `(extended_name, level)` to get `group_weight`.
#'   4. Sum shares by `extended_name` to get `group_total`.
#'   5. Compute `group_share = group_weight / group_total`.
#'   6. Sum shares by `level` across all groups to get `global_weight` and
#'      `global_share = global_weight / sum(global_weight)`.
#' - Assumptions and preconditions:
#'   - `joined_df` contains `citing_id` and `extended_name`. The `level_col`
#'     column must exist and be character-like or factor.
#'   - Documents are represented by `citing_id` and may appear multiple times
#'     with different `level_col` values; these are split equally.
#' - Edge cases / failure modes:
#'   - If no valid rows remain after filtering for `level_col`, the function
#'     returns an empty tibble with the expected columns (no error).
#'   - NA or empty `level_col` entries are excluded from all sums.
#' - Trade-offs:
#'   - Equal-splitting of document weights (`1 / n`) is simple and interpretable;
#'     alternatives (first-author weighting, fractional by affiliation rank) are
#'     possible but out of scope here.
#' - Dependencies:
#'   - Uses dplyr-style verbs (`filter`, `mutate`, `group_by`, `summarize`,
#'     `left_join`) and `rlang::sym` for programmatic column selection.
#'   - No global state is mutated.
#'
#' @implementation
#' The function uses a short pipeline: filter -> compute per-document shares ->
#' aggregate by group/level and by level -> join results and compute proportions.
#'
#' @examples
#' library(tibble)
#' df <- tibble(
#'   citing_id = c(1, 1, 2, 3, 3, 3),
#'   extended_name = c("A", "A", "A", "B", "B", "B"),
#'   country = c("X", "Y", "X", "Y", "Z", "Y")
#' )
#' # Normal case
#' compute_group_stats(df, level_col = "country")
#' # Edge case: no valid level rows
#' compute_group_stats(tibble(citing_id = integer(), extended_name = character(), country = character()), "country")
#'
#' @keywords internal
#' @family aggregation
#' @seealso dplyr::group_by, dplyr::summarize
#' @author Positron Assistant
compute_group_stats <- function(joined_df, level_col, level_name = NULL) {
  # Ensure a usable level name (not used for calculations, but kept for API parity)
  level_name <- if (is.null(level_name)) level_col else level_name

  # Filter out rows with missing/empty level values early for simpler downstream logic
  joined_filtered <- joined_df |>
    filter(!is.na(.data[[level_col]]) & .data[[level_col]] != "")

  # Return an empty tibble with expected columns if nothing to compute
  if (nrow(joined_filtered) == 0) {
    return(tibble::tibble(
      extended_name = character(),
      level = character(),
      group_weight = double(),
      group_total = double(),
      group_share = double(),
      global_weight = double(),
      global_share = double()
    ))
  }

  # Assign each citing_id an equal share across its rows. `.by = citing_id` groups
  # the mutate so `n()` counts rows per citing_id. This splits a document's
  # contribution across multiple affiliations/subject tags.
  with_share <- joined_filtered |>
    mutate(share = 1 / dplyr::n(), .by = citing_id)

  # Compute total weight per group (sum of shares for each extended_name)
  group_totals <- with_share |>
    summarize(group_total = sum(share, na.rm = TRUE), .by = extended_name)

  # Compute group x level weights (e.g. group "A" in country "X")
  # `rlang::sym(level_col)` is used to programmatically refer to the level column.
  group_weights <- with_share |>
    dplyr::group_by(extended_name, !!rlang::sym(level_col)) |>
    summarize(group_weight = sum(share, na.rm = TRUE), .groups = "drop")

  # Compute global weights per level (across all groups) and normalize to shares
  global_weights <- with_share |>
    dplyr::group_by(!!rlang::sym(level_col)) |>
    summarize(global_weight = sum(share, na.rm = TRUE), .groups = "drop") |>
    mutate(global_share = global_weight / sum(global_weight))

  # Join the pieces: group weights + group totals -> compute group_share,
  # then attach global weights. `setNames(level_col, level_col)` supplies the
  # correct join name for the programmatic column.
  stats <- group_weights |>
    left_join(group_totals, by = "extended_name") |>
    mutate(group_share = group_weight / group_total) |>
    left_join(global_weights, by = setNames(level_col, level_col)) |>
    # Rename the programmatic level column to `level` for consistent output
    rename(level = !!rlang::sym(level_col)) |>
    select(
      extended_name,
      level,
      group_weight,
      group_total,
      group_share,
      global_weight,
      global_share
    )

  stats
}

#' @title Plot group-level shares with global baseline points
#' @description
#' Create a faceted bar plot showing the top `level` contributors for each
#' thematic `extended_name` and overlay the global share as a point. The plot
#' is saved to `file.path(figures_path, filename)` and the ggplot object is
#' returned for further modifications.
#'
#' @param stats_df tibble or data.frame. Required. Must contain columns:
#'   - `extended_name` (character or factor): thematic group name.
#'   - `level` (character): categorical level values (will be re-wrapped).
#'   - `group_share` (numeric scalar in [0,1]): share of group represented by `level`.
#'   - `global_share` (numeric scalar in [0,1]): global share of `level` across groups.
#'   Rows with missing or empty `level` or `group_share` are effectively ignored;
#'   the function does not perform heavy validation.
#' @param top_n integer scalar (default 6). Number of top `level` rows to keep
#'   per `extended_name` based on `group_share`. If `top_n <= 0` returns an
#'   empty plot data subset.
#' @param wrap_width integer scalar (default 30). Width in characters used with
#'   `stringr::str_wrap()` to wrap `level` labels before reordering.
#' @param filename character scalar. File name used when saving the plot.
#'   Must be writable in `figures_path`.
#' @param figures_path character scalar. Directory path where the file is saved.
#'   If the directory does not exist `ggsave()` will error unless the caller
#'   creates it first.
#' @param base_size numeric scalar (default 18). Base font size passed to
#'   `theme_minimal()`.
#' @param facet_order character vector. Order of `extended_name` facets; values
#'   not present in `stats_df` are ignored. Default uses `extended_order`.
#' @param ncol integer scalar (default 2). Number of columns in facet layout.
#' @param plot_height numeric scalar (default 12). Height in inches for saved file.
#' @param plot_width numeric scalar (default 16). Width in inches for saved file.
#' @param palette list with numeric `begin`, `end` in [0,1] and integer `direction`
#'   (default list(begin = 0.1, end = 0.85, direction = -1)). Controls
#'   `scico` discrete palette sampling for fill and color scales.
#'
#' @return A `ggplot` object (from ggplot2). Side-effect: writes a PNG file to
#'   `file.path(figures_path, filename)` via `ggsave()`. If saving fails (e.g.,
#'   unwritable path) an error is raised by `ggsave()`.
#'
#' @details
#' This helper prepares per-group plot data by selecting the top `level`
#' categories per thematic group and ordering `level` labels within each facet
#' so bars appear sorted. The global share is shown as a point marker to allow
#' quick comparison between the group's internal composition and the overall
#' baseline.
#'
#' Implementation notes:
#' - Algorithm:
#'   1. For each `extended_name`, keep the top `top_n` rows by `group_share`
#'      (`slice_max(..., with_ties = FALSE)` avoids ambiguous ties).
#'   2. Re-wrap long `level` labels (`str_wrap`) and reorder them within each
#'      facet using `reorder_within()` so ordering is facet-local.
#'   3. Build a ggplot with bars for `group_share` and points for `global_share`.
#'   4. Apply `scico` discrete palette sampling for consistent colors and save
#'      the resulting plot via `ggsave()`.
#' - Assumptions & preconditions:
#'   - `stats_df` contains `extended_name`, `level`, `group_share`, `global_share`.
#'   - `group_share` and `global_share` are proportions (0-1).
#' - Edge cases / failure modes:
#'   - If `stats_df` is empty or `top_n` <= 0, the function returns a plot
#'     object built from an empty data frame and still attempts to save it.
#'   - If `figures_path` is not writable or `filename` is invalid, `ggsave()`
#'     raises an error.
#' - Trade-offs:
#'   - Uses equal sampling of the discrete scico palette to color levels which
#'     gives consistent visual continuity at the cost of fixed palette choices.
#' - Dependencies:
#'   - `ggplot2`, `scico`, `ggtext`, `scales`, `stringr`, and `tidytext` for
#'     `reorder_within()` / `scale_y_reordered()`. The function does not alter
#'     global state.
#'
#' @implementation
#' The function uses a compact pipeline: group-by + slice_max -> mutate to wrap
#' and reorder -> build ggplot and save with `ggsave()`. `reorder_within()` is
#' used to provide facet-local ordering which `ggplot2::facet_wrap()` cannot
#' provide natively.
#'
#' @examples
#' library(tibble)
#' tmp <- tibble::tibble(
#'   extended_name = rep(c("A", "B"), each = 4),
#'   level = c("L very long label 1", "L2", "L3", "L4", "L1", "L2", "L3", "L4"),
#'   group_share = c(0.4, 0.3, 0.2, 0.1, 0.5, 0.2, 0.2, 0.1),
#'   global_share = c(0.25, 0.25, 0.25, 0.25, 0.25, 0.25, 0.25, 0.25)
#' )
#' # Normal case: save to a temp directory
#' plot_group_shares(tmp, top_n = 3, filename = "example_group_shares.png", figures_path = tempdir())
#' # Edge case: empty input still returns a ggplot object and attempts to save
#' plot_group_shares(tibble::tibble(extended_name = character(), level = character(), group_share = double(), global_share = double()), filename = "empty.png", figures_path = tempdir())
#'
#' @keywords internal
#' @family plotting
#' @seealso ggplot2::ggsave, scico::scale_fill_scico_d, ggtext::element_markdown
#' @author Positron Assistant
plot_group_shares <- function(
  stats_df,
  top_n = 6,
  wrap_width = 30,
  filename,
  figures_path,
  plot_title = NULL,
  base_size = 18,
  facet_order = extended_order,
  ncol = 2,
  plot_height = 12,
  plot_width = 16,
  palette = list(begin = 0.1, end = 0.85, direction = -1)
) {
  # Select top `top_n` levels per group. `with_ties = FALSE` avoids ambiguous ties.
  plot_data <- stats_df |>
    dplyr::group_by(extended_name) |>
    dplyr::slice_max(group_share, n = top_n, with_ties = FALSE) |>
    dplyr::ungroup() |>
    mutate(
      # Wrap long level labels for readability and reorder them within each facet.
      # `reorder_within()` creates names like "label__extended_name" used by
      # `scale_y_reordered()` to restore per-facet ordering.
      level = tidytext::reorder_within(
        stringr::str_wrap(level, wrap_width),
        group_share,
        extended_name
      ),
      # Ensure facets appear in the user-specified order (levels not present are ignored).
      extended_name = factor(extended_name, levels = facet_order)
    )

  # Build the plot:
  p <- plot_data |>
    ggplot2::ggplot(ggplot2::aes(x = group_share, y = level, fill = level)) +
    # Bars for group-local shares; position dodge kept simple but not necessary here.
    ggplot2::geom_col(
      position = ggplot2::position_dodge(width = 0.9),
      width = 0.7,
      alpha = 0.85
    ) +
    # Overlay global share as a point marker for visual comparison.
    ggplot2::geom_point(
      ggplot2::aes(x = global_share),
      shape = 21,
      size = 4,
      stroke = 0.9,
      color = "black"
    ) +
    # Use scico discrete palette sampling controlled by `palette`.
    scico::scale_fill_scico_d(
      palette = "lajolla",
      begin = palette$begin,
      end = palette$end,
      direction = palette$direction
    ) +
    scico::scale_color_scico_d(
      palette = "lajolla",
      begin = palette$begin,
      end = palette$end,
      direction = palette$direction
    ) +
    # Facet by group; labeller uses external `labels_vec` for markdown labels.
    ggplot2::facet_wrap(
      ~extended_name,
      ncol = ncol,
      scales = "free_y",
      dir = "v",
      labeller = ggplot2::labeller(extended_name = labels_vec)
    ) +
    ggplot2::guides(fill = "none", color = "none") +
    ggplot2::labs(
      #    x = "Share of articles in the thematic group (bar) vs global share (point)",
      x = NULL,
      y = NULL,
      title = plot_title
    ) +
    # Percent-formatted x axis
    ggplot2::scale_x_continuous(
      labels = scales::percent_format(accuracy = 1),
      expand = c(0.01, 0)
    ) +
    # Restore per-facet y ordering created by reorder_within()
    tidytext::scale_y_reordered() +
    ggplot2::theme_minimal(base_size = base_size) +
    # Use markdown-capable strip text if labels_vec contains markdown tokens (bold)
    ggplot2::theme(
      strip.text = ggtext::element_markdown(),
      plot.margin = ggplot2::margin(8, 40, 8, 8)
    )

  # Save to file. This is a side-effect; callers must ensure `figures_path` exists.
  ggplot2::ggsave(
    file.path(figures_path, filename),
    plot = p,
    width = plot_width,
    height = plot_height,
    dpi = 300
  )

  # Return ggplot object for further customization or testing
  p
}


#' Compute diversity metrics (Shannon, normalized Shannon, HHI) per group
#'
#' @title Diversity metrics by group
#' @description
#' Compute per-group diversity summaries from a long table of level shares.
#' Returns Shannon entropy, a normalized Shannon (divided by log(k)), and the
#' Herfindahl-Hirschman Index (HHI) for each group.
#'
#' @param df tibble or data.frame. Long-format table with one or more rows per
#'   observation. Must contain the grouping column, a column of per-level shares
#'   (non-negative numerics, typically summing to 1 per group), and a column
#'   identifying the level/category. NA shares are ignored in the calculations.
#' @param group_col character scalar. Name of the grouping column (e.g.
#'   `extended_name`). Returned tibble preserves this column name. Default
#'   `"extended_name"`.
#' @param share_col character scalar. Name of the numeric share column (per-row
#'   weight inside a group). Expected to be a numeric scalar per row (e.g.
#'   `group_share`). Missing values are ignored. Default `"group_share"`.
#' @param level_col character scalar. Name of the column that identifies the
#'   categorical level (e.g. country, subject, journal). Used to compute the
#'   number of distinct levels per group for normalization. Default `"level"`.
#' @param epsilon numeric scalar. Tiny constant added to shares when computing
#'   Shannon entropy to avoid `log(0)`. Default `1e-12`.
#'
#' @return A tibble with one row per distinct group (same name as `group_col`)
#'   and the following numeric columns:
#'   - `shannon`: Shannon entropy H = - sum(p * log(p)) computed with a small
#'     epsilon to avoid log(0).
#'   - `normalized_shannon`: `shannon / log(k)` where `k` is the number of
#'     distinct levels for the group; set to `0` when `k <= 1`.
#'   - `hhi`: Herfindahl-Hirschman Index = sum(p^2). Returns numeric(0) for an
#'     empty input.
#'
#' @details
#' Summarize diversity per group from long-format share data. The function is
#' defensive about numerical issues: it adds a tiny constant (`epsilon` from
#' the calling environment) when computing `-p * log(p)` so `log(0)` is avoided.
#'
#' Implementation notes:
#' - Algorithm:
#'   1. Group `df` by `group_col`.
#'   2. For each group compute:
#'      - `shannon = -sum((p + epsilon) * log(p + epsilon))` (na.rm = TRUE),
#'      - `hhi = sum(p^2, na.rm = TRUE)`,
#'      - `k = n_distinct(level_col)` (number of observed levels).
#'   3. Compute `normalized_shannon = shannon / log(k)` when `k > 1`, otherwise
#'      set `normalized_shannon = 0` (single-level groups are maximally
#'      concentrated, so normalized Shannon is 0).
#' - Assumptions / preconditions:
#'   - Input is long (one row per group x level observation). If shares do not
#'     sum to 1 per group, the metrics still compute but interpretation changes.
#' - Edge cases & failure modes:
#'   - Empty input yields an empty tibble with the expected columns.
#'   - Missing `share_col` or `level_col` values are ignored (`na.rm = TRUE`).
#'   - Groups with a single observed level receive `normalized_shannon = 0`.
#' - Trade-offs:
#'   - We use a tiny epsilon instead of removing zero entries to avoid changing
#'     the relative ordering of groups while preventing -Inf from `log(0)`.
#' - Dependencies / side-effects:
#'   - Relies on `dplyr` and `rlang`-compatible `.data` pronoun. Does not alter
#'     global state.
#'
#' @examples
#' library(tibble)
#' df <- tibble::tibble(
#'   extended_name = c("g1", "g1", "g2", "g2"),
#'   level = c("a", "b", "a", "a"),
#'   group_share = c(0.6, 0.4, 1, 0)
#' )
#' diversity_metrics(df, group_col = "extended_name", share_col = "group_share", level_col = "level")
#'
#' # Edge case: group with only a single observed level
#' df2 <- tibble::tibble(extended_name = "g3", level = "x", group_share = 1)
#' diversity_metrics(dplyr::bind_rows(df, df2))
#'
#' @seealso stats::entropy (for alternative entropy functions), dplyr::group_by
#' @keywords internal
#' @export
diversity_metrics <- function(
  df,
  group_col = "extended_name",
  share_col = "group_share",
  level_col = "level",
  epsilon = 1e-12
) {
  df |>
    # group by the requested grouping column (keeps the same column name)
    dplyr::group_by(dplyr::across(dplyr::all_of(group_col))) |>
    dplyr::summarize(
      # Shannon entropy: add a tiny epsilon to avoid log(0); na.rm removes missing shares
      shannon = -sum(
        (.data[[share_col]] + epsilon) * log(.data[[share_col]] + epsilon),
        na.rm = TRUE
      ),
      # number of distinct observed levels for this group (used for normalization)
      n_levels = dplyr::n_distinct(.data[[level_col]]),
      # Herfindahl-Hirschman Index (concentration): sum of squared shares
      hhi = sum((.data[[share_col]])^2, na.rm = TRUE),
      .groups = "drop"
    ) |>
    # normalized Shannon: divide by log(k) when k > 1; set to 0 when k <= 1
    dplyr::mutate(
      normalized_shannon = ifelse(n_levels > 1, shannon / log(n_levels), 0)
    ) |>
    # remove helper column and return a tidy tibble
    dplyr::select(dplyr::all_of(group_col), shannon, normalized_shannon, hhi)
}

#' Rank plot of thematic-group diversity indicators
#'
#' @title Thematic-group ranking across diversity indicators
#' @description
#' Create a rank-line plot that compares thematic groups across a set of
#' prefixed metric columns (e.g. `country_shannon`, `country_hhi`). The plot
#' connects ranks across indicators for each group, labels group names at the
#' left and right ends, and saves a PNG to `figures_path`.
#'
#' @param metrics_df tibble. One row per group, must contain a column named
#'   `extended_name` (group label) and one or more metric columns whose names
#'   start with `prefix` (e.g. `"country_shannon"`, `"country_hhi"`). Metric
#'   columns must be numeric; missing values are allowed but may affect ranks.
#' @param prefix character scalar. Prefix used to select metric columns via
#'   `starts_with(prefix)`. Include the trailing underscore if used in column
#'   names (e.g. `"country_"`). The function errors if no matching columns are
#'   present.
#' @param alluvial_df tibble. Data frame that must contain `extended_name` and
#'   `color` (character hex or palette name); `color` is used to draw and label
#'   lines/points. Additional columns are ignored.
#' @param figures_path character scalar. Directory path where the PNG file will
#'   be written. The function constructs the filename internally; the directory
#'   must exist or saving will fail.
#'
#' @return A ggplot object (class `ggplot`) showing ranks by indicator. The
#'   function also has the side-effect of saving a PNG file named
#'   `thematic_group_rank_by_{str_remove(prefix, '_')}_indicators.png` into
#'   `figures_path`. The saved file name is constructed internally regardless of
#'   the `filename` argument.
#'
#' @details
#' The function reshapes `metrics_df` into long form (one row per group x
#' indicator), converts indicator names to human-readable labels
#' ("Shannon", "Normalized Shannon", "Herfindahl-Hirschman Index"), computes
#' ranks per indicator (higher Shannon -> better rank; lower HHI -> better
#' rank) and plots connecting lines and endpoint labels. Labels are wrapped and
#' positioned with `geom_label_repel()` so they do not overlap.
#'
#' Implementation notes:
#' - Steps:
#'   1. `pivot_longer()` selects metric columns with `starts_with(prefix)` and
#'      creates `indicator` and `value`.
#'   2. Map raw indicator names to friendly labels and set factor ordering.
#'   3. Compute ranks per `indicator` using `dense_rank()`; HHI is ranked
#'      ascending (lower is better), others descending.
#'   4. Join `alluvial_df` to get plotting `color` per group.
#'   5. Plot with `geom_line()`, `geom_point()` and `geom_label_repel()` to
#'      label first and last indicators for each group. Save the plot with
#'      `ggsave()` to `figures_path`.
#' - Assumptions / preconditions:
#'   - `metrics_df` contains `extended_name` and numeric columns matching
#'     `starts_with(prefix)`. If no columns match the function aborts.
#'   - `alluvial_df` contains matching `extended_name` values and a `color`
#'     column.
#' - Edge cases & failure modes:
#'   - If no metric columns match `prefix`, `pivot_longer()` will error.
#'   - If `metrics_df` or `alluvial_df` are empty, the function may error when
#'     computing ranks or plotting.
#'   - The function writes a PNG file; if `figures_path` does not exist or is
#'     not writable, `ggsave()` will error.
#' - Trade-offs:
#'   - The function constructs and saves a plot file automatically. This is
#'     convenient for pipeline usage but reduces caller control over file naming.
#' - Dependencies / side-effects:
#'   - Depends on `dplyr`, `tidyr`, `ggplot2`, `ggrepel`, `stringr`, `glue`
#'     (and their helper functions). It writes a PNG file to disk and returns
#'     the ggplot object; it does not modify global R state otherwise.
#'
#' @examples
#' library(tibble)
#' library(dplyr)
#' # minimal example, prefix = "country_"
#' metrics_df <- tibble::tibble(
#'   extended_name = c("g1", "g2", "g3"),
#'   country_shannon = c(0.8, 0.4, 0.6),
#'   country_normalized_shannon = c(0.9, 0.3, 0.5),
#'   country_hhi = c(0.2, 0.6, 0.4)
#' )
#' alluvial_df <- tibble::tibble(
#'   extended_name = c("g1", "g2", "g3"),
#'   color = c("#1b9e77", "#d95f02", "#7570b3")
#' )
#' # Provide an existing directory for figures_path in real usage
#' p <- make_rank_plot(metrics_df, prefix = "country_", alluvial_df, figures_path = tempdir())
#' p
#'
#' # Edge case: if no metric columns match the prefix the function will error
#' \dontrun{
#' make_rank_plot(metrics_df, prefix = "nonexistent_", alluvial_df, figures_path = tempdir())
#' }
#'
#' @keywords internal
make_rank_plot <- function(
  metrics_df,
  prefix,
  alluvial_df,
  figures_path
) {
  p_data <- metrics_df |>
    select(-contains("normalized")) |> # We remove normalized Shannon for clarity
    pivot_longer(
      cols = starts_with(prefix),
      names_to = "indicator",
      values_to = "value"
    ) |>
    mutate(
      indicator = case_when(
        indicator == paste0(prefix, "shannon") ~ "Shannon",
        indicator == paste0(prefix, "hhi") ~ "Herfindahl-Hirschman Index",
        TRUE ~ as.character(indicator)
      ),
      indicator = factor(
        indicator,
        levels = c(
          "Shannon",
          "Herfindahl-Hirschman Index"
        )
      )
    ) |>
    group_by(indicator) |>
    mutate(
      rank = dplyr::if_else(
        indicator == "Herfindahl-Hirschman Index",
        dplyr::dense_rank(value), # lower HHI -> better rank
        dplyr::dense_rank(desc(value)) # higher Shannon -> better rank
      )
    ) |>
    ungroup() |>
    left_join(
      distinct(alluvial_df, extended_name, color),
      by = "extended_name"
    )

  first_ind <- levels(p_data$indicator)[1]
  last_ind <- levels(p_data$indicator)[length(levels(p_data$indicator))]

  p <- p_data |>
    ggplot(aes(x = indicator, y = rank, group = extended_name)) +
    geom_line(aes(color = color), linewidth = 1.2, show.legend = FALSE) +
    geom_point(aes(color = color), size = 2.5, show.legend = FALSE) +
    geom_label_repel(
      data = p_data |> filter(indicator == first_ind),
      aes(label = str_wrap(extended_name, 25), color = color),
      nudge_x = -0.05,
      hjust = 1,
      direction = "y",
      segment.size = 0.5,
      size = 4,
      show.legend = FALSE
    ) +
    geom_label_repel(
      data = p_data |> filter(indicator == last_ind),
      aes(label = str_wrap(extended_name, 25), color = color),
      nudge_x = 0.05,
      hjust = 0,
      direction = "y",
      segment.size = 0.5,
      size = 4,
      show.legend = FALSE
    ) +
    scale_color_identity() +
    scale_y_reverse(breaks = scales::pretty_breaks(n = max(p_data$rank))) +
    scale_x_discrete(expand = expansion(add = c(0.4, 0.4))) +
    labs(
      title = glue(
        "Thematic-group ranking across {str_remove(prefix, '_')} diversity indicators"
      ),
      x = NULL,
      y = "Rank (1 = top)",
      caption = "Ranks per indicator; HHI ranked ascending (lower is better)"
    ) +
    theme_minimal(base_size = 18) +
    theme(
      axis.text.y = element_text(size = 9),
      axis.text.x = element_text(face = "bold"),
      legend.position = "none"
    ) +
    coord_cartesian(clip = "off")

  ggsave(
    file.path(
      figures_path,
      glue("thematic_group_rank_by_{str_remove(prefix, '_')}_indicators.png")
    ),
    plot = p,
    width = 12,
    height = 10,
    dpi = 300
  )

  p
}

#' Compute group-level connectivity from a graph
#'
#' Build group mixing matrices and simple connectivity summaries for a graph
#' whose nodes carry a categorical group attribute. The function aggregates
#' edge weights by (from_group, to_group), symmetrizes to an undirected view,
#' computes a row-normalized mixing matrix P, per-group summaries (total,
#' within, between, fraction within and conductance) and cosine similarities
#' between groups' connection profiles.
#'
#' This is useful to quantify how much groups link internally versus to other
#' groups and to compare groups by the shape of their connection profiles.
#'
#' @param g A graph object compatible with tidygraph operations. The function
#'   expects to be able to run `g %>% activate("nodes") %>% as_tibble()` and
#'   `g %>% activate("edges") %>% as_tibble()`. Edge table must contain `from`
#'   and `to` columns (node indices). Node table must contain the grouping
#'   attribute named by `node_attr`.
#' @param node_attr Character scalar. Name of the node attribute (column in the
#'   node table) that provides the group label for each node. Defaults to
#'   "extended_name".
#' @param weight_attr Character scalar. Name of the edge attribute to use as a
#'   weight. If the column is missing it will be created and set to 1 for all
#'   edges (unweighted).
#'
#' @return A named list with elements:
#'   - P: numeric matrix (groups x groups). Each row is a group's normalized
#'     connection distribution (row sums = 1 when the group has positive total).
#'   - group_summary: tibble with columns:
#'       * group: group label
#'       * total_weight: total weight incident to the group (row sum of symmetrized matrix)
#'       * within_weight: diagonal (self) weight
#'       * between_weight: total_weight - within_weight
#'       * fraction_within: within_weight / total_weight (NA if total_weight == 0)
#'       * conductance: between_weight / total_weight (NA if total_weight == 0)
#'   - cosine_similarity: numeric matrix (groups x groups) with pairwise cosine
#'     similarity of rows of P (diag set to 1). NA entries indicate missing info.
#'
#' @details Implementation notes (intuitive):
#'   - We aggregate with `xtabs` to quickly get a contingency (mixing) matrix of
#'     total weight per (from_group, to_group).
#'   - The matrix is symmetrized by summing off-diagonal entries in both
#'     directions. Diagonal entries remain as aggregated self-loop weight.
#'   - Row-normalization produces P so each group's connections become a
#'     distribution across groups; this is the basis for cosine similarity.
#'   - Cosine similarity is computed as (P %*% t(P)) / (||row_i|| * ||row_j||).
#'
#' @examples
#' \dontrun{
#' library(tidygraph)
#' # g: a tidygraph graph where nodes have extended_name and edges have weight
#' res <- compute_group_connectivity(g, node_attr = "extended_name", weight_attr = "weight")
#' res$P                 # mixing proportions (groups x groups)
#' res$group_summary     # per-group totals and conductance
#' res$cosine_similarity # profile similarity between groups
#' }
#'
#' @export
compute_group_connectivity <- function(
  g,
  node_attr = "extended_name",
  weight_attr = "weight"
) {
  # nodes / edges tibbles
  nodes <- g %>%
    activate("nodes") %>%
    as_tibble(.name_repair = "minimal") %>%
    mutate(.node = row_number())

  edges <- g %>%
    activate("edges") %>%
    as_tibble(.name_repair = "minimal")

  if (!all(c("from", "to") %in% names(edges))) {
    cli::cli_abort("edges must have from/to columns")
  }
  if (!(node_attr %in% names(nodes))) {
    cli::cli_abort("node attribute not found")
  }
  if (!(weight_attr %in% names(edges))) {
    edges[[weight_attr]] <- 1
  }

  # join group labels for from/to
  edges2 <- edges %>%
    left_join(
      nodes %>% select(.node, from_group = !!rlang::sym(node_attr)),
      by = c("from" = ".node")
    ) %>%
    left_join(
      nodes %>% select(.node, to_group = !!rlang::sym(node_attr)),
      by = c("to" = ".node")
    ) %>%
    mutate(w = coalesce(.data[[weight_attr]], 1)) %>%
    filter(!is.na(from_group), !is.na(to_group))

  # if no valid edges, return empty/zeroed structures
  if (nrow(edges2) == 0) {
    empty_mat <- matrix(numeric(0), nrow = 0, ncol = 0)
    empty_cos <- matrix(NA_real_, nrow = 0, ncol = 0)
    empty_summary <- tibble::tibble(
      group = character(0),
      total_weight = numeric(0),
      within_weight = numeric(0),
      between_weight = numeric(0),
      fraction_within = numeric(0),
      conductance = numeric(0)
    )
    return(list(
      mixing_matrix = empty_mat,
      P = empty_mat,
      group_summary = empty_summary,
      cosine_similarity = empty_cos
    ))
  }

  # build mixing matrix via xtabs (fast, vectorized)
  M0 <- stats::xtabs(w ~ from_group + to_group, data = edges2)
  M <- as.matrix(M0)

  # union of groups (ensure symmetric matrix even if some groups missing in rows/cols)
  groups <- sort(union(rownames(M), colnames(M)))
  if (!identical(rownames(M), groups) || !identical(colnames(M), groups)) {
    mat_full <- matrix(
      0,
      nrow = length(groups),
      ncol = length(groups),
      dimnames = list(groups, groups)
    )
    mat_full[rownames(M), colnames(M)] <- M
    M <- mat_full
  }

  # symmetrize: make off-diagonals represent sum of both directions; keep original diagonal
  M_sym <- M + t(M)
  diag(M_sym) <- diag(M)
  M <- M_sym

  # total weight (unique / undirected convention)
  total_weight <- (sum(M) + sum(diag(M))) / 2

  # per-group summaries
  group_totals <- rowSums(M)
  within <- diag(M)
  between <- group_totals - within
  fraction_within <- ifelse(group_totals > 0, within / group_totals, NA_real_)

  # row-normalized mixing P (rows sum to 1 when group_totals > 0)
  P <- sweep(M, 1, group_totals, FUN = function(x, y) ifelse(y > 0, x / y, 0))
  rownames(P) <- rownames(M)
  colnames(P) <- colnames(M)

  # cosine similarity between group connection profiles (rows of P)
  mat <- P
  norms <- sqrt(rowSums(mat * mat))
  denom <- outer(norms, norms)
  sim_num <- mat %*% t(mat)
  sim <- matrix(
    0,
    nrow = nrow(sim_num),
    ncol = ncol(sim_num),
    dimnames = list(rownames(mat), rownames(mat))
  )
  positive <- denom > 0
  sim[positive] <- sim_num[positive] / denom[positive]
  diag(sim) <- 1

  group_summary <- tibble::tibble(
    group = rownames(M),
    total_weight = group_totals,
    within_weight = within,
    between_weight = between,
    fraction_within = fraction_within,
    conductance = ifelse(group_totals > 0, between / group_totals, NA_real_)
  )

  list(P = P, group_summary = group_summary, cosine_similarity = sim)
}

#' Aggregate network-level mixing, similarity, and group summaries
#'
#' Summarize a list of network statistics into three aggregated outputs:
#' 1) a (possibly weighted) average of per-network normalized mixing matrices,
#' 2) a weighted mean cosine-similarity matrix across networks,
#' 3) a pooled per-group summary (means of group-level metrics across networks).
#'
#' This function is intended to work on a list where each element is a named
#' list (one per network) containing at least some of the following components:
#' - P or mixing_matrix: a square numeric matrix giving observed weights between groups
#'   (rows and columns named by group labels).
#' - cosine_similarity: a square numeric matrix (rows/cols named by group labels)
#'   giving cosine similarity between group attribute vectors; entries may be NA when
#'   a group is absent in that network.
#' - group_summary: a data.frame / tibble with columns at least
#'   group, total_weight, within_weight, conductance (one row per group present).
#'
#' The function takes the union of all group labels observed across networks,
#' pads each per-network matrix to that union (missing rows/columns filled with 0
#' for mixing matrices, NA for cosine matrices), enforces symmetry, and then
#' aggregates according to the chosen weighting.
#'
#' Key behaviors / heuristics (intuitive explanation):
#' - Per-network mixing matrices are normalized by their network total (sum of all
#'   entries) so that each network contributes a normalized pattern of mixing.
#'   Normalizing prevents large networks from dominating the average unless the
#'   user requests weighting by network total.
#' - When weight_by = "network_weight", the per-network normalized matrices are
#'   averaged using weights proportional to the observed total mixing (sum of the
#'   original mixing matrix) so networks with more observed interactions contribute
#'   more to the final mixing pattern.
#' - If all network totals are zero (no observed mixing), equal weights are used
#'   instead of zeros to avoid division-by-zero / degenerate weighting.
#' - Cosine similarity matrices are averaged element-wise using the same
#'   network totals as weights, but NA values (missing group comparisons in a
#'   network) are ignored for that pair: only networks where that pair exists
#'   contribute to the mean.
#' - The pooled group summary is computed by taking the mean (across networks)
#'   of total_weight, within_weight and conductance for each group (NA values are
#'   removed via na.rm = TRUE). Additional derived columns are added:
#'   between_weight = total_weight - within_weight and fraction_within.
#'
#' @param network_stats list. A list of per-network statistic objects. Each
#'   element is expected to be a list (or similar) that may contain:
#'   - $P (mixing matrix) OR a matrix accessible as .x$P: square numeric matrix
#'     with row and column names equal to group labels,
#'   - $cosine_similarity: square numeric matrix with row/col names (may contain NA),
#'   - $group_summary: data.frame / tibble with columns group, total_weight,
#'     within_weight, conductance.
#'   Missing components for a network are tolerated: missing mixing matrices are
#'   treated as all-zeros, missing cosine matrices as all-NA, and missing group
#'   summaries as zero/NA rows for every group.
#' @param weight_by character scalar. One of "none" (default) or "network_weight".
#'   - "none": compute the unweighted mean of per-network normalized mixing matrices
#'     (each network contributes equally after within-network normalization).
#'   - "network_weight": weight each network's normalized mixing matrix by the
#'     network's total observed mixing (sum of the mixing matrix). This gives
#'     larger networks more influence on the final averaged mixing pattern.
#'
#' @return A named list with three elements:
#'   - mixing_norm: numeric matrix (k x k) of aggregated (normalized) mixing,
#'     where k is the number of distinct groups across all networks. Row and
#'     column names are the (sorted) union of group labels. The matrix is
#'     symmetric by construction.
#'   - cosine: numeric matrix (k x k) of aggregated cosine similarities. Element
#'     (i, j) is the weighted mean of available cosine entries across networks
#'     (networks where that pair is missing are ignored). The diagonal is set to 1.
#'   - pooled_group_summary: tibble with one row per group and columns:
#'       group, total_weight, conductance, within_weight, between_weight,
#'       fraction_within. The numeric columns are computed as the mean across
#'       networks (na.rm = TRUE) for each group; derived columns computed from
#'       those means.
#'
#' @details
#' - Padding & symmetry: For mixing matrices, absent rows/columns are filled with
#'   zeros (interpreted as no observed interactions). For cosine matrices, absent
#'   entries are NA (missing). After padding, matrices are symmetrized by taking
#'   (M + t(M)) / 2 to ensure symmetry even if individual networks reported
#'   slightly asymmetric matrices. The cosine diagonal is forced to 1 when
#'   aggregated.
#' - Normalization: Each per-network mixing matrix is divided by its own total
#'   (sum over all entries) to produce a per-network distribution of mixing.
#'   If a network's sum is zero the normalized matrix is treated as all zeros.
#' - Weighting for cosine aggregation: the same per-network totals used for
#'   mixing weighting are used when computing the weighted mean of cosine
#'   similarities; however, for any particular pair (i, j) only networks where
#'   that pair exists (non-NA) are included, and their weights are re-normalized
#'   over that subset.
#'
#' @note
#' - The function is defensive: missing or NULL matrices/summaries are handled
#'   gracefully (converted to appropriate zero/NA matrices or zero/NA summary rows).
#' - The function returns means for group-level metrics; if you prefer sums or
#'   medians, compute them separately from the raw network_stats list.
#'
#' @examples
#' # Typical usage:
#' # result <- aggregate_networks(network_stats_list, weight_by = "network_weight")
#' #
#' # The input list should be like:
#' # network_stats_list[[i]]$P               # mixing matrix for network i
#' # network_stats_list[[i]]$cosine_similarity
#' # network_stats_list[[i]]$group_summary
#'
#' @export
aggregate_networks <- function(
  network_stats,
  weight_by = c("none", "network_weight")
) {
  weight_by <- match.arg(weight_by)
  N <- length(network_stats)
  if (N == 0) {
    stop("network_stats is empty")
  }

  # union of groups across all networks
  union_groups <- sort(unique(unlist(map(
    network_stats,
    ~ rownames(.x$P)
  ))))
  k <- length(union_groups)

  # pad mixing matrix to union of groups, enforce symmetry
  pad_matrix <- function(M, groups, fill = 0) {
    if (is.null(M) || !is.matrix(M)) {
      return(matrix(
        fill,
        nrow = length(groups),
        ncol = length(groups),
        dimnames = list(groups, groups)
      ))
    }
    mat_full <- matrix(
      fill,
      nrow = length(groups),
      ncol = length(groups),
      dimnames = list(groups, groups)
    )
    common_r <- intersect(rownames(M), groups)
    common_c <- intersect(colnames(M), groups)
    mat_full[common_r, common_c] <- M[common_r, common_c, drop = FALSE]
    mat_full <- (mat_full + t(mat_full)) / 2
    diag(mat_full) <- diag(mat_full)
    mat_full
  }

  # pad cosine similarity matrix to union of groups, enforce symmetry, diag = 1
  pad_cosine <- function(S, groups) {
    if (is.null(S) || !is.matrix(S)) {
      return(matrix(
        NA_real_,
        nrow = length(groups),
        ncol = length(groups),
        dimnames = list(groups, groups)
      ))
    }
    mat_full <- matrix(
      NA_real_,
      nrow = length(groups),
      ncol = length(groups),
      dimnames = list(groups, groups)
    )
    common_r <- intersect(rownames(S), groups)
    common_c <- intersect(colnames(S), groups)
    mat_full[common_r, common_c] <- S[common_r, common_c, drop = FALSE]
    mat_full <- (mat_full + t(mat_full)) / 2
    diag(mat_full) <- 1
    mat_full
  }

  # build padded lists
  mixing_list <- map(network_stats, ~ pad_matrix(.x$P, union_groups, fill = 0))
  cosine_list <- map(
    network_stats,
    ~ pad_cosine(.x$cosine_similarity, union_groups)
  )

  # network weights as total observed weight per network (sum of mixing matrix)
  net_weights <- map_dbl(mixing_list, ~ sum(.x, na.rm = TRUE))
  if (all(net_weights == 0)) {
    net_weights[] <- 1
  }

  # 1) per-network normalized matrices (divide by network total) -> then average
  norm_per_net <- map(mixing_list, function(mat) {
    wsum <- sum(mat, na.rm = TRUE)
    if (wsum > 0) mat / wsum else mat * 0
  })
  mixing_norm_unweighted_avg <- Reduce(`+`, norm_per_net) / length(norm_per_net)

  # weighted average of normalized matrices (if requested)
  if (weight_by == "network_weight") {
    wsum_total <- sum(net_weights, na.rm = TRUE)
    mixing_norm_weighted_avg <- Reduce(
      `+`,
      map2(norm_per_net, net_weights, ~ .x * (.y / wsum_total))
    )
  } else {
    mixing_norm_weighted_avg <- mixing_norm_unweighted_avg
  }

  # 2) cosine similarity aggregated: compute weighted mean across networks (weighted by net_weights)
  Nn <- length(cosine_list)
  cos_arr <- array(
    NA_real_,
    dim = c(k, k, Nn),
    dimnames = list(union_groups, union_groups, NULL)
  )
  for (i in seq_len(Nn)) {
    cos_arr[,, i] <- cosine_list[[i]]
  }

  # weighted mean (use net_weights but only where value is not NA)
  cos_mean_weighted <- apply(cos_arr, c(1, 2), function(v) {
    present <- !is.na(v)
    if (!any(present)) {
      return(NA_real_)
    }
    w <- net_weights[present]
    sum(v[present] * w) / sum(w)
  })
  dimnames(cos_mean_weighted) <- list(union_groups, union_groups)

  # 3) pooled per-group summary (sum totals and within weights, mean conductance)
  group_summaries <- map(network_stats, function(st) {
    gs <- st$group_summary
    if (is.null(gs)) {
      tibble(
        group = union_groups,
        total_weight = 0,
        within_weight = 0,
        conductance = NA_real_
      )
    } else {
      as_tibble(gs) |>
        select(
          group,
          total_weight = total_weight,
          within_weight = within_weight,
          conductance = conductance
        )
    }
  })

  pooled_summary <- bind_rows(group_summaries) |>
    group_by(group) |>
    summarize(
      total_weight = mean(total_weight, na.rm = TRUE),
      conductance = mean(conductance, na.rm = TRUE),
      within_weight = mean(within_weight, na.rm = TRUE),
      .groups = "drop"
    ) |>
    mutate(
      between_weight = total_weight - within_weight,
      fraction_within = ifelse(
        total_weight > 0,
        within_weight / total_weight,
        NA_real_
      )
    )

  list(
    mixing_norm = mixing_norm_weighted_avg,
    cosine = cos_mean_weighted,
    pooled_group_summary = pooled_summary
  )
}


#' Plot a clustered heatmap of pairwise group relationships
#'
#' Create a tiled heatmap of values in a square matrix, optionally clustering rows
#' (and reordering columns to match) and applying wrapped, markdown-capable axis labels.
#'
#' @param mat A square numeric matrix or data frame convertible to a matrix. Must have
#'   both rownames and colnames. Values are mapped to tile fill.
#' @param title Character. Title to show on the plot.
#' @param fill_label Character. Legend title for the fill scale.
#' @param figures_path Character. Optional path intended for saving figures. (Not used
#'   inside the function; provided for downstream saving by the caller.)
#' @param filename Character. Optional filename intended for saving the plot. (Not used
#'   inside the function; provided for downstream saving by the caller.)
#' @param wrap_width Integer. Maximum line width (in characters) used to wrap axis
#'   labels before converting line breaks to HTML <br> for ggtext rendering. Default 20.
#' @param cluster_method Character. Linkage method passed to hclust (e.g. "complete",
#'   "ward.D", etc.). Default "complete".
#' @param dist_method Character. Distance method passed to dist for computing row
#'   distances (e.g. "euclidean", "manhattan"). Default "euclidean".
#' @param scico_palette Character. Name of the scico palette to use for the fill scale.
#'   Default "lajolla".
#'
#' @return A ggplot2 object (ggplot). The plot uses ggtext::element_markdown for axis text,
#'   so wrapped Markdown (bold) and HTML <br> line breaks are rendered. The function
#'   returns the plot and does not save it to disk.
#'
#' @details
#' - Validates that the input can be treated as a matrix and that row/column names are present.
#' - If the matrix has more than one row, the function computes pairwise distances between
#'   rows and performs hierarchical clustering (hclust) using the provided methods. The
#'   clustering order is used to reorder both rows and columns so that proximity is
#'   visually consistent on both axes.
#' - Labels that match the case-insensitive patterns "protein", "Socio-", or "Sustainable"
#'   are wrapped with Markdown bold markers (**) before wrapping; all labels are wrapped
#'   to the specified wrap_width and newline characters are converted to HTML <br> so
#'   they render correctly with ggtext.
#' - Visual styling: tiles via geom_tile, color scale from scico::scale_fill_scico
#'   (direction reversed by default), minimal-like theme with larger base text size,
#'   and a bottom legend implemented as a horizontal colorbar.
#'
#' @examples
#' # Create a random symmetric matrix with names and plot it
#' mat <- matrix(runif(16), nrow = 4)
#' mat <- (mat + t(mat)) / 2
#' rownames(mat) <- colnames(mat) <- c("protein A", "Group B", "Socio-eco C", "Sustainable D")
#' p <- plot_heatmap(mat, title = "Example heatmap", fill_label = "Value")
#' print(p)
#'
#' @seealso ggplot2::geom_tile, scico::scale_fill_scico, ggtext::element_markdown
#' @export
plot_heatmap <- function(
  mat,
  title = NULL,
  fill_label,
  figures_path,
  filename,
  wrap_width = 20,
  cluster_method = "complete",
  dist_method = "euclidean",
  scico_palette = "lajolla"
) {
  # ensure matrix
  mat_clust <- as.matrix(mat)
  if (is.null(rownames(mat_clust)) || is.null(colnames(mat_clust))) {
    stop("mat must have rownames and colnames")
  }

  # compute distance on rows and hierarchical clustering
  # if there is only one group, keep order as is
  if (nrow(mat_clust) > 1) {
    d <- dist(mat_clust, method = dist_method)
    hc <- hclust(d, method = cluster_method)
    ord <- hc$order
    new_order <- rownames(mat_clust)[ord]
  } else {
    new_order <- rownames(mat_clust)
  }

  # reorder matrix rows and columns by the same order (proximity)
  mat_reordered <- mat_clust[new_order, new_order, drop = FALSE]

  # Build wrapped labels and bold selected communities (same pattern used earlier)
  bold_pattern <- stringr::regex(
    "protein|Socio-|Sustainable",
    ignore_case = TRUE
  )

  wrap_and_bold <- function(x) {
    x_b <- dplyr::if_else(
      stringr::str_detect(
        x,
        stringr::regex("protein|Socio-|Sustainable", ignore_case = TRUE)
      ),
      paste0("**", x, "**"),
      x
    )
    x_b <- stringr::str_wrap(x_b, width = wrap_width)
    x_b <- stringr::str_replace_all(x_b, "\n", "<br>")
    return(x_b)
  }

  # melt for ggplot and wrap labels
  mat_melt <- as.data.frame(as.table(mat_reordered)) |>
    dplyr::rename(from_group = Var1, to_group = Var2, value = Freq) |>
    dplyr::mutate(
      from_group_wr = wrap_and_bold(from_group),
      to_group_wr = wrap_and_bold(to_group),
      # keep factor ordering consistent with clustering
      from_group_wr = factor(from_group_wr, levels = unique(from_group_wr)),
      to_group_wr = factor(to_group_wr, levels = unique(to_group_wr))
    )

  # mimic theme_minimal with ggtext + larger base size
  base_size <- 18

  p <- ggplot2::ggplot(
    mat_melt,
    ggplot2::aes(x = to_group_wr, y = from_group_wr, fill = value)
  ) +
    ggplot2::geom_tile() +
    scico::scale_fill_scico(
      palette = scico_palette,
      direction = -1,
      na.value = "grey90"
    ) +
    ggplot2::labs(
      title = title,
      x = NULL,
      y = NULL,
      fill = fill_label
    ) +
    ggplot2::theme(
      # base text
      text = ggplot2::element_text(size = base_size),
      # title
      plot.title = ggplot2::element_text(size = base_size * 1.1, face = "bold"),
      # axis text rendered with ggtext
      axis.text.x = ggtext::element_markdown(
        size = base_size * 0.85,
        angle = 0,
        hjust = 0.5,
        vjust = 0.5,
        margin = ggplot2::margin(t = 4)
      ),
      axis.text.y = ggtext::element_markdown(
        size = base_size * 0.85,
        hjust = 1
      ),
      axis.title = ggplot2::element_text(size = base_size * 0.9),
      axis.ticks = ggplot2::element_blank(),
      # panel / grid similar to theme_minimal
      panel.background = ggplot2::element_rect(fill = "white", colour = NA),
      plot.background = ggplot2::element_rect(fill = "white", colour = NA),
      panel.grid.major = ggplot2::element_line(
        colour = "grey92",
        linewidth = 0.35
      ),
      panel.grid.minor = ggplot2::element_blank(),
      panel.border = ggplot2::element_blank(),
      # legend styling
      legend.position = "bottom",
      legend.title = ggplot2::element_text(size = base_size * 0.9),
      legend.text = ggplot2::element_text(size = base_size * 0.85),
      # tighten margins like theme_minimal
      plot.margin = ggplot2::margin(8, 8, 8, 8)
    ) +
    ggplot2::guides(
      fill = ggplot2::guide_colorbar(
        barwidth = grid::unit(10, "cm"),
        barheight = grid::unit(0.6, "cm"),
        title.position = "top",
        title.hjust = 0.5
      )
    )

  p
}

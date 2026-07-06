#### ================================================================
#### IMPUTATION LINEAGE ENGINE
#### ------------------------------------------------
#### Given an imputed RDS id "dfXXX", reconstruct:
#### - Its immediate parent input RDS file
#### - The full chain of imputation steps from the root
####   (e.g., Survival_mean → CNV_mode → Mutation_knn → Continuous_iSVD)
#### ================================================================

## ----------------------------
## 0. Helper: parse "dfXXX" → integer
## ----------------------------
parse_df_index <- function(x) {
  # x: character vector, e.g., c("dfXXX", "dfXXX")
  as.integer(sub("^df", "", x))
}

## ----------------------------
## 1. Load and prepare block table
## ----------------------------
load_imputation_block_table <- function(path = "imputation_block_table.tsv") {
  bt <- utils::read.delim(path, header = TRUE, sep = "\t", stringsAsFactors = FALSE)
  
  # Add numeric indices for convenient range logic
  bt$input_start_num  <- parse_df_index(bt$input_start)
  bt$input_end_num    <- parse_df_index(bt$input_end)
  bt$output_start_num <- parse_df_index(bt$output_start)
  bt$output_end_num   <- parse_df_index(bt$output_end)
  
  bt
}

## ------------------------------------------------
## 2. Build explicit parent→child edge list
##    Each row in the block table defines a set of
##    1:1 mappings between input and output dfXXX.
## ------------------------------------------------
build_imputation_edges <- function(block_table) {
  edge_list <- lapply(seq_len(nrow(block_table)), function(i) {
    row <- block_table[i, ]
    
    n_in  <- row$input_end_num  - row$input_start_num  + 1L
    n_out <- row$output_end_num - row$output_start_num + 1L
    
    if (n_in != n_out) {
      stop(sprintf(
        "Row %d (%s) has mismatched input/output counts: n_in=%d, n_out=%d",
        i, row$block, n_in, n_out
      ))
    }
    
    offset <- seq_len(n_in) - 1L
    
    parent_ids <- sprintf("df%03d", row$input_start_num  + offset)
    child_ids  <- sprintf("df%03d", row$output_start_num + offset)
    
    data.frame(
      parent_id       = parent_ids,
      child_id        = child_ids,
      block           = row$block,
      type            = row$type,
      method          = row$method,
      module_function = row$module_function,
      stringsAsFactors = FALSE
    )
  })
  
  do.call(rbind, edge_list)
}

## ------------------------------------------------
## 3. Trace lineage for a single dfXXX
##    Walk backwards: target → parent → parent → ...
## ------------------------------------------------
trace_imputation_lineage_single <- function(target_id,
                                            edges) {
  if (!is.character(target_id) || length(target_id) != 1L) {
    stop("trace_imputation_lineage_single() expects a single character id like 'df125'.")
  }
  
  lineage <- list()
  current <- target_id
  
  repeat {
    # Find edge(s) where this df is the child
    hit <- which(edges$child_id == current)
    
    if (length(hit) == 0L) {
      # No parent found: current is the root (e.g., dfXXX or raw)
      break
    }
    
    if (length(hit) > 1L) {
      warning(sprintf(
        "Multiple parents found for '%s'. Using the first one. Check edge consistency.",
        current
      ))
      hit <- hit[1L]
    }
    
    row <- edges[hit, ]
    lineage[[length(lineage) + 1L]] <- row
    current <- row$parent_id
  }
  
  if (length(lineage) == 0L) {
    # No edges at all: orphan or root with no imputation
    lineage_df <- data.frame(
      step            = integer(0),
      from_id         = character(0),
      to_id           = character(0),
      type            = character(0),
      block           = character(0),
      method          = character(0),
      module_function = character(0),
      stringsAsFactors = FALSE
    )
    attr(lineage_df, "root") <- target_id
    return(lineage_df)
  }
  
  # lineage is from target backwards; reverse to get root → target
  lineage_df <- do.call(rbind, rev(lineage))
  
  # Add a step index and clearer column names
  lineage_df <- data.frame(
    step            = seq_len(nrow(lineage_df)),
    from_id         = lineage_df$parent_id,
    to_id           = lineage_df$child_id,
    type            = lineage_df$type,
    block           = lineage_df$block,
    method          = lineage_df$method,
    module_function = lineage_df$module_function,
    stringsAsFactors = FALSE
  )
  
  # Root is the first "from_id" that no longer has a parent edge
  attr(lineage_df, "root") <- lineage_df$from_id[1L]
  
  lineage_df
}

## ------------------------------------------------
## 4. Vectorized wrapper for multiple dfXXX targets
## ------------------------------------------------
trace_imputation_lineage <- function(target_ids,
                                     block_table_path = "imputation_block_table.tsv") {
  bt    <- load_imputation_block_table(block_table_path)
  edges <- build_imputation_edges(bt)
  
  res_list <- lapply(target_ids, function(id) {
    df <- trace_imputation_lineage_single(id, edges)
    attr(df, "target") <- id
    df
  })
  
  names(res_list) <- target_ids
  res_list
}

## ------------------------------------------------
## 5. Convenience: compact method signature for a dfXXX
## ------------------------------------------------
summarize_imputation_methods <- function(target_id,
                                         block_table_path = "imputation_block_table.tsv") {
  lineage_list <- trace_imputation_lineage(target_id, block_table_path = block_table_path)
  lineage_df   <- lineage_list[[1L]]
  
  if (nrow(lineage_df) == 0L) {
    return(list(
      target_id        = target_id,
      root_id          = target_id,
      lineage_table    = lineage_df,
      method_signature = NA_character_
    ))
  }
  
  # e.g. "Survival:mean -> CNV:mode -> Mutation:knn -> Continuous:iSVD"
  sig_parts <- paste0(lineage_df$type, ":", lineage_df$method)
  sig <- paste(sig_parts, collapse = " -> ")
  
  list(
    target_id        = target_id,
    root_id          = attr(lineage_df, "root"),
    lineage_table    = lineage_df,
    method_signature = sig
  )
}

#### ================================================================
#### Example usage
#### ================================================================
# Suppose the table is stored as "imputation_block_table.tsv"
# and you want to inspect df149:

library(rio)
imputation_block_table <- import("imputation_block_table.tsv")

res <- summarize_imputation_methods("df377", "imputation_block_table.tsv")
res$root_id          # e.g. "df377"
res$method_signature # e.g. "Survival:mean -> CNV:mode"
res$lineage_table    # full step-by-step from df377 to dfXXX

res_lin_table <- res$lineage_table

# For multiple targets:
lineage_all <- trace_imputation_lineage(
  c("df125", "df009", "df377", "df149", "df260"),
  "imputation_block_table.tsv"
)
lineage_all[["df377"]]  # lineage table specifically for df149

### Multiple selected rds (here maximum positive delta C-index or unchanged in the maximal number of survival metrics per cancer, nimproved_unchanged cases within the rds range df006 to df377)
# Vector of dfXXX .rds filenames from improved_unchanged_best_fullset.tsv 
df_list_rds <- c(
  "df377.rds", "df155.rds", "df008.rds", "df017.rds", "df368.rds",
  "df161.rds", "df374.rds", "df160.rds", "df147.rds", "df305.rds",
  "df157.rds"
)

### Multiple selected rds (here maximum positive delta C-index, nimproved case over 24 and within the rds range df054 to df377)
# df_list_rds <- c(
#   "df165.rds", "df166.rds", "df167.rds", "df174.rds", "df175.rds", "df176.rds",
#   "df183.rds", "df184.rds", "df185.rds", "df201.rds", "df202.rds", "df203.rds",
#   "df210.rds", "df211.rds", "df212.rds", "df219.rds", "df220.rds", "df221.rds",
#   "df237.rds", "df238.rds", "df239.rds", "df246.rds", "df247.rds", "df248.rds",
#   "df255.rds", "df256.rds", "df257.rds", "df309.rds", "df310.rds", "df311.rds",
#   "df318.rds", "df319.rds", "df320.rds", "df327.rds", "df328.rds", "df329.rds",
#   "df345.rds", "df346.rds", "df347.rds", "df354.rds", "df355.rds", "df356.rds",
#   "df363.rds", "df364.rds", "df365.rds"
# )


###############################################################################
# Construct full dfXXX vector (df006.rds → df377.rds)
###############################################################################

#### Plotting the whole set of dfXXX rds files
# # Sequence of integers corresponding to dfXXX names
# df_nums <- 6:377
# 
# # Format with zero-padding and append .rds
# df_list_rds <- paste0("df", sprintf("%03d", df_nums), ".rds")
# 
# # Print in the requested format
# cat("# Vector of dfXXX .rds filenames\n",
#     "df_list_rds <- c(\n  \"",
#     paste(df_list_rds, collapse = "\", \""),
#     "\")\n",
#     sep = "")

# Remove .rds extension safely
df_list <- gsub("\\.rds$", "", df_list_rds)

# Run lineage tracing on all targets
lineage_all <- trace_imputation_lineage(
  df_list,
  "imputation_block_table.tsv"
)

###############################################################################
# Combined lineage summary table for all dfXXX targets
###############################################################################

library(dplyr)
library(purrr)

lineage_summary_all <- purrr::imap_dfr(
  lineage_all,
  function(lineage_tbl, df_name) {
    
    # Ensure lineage_tbl is data frame
    lineage_tbl <- as.data.frame(lineage_tbl)
    
    # Define the REAL expected columns
    required_cols <- c(
      "step",
      "from_id",
      "to_id",
      "type",
      "block",
      "method",
      "module_function"
    )
    
    # Safety check
    missing_cols <- setdiff(required_cols, colnames(lineage_tbl))
    if (length(missing_cols) > 0) {
      stop(paste(
        "Missing expected columns in lineage table for", df_name, ":",
        paste(missing_cols, collapse = ", ")
      ))
    }
    
    # Build the combined summary for this dfXXX
    lineage_tbl %>%
      dplyr::mutate(
        target_df = df_name,
        root_df = dplyr::first(from_id),           # starting lineage df
        final_df = dplyr::last(to_id),             # should match target_df
        total_steps = max(step, na.rm = TRUE)
      ) %>%
      dplyr::select(
        target_df,
        step,
        from_id,
        to_id,
        type,
        block,
        method,
        module_function,
        root_df,
        final_df,
        total_steps
      )
  }
)

###############################################################################
# Display summary
###############################################################################

cat("\n==================== COMBINED LINEAGE SUMMARY ====================\n")
print(lineage_summary_all)

cat("\n==================== DISTINCT TARGETS ====================\n")
print(unique(lineage_summary_all$target_df))

cat("\n==================== TOTAL ROWS ====================\n")
print(nrow(lineage_summary_all))

write.csv(lineage_summary_all, "combined_lineage_summary.csv", row.names = FALSE)


###############################################################################
# Sankey Diagram of all lineage transitions
###############################################################################

library(dplyr)
library(plotly)

# lineage_summary_all must already exist

# ---------------------------------------------------------------------------
# 1. Build node list (unique df IDs)
# ---------------------------------------------------------------------------
nodes <- lineage_summary_all %>%
  select(from_id, to_id) %>%
  tidyr::pivot_longer(cols = everything(), values_to = "df_id") %>%
  distinct(df_id) %>%
  mutate(node_id = row_number() - 1)   # Plotly requires 0-based node indices

# ---------------------------------------------------------------------------
# 2. Build link list (edges)
# ---------------------------------------------------------------------------
links <- lineage_summary_all %>%
  transmute(
    source = nodes$node_id[match(from_id, nodes$df_id)],
    target = nodes$node_id[match(to_id, nodes$df_id)],
    value  = 1,   # equal weighting; can use method quality, delta gain, etc.
    label  = paste(type, method, sep = ": ")
  )

# ---------------------------------------------------------------------------
# 3. Plotly Sankey diagram
# ---------------------------------------------------------------------------
p_sankey <- plot_ly(
  type = "sankey",
  arrangement = "snap",
  node = list(
    label = nodes$df_id,
    color = "#4B79A1",
    pad = 15,
    thickness = 15,
    line = list(color = "black", width = 0.5)
  ),
  link = list(
    source = links$source,
    target = links$target,
    value  = links$value,
    label  = links$label,
    color  = "rgba(200, 30, 30, 0.4)"  # translucent red links
  )
) %>%
  layout(
    title = "Sankey Diagram — Imputation Lineage Flow",
    font = list(size = 12)
  )

p_sankey


htmlwidgets::saveWidget(as_widget(p_sankey),
                        "sankey_lineage.html",
                        selfcontained = TRUE)



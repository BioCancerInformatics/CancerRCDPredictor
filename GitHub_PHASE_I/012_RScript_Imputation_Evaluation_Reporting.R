###############################################################################
# MODULES 12–16 · MULTI-OMIC IMPUTATION EVALUATION & REPORTING
# -----------------------------------------------------------------------------
# Purpose
#   End-to-end, fault-tolerant evaluation of imputed TCGA-scale multi-omic
#   datasets (df006+). Computes Cox C-indices for OS/DSS/DFI/PFI using a
#   harmonized predictor space anchored to df005, records method provenance
#   (survival/CNV/mutation/continuous), resumes safely, and exports results
#   and figures for downstream interpretation.
#
# Inputs (required files in working directory)
#   • df005.rds                    : Baseline schema anchor (predictor set)
#   • df006+.rds                   : Imputed datasets to evaluate (exclude df001–df005)
#   • imputation_block_table.tsv   : Method/interval registry (type/output_start/output_end/method)
#
# Core Outputs
#   • imputation_evaluation_results.rds         : Incremental, resumable results (schema-fixed)
#   • full_imputation_evaluation.tsv            : Flat export of final results
#   • method_performance_summary.tsv            : Grouped performance summary
#   • endpoint_correlation_matrix.tsv           : Correlations among endpoint C-indices (if >1 endpoint)
#   • High-res plots (TIFF, PDF, SVG)           : Method benchmarking & missingness analyses
#
# Design Guarantees
#   • Schema stability: evaluate_imputation_safe() returns fixed columns even on error.
#   • Predictor harmonization: features restricted to df005[63:ncol] and omic tokens .1–.7.
#   • Name safety: make.names() sanitization before formula construction.
#   • Resume & checkpoints: idempotent RDS accumulation with batch processing.
#   • Memory hygiene: batch_size control, GC, and minimal in-memory objects.
#
# Assumptions/Conventions
#   • Survival endpoints named {OS,DSS,DFI,PFI} and times {OS.time,...}.
#   • Predictor columns start at index 63 (metadata occupy 1–62).
#   • Omic columns follow NAME.<omic_token>.* where omic_token ∈ {1..7}.
#
# Reproducibility Notes
#   • All logs are sent via log_msg() if provided; falls back to message().
#   • All exports use explicit devices/resolutions matching journal standards.
#   • No random seeds are required here (modeling uses Cox with linear predictors).
###############################################################################

##### 
##### MUST CORRECT THE VALIDATION MODULES
##### 
##### ============================================================================
##### STEP (2) · Define Evaluation Variables: Responses and Predictors
##### ============================================================================
##### Rationale: enforce identical row/column semantics across imputations.

# ✔️ Structural invariants across imputed datasets:
#   - Rows map 1:1 to the same biological samples across all imputations.
#   - Columns include a fixed set of response variables and a harmonized predictor set.

# ➤ Response variables (binary survival endpoints used in Cox models):
#   • DSS (Disease-Specific Survival)
#   • DFI (Disease-Free Interval)
#   • PFI (Progression-Free Interval)
#   • OS  (Overall Survival)

# ➤ Predictor variables:
#   • Columns 63:ncol(df) of each imputed dataframe.
#   • Multi-omic features only (e.g., Protein.1, Mutation.2, CNV.3, …).

# ⚠️ Modeling interface (keep consistent across all analyses):
#   • Response ~ Predictors[63:ncol(df)]
#   • Apply any feature filtering/selection *before* fitting for comparability.

### MODULE 12 ### (transposed earlier to govern global logic)

### MODULE 13 ###
# =============================================================================
# MEMGUARD IMPUTATION EVALUATOR — “Forgetting-Prevented Feature Factory”
# 
# Efficiently evaluates ~15k-feature datasets without OOM, tracks method
# provenance per omic layer, batches with atomic checkpoints, and produces
# publication-grade metric tables/plots.
# =============================================================================

# =============================================================================
# Key attributes (for quick orientation):
# -----------------------------------------------------------------------------
# • Memory safety — batch gating and GC minimize RAM pressure.
# • Scale handling — robust with tens of thousands of features.
# • Survival focus — standardized Cox C-index across endpoints.
# • Workflow integrity — fixed schema, resumable execution, checkpoints.
# • Automated reporting — TSV exports and high-res figures.
# =============================================================================


######
######
######
suppressPackageStartupMessages({
  library(survival)
  library(pROC)
  library(rio)
})

# --- Safe logger (no-op fallback if absent) ----------------------------------
if (!exists("log_msg")) log_msg <- function(...) message(paste0(...))

# --- Configuration -----------------------------------------------------------
batch_size   <- 5
results_file <- "imputation_evaluation_results.rds"

# Validate batch_size to avoid invalid seq() step
batch_size <- suppressWarnings(as.integer(batch_size))
if (is.na(batch_size) || batch_size < 1L) {
  warning("Invalid batch_size; forcing batch_size <- 1L")
  batch_size <- 1L
}

# Load block table once; coerce method to character to avoid logical NA leakage
block_table <- rio::import("imputation_block_table.tsv")
block_table$method <- as.character(block_table$method)

# --- Safe RDS reader (idempotent; schema tolerant) --------------------------
if (!exists("safe_readRDS", mode = "function")) {
  safe_readRDS <- function(path, expected = c("data.frame", "data.table", "tbl_df")) {
    if (!file.exists(path)) stop("File not found: ", path, call. = FALSE)
    obj <- readRDS(path)
    if (!any(expected %in% class(obj))) obj <- as.data.frame(obj)
    if (is.null(names(obj))) names(obj) <- character(ncol(obj))
    if (anyDuplicated(names(obj))) {
      warning("Duplicated column names detected; enforcing uniqueness via make.unique().")
      names(obj) <- make.unique(names(obj), sep = "_")
    }
    obj
  }
}

# ---------- Baseline schema anchor (df005) -----------------------------------
# Use df005 to define the canonical predictor namespace (excludes audit/meta).
df005 <- safe_readRDS("df005.rds")
BASE_PRED_START <- 63L
BASE_PRED_NAMES <- if (ncol(df005) >= BASE_PRED_START) {
  names(df005)[BASE_PRED_START:ncol(df005)]
} else character(0)

# Helper: retain only features with valid omic token (.1–.7 as second token)
.is_omic_set   <- c("1","2","3","4","5","6","7")
.is_omic_token <- function(nm) {
  toks <- strsplit(nm, "\\.", fixed = FALSE)
  vapply(toks, function(x) length(x) >= 2 && x[2] %in% .is_omic_set, logical(1))
}

# ---------- Method lookup (scalar; robust to intervals) ----------------------
lookup_method_scalar <- function(df_num, layer) {
  block_table$method <- as.character(block_table$method)
  out <- NA_character_
  if (layer == "Survival"  && df_num >=  6 && df_num <=   8) {
    out <- block_table$method[block_table$output_start == df_num]
  } else if (layer == "CNV" && df_num >=  9 && df_num <=  17) {
    out <- block_table$method[
      block_table$type == "CNV" &
        block_table$output_start <= df_num &
        block_table$output_end   >= df_num
    ]
  } else if (layer == "Mutation" && df_num >= 18 && df_num <= 53) {
    out <- block_table$method[
      block_table$type == "Mutation" &
        block_table$output_start <= df_num &
        block_table$output_end   >= df_num
    ]
  } else if (layer == "Continuous" && df_num >= 54 && df_num <= 341) {
    out <- block_table$method[
      block_table$type == "Continuous" &
        block_table$output_start <= df_num &
        block_table$output_end   >= df_num
    ]
  }
  if (length(out) == 0) return(NA_character_)
  if (length(out) > 1)  return(paste(unique(out), collapse = "/"))
  as.character(out)
}

# ---------- Predictor selection anchored to df005 ----------------------------
# Steps: (1) intersect with baseline, (2) keep valid omic tokens,
# (3) numeric-coerce and drop all-NA, (4) drop constants, (5) rank by
# observed rate and cap by max_feat.
select_predictors <- function(df, max_feat = 100L) {
  cand <- intersect(names(df), BASE_PRED_NAMES)
  if (!length(cand)) return(character(0))
  X <- df[, cand, drop = FALSE]
  
  keep_token <- .is_omic_token(colnames(X))
  X <- X[, keep_token, drop = FALSE]
  if (!ncol(X)) return(character(0))
  
  Xnum <- as.data.frame(lapply(X, function(col) {
    if (is.numeric(col)) return(col)
    suppressWarnings(as.numeric(col))
  }), check.names = FALSE)
  
  non_all_na <- vapply(Xnum, function(col) !all(is.na(col)), logical(1))
  Xnum <- Xnum[, non_all_na, drop = FALSE]
  if (!ncol(Xnum)) return(character(0))
  
  has_var <- vapply(Xnum, function(col) {
    z <- col[!is.na(col)]
    if (length(z) <= 1) return(FALSE)
    length(unique(z)) > 1
  }, logical(1))
  Xnum <- Xnum[, has_var, drop = FALSE]
  if (!ncol(Xnum)) return(character(0))
  
  obs_rate <- colMeans(!is.na(Xnum))
  keep <- names(sort(obs_rate, decreasing = TRUE))
  head(keep, max_feat)
}

# ---------- Cox C-index with name sanitization -------------------------------
# • Coerces predictors to numeric
# • Complete-case row filter
# • Drops constant predictors post-filter
# • Caps predictors at min(100, floor(n/10))
# • Sanitizes names for safe reformulate() usage
.safe_numeric <- function(v) {
  if (is.numeric(v)) return(v)
  suppressWarnings(as.numeric(v))
}

safe_cox_cindex <- function(df, time_col, status_col, preds) {
  if (!all(c(time_col, status_col) %in% names(df))) return(NA_real_)
  preds <- preds[preds %in% names(df)]
  if (!length(preds)) return(NA_real_)
  
  keep_cols <- c(time_col, status_col, preds)
  d <- df[, keep_cols, drop = FALSE]
  for (nm in preds) d[[nm]] <- .safe_numeric(d[[nm]])
  
  d <- d[stats::complete.cases(d), , drop = FALSE]
  if (nrow(d) < 20) return(NA_real_)
  
  nonconst <- vapply(d[, preds, drop = FALSE],
                     function(col) length(unique(col)) > 1, logical(1))
  preds <- preds[nonconst]
  if (!length(preds)) return(NA_real_)
  
  cap <- min(100L, max(1L, floor(nrow(d) / 10)))
  if (length(preds) > cap) preds <- preds[seq_len(cap)]
  
  # Sanitize predictor names used in the formula
  syn <- make.names(preds, unique = TRUE)
  names(syn) <- preds
  
  d_local <- d
  for (old in names(syn)) {
    new <- syn[[old]]
    if (!identical(old, new)) {
      d_local[[new]] <- d_local[[old]]
      d_local[[old]] <- NULL
    }
  }
  
  rhs_terms <- unname(syn)
  fml <- reformulate(termlabels = rhs_terms,
                     response   = as.formula(paste0("Surv(", time_col, ",", status_col, ")")))
  
  out <- tryCatch({
    fit <- survival::coxph(fml, data = d_local, ties = "efron")
    as.numeric(summary(fit)$concordance[1])
  }, warning = function(w) {
    tryCatch({
      fit <- survival::coxph(fml, data = d_local, ties = "efron")
      as.numeric(summary(fit)$concordance[1])
    }, error = function(e2) NA_real_)
  }, error = function(e) {
    # Fallback: risk-score surrogate with survConcordance
    rs <- tryCatch({
      Z <- scale(as.matrix(d_local[, rhs_terms, drop = FALSE]))
      drop(Z %*% rep(1, ncol(Z)))
    }, error = function(e2) NA_real_)
    if (all(is.na(rs))) return(NA_real_)
    tryCatch({
      cc <- survival::survConcordance(Surv(d[[time_col]], d[[status_col]]) ~ rs)
      cc$concordance
    }, error = function(e3) NA_real_)
  })
  
  out
}

# ---------- Single-dataset evaluator (1-row, fixed schema) -------------------
evaluate_imputation <- function(df_path) {
  df     <- safe_readRDS(df_path)
  df_num <- as.integer(gsub("\\D", "", basename(df_path)))
  
  surv_method <- lookup_method_scalar(df_num, "Survival")
  cnv_method  <- lookup_method_scalar(df_num, "CNV")
  mut_method  <- lookup_method_scalar(df_num, "Mutation")
  cont_method <- lookup_method_scalar(df_num, "Continuous")
  
  preds <- select_predictors(df, max_feat = 100L)
  
  os_c  <- safe_cox_cindex(df, "OS.time",  "OS",  preds)
  dss_c <- safe_cox_cindex(df, "DSS.time", "DSS", preds)
  dfi_c <- safe_cox_cindex(df, "DFI.time", "DFI", preds)
  pfi_c <- safe_cox_cindex(df, "PFI.time", "PFI", preds)
  
  # % missing computed over the exact predictor set used in the model
  pct_miss <- if (length(preds)) mean(is.na(df[, preds, drop = FALSE])) * 100 else NA_real_
  
  out <- data.frame(
    df_name           = basename(df_path),
    n_samples         = nrow(df),
    n_features        = length(preds),
    pct_missing       = pct_miss,
    os_cindex         = os_c,
    dss_cindex        = dss_c,
    dfi_cindex        = dfi_c,
    pfi_cindex        = pfi_c,
    survival_method   = surv_method,
    cnv_method        = cnv_method,
    mutation_method   = mut_method,
    continuous_method = cont_method,
    eval_time         = Sys.time(),
    error             = NA_character_,
    stringsAsFactors  = FALSE
  )
  
  rm(df); gc()
  out
}

# ---------- Error wrapper (schema never drifts) ------------------------------
evaluate_imputation_safe <- function(df_path) {
  tryCatch(
    evaluate_imputation(df_path),
    error = function(e) {
      data.frame(
        df_name           = basename(df_path),
        n_samples         = NA_real_,
        n_features        = NA_real_,
        pct_missing       = NA_real_,
        os_cindex         = NA_real_,
        dss_cindex        = NA_real_,
        dfi_cindex        = NA_real_,
        pfi_cindex        = NA_real_,
        survival_method   = NA_character_,
        cnv_method        = NA_character_,
        mutation_method   = NA_character_,
        continuous_method = NA_character_,
        eval_time         = Sys.time(),
        error             = conditionMessage(e),
        stringsAsFactors  = FALSE
      )
    }
  )
}

# ---------- Row type normalizer (prevents bind_rows clashes) -----------------
normalize_row_types <- function(dd) {
  chr_cols <- c("df_name","survival_method","cnv_method",
                "mutation_method","continuous_method","error")
  for (cc in intersect(chr_cols, names(dd))) dd[[cc]] <- as.character(dd[[cc]])
  
  num_cols <- c("n_samples","n_features","pct_missing",
                "os_cindex","dss_cindex","dfi_cindex","pfi_cindex")
  for (cc in intersect(num_cols, names(dd))) dd[[cc]] <- as.numeric(dd[[cc]])
  
  if ("eval_time" %in% names(dd) && !inherits(dd$eval_time, "POSIXct")) {
    dd$eval_time <- as.POSIXct(dd$eval_time, tz = "")
  }
  dd
}

# ---------- Expected column order (contract) ---------------------------------
expected_cols <- c(
  "df_name","n_samples","n_features","pct_missing",
  "os_cindex","dss_cindex","dfi_cindex","pfi_cindex",
  "survival_method","cnv_method","mutation_method","continuous_method",
  "eval_time","error"
)

# ---------- File discovery (skip df001–df005) --------------------------------
all_rds <- list.files(pattern = "df\\d{3}\\.rds")
all_rds <- all_rds[!grepl("^df00[1-5]\\.rds$", all_rds)]
all_rds <- all_rds[order(as.numeric(gsub("\\D", "", all_rds)))]

# ---------- Resume logic -----------------------------------------------------
if (file.exists(results_file)) {
  completed  <- readRDS(results_file)
  if (nrow(completed)) {
    completed <- normalize_row_types(completed)
    miss <- setdiff(expected_cols, names(completed))
    for (cc in miss) completed[[cc]] <- NA
    extra <- setdiff(names(completed), expected_cols)
    if (length(extra)) completed <- completed[, setdiff(names(completed), extra), drop = FALSE]
    completed <- completed[, expected_cols, drop = FALSE]
  }
  done_files <- completed$df_name
} else {
  completed  <- data.frame()
  done_files <- character()
}

remaining_files <- setdiff(all_rds, done_files)

# ---------- Optional smoke test (first pending file) -------------------------
if (length(remaining_files)) {
  cat("Smoke test on:", remaining_files[1], "\n")
  print(normalize_row_types(evaluate_imputation_safe(remaining_files[1]))[, expected_cols, drop = FALSE])
}

# ---------- Batch loop with checkpoints --------------------------------------
if (length(remaining_files) > 0L) {
  idx_seq <- seq.int(from = 1L, to = length(remaining_files), by = batch_size)
  for (i in idx_seq) {
    batch <- remaining_files[i:min(i + batch_size - 1L, length(remaining_files))]
    log_msg("\nProcessing batch: ", paste(batch, collapse = ", "), "\n")
    
    batch_results <- lapply(batch, function(file) {
      log_msg("Evaluating ", file, "... ")
      t0  <- Sys.time()
      res <- evaluate_imputation_safe(file)
      res <- normalize_row_types(res)
      res <- res[, expected_cols, drop = FALSE]
      dt  <- round(difftime(Sys.time(), t0, units = "secs"), 1)
      log_msg("done (", dt, "s)\n")
      res
    })
    
    batch_df  <- do.call(rbind, batch_results)
    completed <- if (nrow(completed)) rbind(completed, batch_df) else batch_df
    
    saveRDS(completed, results_file)
    log_msg("Checkpoint saved. Completed ", nrow(completed), " of ", length(all_rds), "\n")
  }
} else {
  log_msg("No pending .rds files to evaluate (remaining_files is empty).")
}

# ---------- Quick post-run sanity checks -------------------------------------
res <- readRDS("imputation_evaluation_results.rds")
stopifnot(is.data.frame(res))
stopifnot(identical(colnames(res), expected_cols))

table(is.na(res$error))  # Expect majority TRUE (no evaluation errors)
head(res[, c("df_name","os_cindex","dss_cindex","dfi_cindex","pfi_cindex")], 10)
######
######
######
######


# 4. Results Analysis ---------------------------------------------------------
if (file.exists(results_file)) {
  final_results <- readRDS(results_file)
  
  log_msg("\nEvaluation completed for", nrow(final_results), "datasets\n")
  print(head(final_results))
  
  if (!"error" %in% colnames(final_results)) {
    library(dplyr)
    
    performance_summary <- final_results %>%
      group_by(survival_method, cnv_method, mutation_method, continuous_method) %>%
      summarise(
        mean_os_cindex = mean(os_cindex, na.rm = TRUE),
        mean_dss_cindex = mean(dss_cindex, na.rm = TRUE),
        mean_missing = mean(pct_missing, na.rm = TRUE),
        n = n(),
        .groups = "drop"
      )
    
    print(performance_summary)
    
    safe_export_tsv(as.data.frame(final_results), "full_imputation_evaluation.tsv")
    safe_export_tsv(as.data.frame(performance_summary), "method_performance_summary.tsv")
  }
}

### MODULE 14 ###
# =============================================================================
# INTERPRETATION (Non-executing notes; preserve as narrative commentary)
# =============================================================================
# Summarizes empirical findings (e.g., continuous-layer XGBoost stability under
# extreme missingness; categorical mode/random adequacy; missForest caveats).
# Provides actionable recommendations and dataset descriptors.
# =============================================================================

# =============================================================================
# Rationale for prioritizing OS/DSS endpoints:
# -----------------------------------------------------------------------------
# • Clinical salience: OS (gold standard), DSS (disease attribution clarity)
# • Data completeness: higher coverage vs. DFI/PFI
# • Compute trade-offs: DFI/PFI inflate model count with limited gain
# • Empirical coverage: OS/DSS explain bulk of endpoint variance
# • Benchmarking continuity: aligns with TCGA/ICGC practices
# =============================================================================

#####
#####
#####
#####
#####
# =============================================================================
# MULTI-OMIC SURVIVAL IMPUTATION EVALUATOR — Comprehensive Feature Factory
# =============================================================================

library(survival)
library(pROC)
library(dplyr)

# Setup -----------------------------------------------------------------------
batch_size <- 3  # Conservative due to model overhead
results_file <- "full_imputation_evaluation.rds"
block_table <- rio::import("imputation_block_table.tsv")

# Discover inputs (skip df001–df005) -----------------------------------------
all_rds <- list.files(pattern = "^df\\d{3}\\.rds$") %>%
  .[!grepl("^df00[1-5]\\.rds$", .)] %>%
  .[order(as.numeric(gsub("\\D", "", .)))]

# Evaluation function (compact variant) ---------------------------------------
evaluate_imputation <- function(df_path) {
  df <- readRDS(df_path)
  df_num <- as.numeric(gsub("\\D", "", basename(df_path)))
  
  endpoint_check <- sapply(c("OS", "DSS", "DFI", "PFI"), 
                           function(x) all(paste0(x, c("", ".time")) %in% colnames(df)))
  
  if (sum(endpoint_check) < 2) {
    return(list(
      df_name = basename(df_path),
      error = paste("Insufficient endpoints. Available:", 
                    paste(names(endpoint_check)[endpoint_check], collapse = ", "))
    ))
  }
  
  methods <- list(
    survival = if (df_num >= 6 && df_num <= 8) block_table$method[block_table$output_start == df_num] else NA,
    cnv = if (df_num >= 9 && df_num <= 17) block_table$method[block_table$type == "CNV" & block_table$output_start <= df_num & block_table$output_end >= df_num] else NA,
    mutation = if (df_num >= 18 && df_num <= 53) block_table$method[block_table$type == "Mutation" & block_table$output_start <= df_num & block_table$output_end >= df_num] else NA,
    continuous = if (df_num >= 54 && df_num <= 341) block_table$method[block_table$type == "Continuous" & block_table$output_start <= df_num & block_table$output_end >= df_num] else NA
  )
  
  metrics <- tryCatch({
    predictors <- names(sort(colMeans(!is.na(df[, 63:ncol(df)])), decreasing = TRUE))[1:100]
    
    results <- list(
      df_name = basename(df_path),
      n_samples = nrow(df),
      n_features = ncol(df) - 62,
      pct_missing = mean(is.na(df[, 63:ncol(df)])) * 100,
      endpoints_available = paste(names(endpoint_check)[endpoint_check], collapse = "|")
    )
    
    if (endpoint_check["OS"]) {
      results$os_cindex <- summary(coxph(Surv(OS.time, OS) ~ ., data = df[, c("OS", "OS.time", predictors)]))$concordance[1]
    }
    if (endpoint_check["DSS"]) {
      results$dss_cindex <- summary(coxph(Surv(DSS.time, DSS) ~ ., data = df[, c("DSS", "DSS.time", predictors)]))$concordance[1]
    }
    if (endpoint_check["DFI"]) {
      results$dfi_cindex <- summary(coxph(Surv(DFI.time, DFI) ~ ., data = df[, c("DFI", "DFI.time", predictors)]))$concordance[1]
    }
    if (endpoint_check["PFI"]) {
      results$pfi_cindex <- summary(coxph(Surv(PFI.time, PFI) ~ ., data = df[, c("PFI", "PFI.time", predictors)]))$concordance[1]
    }
    
    c(results, list(
      survival_method = methods$survival,
      cnv_method = methods$cnv,
      mutation_method = methods$mutation,
      continuous_method = methods$continuous,
      eval_time = Sys.time()
    ))
    
  }, error = function(e) {
    list(df_name = basename(df_path), error = conditionMessage(e))
  })
  
  rm(df); gc()
  return(metrics)
}

# Robust batch processing & resume -------------------------------------------
if (file.exists(results_file)) {
  completed <- tryCatch(
    readRDS(results_file),
    error = function(e) {
      warning("Corrupt results file - starting fresh")
      data.frame()
    }
  )
  done_files <- unique(completed$df_name)
} else {
  completed <- data.frame()
  done_files <- character()
}

remaining_files <- setdiff(all_rds, done_files)

if (length(remaining_files) == 0) {
  log_msg("\n✓ All", length(all_rds), "files already processed\n")
  log_msg("Output file:", normalizePath(results_file), "\n")
} else {
  log_msg(sprintf(
    "\nProcessing %d/%d files (batch size = %d)...\n",
    length(remaining_files), length(all_rds), batch_size
  ))
  
  pb <- txtProgressBar(min = 0, max = length(remaining_files), style = 3)
  
  for (i in seq(from = 1, to = length(remaining_files), by = batch_size)) {
    batch <- remaining_files[i:min(i + batch_size - 1, length(remaining_files))]
    
    log_msg(sprintf("\nBatch %d/%d: %s\n",
                    ceiling(i / batch_size),
                    ceiling(length(remaining_files) / batch_size),
                    paste(basename(batch), collapse = ", ")))
    
    batch_results <- lapply(batch, function(file) {
      setTxtProgressBar(pb, match(file, remaining_files))
      log_msg("  ", basename(file), "... ", sep = "")
      start <- Sys.time()
      
      result <- tryCatch({
        res <- evaluate_imputation(file)
        log_msg(round(difftime(Sys.time(), start, units = "secs"), 1), "s\n")
        res
      }, error = function(e) {
        log_msg("FAILED:", conditionMessage(e), "\n")
        list(df_name = file, error = conditionMessage(e))
      })
      
      return(result)
    })
    
    completed <- bind_rows(completed, bind_rows(batch_results))
    saveRDS(completed, results_file)
    log_msg("  ↳ Checkpoint saved. Completed", nrow(completed), "/", length(all_rds), 
            sprintf("(%.1f%%)", 100 * nrow(completed) / length(all_rds)), "\n")
  }
  
  close(pb)
  log_msg("\n✅ Processing complete. Final results saved to:\n", normalizePath(results_file), "\n")
}

# Final validation summary ----------------------------------------------------
if (file.exists(results_file)) {
  final_data <- readRDS(results_file)
  log_msg("\nFinal validation:\n")
  log_msg("- Unique files processed:", length(unique(final_data$df_name)), "\n")
  log_msg("- Last file processed:", tail(final_data$df_name, 1), "\n")
  log_msg("- Errors encountered:", sum(!is.na(final_data$error)), "\n")
}

# Enhanced results analysis/export -------------------------------------------
if (file.exists(results_file)) {
  final_results <- readRDS(results_file)
  
  log_msg("\nEvaluation completed for", nrow(final_results), "datasets\n")
  log_msg("Endpoint coverage:\n")
  print(table(final_results$endpoints_available))
  
  if (!"error" %in% colnames(final_results)) {
    performance_summary <- final_results %>%
      group_by(survival_method, cnv_method, mutation_method, continuous_method) %>%
      summarise(
        across(
          ends_with("_cindex"),
          list(
            mean = ~mean(.x, na.rm = TRUE),
            sd = ~sd(.x, na.rm = TRUE),
            n = ~sum(!is.na(.x))
          ),
          .names = "{.col}_{.fn}"
        ),
        mean_missing = mean(pct_missing, na.rm = TRUE),
        endpoint_coverage = paste(unique(endpoints_available), collapse = "; "),
        .groups = "drop"
      )
    
    print(performance_summary)
    
    safe_export_tsv(as.data.frame(final_results), "full_imputation_evaluation.tsv")
    safe_export_tsv(as.data.frame(performance_summary), "method_performance_summary.tsv")
    
    cindex_cols <- grep("_cindex$", names(final_results), value = TRUE)
    if (length(cindex_cols) > 1) {
      cor_matrix <- cor(final_results[, cindex_cols], use = "pairwise.complete.obs")
      safe_export_tsv(as.data.frame(cor_matrix), "endpoint_correlation_matrix.tsv")
      log_msg("🧠 Correlation matrix saved to 'endpoint_correlation_matrix.tsv'\n")
    }
  } else {
    log_msg("\n⚠️ Some datasets contain evaluation errors. Check 'final_results$error'.\n")
  }
}

### MODULE 15 ###
# =============================================================================
# MULTI-OMIC IMPUTATION METHOD EVALUATION DASHBOARD
# =============================================================================
# Visual benchmarking across OS/DSS/DFI/PFI with QC plots and summaries.
# =============================================================================

library(tidyverse)
library(ggpubr)
library(rio)

# Load data -------------------------------------------------------------------
df <- read_tsv("full_imputation_evaluation.tsv")

# 1) Overall performance by continuous-layer method ---------------------------
method_perf <- df %>%
  group_by(continuous_method) %>%
  summarise(across(
    ends_with("_cindex"),
    list(mean = ~mean(.x, na.rm = TRUE), sd = ~sd(.x, na.rm = TRUE)),
    .names = "{.col}_{.fn}"
  ))

print(method_perf)
safe_export_tsv(as.data.frame(method_perf), "Performance_evaluation_summary.tsv")

### INTERPRETATION BLOCK — preserved verbatim (commentary only)

# 2) Boxplots across endpoints ------------------------------------------------
plot_data <- df %>%
  select(continuous_method, ends_with("_cindex")) %>%
  pivot_longer(cols = -continuous_method,
               names_to = "endpoint",
               values_to = "cindex") %>%
  mutate(endpoint = str_remove(endpoint, "_cindex") %>% toupper())

gg_plot <- ggplot(plot_data, aes(x = continuous_method, y = cindex, fill = endpoint)) +
  geom_boxplot() +
  labs(title = "Imputation Method Performance Across Survival Endpoints",
       x = "Imputation Method",
       y = "C-index") +
  theme_minimal(base_size = 14) +
  theme(legend.position = "top")

# Timestamped multi-format export
plot_name <- paste0("imputation_performance_BARPLOT_", format(Sys.time(), "%Y%m%d_%H%M%S"))

ggsave(paste0(plot_name, ".tiff"), gg_plot, device = "tiff", dpi = 600, width = 12, height = 10, compression = "lzw")
ggsave(paste0(plot_name, ".svg"),  gg_plot, device = "svg",  width = 12, height = 10)
ggsave(paste0(plot_name, ".pdf"),  gg_plot, device = "pdf",  dpi = 600, width = 12, height = 10)

log_msg("\n✅ Saved plot files:\n")
print(list.files(pattern = paste0(plot_name, "\\..*")))

# 3) OS performance vs missingness -------------------------------------------
OS_performance_plot <- ggplot(df, aes(x = pct_missing, y = os_cindex, color = continuous_method)) +
  geom_point(alpha = 0.6, size = 2) +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(~continuous_method) +
  labs(
    title = "OS Performance vs. Missingness Percentage",
    x = "Missingness (%) in Predictors",
    y = "OS C-index"
  ) +
  theme_pubr(base_size = 13)

ggsave("OS_performance_vs_missingness.tiff", plot = OS_performance_plot, device = "tiff",
       dpi = 600, width = 12, height = 10, compression = "lzw")

ggsave("OS_performance_vs_missingness.pdf", plot = OS_performance_plot, device = cairo_pdf,
       dpi = 600, width = 12, height = 6)

# Observation (commentary): XGBoost stability at very high missingness; mean/median degrade.

### MODULE 16 ###
# =============================================================================
# PERFORMANCE RADAR PLOT (Mean C-index across endpoints)
# =============================================================================

library(tidyverse)
library(ggpubr)
library(rio)

# Prepare data ----------------------------------------------------------------
imputation_results <- read_tsv("full_imputation_evaluation.tsv") %>%
  mutate(method_combo = case_when(
    !is.na(continuous_method) ~ continuous_method,
    !is.na(mutation_method) ~ mutation_method,
    !is.na(cnv_method) ~ cnv_method,
    TRUE ~ survival_method
  ))

method_performance <- imputation_results %>%
  group_by(method_combo) %>%
  summarise(across(
    ends_with("_cindex"),
    list(mean = ~mean(.x, na.rm = TRUE), sd = ~sd(.x, na.rm = TRUE)),
    .names = "{.col}_{.fn}"
  )) %>%
  arrange(desc(os_cindex_mean))

plot_data <- method_performance %>%
  select(method_combo, ends_with("_mean")) %>%
  pivot_longer(cols = -method_combo,
               names_to = "endpoint",
               values_to = "cindex") %>%
  mutate(endpoint = str_remove(endpoint, "_cindex_mean") %>%
           toupper())

# Radar-style profile ---------------------------------------------------------
radar_plot <- ggplot(plot_data, aes(x = endpoint, y = cindex, group = method_combo)) +
  geom_line(aes(color = method_combo), linewidth = 1.1, alpha = 0.85) +
  geom_point(aes(color = method_combo), size = 2.8) +
  coord_polar() +
  labs(
    title = "Radar Chart of Imputation Method Performance",
    subtitle = "Mean C-index across OS, DSS, DFI, PFI",
    x = NULL,
    y = "C-index",
    color = "Imputation Method"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "right",
    axis.text.y = element_blank(),
    panel.grid.major.y = element_blank(),
    plot.title = element_text(face = "bold")
  )

# High-res exports ------------------------------------------------------------
plot_name <- paste0("imputation_performance_RADAR_", format(Sys.time(), "%Y%m%d_%H%M%S"))

tiff_file <- paste0(plot_name, ".tiff")
log_msg("📌 Saving high-resolution TIFF as:", tiff_file, "\n")
ggsave(tiff_file, radar_plot,
       device = "tiff", width = 10, height = 8, dpi = 600, compression = "lzw")

svg_file <- paste0(plot_name, ".svg")
log_msg("📌 Saving editable SVG as:", svg_file, "\n")
ggsave(svg_file, radar_plot,
       device = "svg", width = 10, height = 8)

pdf_file <- paste0(plot_name, ".pdf")
log_msg("📌 Saving vector PDF as:", pdf_file, "\n")
ggsave(pdf_file, radar_plot,
       device = "pdf", width = 10, height = 8, dpi = 600)

log_msg("\n✅ Saved radar plot files:\n")
print(list.files(pattern = paste0(plot_name, "\\..*")))

### MODULE 16 ###
####
#### Multi-Layer Evaluation of Imputation Methods Across Survival Endpoints
#### Panels:
#   A. C-index vs. missingness by endpoint
#   B. Method comparison across endpoints
#   C. Full pipeline performance by method combination
#   D. Missingness–performance correlation matrix
####

library(tidyverse)
library(ggpubr)
library(rio)

# Load data -------------------------------------------------------------------
df <- read_tsv("full_imputation_evaluation.tsv")

# 1) Long format for plotting -------------------------------------------------
plot_data <- df %>%
  select(pct_missing, survival_method, cnv_method, mutation_method, continuous_method,
         os_cindex, dss_cindex, pfi_cindex, dfi_cindex) %>%
  pivot_longer(cols = ends_with("cindex"), names_to = "endpoint", values_to = "c_index") %>%
  mutate(
    endpoint = factor(
      case_when(
        endpoint == "os_cindex"  ~ "Overall Survival",
        endpoint == "dss_cindex" ~ "Disease-Specific",
        endpoint == "pfi_cindex" ~ "Progression-Free",
        endpoint == "dfi_cindex" ~ "Disease-Free"
      ),
      levels = c("Overall Survival", "Disease-Specific", "Progression-Free", "Disease-Free")
    ),
    method_combo = paste(survival_method, cnv_method, mutation_method, continuous_method, sep = " + ")
  )

# 2) Visual panels ------------------------------------------------------------

# Panel A
p1 <- ggplot(plot_data, aes(x = pct_missing, y = c_index, color = continuous_method)) +
  geom_point(alpha = 0.6, size = 2) +
  geom_smooth(method = "loess", se = FALSE, linewidth = 1) +
  facet_wrap(~endpoint, ncol = 2) +
  scale_color_viridis_d(option = "plasma") +
  labs(title = "A. Performance vs. Missingness by Endpoint",
       x = "Percentage Missing Values",
       y = "Concordance Index (C-index)",
       color = "Imputation Method") +
  theme_pubr(base_size = 13) +
  theme(legend.position = "bottom")

# Panel B
p2 <- ggplot(plot_data, aes(x = continuous_method, y = c_index, fill = endpoint)) +
  geom_boxplot(alpha = 0.8) +
  stat_compare_means(aes(group = endpoint), label = "p.signif", method = "t.test") +
  scale_fill_viridis_d(option = "viridis") +
  labs(title = "B. Method Performance Comparison",
       x = "Continuous Imputation Method",
       y = "C-index",
       fill = "Survival Endpoint") +
  theme_pubr(base_size = 13) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Panel C
p3 <- ggplot(plot_data, aes(x = method_combo, y = c_index, color = endpoint)) +
  geom_jitter(width = 0.2, alpha = 0.7, size = 2) +
  geom_boxplot(aes(fill = endpoint), alpha = 0.2, outlier.shape = NA) +
  scale_color_viridis_d(option = "rocket") +
  scale_fill_viridis_d(option = "rocket") +
  labs(title = "C. Full Imputation Pipeline Performance",
       subtitle = "Grouped by method combination (Survival → CNV → Mutation → Continuous)",
       x = NULL,
       y = "C-index") +
  theme_pubr(base_size = 11) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
        legend.position = "bottom")

# Panel D
cor_data <- plot_data %>%
  group_by(continuous_method, endpoint) %>%
  summarise(correlation = cor(pct_missing, c_index, use = "complete.obs"), .groups = "drop")

p4 <- ggplot(cor_data, aes(x = continuous_method, y = endpoint, fill = correlation)) +
  geom_tile(color = "white", linewidth = 0.5) +
  geom_text(aes(label = sprintf("%.2f", correlation)), color = "black", size = 4) +
  scale_fill_gradient2(low = "#2c7bb6", mid = "white", high = "#d7191c", midpoint = 0) +
  labs(title = "D. Missingness–Performance Correlation",
       x = "Continuous Imputation Method",
       y = "Survival Endpoint",
       fill = "Pearson's r") +
  theme_pubr(base_size = 13) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 3) Assemble figure and export ----------------------------------------------
final_plot <- ggarrange(
  ggarrange(p1, p2, ncol = 1, labels = c("A", "B"), heights = c(1.2, 1)),
  ggarrange(p3, p4, ncol = 1, labels = c("C", "D"), heights = c(1.3, 1)),
  nrow = 1,
  widths = c(1, 1.3)
) + theme(plot.margin = margin(1, 1, 1, 1, "cm"))

ggsave("imputation_analysis.tiff", final_plot,
       device = "tiff", dpi = 600, width = 16, height = 12, units = "in", compression = "lzw")

ggsave("imputation_analysis.pdf", final_plot,
       device = cairo_pdf, dpi = 600, width = 16, height = 12, units = "in")

# 4) Display in session -------------------------------------------------------
print(final_plot)
print(p1); print(p2); print(p3); print(p4)

# ------------------------------------------------------------------------------
# Commentary on Panel D (interpretation hints for readers)
# ------------------------------------------------------------------------------
# • Gray tiles (NA) denote no linear relationship: desirable robustness.
# • Strong positive r for “NA” (no imputation) suggests complete-case bias.

# ------------------------------------------------------------------------------
# Recommended practices (narrative only)
# ------------------------------------------------------------------------------
# • Prefer robust imputers (e.g., missForest/XGBoost) with neutral correlations.
# • Avoid complete-case analysis due to selection artifacts.

# Render correlation heatmap with highlights ----------------------------------
print(p4)


# ---- MISSINGNESS–PERFORMANCE CORRELATION HEATMAP (enhanced labeling) -------
# Highlights potential artifacts in the “NA” (no imputation) condition.

p4 <- ggplot(cor_data, 
             aes(x = fct_relevel(continuous_method, "NA", after = Inf),
                 y = endpoint, 
                 fill = correlation)) +
  
  geom_tile(color = "white", linewidth = 0.5, na.rm = TRUE) +
  geom_text(aes(label = case_when(
    is.na(correlation) ~ "",
    abs(correlation) < 0.3 ~ "",
    TRUE ~ sprintf("%.2f", correlation)
  ), color = ifelse(abs(correlation) > 0.7, "white", "black")),
  size = 3.5) +
  
  scale_fill_gradient2(
    low = "#2166ac", mid = "#f7f7f7", high = "#b2182b", midpoint = 0,
    na.value = "#f0f0f0", limits = c(-1, 1),
    breaks = c(-1, -0.5, 0, 0.5, 1),
    labels = c("-1.0 (Worse)", "-0.5", "0 (Stable)", "0.5", "1.0 (Artifact)")
  ) +
  
  labs(
    title = "Performance Stability Under Missing Data",
    subtitle = "Pearson correlation between % missing values and C-index\nGray = No relationship (ideal imputation)",
    x = NULL, y = "Clinical Endpoint", fill = "Correlation\n(Missingness ~ C-index)"
  ) +
  
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, face = c(rep("plain",6),"bold")),
    panel.grid = element_blank(),
    legend.position = "bottom",
    plot.title.position = "plot"
  ) +
  
  geom_rect(
    data = ~filter(.x, continuous_method == "NA"),
    aes(xmin = as.numeric(continuous_method)-0.5, 
        xmax = as.numeric(continuous_method)+0.5,
        ymin = as.numeric(endpoint)-0.5, 
        ymax = as.numeric(endpoint)+0.5),
    fill = NA, color = "#ff0000", linewidth = 1.5
  )

# Guidance (logs only) --------------------------------------------------------
log_msg(
  "\nInsights:\n",
  "• Gray tiles = robustness to missingness\n",
  "• Red-boxed 'NA' = complete-case artifact risk\n",
  "• Blank labels = |r| < 0.3 (weak association)\n"
)

print(p4)

# ---- ENHANCED PERFORMANCE vs. MISSINGNESS (GAM) -----------------------------
p1_enhanced <- ggplot(
  data = plot_data, 
  mapping = aes(
    x = pct_missing, 
    y = c_index, 
    color = continuous_method,
    fill = continuous_method,
    shape = continuous_method
  )
) +
  geom_point(alpha = 0.8, size = 2.5, stroke = 0.5) +
  geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs"),
              se = TRUE, linewidth = 1.2, alpha = 0.2) +
  facet_wrap(facets = ~endpoint, ncol = 2, labeller = labeller(endpoint = label_wrap_gen(10))) +
  scale_color_viridis_d(option = "plasma", end = 0.95,
                        labels = function(x) ifelse(x == "NA", "No Imputation", x)) +
  scale_fill_viridis_d(option = "plasma", end = 0.95) +
  scale_shape_manual(values = c(16, 17, 15, 18, 3, 4, 8),
                     labels = function(x) ifelse(x == "NA", "No Imputation", x)) +
  scale_y_continuous(limits = c(0.5, 1), breaks = seq(0.5, 1, by = 0.1)) +
  labs(
    title = "Model Performance Under Increasing Missing Data",
    subtitle = "GAM fits with 95% confidence bands",
    x = "Percentage of Missing Values",
    y = "Concordance Index (C-index)",
    color = "Imputation Method", fill = "Imputation Method", shape = "Imputation Method"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "bottom",
    legend.box = "horizontal",
    legend.spacing.x = unit(0.5, "cm"),
    panel.grid.minor = element_blank(),
    strip.text = element_text(face = "bold", size = 11),
    plot.subtitle = element_text(color = "grey40", margin = margin(b = 15)),
    plot.margin = margin(1, 1, 1, 1, "cm")
  ) +
  geom_hline(yintercept = 0.75, linetype = "dashed", color = "grey30", alpha = 0.5) +
  annotate("text", x = 90, y = 0.72, label = "Clinical Relevance Threshold",
           color = "grey30", size = 3.5)

print(p1_enhanced)

# High-res exports (GAM plot) -------------------------------------------------
ggsave(filename = "performance_vs_missingness.tiff", plot = p1_enhanced,
       device = "tiff", width = 12, height = 10, units = "in", dpi = 600, compression = "lzw")

ggsave(filename = "performance_vs_missingness.pdf", plot = p1_enhanced,
       device = "pdf", width = 12, height = 10, units = "in", dpi = 600, bg = "transparent")

# Universal exporter (utility) ------------------------------------------------
save_plot <- function(plot_obj, 
                      base_name = "performance_vs_missingness",
                      width = 12, 
                      height = 10) {
  if (!dir.exists("figures")) {
    dir.create("figures")
    message("Created 'figures' directory")
  }
  timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  tiff_path <- file.path("figures", paste0(base_name, "_", timestamp, ".tiff"))
  ggsave(filename = tiff_path, plot = plot_obj, device = "tiff",
         width = width, height = height, units = "in", dpi = 600, compression = "lzw", bg = "white")
  message(sprintf("Saved TIFF to: %s", tiff_path))
  pdf_path <- file.path("figures", paste0(base_name, "_", timestamp, ".pdf"))
  ggsave(filename = pdf_path, plot = plot_obj, device = cairo_pdf,
         width = width, height = height, units = "in", dpi = 600, bg = "transparent", useDingbats = FALSE)
  message(sprintf("Saved PDF to: %s", pdf_path))
  png_path <- file.path("figures", paste0(base_name, "_preview.png"))
  ggsave(filename = png_path, plot = plot_obj, device = "png",
         width = width, height = height, units = "in", dpi = 600)
}

tryCatch({ save_plot(p1_enhanced) },
         error = function(e) { message("ERROR in saving: ", e$message)
           message("Attempting fallback save...")
           ggsave("fallback_plot.pdf", plot = p1_enhanced) })

# COMPLETE PLOT (single instance of threshold label; unclipped) ---------------
p1_enhanced <- ggplot(
  data = plot_data, 
  mapping = aes(
    x = pct_missing, y = c_index, 
    color = continuous_method, fill = continuous_method, shape = continuous_method
  )
) +
  geom_point(alpha = 0.8, size = 2.5, stroke = 0.5) +
  geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs"),
              se = TRUE, linewidth = 1.2, alpha = 0.2) +
  facet_wrap(facets = ~endpoint, ncol = 2, labeller = labeller(endpoint = label_wrap_gen(10))) +
  scale_color_viridis_d(option = "plasma", end = 0.95,
                        labels = function(x) ifelse(x == "NA", "No Imputation", x)) +
  scale_fill_viridis_d(option = "plasma", end = 0.95) +
  scale_shape_manual(values = c(16, 17, 15, 18, 3, 4, 8),
                     labels = function(x) ifelse(x == "NA", "No Imputation", x)) +
  scale_y_continuous(limits = c(0.5, 1), breaks = seq(0.5, 1, by = 0.1)) +
  geom_hline(yintercept = 0.75, linetype = "dashed", color = "grey30", alpha = 0.5) +
  annotate("text", x = max(plot_data$pct_missing, na.rm = TRUE) * 0.9, y = 0.72,
           label = "Clinical Relevance Threshold", color = "grey30", size = 3.5, hjust = 1) +
  labs(
    title = "Model Performance Under Increasing Missing Data",
    subtitle = "GAM fits with 95% confidence bands",
    x = "Percentage of Missing Values", y = "Concordance Index (C-index)",
    color = "Imputation Method", fill = "Imputation Method", shape = "Imputation Method"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "bottom", legend.box = "horizontal", legend.spacing.x = unit(0.5, "cm"),
    panel.grid.minor = element_blank(), strip.text = element_text(face = "bold", size = 11),
    plot.subtitle = element_text(color = "grey40", margin = margin(b = 15)),
    plot.margin = margin(1, 3, 1, 1, "cm")
  ) +
  coord_cartesian(clip = 'off')

# Save unclipped versions -----------------------------------------------------
ggsave(filename = "performance_plot.tiff", plot = p1_enhanced,
       device = "tiff", width = 8, height = 6, units = "in", dpi = 600, compression = "lzw")

print(p1_enhanced)

ggsave(filename = "performance_plot.pdf",
       plot = p1_enhanced + coord_cartesian(clip = 'off') +
         theme(plot.margin = margin(1, 3, 1, 1, "cm")),
       device = pdf, width = 8, height = 6, units = "in",
       bg = "transparent", useDingbats = FALSE)

ggsave(filename = "performance_plot_cairo.pdf",
       plot = p1_enhanced + coord_cartesian(clip = 'off') +
         theme(plot.margin = margin(1, 3, 1, 1, "cm")),
       device = cairo_pdf, width = 8, height = 6, units = "in", bg = "transparent")

library(ggplot2)
library(ggtext)  # element_markdown() for rich subtitles

# Nature-style theme helper ---------------------------------------------------
nature_theme <- function() {
  theme_minimal(base_size = 8) +
    theme(
      text = element_text(family = "Arial", color = "black"),
      plot.title = element_text(face = "bold", size = 10, hjust = 0.5),
      plot.subtitle = element_markdown(size = 8, hjust = 0.5, lineheight = 1.2),
      axis.title = element_text(size = 8, face = "plain"),
      axis.text = element_text(size = 7),
      legend.title = element_text(size = 8),
      legend.text = element_text(size = 7),
      legend.position = "bottom",
      legend.key.size = unit(0.4, "cm"),
      panel.grid.major = element_line(linewidth = 0.25, color = "gray90"),
      panel.grid.minor = element_blank(),
      strip.text = element_text(face = "bold", size = 8),
      plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm")
    )
}

# Nature-styled OS trend plots ------------------------------------------------
nature_plot <- ggplot(
  df %>% filter(!is.na(continuous_method)),
  aes(x = pct_missing, y = os_cindex, color = continuous_method)
) +
  geom_hline(yintercept = 0.75, linetype = "dotted", color = "red", linewidth = 0.5, alpha = 0.7) +
  geom_text(data = data.frame(
    x = max(df$pct_missing, na.rm = TRUE), y = 0.76,
    label = "Clinical Threshold (0.75)",
    continuous_method = levels(factor(df$continuous_method))[1]
  ),
  aes(x = x, y = y, label = label),
  color = "red", size = 2.5, hjust = 1.1, vjust = 0, inherit.aes = FALSE
  ) +
  geom_point(size = 1.5, alpha = 0.8, shape = 16) +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE, linewidth = 0.5) +
  scale_color_manual(
    values = c("XGBoost"="#E69F00","missForest"="#56B4E9","mean"="#009E73","median"="#0072B2","knn"="#CC79A7"),
    name = "Imputation Method"
  ) +
  labs(
    title = "Impact of Missing Data on Survival Prediction Accuracy",
    subtitle = "OS concordance index (C-index)",
    x = "Percentage of missing values", y = "C-index (OS)"
  ) +
  nature_theme() +
  facet_wrap(~continuous_method, ncol = 3)

print(nature_plot)

# Nature-compliant exports ----------------------------------------------------
ggsave("Fig1_method_comparison.tiff", plot = nature_plot,
       device = "tiff", width = 8.9, height = 7.5, units = "cm", dpi = 600, compression = "lzw")

ggsave(filename = "Fig1_method_comparison.pdf", plot = nature_plot,
       device = cairo_pdf, width = 12, height = 7.5, units = "cm", dpi = 600, bg = "white")

# Optional verification (requires pdftools/magick) ---------------------------
if (!require(pdftools)) install.packages("pdftools")
library(pdftools)
pdf_info("Fig1_method_comparison.pdf")[c("pages", "version", "encrypted")]

# install.packages(c("pdftools", "magick"))  # one-time
magick::image_read_pdf("Fig1_method_comparison.pdf", density = 600) %>% image_info()

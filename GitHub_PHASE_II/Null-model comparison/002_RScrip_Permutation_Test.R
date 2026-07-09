###############################################################################
# R1.2 — Permutation Testing: MVL + XGBoost (All 96 Strata)
# ============================================================================
# Tests whether observed C-index exceeds null distribution for both
# MVL SuperLearner and XGBoost across all strata.
#
# MVL risk scores from MVL_Synthesis/Patient_Probabilities.tsv
# XGBoost risk scores from predict() on original dfXXX matrices
#
# Output: permutation_results.tsv
###############################################################################

library(survival)
library(xgboost)

setwd("~/students/aluno0549-6/PHASE_III/PHASE_III_Megarun_5_0_complete_ZIMA_Suit_Generator_final")
MODELS_DIR <- "PHASE_III_ML_Models"
DF_DIR     <- "~/students/aluno0549-6"
N_PERM     <- 1000
SEED       <- 123

# Cache loaded df matrices
df_cache <- list()

load_df <- function(df_name) {
  if (df_name %in% names(df_cache)) return(df_cache[[df_name]])
  path <- file.path(DF_DIR, paste0(df_name, ".rds"))
  if (!file.exists(path)) path <- file.path(DF_DIR, "dfXXX_series", paste0(df_name, ".rds"))
  if (!file.exists(path)) stop(sprintf("Cannot find %s", df_name))
  cat(sprintf("  Loading %s...\n", df_name))
  df <- readRDS(path)
  df_cache[[df_name]] <<- df
  df
}

# Helper to rbind results safely (handle list columns)
rbind_results <- function(res_list) {
  flat <- lapply(res_list, function(r) {
    as.data.frame(lapply(r, function(x) if(is.list(x)) NA else x), 
                  stringsAsFactors = FALSE)
  })
  as.data.frame(do.call(rbind, flat), stringsAsFactors = FALSE)
}

permutation_test <- function(cohort, idx, total) {
  cat(sprintf("[%d/%d] %s ... ", idx, total, cohort))
  
  prob_file <- file.path(MODELS_DIR, cohort, "MVL_Synthesis",
                         paste0(cohort, "_MVL_Synthesis_Patient_Probabilities.tsv"))
  bundle_file <- file.path(MODELS_DIR, cohort, paste0("model_bundle_", cohort, ".rds"))
  
  probs  <- read.table(prob_file, header = TRUE, sep = "\t")
  bundle <- readRDS(bundle_file)
  
  surv <- as.data.frame(bundle$RSF$yvar)
  surv$Patient_ID <- gsub("-[0-9]{2}[A-Z]?$", "", rownames(surv))
  
  m <- merge(probs[, c("Patient_ID", "Event_Prob_1Yr")],
             surv[, c("Patient_ID", "time", "status")],
             by = "Patient_ID")
  
  n <- nrow(m); n_events <- sum(m$status == 1)
  
  # ---- XGBoost predictions ----
  xgb_model <- bundle$XGBoost
  feat_names <- xgb_model$feature_names
  df_var <- gsub(".*_(df[0-9]+)$", "\\1", cohort)
  cancer_type <- strsplit(cohort, "_")[[1]][1]
  
  df <- load_df(df_var)
  sub <- df[df$type == cancer_type, ]
  pred_cols <- intersect(feat_names, names(sub))
  sub$Patient_ID <- gsub("-[0-9]{2}[A-Z]?$", "", sub$patient)
  
  matched_idx <- match(m$Patient_ID, sub$Patient_ID)
  sub_m <- sub[matched_idx[!is.na(matched_idx)], pred_cols, drop = FALSE]
  
  xgb_risk <- tryCatch({
    mat <- as.matrix(sub_m)
    mode(mat) <- "numeric"  # force character CNV cols to numeric
    as.numeric(predict(xgb_model, newdata = mat))
  }, error = function(e) {
    cat(sprintf("XGB failed(%s). ", e$message)); NULL
  })
  
  has_xgb <- !is.null(xgb_risk) && length(xgb_risk) == nrow(m)
  
  # ---- Observed C-index ----
  c_obs_mvl <- concordance(Surv(time, status) ~ Event_Prob_1Yr, data = m, reverse = TRUE)
  c_obs_xgb <- if (has_xgb) concordance(Surv(time, status) ~ xgb_risk, data = m, reverse = TRUE) else list(concordance = NA)
  
  # ---- Permutation ----
  set.seed(SEED + idx)
  perm_mvl <- numeric(N_PERM)
  perm_xgb <- numeric(N_PERM)
  m_perm <- m
  
  for (i in seq_len(N_PERM)) {
    m_perm$time   <- sample(m$time)
    m_perm$status <- sample(m$status)
    perm_mvl[i] <- concordance(Surv(time, status) ~ Event_Prob_1Yr, data = m_perm, reverse = TRUE)$concordance
    if (has_xgb) {
      perm_xgb[i] <- concordance(Surv(time, status) ~ xgb_risk, data = m_perm, reverse = TRUE)$concordance
    }
  }
  
  p_mvl <- sum(perm_mvl >= c_obs_mvl$concordance) / N_PERM
  ci_mvl <- quantile(perm_mvl, probs = c(0.025, 0.5, 0.975))
  
  result <- list(
    cohort = cohort, n = n, n_events = n_events,
    c_mvl_obs = c_obs_mvl$concordance,
    c_mvl_null_med = ci_mvl["50%"], c_mvl_null_lo = ci_mvl["2.5%"], c_mvl_null_hi = ci_mvl["97.5%"],
    p_mvl = p_mvl
  )
  
  if (has_xgb) {
    p_xgb <- sum(perm_xgb >= c_obs_xgb$concordance) / N_PERM
    ci_xgb <- quantile(perm_xgb, probs = c(0.025, 0.5, 0.975))
    result$c_xgb_obs <- c_obs_xgb$concordance
    result$c_xgb_null_med <- ci_xgb["50%"]
    result$c_xgb_null_lo <- ci_xgb["2.5%"]
    result$c_xgb_null_hi <- ci_xgb["97.5%"]
    result$p_xgb <- p_xgb
  } else {
    result$c_xgb_obs <- NA; result$c_xgb_null_med <- NA
    result$c_xgb_null_lo <- NA; result$c_xgb_null_hi <- NA; result$p_xgb <- NA
  }
  
  cat(sprintf("MVL=%.4f (p=%.3f)", c_obs_mvl$concordance, p_mvl))
  if (has_xgb) cat(sprintf(", XGB=%.4f (p=%.3f)", c_obs_xgb$concordance, p_xgb))
  cat(sprintf(" [n=%d, ev=%d]\n", n, n_events))
  
  result
}

# ── Run ──────────────────────────────────────────────────────────────────────
strata <- list.dirs(MODELS_DIR, full.names = FALSE, recursive = FALSE)
strata <- strata[grepl("_df[0-9]+$", strata)]
cat(sprintf("Found %d strata.\n\n", length(strata)))

results <- list()
for (i in seq_along(strata)) {
  res <- tryCatch(
    permutation_test(strata[i], i, length(strata)),
    error = function(e) {
      cat(sprintf("ERROR: %s\n", e$message))
      NULL
    }
  )
  if (!is.null(res)) results[[strata[i]]] <- res
  
  if (i %% 10 == 0 && length(results) > 0) {
    dt <- rbind_results(results)
    write.table(dt, "permutation_results.tsv", sep = "\t", row.names = FALSE, quote = FALSE)
    cat(sprintf("  [Saved %d]\n", nrow(dt)))
  }
}

if (length(results) > 0) {
  dt <- rbind_results(results)
  write.table(dt, "permutation_results.tsv", sep = "\t", row.names = FALSE, quote = FALSE)
  cat(sprintf("\n=== DONE: %d strata ===\n", nrow(dt)))
  cat(sprintf("MVL p<0.001: %d/%d\n", sum(sapply(results, function(r) r$p_mvl < 0.001)), length(results)))
  n_xgb <- sum(sapply(results, function(r) !is.na(r$p_xgb)))
  if (n_xgb > 0) cat(sprintf("XGB p<0.001: %d/%d\n", sum(sapply(results, function(r) !is.na(r$p_xgb) && r$p_xgb < 0.001)), n_xgb))
}

###############################################################################
# R1.7 — Bootstrap CIs for MVL C-index — ALL THREE TIME HORIZONS
# ============================================================================
# Computes 1Yr, 3Yr, and 5Yr bootstrap CIs in a single pass per stratum.
# This provides complete transparency: no single Breslow horizon is optimal
# for all cancer types. Table S10 reports all three; the raw linear predictor
# C-index (Table S10 original) remains the gold standard.
#
# Output: bootstrap_ci_results_3horizons.tsv
###############################################################################

library(survival)

MASTER_DIR <- "~/students/aluno0549-6/PHASE_III/PHASE_III_Megarun_5_0_complete_ZIMA_Suit_Generator_final"
setwd(MASTER_DIR)

MODELS_DIR <- "PHASE_III_ML_Models"
OUTPUT     <- "bootstrap_ci_results_3horizons.tsv"
N_BOOT     <- 1000
SEED       <- 42

# ── Bootstrap C-index ────────────────────────────────────────────────────────
bootstrap_cindex <- function(risk, time, event, R = N_BOOT, stratum_seed = SEED) {
  set.seed(stratum_seed)
  n <- length(time)
  if (length(unique(risk)) == 1) {
    return(c(cindex = NA, ci_low = NA, ci_high = NA, ci_width = NA))
  }
  
  c_obs <- concordance(Surv(time, event) ~ risk, reverse = TRUE)
  c_val <- c_obs$concordance
  
  boot_vals <- numeric(R)
  for (r in seq_len(R)) {
    idx <- sample(seq_len(n), replace = TRUE)
    boot_vals[r] <- as.numeric(concordance(Surv(time[idx], event[idx]) ~ risk[idx],
                                           reverse = TRUE)$concordance)
  }
  ci <- unname(quantile(boot_vals, probs = c(0.025, 0.975), na.rm = TRUE))
  
  c(cindex = c_val, ci_low = ci[1], ci_high = ci[2], ci_width = ci[2] - ci[1])
}

# ── Main loop ────────────────────────────────────────────────────────────────
strata <- list.dirs(MODELS_DIR, full.names = FALSE, recursive = FALSE)
strata <- strata[grepl("_df[0-9]+$", strata)]
cat(sprintf("Found %d strata.\n\n", length(strata)))

results <- list()
errors  <- character()

for (i in seq_along(strata)) {
  s <- strata[i]
  cat(sprintf("[%d/%d] %s ... ", i, length(strata), s))
  
  prob_file <- file.path(MODELS_DIR, s, "MVL_Synthesis",
                         paste0(s, "_MVL_Synthesis_Patient_Probabilities.tsv"))
  bundle_file <- file.path(MODELS_DIR, s, paste0("model_bundle_", s, ".rds"))
  
  if (!file.exists(prob_file)) {
    cat("SKIP: no probability file\n")
    errors[s] <- "Missing Patient_Probabilities.tsv"
    next
  }
  if (!file.exists(bundle_file)) {
    cat("SKIP: no bundle\n")
    errors[s] <- "Missing model_bundle.rds"
    next
  }
  
  tryCatch({
    probs  <- read.table(prob_file, header = TRUE, sep = "\t")
    bundle <- readRDS(bundle_file)
    
    surv <- as.data.frame(bundle$RSF$yvar)
    surv$Patient_ID <- gsub("-[0-9]{2}[A-Z]?$", "", rownames(surv))
    
    # Build merge: select only columns that exist in probs
    prob_cols <- c("Patient_ID")
    for (col in c("Event_Prob_1Yr", "Event_Prob_3Yr", "Event_Prob_5Yr")) {
      if (col %in% names(probs)) prob_cols <- c(prob_cols, col)
    }
    m <- merge(probs[, prob_cols, drop = FALSE],
               surv[, c("Patient_ID", "time", "status")],
               by = "Patient_ID")
    
    has_1yr <- "Event_Prob_1Yr" %in% names(m)
    has_3yr <- "Event_Prob_3Yr" %in% names(m)
    has_5yr <- "Event_Prob_5Yr" %in% names(m)
    
    if (nrow(m) < 10) {
      cat(sprintf("SKIP: %d patients\n", nrow(m)))
      errors[s] <- sprintf("Too few patients: %d", nrow(m))
      next
    }
    
    ci1 <- if (has_1yr) bootstrap_cindex(m$Event_Prob_1Yr, m$time, m$status, stratum_seed = SEED + i)       else c(cindex = NA, ci_low = NA, ci_high = NA, ci_width = NA)
    ci3 <- if (has_3yr) bootstrap_cindex(m$Event_Prob_3Yr, m$time, m$status, stratum_seed = SEED + i + 100) else c(cindex = NA, ci_low = NA, ci_high = NA, ci_width = NA)
    ci5 <- if (has_5yr) bootstrap_cindex(m$Event_Prob_5Yr, m$time, m$status, stratum_seed = SEED + i + 200) else c(cindex = NA, ci_low = NA, ci_high = NA, ci_width = NA)
    
    results[[s]] <- c(
      cohort = s,
      c1yr = ci1["cindex"], c1yr_lo = ci1["ci_low"], c1yr_hi = ci1["ci_high"], c1yr_w = ci1["ci_width"],
      c3yr = ci3["cindex"], c3yr_lo = ci3["ci_low"], c3yr_hi = ci3["ci_high"], c3yr_w = ci3["ci_width"],
      c5yr = ci5["cindex"], c5yr_lo = ci5["ci_low"], c5yr_hi = ci5["ci_high"], c5yr_w = ci5["ci_width"],
      n = nrow(m), n_events = sum(m$status == 1)
    )
    
    cat(sprintf("1Yr=%.4f 3Yr=%.4f 5Yr=%.4f (n=%d, ev=%d)\n",
                ci1["cindex"], ci3["cindex"], ci5["cindex"],
                nrow(m), sum(m$status == 1)))
    
  }, error = function(e) {
    cat(sprintf("ERROR: %s\n", e$message))
    errors[s] <<- e$message
  })
  
  if (i %% 10 == 0 && length(results) > 0) {
    dt <- as.data.frame(do.call(rbind, results), stringsAsFactors = FALSE)
    for (col in names(dt)) dt[[col]] <- type.convert(dt[[col]], as.is = TRUE)
    write.table(dt, OUTPUT, sep = "\t", row.names = FALSE, quote = FALSE)
    cat(sprintf("  [Saved %d]\n", nrow(dt)))
  }
}

# ── Final ────────────────────────────────────────────────────────────────────
if (length(results) > 0) {
  dt <- as.data.frame(do.call(rbind, results), stringsAsFactors = FALSE)
  for (col in names(dt)) dt[[col]] <- type.convert(dt[[col]], as.is = TRUE)
  write.table(dt, OUTPUT, sep = "\t", row.names = FALSE, quote = FALSE)
  
  cat(sprintf("\n=== DONE: %d strata ===\n", nrow(dt)))
  cat(sprintf("Median C-index 1Yr: %.4f\n", median(dt$c1yr, na.rm = TRUE)))
  cat(sprintf("Median C-index 3Yr: %.4f\n", median(dt$c3yr, na.rm = TRUE)))
  cat(sprintf("Median C-index 5Yr: %.4f\n", median(dt$c5yr, na.rm = TRUE)))
  cat(sprintf("Saved: %s\n", OUTPUT))
}

if (length(errors) > 0) {
  cat(sprintf("\n%d errors:\n", length(errors)))
  for (s in names(errors)) cat(sprintf("  %s: %s\n", s, errors[s]))
}

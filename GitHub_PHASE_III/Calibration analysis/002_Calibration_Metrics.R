###############################################################################
# R2.6 — Calibration Metrics for MVL SuperLearner (All 96 Strata)
# ============================================================================
# Computes per-stratum:
#   - Calibration slope: coxph(Surv ~ lp) where lp = log(-log(1-p_1yr))
#   - Brier Score at 1, 3, 5 years (IPCW-weighted)
#   - Integrated Brier Score (IBS)
#
# Dependencies: survival only (no pec needed)
# Output: calibration_metrics.tsv
###############################################################################

library(survival)

setwd("~/students/aluno0549-6/PHASE_III/PHASE_III_Megarun_5_0_complete_ZIMA_Suit_Generator_final/Calibration_analysis")
MODELS_DIR <- "../PHASE_III_ML_Models"

strata <- list.dirs(MODELS_DIR, full.names = FALSE, recursive = FALSE)
strata <- strata[grepl("_df[0-9]+$", strata)]
cat(sprintf("Found %d strata.\n", length(strata)))

# Inverse probability of censoring weights (IPCW)
# G(t) = KM estimate of censoring survival
ipcw_brier <- function(time, status, predicted_prob, eval_time) {
  # predicted_prob is P(T <= eval_time) — event probability
  # Survival probability: S(t) = 1 - P(T <= t)
  surv_pred <- 1 - predicted_prob
  
  # Censoring distribution: "event" for censoring is !status
  cens_km <- survfit(Surv(time, 1 - status) ~ 1)
  
  # G(T_i) for events, G(t) for censored
  G_T <- approx(cens_km$time, cens_km$surv, xout = time, rule = 2)$y
  G_t <- approx(cens_km$time, cens_km$surv, xout = eval_time, rule = 2)$y
  
  # Brier score
  n <- length(time)
  loss <- rep(NA, n)
  
  # Event before eval_time: weight = 1/G(T_i)
  idx_event <- which(status == 1 & time <= eval_time)
  loss[idx_event] <- (0 - surv_pred[idx_event])^2 / G_T[idx_event]
  
  # Censored after eval_time or alive past eval_time: weight = 1/G(t)
  idx_cens <- which(time > eval_time)
  loss[idx_cens] <- (1 - surv_pred[idx_cens])^2 / G_t
  
  # Events after eval_time: weight = 1/G(t) (alive at eval_time)
  idx_late <- which(status == 1 & time > eval_time)
  loss[idx_late] <- (1 - surv_pred[idx_late])^2 / G_t
  
  # Handle censored before eval_time: excluded (zero weight)
  mean(loss, na.rm = TRUE)
}

compute_calibration <- function(cohort, idx, total) {
  cat(sprintf("[%d/%d] %s ... ", idx, total, cohort))
  
  prob_file <- file.path(MODELS_DIR, cohort, "MVL_Synthesis",
                         paste0(cohort, "_MVL_Synthesis_Patient_Probabilities.tsv"))
  bundle_file <- file.path(MODELS_DIR, cohort,
                           paste0("model_bundle_", cohort, ".rds"))
  
  if (!file.exists(prob_file) || !file.exists(bundle_file)) {
    cat("MISSING\n")
    return(NULL)
  }
  
  probs  <- read.table(prob_file, header = TRUE, sep = "\t")
  bundle <- readRDS(bundle_file)
  
  surv <- as.data.frame(bundle$RSF$yvar)
  surv$Patient_ID <- gsub("-[0-9]{2}[A-Z]?$", "", rownames(surv))
  
  m <- merge(probs, surv[, c("Patient_ID", "time", "status")], by = "Patient_ID")
  
  n <- nrow(m)
  n_events <- sum(m$status == 1)
  
  if (n < 20 || n_events < 5) {
    cat(sprintf("SKIP n=%d ev=%d\n", n, n_events))
    return(data.frame(cohort = cohort, n = n, n_events = n_events,
                       cal_slope = NA, cal_se = NA,
                       brier_1yr = NA, brier_3yr = NA, brier_5yr = NA, 
                       ibs = NA, stringsAsFactors = FALSE))
  }
  
  # ---- Calibration slope ----
  # Linear predictor from 1yr probability: lp = log(-log(1-p))
  p_clip <- pmax(pmin(m$Event_Prob_1Yr, 0.9999), 0.0001)
  m$lp <- log(-log(1 - p_clip))
  
  cal_fit <- tryCatch(
    coxph(Surv(time, status) ~ lp, data = m),
    error = function(e) NULL
  )
  
  if (is.null(cal_fit)) {
    cal_slope <- NA; cal_se <- NA; cal_intercept <- NA
  } else {
    cal_slope <- coef(cal_fit)
    cal_se <- summary(cal_fit)$coefficients[1, "se(coef)"]
    # Calibration-in-the-large: ratio of observed to expected events
    # Using Grønnesby-Borgan test: observed vs expected within risk groups
    n_groups <- min(4, floor(n / 20))
    if (n_groups >= 2) {
      breaks <- unique(quantile(m$lp, probs = seq(0, 1, length.out = n_groups + 1)))
      if (length(breaks) >= 3) {
        risk_quantile <- cut(m$lp, breaks = breaks, include.lowest = TRUE, labels = FALSE)
        obs_events <- tapply(m$status * (m$time <= 365), risk_quantile, sum)
        exp_prob <- tapply(m$Event_Prob_1Yr, risk_quantile, mean)
        n_risk <- table(risk_quantile)
        exp_events <- exp_prob * as.numeric(n_risk)
        cal_intercept <- sum(obs_events, na.rm = TRUE) / sum(exp_events, na.rm = TRUE)
      } else {
        cal_intercept <- NA
      }
    } else {
      cal_intercept <- NA
    }
  }
  
  # ---- Brier Scores (IPCW-weighted) ----
  brier_1yr <- tryCatch(
    ipcw_brier(m$time, m$status, m$Event_Prob_1Yr, 365),
    error = function(e) NA
  )
  brier_3yr <- tryCatch(
    ipcw_brier(m$time, m$status, m$Event_Prob_3Yr, 1095),
    error = function(e) NA
  )
  brier_5yr <- tryCatch(
    ipcw_brier(m$time, m$status, m$Event_Prob_5Yr, 1825),
    error = function(e) NA
  )
  
  # ---- Integrated Brier Score (0 to min(1825, max_time)) ----
  max_t <- min(1825, max(m$time))
  times_seq <- seq(0, max_t, length.out = min(50, max_t/30 + 1))
  ibs <- tryCatch({
    ibs_vals <- sapply(times_seq, function(t) {
      # For intermediate times, interpolate between nearest horizon probabilities
      if (t <= 365) p <- m$Event_Prob_1Yr
      else if (t <= 1095) p <- m$Event_Prob_3Yr
      else p <- m$Event_Prob_5Yr
      ipcw_brier(m$time, m$status, p, t)
    })
    mean(ibs_vals, na.rm = TRUE)
  }, error = function(e) NA)
  
  cat(sprintf("slope=%.3f B1=%.4f B3=%.4f B5=%.4f IBS=%.4f [%d,%d]\n",
              cal_slope, brier_1yr, brier_3yr, brier_5yr, ibs, n, n_events))
  
  data.frame(
    cohort = cohort, n = n, n_events = n_events,
    cal_slope = round(cal_slope, 4),
    cal_se = round(cal_se, 4),
    cal_intercept = round(cal_intercept, 4),
    brier_1yr = round(brier_1yr, 6),
    brier_3yr = round(brier_3yr, 6),
    brier_5yr = round(brier_5yr, 6),
    ibs = round(ibs, 6),
    stringsAsFactors = FALSE
  )
}

# ── Run ──────────────────────────────────────────────────────────────────────
results <- list()
for (i in seq_along(strata)) {
  res <- tryCatch(
    compute_calibration(strata[i], i, length(strata)),
    error = function(e) {
      cat(sprintf("ERROR: %s\n", e$message))
      NULL
    }
  )
  if (!is.null(res)) results[[strata[i]]] <- res
  
  if (i %% 10 == 0 && length(results) > 0) {
    dt <- do.call(rbind, results)
    write.table(dt, "calibration_metrics.tsv", sep = "\t", row.names = FALSE, quote = FALSE)
    cat(sprintf("  [Saved %d]\n", nrow(dt)))
  }
}

if (length(results) > 0) {
  dt <- do.call(rbind, results)
  write.table(dt, "calibration_metrics.tsv", sep = "\t", row.names = FALSE, quote = FALSE)
  
  cat(sprintf("\n=== DONE: %d strata ===\n", nrow(dt)))
  cat(sprintf("Median calibration slope: %.3f\n", median(dt$cal_slope, na.rm = TRUE)))
  cat(sprintf("Median Brier (1yr): %.4f\n", median(dt$brier_1yr, na.rm = TRUE)))
  cat(sprintf("Median IBS: %.4f\n", median(dt$ibs, na.rm = TRUE)))
  n_valid <- sum(!is.na(dt$cal_slope))
  n_well <- sum(dt$cal_slope >= 0.7 & dt$cal_slope <= 1.3, na.rm = TRUE)
  cat(sprintf("Strata with slope in [0.7, 1.3]: %d/%d (%.1f%%)\n", 
              n_well, n_valid, 100 * n_well / n_valid))
}

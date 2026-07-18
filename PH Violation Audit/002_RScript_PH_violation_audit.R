# =============================================================================
# PH VIOLATION AUDIT — 120 CANARY-tested strata (replicates CANARY data pipeline)
# Uses SAME functions as Megarun 5.0: filter_survival_complete + build_combined_X_matrix
# Different test: unpenalized Cox PH + Schoenfeld (vs CANARY's CoxNet)
# =============================================================================

setwd("~/students/aluno0549-6/PHASE_III/PHASE_III_Megarun_5_0_complete_ZIMA_Suit_Generator_final")

suppressMessages({
  library(survival)
  library(data.table)
  library(dplyr)
})

DF_ROOT    <- "~/students/aluno0549-6/dfXXX_series"
TAR_TABLE  <- "~/students/aluno0549-6/PHASE_III/CoxNet_phaseII_feasibility_log.rds"
OUT_DIR    <- "PH_Violation_Audit"
dir.create(OUT_DIR, showWarnings = FALSE, recursive = TRUE)

# ── Replicate CANARY functions exactly ──────────────────────────────────
filter_survival_complete <- function(df_sub, metric) {
  time  <- as.numeric(as.character(df_sub[[paste0(metric, ".time")]]))
  event <- as.numeric(as.character(df_sub[[metric]]))
  mask <- is.finite(time) & is.finite(event) & !is.na(event) & event %in% c(0, 1)
  list(df_masked = df_sub[mask, , drop = FALSE], time = time[mask], event = event[mask])
}

build_combined_X_matrix <- function(df_masked, cancer_type) {
  pattern <- paste0("^", cancer_type, "-")
  preds <- names(df_masked)[grepl(pattern, names(df_masked))]
  if (length(preds) == 0L) return(NULL)
  df_X <- df_masked[, preds, drop = FALSE]
  df_X[] <- lapply(df_X, function(col) {
    if (is.character(col) || is.factor(col)) as.numeric(as.factor(col)) else col
  })
  as.matrix(df_X)
}

# ── Load ALL 120 CANARY-tested combinations ─────────────────────────────
cat("Loading CANARY audit log...\n")
dfinput_raw <- readRDS(TAR_TABLE)
cat(sprintf("Total CANARY-tested combinations: %d\n", nrow(dfinput_raw)))

# Process ALL rows (include both ELIGIBLE and non-ELIGIBLE)
results      <- data.frame()
loglog_list  <- list()
schoenfeld_list <- list()

for (i in 1:nrow(dfinput_raw)) {
  row  <- dfinput_raw[i, ]
  c    <- trimws(as.character(row$cancer_type))
  m    <- trimws(as.character(row$metric))
  d    <- trimws(as.character(row$df_file))
  
  # Load LiSHMOM variant
  clean_d <- ifelse(grepl("\\.rds$", d, ignore.case = TRUE), d, paste0(d, ".rds"))
  df_path <- file.path(DF_ROOT, clean_d)
  if (!file.exists(df_path)) next
  
  df_raw <- readRDS(df_path)
  
  # Filter by cancer type (SAME as CANARY)
  df_cancer <- df_raw[df_raw$type == c, , drop = FALSE]
  if (nrow(df_cancer) == 0) next
  
  # Filter by survival metric (SAME as CANARY)
  surv <- tryCatch(filter_survival_complete(df_cancer, m), error = function(e) NULL)
  if (is.null(surv)) next
  
  # Build predictor matrix (SAME as CANARY)
  X <- build_combined_X_matrix(surv$df_masked, c)
  if (is.null(X) || ncol(X) == 0) next
  
  # Exclude ≥35% missingness (SAME as CANARY)
  na_props <- rowSums(is.na(X)) / ncol(X)
  keep_idx <- which(na_props < 0.35)
  
  if (length(keep_idx) < 30) {
    results <- rbind(results, data.frame(
      Stratum = paste(c, m, sep = "_"), Cancer = c, Endpoint = m, df = d,
      N = length(keep_idx), N_events = sum(surv$event[keep_idx]),
      Cox_Converged = FALSE, Schoenfeld_Global_p = NA,
      Schoenfeld_Global_chisq = NA, Significant_Covariates = NA,
      CANARY_Status = trimws(as.character(row$PHASE_III_logic)),
      stringsAsFactors = FALSE))
    cat(sprintf("  %s_%s N=%d E=%d -> INSUFFICIENT (CANARY: %s)\n",
                c, m, length(keep_idx), sum(surv$event[keep_idx]), 
                row$PHASE_III_logic))
    next
  }
  
  X_clean <- X[keep_idx, , drop = FALSE]
  y_time  <- surv$time[keep_idx]
  y_event <- surv$event[keep_idx]
  
  n        <- nrow(X_clean)
  n_events <- sum(y_event)
  
  # ── Cox PH (DIFFERENT from CANARY's CoxNet) ──
  df_cox <- data.frame(time = y_time, status = y_event)
  df_cox <- cbind(df_cox, as.data.frame(X_clean))
  
  # Remove columns with zero variance
  zero_var <- apply(df_cox[, -(1:2), drop = FALSE], 2, var, na.rm = TRUE) == 0
  zero_var[is.na(zero_var)] <- TRUE
  if (any(zero_var)) df_cox <- df_cox[, c(TRUE, TRUE, !zero_var)]
  
  df_cox <- na.omit(df_cox)
  cox_fit <- tryCatch(
    coxph(Surv(time, status) ~ ., data = df_cox),
    error = function(e) NULL
  )
  
  if (is.null(cox_fit)) {
    results <- rbind(results, data.frame(
      Stratum = paste(c, m, sep = "_"), Cancer = c, Endpoint = m, df = d,
      N = n, N_events = n_events,
      Cox_Converged = FALSE, Schoenfeld_Global_p = NA,
      Schoenfeld_Global_chisq = NA, Significant_Covariates = NA,
      CANARY_Status = trimws(as.character(row$PHASE_III_logic)),
      stringsAsFactors = FALSE))
    cat(sprintf("  %s_%s N=%d E=%d -> NON-CONVERGENCE (CANARY: %s)\n",
                c, m, n, n_events, row$PHASE_III_logic))
    next
  }
  
  # ── Schoenfeld (DIFFERENT from CANARY) ──
  zph <- tryCatch(cox.zph(cox_fit), error = function(e) NULL)
  
  if (is.null(zph)) {
    results <- rbind(results, data.frame(
      Stratum = paste(c, m, sep = "_"), Cancer = c, Endpoint = m, df = d,
      N = n, N_events = n_events,
      Cox_Converged = TRUE, Schoenfeld_Global_p = NA,
      Schoenfeld_Global_chisq = NA, Significant_Covariates = NA,
      CANARY_Status = trimws(as.character(row$PHASE_III_logic)),
      stringsAsFactors = FALSE))
    cat(sprintf("  %s_%s N=%d E=%d -> Converged, zph FAILED\n", c, m, n, n_events))
    next
  }
  
  global_p <- zph$table["GLOBAL", "p"]
  global_chisq <- zph$table["GLOBAL", "chisq"]
  sig_covs <- sum(zph$table[rownames(zph$table) != "GLOBAL", "p"] < 0.05, na.rm = TRUE)
  
  results <- rbind(results, data.frame(
    Stratum = paste(c, m, sep = "_"), Cancer = c, Endpoint = m, df = d,
    N = n, N_events = n_events,
    Cox_Converged = TRUE, Schoenfeld_Global_p = global_p,
    Schoenfeld_Global_chisq = global_chisq, Significant_Covariates = sig_covs,
    CANARY_Status = trimws(as.character(row$PHASE_III_logic)),
    stringsAsFactors = FALSE))
  
  cat(sprintf("  %s_%s N=%d E=%d p=%.4f (%d sig covs) CANARY=%s\n",
              c, m, n, n_events, global_p, sig_covs, row$PHASE_III_logic))
}

# ── Save ───────────────────────────────────────────────────────────────
fwrite(results, file.path(OUT_DIR, "Schoenfeld_120_Strata.tsv"), sep = "\t")

# ── Summary ────────────────────────────────────────────────────────────
cat(paste0("\n", paste(rep("=", 60), collapse = ""), "\n"))
cat("PH VIOLATION AUDIT — SUMMARY (Replicates CANARY data pipeline)\n")
cat(paste0(paste(rep("=", 60), collapse = ""), "\n"))
cat(sprintf("Total strata: %d\n", nrow(results)))

# By CANARY status
for (status in unique(results$CANARY_Status)) {
  sub <- results[results$CANARY_Status == status, ]
  cat(sprintf("\nCANARY: %s (n=%d)\n", status, nrow(sub)))
  cat(sprintf("  Non-convergence: %d (%.0f%%)\n", sum(!sub$Cox_Converged), 
              100*sum(!sub$Cox_Converged)/nrow(sub)))
  cv <- sub[sub$Cox_Converged & !is.na(sub$Schoenfeld_Global_p), ]
  if (nrow(cv) > 0) {
    cat(sprintf("  Schoenfeld p<0.05: %d (%.0f%% of converged)\n",
                sum(cv$Schoenfeld_Global_p < 0.05),
                100*sum(cv$Schoenfeld_Global_p < 0.05)/nrow(cv)))
    cat(sprintf("  Schoenfeld p<0.01: %d (%.0f%%)\n",
                sum(cv$Schoenfeld_Global_p < 0.01),
                100*sum(cv$Schoenfeld_Global_p < 0.01)/nrow(cv)))
  }
}

# Overall
cat(sprintf("\nOVERALL:\n"))
cv_all <- results[results$Cox_Converged & !is.na(results$Schoenfeld_Global_p), ]
total_violations <- sum(!results$Cox_Converged) + sum(cv_all$Schoenfeld_Global_p < 0.05)
cat(sprintf("  TOTAL PH VIOLATION: %d/%d (%.0f%%)\n",
            total_violations, nrow(results), 100*total_violations/nrow(results)))
if (nrow(cv_all) > 0) {
  cat(sprintf("  Median global p: %.4f\n", median(cv_all$Schoenfeld_Global_p)))
}

cat(sprintf("\nDone. Output: %s/\n", OUT_DIR))

# ── Generate Log-Log Plots (4 representative strata) ──────────────────
# Select 4 CANARY-passed strata with worst Schoenfeld p-values
worst_p <- results[results$Cox_Converged & results$CANARY_Status == "ELIGIBLE_NONCOX__GEOMETRY_MU_EXHAUSTED", ]
worst_p <- worst_p[worst_p$Schoenfeld_Global_p != "" & worst_p$Schoenfeld_Global_p != "FAILED" & worst_p$Schoenfeld_Global_p != "NaN", ]
worst_p <- worst_p[order(as.numeric(worst_p$Schoenfeld_Global_p)), ]
plot_strata <- worst_p$Stratum[1:min(4, nrow(worst_p))]

if (length(plot_strata) > 0) {
  # Build plot objects first
  plot_list <- list()
  for (sid in plot_strata) {
    parts <- strsplit(sid, "_")[[1]]; ct <- parts[1]; mt <- parts[2]
    row_match <- dfinput_raw[dfinput_raw$cancer_type == ct & dfinput_raw$metric == mt, ][1, ]
    d_var <- trimws(as.character(row_match$df_file))
    clean_dv <- ifelse(grepl("\\.rds$", d_var, ignore.case = TRUE), d_var, paste0(d_var, ".rds"))
    
    df_raw <- readRDS(file.path(DF_ROOT, clean_dv))
    df_cancer <- df_raw[df_raw$type == ct, , drop = FALSE]
    surv <- filter_survival_complete(df_cancer, mt)
    X <- build_combined_X_matrix(surv$df_masked, ct)
    
    na_props <- rowSums(is.na(X)) / ncol(X)
    keep_idx <- which(na_props < 0.35)
    df_cox <- data.frame(time = surv$time[keep_idx], status = surv$event[keep_idx])
    df_cox <- cbind(df_cox, as.data.frame(X[keep_idx, , drop = FALSE]))
    zero_var <- apply(df_cox[, -(1:2), drop = FALSE], 2, var, na.rm = TRUE) == 0
    zero_var[is.na(zero_var)] <- TRUE
    if (any(zero_var)) df_cox <- df_cox[, c(TRUE, TRUE, !zero_var)]
    df_cox <- na.omit(df_cox)
    
    cox_fit <- coxph(Surv(time, status) ~ ., data = df_cox)
    risk <- predict(cox_fit, type = "risk")
    risk_group <- cut(risk, breaks = quantile(risk, probs = c(0, 0.33, 0.66, 1), na.rm = TRUE),
                      labels = c("Low", "Medium", "High"), include.lowest = TRUE)
    df_plot <- data.frame(time = df_cox$time, status = df_cox$status, risk_group = risk_group)
    plot_list[[sid]] <- list(fit = survfit(Surv(time, status) ~ risk_group, data = df_plot), sid = sid)
  }
  
  # Output PDF
  pdf(file.path(OUT_DIR, "LogLog_Survival_Plots.pdf"), width = 14, height = 10)
  par(mfrow = c(2, 2), mar = c(5, 5, 4, 2))
  for (info in plot_list) {
    plot(info$fit, fun = "cloglog", col = c("blue", "orange", "red"), lwd = 2,
         main = info$sid, xlab = "log(Time)", ylab = "log(-log(Survival))", cex.main = 1.5, cex.lab = 1.3)
    legend("bottomright", c("Low", "Medium", "High"), col = c("blue", "orange", "red"), lwd = 2, cex = 1.2)
  }
  dev.off()
  
  # Output TIFF 600 dpi
  tiff(file.path(OUT_DIR, "LogLog_Survival_Plots.tiff"), width = 14, height = 10,
       units = "in", res = 600, compression = "lzw")
  par(mfrow = c(2, 2), mar = c(5, 5, 4, 2))
  for (info in plot_list) {
    plot(info$fit, fun = "cloglog", col = c("blue", "orange", "red"), lwd = 2,
         main = info$sid, xlab = "log(Time)", ylab = "log(-log(Survival))", cex.main = 1.5, cex.lab = 1.3)
    legend("bottomright", c("Low", "Medium", "High"), col = c("blue", "orange", "red"), lwd = 2, cex = 1.2)
  }
  dev.off()
  cat(sprintf("Log-log plots saved (%d strata): PDF + TIFF 600 dpi\n", length(plot_strata)))
}

# ── Generate Scaled Schoenfeld Plots (strata with valid zph) ──────────
zph_ok <- results[results$Cox_Converged & results$Schoenfeld_Global_p != "" & results$Schoenfeld_Global_p != "FAILED" & results$Schoenfeld_Global_p != "NaN", ]
if (nrow(zph_ok) > 0) {
  plot_subset <- zph_ok[order(zph_ok$Schoenfeld_Global_p), ][1:min(8, nrow(zph_ok)), ]
  
  pdf(file.path(OUT_DIR, "Scaled_Schoenfeld_Plots.pdf"), width = 16, height = 12)
  tiff(file.path(OUT_DIR, "Scaled_Schoenfeld_Plots.tiff"), width = 16, height = 12,
       units = "in", res = 600, compression = "lzw")
  
  for (i in 1:nrow(plot_subset)) {
    sid <- plot_subset$Stratum[i]
    parts <- strsplit(sid, "_")[[1]]
    ct <- parts[1]; mt <- parts[2]
    row_match <- dfinput_raw[dfinput_raw$cancer_type == ct & dfinput_raw$metric == mt, ][1, ]
    
    d_var <- trimws(as.character(row_match$df_file))
    clean_dv <- ifelse(grepl("\\.rds$", d_var, ignore.case = TRUE), d_var, paste0(d_var, ".rds"))
    df_raw <- readRDS(file.path(DF_ROOT, clean_dv))
    df_cancer <- df_raw[df_raw$type == ct, , drop = FALSE]
    surv <- filter_survival_complete(df_cancer, mt)
    X <- build_combined_X_matrix(surv$df_masked, ct)
    
    na_props <- rowSums(is.na(X)) / ncol(X)
    keep_idx <- which(na_props < 0.35)
    X_clean <- X[keep_idx, , drop = FALSE]
    y_time <- surv$time[keep_idx]
    y_event <- surv$event[keep_idx]
    
    df_cox <- data.frame(time = y_time, status = y_event)
    df_cox <- cbind(df_cox, as.data.frame(X_clean))
    zero_var <- apply(df_cox[, -(1:2), drop = FALSE], 2, var, na.rm = TRUE) == 0
    zero_var[is.na(zero_var)] <- TRUE
    if (any(zero_var)) df_cox <- df_cox[, c(TRUE, TRUE, !zero_var)]
    
    df_cox <- na.omit(df_cox)
    cox_fit <- coxph(Surv(time, status) ~ ., data = df_cox)
    zph <- tryCatch(cox.zph(cox_fit), error = function(e) NULL)
    if (is.null(zph)) next
    
    # Exclude GLOBAL row, then pick top 4 covariates by chi-sq
    cov_rows <- zph$table[rownames(zph$table) != "GLOBAL", , drop = FALSE]
    if (nrow(cov_rows) == 0) next
    top_idx <- order(cov_rows[, "chisq"], decreasing = TRUE)[1:min(4, nrow(cov_rows))]
    # Map back to original zph table indices (which include GLOBAL)
    cov_names <- rownames(cov_rows)[top_idx]
    plot(zph, var = cov_names)
    title(main = paste(sid, sprintf("(Global p = %.4f)", zph$table["GLOBAL", "p"])),
          cex.main = 1.5)
  }
  dev.off()
  dev.off()
  cat(sprintf("Scaled Schoenfeld plots saved (%d strata): PDF + TIFF 600 dpi\n", nrow(plot_subset)))
}

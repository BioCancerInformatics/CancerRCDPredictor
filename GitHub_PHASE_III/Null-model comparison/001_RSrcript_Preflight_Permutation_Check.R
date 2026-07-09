###############################################################################
# R1.2 — Pre-flight Check: Verify permutation test requirements on ZIMA
# ============================================================================
# Checks: df matrices, model bundles, XGBoost feature alignment, patient IDs
###############################################################################

library(survival)
library(xgboost)

setwd("~/students/aluno0549-6/PHASE_III/PHASE_III_Megarun_5_0_complete_ZIMA_Suit_Generator_final")
MODELS_DIR <- "PHASE_III_ML_Models"
DF_DIR     <- "~/students/aluno0549-6"

# ── 1. Check all strata have bundles and MVL files ───────────────────────────
cat("========================================\n")
cat("1. Model bundles & MVL files\n")
cat("========================================\n")
strata <- list.dirs(MODELS_DIR, full.names = FALSE, recursive = FALSE)
strata <- strata[grepl("_df[0-9]+$", strata)]
cat(sprintf("Found %d strata.\n", length(strata)))

missing_bundle <- 0
missing_probs  <- 0
for (s in strata) {
  if (!file.exists(file.path(MODELS_DIR, s, paste0("model_bundle_", s, ".rds"))))
    missing_bundle <- missing_bundle + 1
  if (!file.exists(file.path(MODELS_DIR, s, "MVL_Synthesis", 
                             paste0(s, "_MVL_Synthesis_Patient_Probabilities.tsv"))))
    missing_probs <- missing_probs + 1
}
cat(sprintf("Missing bundles: %d\n", missing_bundle))
cat(sprintf("Missing MVL files: %d\n", missing_probs))

# ── 2. Check dfXXX files ─────────────────────────────────────────────────────
cat("\n========================================\n")
cat("2. dfXXX training matrices\n")
cat("========================================\n")
df_variants <- unique(gsub(".*_(df[0-9]+)$", "\\1", strata))
cat(sprintf("Unique df variants needed: %s\n", paste(df_variants, collapse=", ")))

missing_df <- 0
df_sizes <- list()
for (dfv in df_variants) {
  path <- file.path(DF_DIR, paste0(dfv, ".rds"))
  if (!file.exists(path)) path <- file.path(DF_DIR, "dfXXX_series", paste0(dfv, ".rds"))
  if (file.exists(path)) {
    sz <- file.size(path)
    df_sizes[[dfv]] <- sz
    cat(sprintf("  %s: %s\n", dfv, format(sz, big.mark=",")))
  } else {
    cat(sprintf("  %s: MISSING\n", dfv))
    missing_df <- missing_df + 1
  }
}
cat(sprintf("Missing df files: %d\n", missing_df))

# ── 3. Test XGBoost predict on one exemplar ──────────────────────────────────
cat("\n========================================\n")
cat("3. XGBoost prediction test (READ_OS_df160)\n")
cat("========================================\n")

test_cohort <- "READ_OS_df160"
b <- readRDS(file.path(MODELS_DIR, test_cohort, paste0("model_bundle_", test_cohort, ".rds")))
feat_names <- b$XGBoost$feature_names
cat(sprintf("XGBoost features: %d\n", length(feat_names)))

# Load df160
df160 <- readRDS(file.path(DF_DIR, "df160.rds"))
cat(sprintf("df160: %d rows x %d cols\n", nrow(df160), ncol(df160)))

# Subset to READ
sub <- df160[df160$type == "READ", ]
cat(sprintf("READ subset: %d rows\n", nrow(sub)))

# Find prediction columns
pred_cols <- intersect(feat_names, names(sub))
cat(sprintf("Feature match: %d/%d\n", length(pred_cols), length(feat_names)))
missing_feats <- setdiff(feat_names, names(sub))
if (length(missing_feats) > 0) {
  cat(sprintf("Missing features: %d (e.g., %s)\n", 
              length(missing_feats), paste(head(missing_feats, 3), collapse=", ")))
}

# Build matrix and predict
if (length(pred_cols) >= 2) {
  sub$Patient_ID <- gsub("-[0-9]{2}[A-Z]?$", "", sub$patient)
  
  # Get survival and MVL for matching
  probs <- read.table(file.path(MODELS_DIR, test_cohort, "MVL_Synthesis",
                               paste0(test_cohort, "_MVL_Synthesis_Patient_Probabilities.tsv")),
                      header = TRUE, sep = "\t")
  surv <- as.data.frame(b$RSF$yvar)
  surv$Patient_ID <- gsub("-[0-9]{2}[A-Z]?$", "", rownames(surv))
  
  m <- merge(probs[, c("Patient_ID", "Event_Prob_1Yr")],
             surv[, c("Patient_ID", "time", "status")],
             by = "Patient_ID")
  
  # Match prediction rows to merged patients
  sub_m <- sub[sub$Patient_ID %in% m$Patient_ID, pred_cols, drop = FALSE]
  sub_m <- sub_m[match(m$Patient_ID, sub$Patient_ID[sub$Patient_ID %in% m$Patient_ID]), , drop = FALSE]
  
  cat(sprintf("Prediction matrix: %d rows x %d cols\n", nrow(sub_m), ncol(sub_m)))
  
  xgb_risk <- tryCatch(
    as.numeric(predict(b$XGBoost, newdata = as.matrix(sub_m))),
    error = function(e) { cat(sprintf("PREDICT FAILED: %s\n", e$message)); NULL }
  )
  
  if (!is.null(xgb_risk)) {
    cat(sprintf("XGBoost risk scores: %d values, range [%.4f, %.4f]\n", 
                length(xgb_risk), min(xgb_risk), max(xgb_risk)))
    c_xgb <- concordance(Surv(time, status) ~ xgb_risk, data = m, reverse = TRUE)
    cat(sprintf("XGBoost C-index: %.4f (Table S10: 0.999)\n", c_xgb$concordance))
  }
}

# ── 4. Summary ───────────────────────────────────────────────────────────────
cat("\n========================================\n")
cat("4. READINESS\n")
cat("========================================\n")
issues <- missing_bundle + missing_probs + missing_df
if (issues == 0) {
  cat("All checks passed. Ready to run permutation_test.R\n")
} else {
  cat(sprintf("%d issue(s) found.\n", issues))
}

# ==============================================================================
# AUDIT SCRIPT: Megarun 5.0 & ZIMA Cube Generator Completeness Check
# ==============================================================================
# Run this on your workstation (ZIMA Cube) inside the PHASE_III directory.

WORKING_DIR <- "~/students/aluno0549-6/PHASE_III"
MODELS_DIR <- file.path(WORKING_DIR, "PHASE_III_ML_Models")
MASTER_ZIMA <- file.path(WORKING_DIR, "Master_ZIMA_Mathematical_Interaction_Proof_Matrix.csv")

# 1. Check if the Models Directory exists
if(!dir.exists(MODELS_DIR)) {
  stop(sprintf("ERROR: Models directory '%s' not found.", MODELS_DIR))
}

cohort_dirs <- list.dirs(MODELS_DIR, recursive = FALSE)
cat(sprintf("Found %d Cohort Directories.\n", length(cohort_dirs)))

audit_results <- data.frame(
  Cohort = character(),
  Megarun_Bundle_Exists = logical(),
  Megarun_SHAP_Summary_Exists = logical(),
  ZIMA_Interaction_Matrix_Exists = logical(),
  ZIMA_Interactions_Count = numeric(),
  stringsAsFactors = FALSE
)

total_zima_interactions <- 0

for (cdir in cohort_dirs) {
  cohort_name <- basename(cdir)
  
  # Megarun 5.0 Checks
  bundle_path <- file.path(cdir, paste0("model_bundle_", cohort_name, ".rds"))
  shap_csv <- file.path(cdir, "XGBoost", paste0(cohort_name, "_XGBoost_SHAP_Summary.tsv"))
  
  has_bundle <- file.exists(bundle_path)
  has_shap <- file.exists(shap_csv)
  
  # ZIMA Cube Checks
  zima_matrix_path <- file.path(cdir, "ZIMA_Exhaustive_SHAP_C_Suite", paste0(cohort_name, "_Exhaustive_Interaction_Matrix.csv"))
  has_zima_matrix <- file.exists(zima_matrix_path)
  
  zima_count <- 0
  if (has_zima_matrix) {
    zima_df <- tryCatch(read.csv(zima_matrix_path, stringsAsFactors = FALSE), error = function(e) NULL)
    if (!is.null(zima_df)) {
      zima_count <- nrow(zima_df)
      total_zima_interactions <- total_zima_interactions + zima_count
    }
  }
  
  audit_results <- rbind(audit_results, data.frame(
    Cohort = cohort_name,
    Megarun_Bundle_Exists = has_bundle,
    Megarun_SHAP_Summary_Exists = has_shap,
    ZIMA_Interaction_Matrix_Exists = has_zima_matrix,
    ZIMA_Interactions_Count = zima_count,
    stringsAsFactors = FALSE
  ))
}

# Summary
cat("\n============================================================\n")
cat("AUDIT SUMMARY:\n")
cat("============================================================\n")
cat(sprintf("Total Cohorts Evaluated: %d\n", nrow(audit_results)))

megarun_complete <- sum(audit_results$Megarun_Bundle_Exists & audit_results$Megarun_SHAP_Summary_Exists)
cat(sprintf("Cohorts with Complete Megarun 5.0 Outputs (Bundle + SHAP): %d / %d\n", megarun_complete, nrow(audit_results)))

zima_complete <- sum(audit_results$ZIMA_Interaction_Matrix_Exists)
cat(sprintf("Cohorts with ZIMA Interaction Matrices: %d / %d\n", zima_complete, nrow(audit_results)))
cat(sprintf("Total ZIMA Interactions (Sum of individual matrices): %d\n", total_zima_interactions))

if (file.exists(MASTER_ZIMA)) {
  master_df <- read.csv(MASTER_ZIMA, stringsAsFactors = FALSE)
  cat(sprintf("Global Master ZIMA Matrix exists with %d rows.\n", nrow(master_df)))
  if (nrow(master_df) == total_zima_interactions) {
    cat("✅ SUCCESS: Global Master Matrix row count matches the sum of individual matrices!\n")
  } else {
    cat("❌ WARNING: Global Master Matrix row count DOES NOT MATCH individual matrices sum!\n")
  }
} else {
  cat("❌ WARNING: Global Master ZIMA Matrix NOT FOUND in root directory.\n")
}

# Identify Missing Cohorts
missing_megarun <- audit_results[!audit_results$Megarun_Bundle_Exists | !audit_results$Megarun_SHAP_Summary_Exists, "Cohort"]
if (length(missing_megarun) > 0) {
  cat("\nCohorts missing Megarun 5.0 files:\n")
  print(missing_megarun)
}

missing_zima <- audit_results[!audit_results$ZIMA_Interaction_Matrix_Exists, "Cohort"]
if (length(missing_zima) > 0) {
  cat("\nCohorts missing ZIMA Interaction Matrices (Note: some may intentionally have 0 active features):\n")
  print(missing_zima)
}

write.csv(audit_results, "Megarun5_ZIMA_Audit_Report.csv", row.names = FALSE)
cat("\nDetailed audit report saved to 'Megarun5_ZIMA_Audit_Report.csv'\n")

# AUTONOMOUS AUDIT SCRIPT: audit_v2_strict.R
# =========================================================================
message("🚀 Launching AUDIT SCRIPT: audit_v2_strict.R autonomously...")
source("audit_v2_strict.R")
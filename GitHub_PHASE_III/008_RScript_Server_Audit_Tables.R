# ==============================================================================
# Script: Generate Table SXY (NA Panorama) and SXZ (Penetrance)
# Execution Environment: ZIMA Server
# ==============================================================================

setwd("~/students/aluno0549-6/PHASE_III/PHASE_III_Megarun_5_0_complete_ZIMA_Suit_Generator_final/ML_Internal_validation_dataset/Blind_Predictions")

if(!requireNamespace("writexl", quietly=TRUE)) {
  install.packages("writexl", repos="http://cran.us.r-project.org")
}

pred_dir <- getwd()
parent_dir <- dirname(pred_dir)

# 1. Load Pristine Lineage Ledger
ledger_path <- file.path(parent_dir, "predictive_variables_distribution.rds")
if(!file.exists(ledger_path)) {
    stop("CRITICAL ERROR: Pristine Lineage Ledger (predictive_variables_distribution.rds) not found. Lineage unbroken proof impossible.")
}
pristine_ledger <- readRDS(ledger_path)

files <- list.files(pred_dir, pattern="*Blind_Predictions.tsv$", full.names=TRUE)

results_sxy <- data.frame()
results_sxz <- data.frame()

# Single robust loop to parse files for BOTH tables simultaneously
for(f in files) {
  df <- read.delim(f, stringsAsFactors=FALSE)
  if(nrow(df) > 0) {
    # Parse identifiers
    cohort_metric <- gsub("_Blind_Predictions.tsv", "", basename(f))
    parts <- strsplit(cohort_metric, "_")[[1]]
    cohort <- parts[1]
    
    total_processed <- nrow(df)
    
    # ---------------------------
    # Logic for Table SXY (NA Panorama)
    # ---------------------------
    results_sxy <- rbind(results_sxy, data.frame(
      Cohort = cohort,
      Model_Identifier = cohort_metric,
      Total_Validation_Patients = total_processed,
      RSF_NAs = sum(is.na(df$RSF_Risk)),
      XGBoost_NAs = sum(is.na(df$XGBoost_Risk)),
      Boruta_NAs = sum(is.na(df$Boruta_Risk)),
      MTLR_NAs = sum(is.na(df$MTLR_Risk)),
      SuperLearner_NAs = sum(is.na(df$SuperLearner_Risk)),
      stringsAsFactors=FALSE
    ))
    
    # ---------------------------
    # Logic for Table SXZ (Algorithmic Penetrance - Dual Track)
    # ---------------------------
    # Look up pristine N from ledger
    ledger_match <- pristine_ledger[pristine_ledger$Cancer == cohort, ]
    pristine_n <- ifelse(nrow(ledger_match) > 0, ledger_match$Samples[1], NA)
    
    # Path A: Intact records successfully scored by the full SuperLearner ensemble
    path_A_n <- sum(!is.na(df$SuperLearner_Risk))
    
    # Path B: Fragmented records safely masked by SuperLearner but successfully scored by Native XGBoost
    path_B_n <- sum(is.na(df$SuperLearner_Risk) & !is.na(df$XGBoost_Risk))
    
    # Complete Pipeline Failure (Both tracks failed to generate a score)
    failed_n <- sum(is.na(df$SuperLearner_Risk) & is.na(df$XGBoost_Risk))
    
    # The true penetrance is the sum of both successful paths!
    total_successful_n <- path_A_n + path_B_n
    penetrance_rate <- ifelse(total_processed > 0, round((total_successful_n / total_processed) * 100, 2), 0)
    
    lineage_intact <- (total_processed == pristine_n)
    
    results_sxz <- rbind(results_sxz, data.frame(
      Cohort = cohort,
      Endpoint_Model = cohort_metric,
      Pristine_Lineage_N = pristine_n,
      Processed_N = total_processed,
      Lineage_Intact = lineage_intact,
      SuperLearner_Path_A_N = path_A_n,
      XGBoost_Fallback_Path_B_N = path_B_n,
      Pipeline_Failed_N = failed_n,
      Dual_Track_Penetrance_Percent = penetrance_rate,
      stringsAsFactors=FALSE
    ))
  }
}

# ==========================================
# Finalize and Save Table SXY (NA Panorama)
# ==========================================
results_sxy <- results_sxy[order(results_sxy$Cohort, results_sxy$Model_Identifier), ]
output_path_sxy <- file.path(parent_dir, "Table_SXY_Algorithmic_NA_Panorama.xlsx")
writexl::write_xlsx(results_sxy, output_path_sxy)

# ==========================================
# Finalize and Save Table SXZ (Penetrance)
# ==========================================
results_sxz <- results_sxz[order(results_sxz$Cohort, results_sxz$Endpoint_Model), ]
output_path_sxz <- file.path(parent_dir, "Table_SXZ_Algorithmic_Penetrance.xlsx")
writexl::write_xlsx(results_sxz, output_path_sxz)

# Print Final Confirmations
cat("\n======================================================\n")
cat("          ZIMA AUDIT & PENETRANCE COMPLETE            \n")
cat("======================================================\n")

if(nrow(results_sxz) > 0 && all(results_sxz$Lineage_Intact, na.rm=TRUE)) {
    cat("[SUCCESS] Lineage Unbroken! All processed patient counts perfectly match the Pristine Ledger.\n")
} else {
    cat("[WARNING] Lineage Discrepancy Detected! Some processed counts do not match the Pristine Ledger.\n")
}

cat("\nPanorama Table SXY (NA Counts) saved at:\n ->", output_path_sxy, "\n")
cat("Penetrance Table SXZ (Success Rates) saved at:\n ->", output_path_sxz, "\n\n")

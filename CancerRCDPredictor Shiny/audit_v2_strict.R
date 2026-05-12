# ==============================================================================
# AUDIT SCRIPT v2: Megarun 5.0 (Patient Trajectories) & ZIMA Strict Classification
# ==============================================================================
# Fixed: Removed dependency on xgboost's 'getinfo()' function.
# Added: Master Decoder Merge to reconstruct the fully elaborated Table S15!
# ==============================================================================

if(!requireNamespace("dplyr", quietly=TRUE)) install.packages("dplyr")
if(!requireNamespace("xgboost", quietly=TRUE)) install.packages("xgboost")
if(!requireNamespace("openxlsx", quietly=TRUE)) install.packages("openxlsx")
library(dplyr)
library(xgboost)

WORKING_DIR <- "~/students/aluno0549-6/PHASE_III/PHASE_III_Megarun_5_0_complete_ZIMA_Suit_Generator_final"
MODELS_DIR <- file.path(WORKING_DIR, "PHASE_III_ML_Models")
MASTER_ZIMA <- file.path(WORKING_DIR, "Master_ZIMA_Mathematical_Interaction_Proof_Matrix.csv")
DECODER_PATH <- file.path(WORKING_DIR, "Dataset_S1_df1157_Unified_Baseline_Decoder.csv")

if(!dir.exists(MODELS_DIR)) {
  stop(sprintf("ERROR: Models directory '%s' not found.", MODELS_DIR))
}

cohort_dirs <- list.dirs(MODELS_DIR, recursive = FALSE)

audit_results <- data.frame(
  Cohort = character(),
  Megarun_Bundle_Exists = logical(),
  Expected_Patients = numeric(),
  PDF_Trajectories_Generated = numeric(),
  TIFF_Trajectories_Generated = numeric(),
  Trajectories_Complete = logical(),
  ZIMA_Matrix_Exists = logical(),
  ZIMA_Raw_Count = numeric(),
  stringsAsFactors = FALSE
)

total_pdf_trajectories <- 0
total_tiff_trajectories <- 0

for (cdir in cohort_dirs) {
  cohort_name <- basename(cdir)
  
  # 1. Trajectory Exhaustive Audit
  bundle_path <- file.path(cdir, paste0("model_bundle_", cohort_name, ".rds"))
  has_bundle <- file.exists(bundle_path)
  
  expected_n <- NA
  pdf_count <- 0
  tiff_count <- 0
  
  if (has_bundle) {
    bundle <- tryCatch(readRDS(bundle_path), error=function(e) NULL)
    if (!is.null(bundle) && !is.null(bundle$RSF$xvar)) {
      # RSF and XGBoost trained on the exact same mathematically identical X_mat inside the Megarun 5.0 loop.
      # We extract the number of patients (rows) from the preserved matrix geometry.
      expected_n <- nrow(bundle$RSF$xvar)
    }
  }
  
  xgboost_dir <- file.path(cdir, "XGBoost")
  if (dir.exists(xgboost_dir)) {
    # Specifically audit the graphical outputs (PDF and TIFF)
    pdf_files <- list.files(xgboost_dir, pattern = "_SHAP_Decision_.*\\.pdf$")
    tiff_files <- list.files(xgboost_dir, pattern = "_SHAP_Decision_.*\\.tiff$")
    
    pdf_count <- length(pdf_files)
    tiff_count <- length(tiff_files)
    
    total_pdf_trajectories <- total_pdf_trajectories + pdf_count
    total_tiff_trajectories <- total_tiff_trajectories + tiff_count
  }
  
  traj_complete <- !is.na(expected_n) && (pdf_count >= expected_n) && (tiff_count >= expected_n)
  
  # 2. ZIMA Audit
  zima_matrix_path <- file.path(cdir, "ZIMA_Exhaustive_SHAP_C_Suite", paste0(cohort_name, "_Exhaustive_Interaction_Matrix.csv"))
  has_zima_matrix <- file.exists(zima_matrix_path)
  zima_count <- 0
  if (has_zima_matrix) {
    zima_df <- tryCatch(read.csv(zima_matrix_path, stringsAsFactors = FALSE), error = function(e) NULL)
    if (!is.null(zima_df)) zima_count <- nrow(zima_df)
  }
  
  audit_results <- rbind(audit_results, data.frame(
    Cohort = cohort_name,
    Megarun_Bundle_Exists = has_bundle,
    Expected_Patients = expected_n,
    PDF_Trajectories_Generated = pdf_count,
    TIFF_Trajectories_Generated = tiff_count,
    Trajectories_Complete = traj_complete,
    ZIMA_Matrix_Exists = has_zima_matrix,
    ZIMA_Raw_Count = zima_count,
    stringsAsFactors = FALSE
  ))
}

write.csv(audit_results, file.path(WORKING_DIR, "Megarun5_Trajectory_Audit_Report.csv"), row.names = FALSE)

# ==============================================================================
# 3. ZIMA STRICT MATHEMATICAL RE-CLASSIFICATION (Manuscript-Grade)
# ==============================================================================
if (file.exists(MASTER_ZIMA)) {
  df <- read.csv(MASTER_ZIMA, stringsAsFactors = FALSE)
  
  # Step 1: Calculate Benjamini-Hochberg False Discovery Rate (FDR)
  df$FDR_HighZone <- p.adjust(df$PValue_HighZone, method = "BH")
  df$FDR_MidZone <- p.adjust(df$PValue_MidZone, method = "BH")
  
  # Step 2: Apply the rigorous statistical logic (Table S15 architecture)
  # We OVERWRITE the legacy Mathematical_Classification column to avoid confusion
  df$Mathematical_Classification <- "NOT SIGNIFICANT"
  
  for (i in 1:nrow(df)) {
    rho_high <- df$Spearman_HighZone_CrossTalk[i]
    fdr_high <- df$FDR_HighZone[i]
    rho_mid <- df$Spearman_MidZone_CrossTalk[i]
    fdr_mid <- df$FDR_MidZone[i]
    
    if(is.na(rho_high)) rho_high <- 0
    if(is.na(fdr_high)) fdr_high <- 1
    if(is.na(rho_mid)) rho_mid <- 0
    if(is.na(fdr_mid)) fdr_mid <- 1
    
    if (rho_high >= 0.30 && fdr_high < 0.01) {
      df$Mathematical_Classification[i] <- "SYNERGY (Hazard Amplification)"
    } else if (rho_high <= -0.30 && fdr_high < 0.01) {
      df$Mathematical_Classification[i] <- "ANTAGONISM (Rescue/Protective Effect)"
    } else if (abs(rho_mid) >= 0.30 && fdr_mid < 0.01) {
      df$Mathematical_Classification[i] <- "CONTEXT-DEPENDENT BIFURCATION (Three-Tiered)"
    }
  }
  
  # Remove any redundant Strict_Classification column if it exists
  df$Strict_Classification <- NULL
  
  # ==============================================================================
  # 4. MASTER DECODER MERGE (Re-create Table S15 elaborations)
  # ==============================================================================
  if (file.exists(DECODER_PATH)) {
    cat("Integrating Master Decoder to append Genetic Elements...\n")
    decoder <- read.csv(DECODER_PATH, stringsAsFactors = FALSE)
    
    # We need Nomenclature, Signature, and Decoded_Genetic_Element
    decoder_sub <- decoder %>% select(Nomenclature, Signature, Decoded_Genetic_Element) %>% distinct()
    
    # Merge for Primary Signature
    df <- df %>% 
      left_join(decoder_sub, by = c("Primary_Signature" = "Nomenclature")) %>%
      rename(Decoded_Genetic_Element_Primary = Decoded_Genetic_Element,
             Signature_Primary = Signature)
    
    # Merge for Partner Signature
    df <- df %>% 
      left_join(decoder_sub, by = c("color_var_Partner" = "Nomenclature")) %>%
      rename(Decoded_Genetic_Element_Partner = Decoded_Genetic_Element,
             Signature_Partner = Signature)
      
    # Reorganize columns for clarity
    df <- df %>%
      relocate(Signature_Primary, .after = Primary_Signature) %>%
      relocate(Decoded_Genetic_Element_Primary, .after = Signature_Primary) %>%
      relocate(Signature_Partner, .after = color_var_Partner) %>%
      relocate(Decoded_Genetic_Element_Partner, .after = Signature_Partner)
  } else {
    cat("WARNING: Dataset_S1_df1157_Unified_Baseline_Decoder.csv not found. Skipping decode merge.\n")
  }
  
  # Export the final Table S15 Master
  write.csv(df, file.path(WORKING_DIR, "Table_S15_Master_ZIMA_Strict_Mathematical_Classification.csv"), row.names = FALSE)
  
  cat("Exporting Table S15 to Excel (.xlsx) format...\n")
  openxlsx::write.xlsx(df, file.path(WORKING_DIR, "Table_S15_Master_ZIMA_Strict_Mathematical_Classification.xlsx"), overwrite = TRUE)
  
  cat("\n============================================================\n")
  cat("ZIMA STRICT STATISTICAL SIGNIFICANCE AUDIT (|rho| >= 0.30, FDR < 0.01)\n")
  cat("============================================================\n")
  print(table(df$Mathematical_Classification))
}

cat("\n============================================================\n")
cat("TRAJECTORY GENERATION AUDIT\n")
cat("============================================================\n")
incomplete_traj <- sum(!audit_results$Trajectories_Complete, na.rm=TRUE)
cat(sprintf("Cohorts with ALL patient trajectories generated (Total PDFs: %d | Total TIFFs: %d): %d / %d\n", total_pdf_trajectories, total_tiff_trajectories, nrow(audit_results) - incomplete_traj, nrow(audit_results)))

if (incomplete_traj > 0) {
  cat("\nWARNING: The following cohorts are missing personalized trajectories:\n")
  print(audit_results[!audit_results$Trajectories_Complete, c("Cohort", "Expected_Patients", "PDF_Trajectories_Generated", "TIFF_Trajectories_Generated")])
} else {
  cat("✅ SUCCESS: Every single patient trajectory across all 96 cohorts was generated!\n")
}

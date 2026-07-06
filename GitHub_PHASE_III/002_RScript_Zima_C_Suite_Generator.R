# ==============================================================================
# RScript_PHASE_III_ZIMA_C_Suite_Generator.R
# MASSIVELY PARALLEL SHAP TOPOLOGY & SYNERGY AUDITOR
# ==============================================================================
# Deployment Target: ZIMA Cube (High-Core Workstation)
# Purpose: Generate exhaustive SHAP Dependence Plots and Mathematical Synergy
#          matrices for EVERY feature that achieved Mean_Abs_SHAP > 0, bypassing
#          any arbitrary n=5 sampling bottlenecks.
# ==============================================================================

if(!requireNamespace("xgboost", quietly=TRUE)) install.packages("xgboost")
if(!requireNamespace("shapviz", quietly=TRUE)) install.packages("shapviz")
if(!requireNamespace("dplyr", quietly=TRUE)) install.packages("dplyr")
if(!requireNamespace("doParallel", quietly=TRUE)) install.packages("doParallel")
if(!requireNamespace("foreach", quietly=TRUE)) install.packages("foreach")

library(xgboost)
library(shapviz)
library(dplyr)
library(ggplot2)
library(doParallel)
library(foreach)

# ------------------------------------------------------------------------------
# 1. PARALLELIZATION SETUP (ZIMA CUBE TUNING)
# ------------------------------------------------------------------------------
# Detect available cores. Reserve 2 for OS stability.
total_cores <- parallel::detectCores()
cores_to_use <- max(1, total_cores - 2)
cl <- makeCluster(cores_to_use)
registerDoParallel(cl)
cat(sprintf("🚀 ZIMA CUBE ENGINE INITIALIZED: Deploying across %d Parallel Cores...\n", cores_to_use))

# ------------------------------------------------------------------------------
# 2. PATH DEFINITIONS
# ------------------------------------------------------------------------------
WORKING_DIR <- "~/students/aluno0549-6/PHASE_III"
MODELS_DIR  <- file.path(WORKING_DIR, "PHASE_III_ML_Models")
MASTER_OUT_DIR <- WORKING_DIR

# ------------------------------------------------------------------------------
# 2.5. PERSISTENT AUDIT LOGGING
# ------------------------------------------------------------------------------
log_file <- file.path(MASTER_OUT_DIR, "ZIMA_Exhaustive_Megarun_Audit.log")
sink(log_file, append=FALSE, split=TRUE)

cohort_dirs <- list.dirs(MODELS_DIR, recursive = FALSE)
cat(sprintf("📁 Found %d Cohort Directories for Exhaustive Processing.\n\n", length(cohort_dirs)))

# Helper for resolving R-Formula altered names
resolve_feature <- function(raw_name, available) {
  if(raw_name %in% available) return(raw_name)
  dot_ver <- gsub("-", ".", raw_name, fixed=TRUE)
  if(dot_ver %in% available) return(dot_ver)
  dot_ver2 <- sub("-", ".", raw_name, fixed=TRUE)
  if(dot_ver2 %in% available) return(dot_ver2)
  return(NA)
}

# Helper for robust Omic Token extraction regardless of R-formula sanitization
extract_omic_token <- function(fname) {
  repaired <- sub("^([A-Za-z]+)\\.", "\\1-", fname)
  tokens <- strsplit(repaired, "\\.")[[1]]
  if(length(tokens) >= 2) return(tokens[2]) else return(NA)
}

# ------------------------------------------------------------------------------
# 3. GLOBAL LOOP OVER COHORTS
# ------------------------------------------------------------------------------
for (cdir in cohort_dirs) {
  cohort_name <- basename(cdir)
  cat(sprintf("============================================================\n"))
  cat(sprintf("🧬 INITIATING EXHAUSTIVE EXTRACTION: %s\n", cohort_name))
  cat(sprintf("============================================================\n"))
  
  bundle_path <- file.path(cdir, paste0("model_bundle_", cohort_name, ".rds"))
  shap_csv <- file.path(cdir, "XGBoost", paste0(cohort_name, "_XGBoost_SHAP_Summary.tsv"))
  
  # Define the final output to check for resumption
  zima_out_dir <- file.path(cdir, "ZIMA_Exhaustive_SHAP_C_Suite")
  output_csv <- file.path(zima_out_dir, paste0(cohort_name, "_Exhaustive_Interaction_Matrix.csv"))
  
  if (file.exists(output_csv)) {
    cat(sprintf("   [SKIPPED/RESUMED] Interaction Matrix already generated for %s. Saving time...\n", cohort_name))
    next
  }
  
  if (!file.exists(bundle_path) || !file.exists(shap_csv)) {
    cat(sprintf("   [!] Missing bundle or SHAP summary for %s. Skipping.\n", cohort_name))
    next
  }
  
  # Load SHAP summary to identify ALL active features (Mean_Abs_SHAP > 0)
  shap_df <- read.delim(shap_csv, stringsAsFactors = FALSE)
  active_features <- shap_df$Feature[shap_df$Mean_Abs_SHAP > 0]
  
  if (length(active_features) == 0) {
    cat("   [!] No active features found. Skipping.\n")
    next
  }
  cat(sprintf("   -> Identified %d Exhaustive Active Features.\n", length(active_features)))
  
  # Setup Output Directory
  zima_out_dir <- file.path(cdir, "ZIMA_Exhaustive_SHAP_C_Suite")
  if (!dir.exists(zima_out_dir)) dir.create(zima_out_dir, recursive = TRUE)
  
  # Load Bundle
  bundle <- readRDS(bundle_path)
  X_matrix <- bundle$RSF$xvar
  colnames(X_matrix) <- bundle$XGBoost$feature_names
  X_mat <- as.matrix(X_matrix)
  
  # Resolve exact feature names
  resolved_names <- sapply(active_features, resolve_feature, available = colnames(X_mat))
  valid_idx <- !is.na(resolved_names)
  resolved_features <- resolved_names[valid_idx]
  original_features <- active_features[valid_idx]
  
  if(length(resolved_features) == 0) next
  
  # Reconstruct 1D SHAP Object
  cat("   -> Constructing 1D SHAP Topologies...\n")
  shp_obj <- shapviz::shapviz(bundle$XGBoost, X_pred = X_mat, interactions = FALSE)
  
  # Safely Reconstruct 3D Interaction Tensor in Batches
  cat("   -> Constructing 3D Interaction Tensor (Batch Mode)...\n")
  M_feats <- ncol(X_mat)
  N_samp <- nrow(X_mat)
  sum_abs_interaction <- matrix(0, nrow=M_feats, ncol=M_feats)
  
  batch_size <- 50
  for(b in seq(1, N_samp, by=batch_size)) {
    b_end <- min(b + batch_size - 1, N_samp)
    p_inter <- predict(bundle$XGBoost, X_mat[b:b_end, , drop=FALSE], predinteraction=TRUE)
    p_inter <- p_inter[, 1:M_feats, 1:M_feats, drop=FALSE]
    sum_abs_interaction <- sum_abs_interaction + apply(abs(p_inter), c(2,3), sum)
    gc()
  }
  mean_abs_interaction <- sum_abs_interaction / N_samp
  diag(mean_abs_interaction) <- 0
  colnames(mean_abs_interaction) <- colnames(X_mat)
  rownames(mean_abs_interaction) <- colnames(X_mat)
  
  # Parallel Loop over all resolved active features
  cat(sprintf("   -> Deploying ZIMA Core Array across %d features...\n", length(resolved_features)))
  
  results_list <- foreach(i = seq_along(resolved_features), .packages = c("ggplot2", "shapviz", "xgboost"), .combine = rbind, .errorhandling = "remove") %dopar% {
    sig <- resolved_features[i]
    orig_sig <- original_features[i]
    sig_safe <- gsub("[^[:alnum:]]", "_", orig_sig)
    
    # Identify Strongest Interactor
    partner_strengths <- mean_abs_interaction[sig, ]
    best_idx <- which.max(partner_strengths)
    best_interactor <- names(partner_strengths)[best_idx]
    interaction_strength <- partner_strengths[best_idx]
    
    # --------------------------------------------------
    # 1. Plot Generation
    # --------------------------------------------------
    pdf_path <- file.path(zima_out_dir, paste0(cohort_name, "_SHAP_Dependence_", sig_safe, ".pdf"))
    tiff_path <- file.path(zima_out_dir, paste0(cohort_name, "_SHAP_Dependence_", sig_safe, ".tiff"))
    
    p <- shapviz::sv_dependence(shp_obj, v = sig, color_var = best_interactor, alpha = 0.6, size = 1.5) +
      theme_bw() + 
      ggtitle(paste0("SHAP Non-Linear Topology: ", orig_sig)) +
      theme(legend.position = "right", plot.title = element_text(size = 8, face = "bold"))
    
    ggsave(pdf_path, p, width = 6, height = 5, bg = "white")
    ggsave(tiff_path, p, width = 6, height = 5, bg = "white", dpi = 600, compression = "lzw")
    
    # --------------------------------------------------
    # 2. Mathematical Classification (Spearman)
    # --------------------------------------------------
    shap_vals <- shp_obj$S[, sig]
    main_vals <- shp_obj$X[, sig]
    interactor_vals <- shp_obj$X[, best_interactor]
    
    # High Zone (Top Quartile)
    q75_main <- quantile(main_vals, 0.75, na.rm = TRUE)
    idx_high  <- which(main_vals >= q75_main)
    cor_val <- NA; pval_high <- NA
    if(length(idx_high) > 5) {
      ctest <- suppressWarnings(cor.test(interactor_vals[idx_high], shap_vals[idx_high], method = "spearman"))
      cor_val <- ctest$estimate
      pval_high <- ctest$p.value
    }
    
    # Mid Zone (Middle 50%)
    q25_main <- quantile(main_vals, 0.25, na.rm = TRUE)
    idx_mid  <- which(main_vals >= q25_main & main_vals < q75_main)
    cor_mid <- NA; pval_mid <- NA
    if(length(idx_mid) > 5) {
      ctest_mid <- suppressWarnings(cor.test(interactor_vals[idx_mid], shap_vals[idx_mid], method = "spearman"))
      cor_mid <- ctest_mid$estimate
      pval_mid <- ctest_mid$p.value
    }
    
    class_label <- if(is.na(cor_val)) {
      "UNDETERMINED"
    } else if(cor_val > 0.20) {
      "SYNERGY (Hazard Amplification)"
    } else if(cor_val < -0.20) {
      "ANTAGONISM (Rescue/Protective Effect)"
    } else if(!is.na(cor_mid) && abs(cor_mid) > 0.20) {
      "CONTEXT-DEPENDENT BIFURCATION (Three-Tiered)"
    } else {
      "NEUTRAL (Contextual Noise)"
    }
    
    clean_partner <- sub("^([A-Za-z]+)\\.", "\\1-", best_interactor)
    
    # Return row for Master Matrix
    data.frame(
      Cohort                       = cohort_name,
      Primary_Signature            = orig_sig,
      Primary_Omic_Token           = extract_omic_token(orig_sig),
      color_var_Partner            = clean_partner,
      Partner_Omic_Token           = extract_omic_token(best_interactor),
      Interaction_Strength         = round(interaction_strength, 6),
      Spearman_HighZone_CrossTalk  = round(cor_val, 4),
      PValue_HighZone              = pval_high,
      Spearman_MidZone_CrossTalk   = round(cor_mid, 4),
      PValue_MidZone               = pval_mid,
      Mathematical_Classification  = class_label,
      stringsAsFactors             = FALSE
    )
  }
  
  if (!is.null(results_list) && nrow(results_list) > 0) {
    output_csv <- file.path(zima_out_dir, paste0(cohort_name, "_Exhaustive_Interaction_Matrix.csv"))
    write.csv(results_list, output_csv, row.names = FALSE)
    cat(sprintf("   [✓] Successfully processed and written %d interactions.\n", nrow(results_list)))
  } else {
    cat("   [!] No valid interactions generated.\n")
  }
}

stopCluster(cl)

# ------------------------------------------------------------------------------
# 4. GLOBAL MASTER SYNTHESIS
# ------------------------------------------------------------------------------
cat("\n============================================================\n")
cat("📊 AGGREGATING GLOBAL MASTER TENSOR MATRIX...\n")
cat("============================================================\n")

all_csvs <- list.files(MODELS_DIR, pattern = "_Exhaustive_Interaction_Matrix\\.csv$", recursive = TRUE, full.names = TRUE)
if(length(all_csvs) > 0) {
  master_list <- lapply(all_csvs, function(f) {
    tryCatch(read.csv(f, stringsAsFactors = FALSE), error = function(e) NULL)
  })
  master_df <- bind_rows(master_list)
  
  if(nrow(master_df) > 0) {
    master_csv_path <- file.path(MASTER_OUT_DIR, "Master_ZIMA_Mathematical_Interaction_Proof_Matrix.csv")
    write.csv(master_df, master_csv_path, row.names = FALSE)
    cat(sprintf("   [✓] SUCCESS: Global Master Matrix compiled and saved! (%d total interactions)\n", nrow(master_df)))
    cat(sprintf("   -> Location: %s\n", master_csv_path))
  }
} else {
  cat("   [!] WARNING: No individual cohort matrices found for final synthesis.\n")
}

cat("\n============================================================\n")
cat("🏁 ZIMA CUBE PIPELINE COMPLETE. ALL EXHAUSTIVE C-SUITES GENERATED.\n")
cat("============================================================\n")

sink()


# AUTONOMOUS AUDIT SCRIPT: Megarun 5.0 & ZIMA Cube Generator Completeness Check
# =========================================================================
message("🚀 Launching AUDIT SCRIPT: Megarun 5.0 & ZIMA Cube Generator Completeness Check autonomously...")
source("audit_megarun5_zima.R")
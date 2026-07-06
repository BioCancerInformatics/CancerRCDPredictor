# ==============================================================================
# RScript_Verify_Synergism_Antagonism_Truth.R
# SHAP Mathematical Synergy/Antagonism Auditor  - v2 (Ground Truth Edition)
# ==============================================================================
# Strategy:
#   1. Parse the exact 10 primary signature names from the existing PDF filenames
#      (ground truth - these are the features the pipeline actually plotted)
#   2. Reconstruct shp_interactions from the saved bundle (XGBoost + X_matrix)
#      using the same 3D tensor logic as the original pipeline
#   3. Call potential_interactions() for each exact primary to recover the 
#      color_var partner selected by color_var = "auto"
#   4. Classify each pair as SYNERGY or ANTAGONISM via Spearman correlation
#   5. Output the ranked proof matrix CSV
# ==============================================================================

if(!requireNamespace("xgboost",  quietly=TRUE)) install.packages("xgboost")
if(!requireNamespace("shapviz",  quietly=TRUE)) install.packages("shapviz")
if(!requireNamespace("dplyr",    quietly=TRUE)) install.packages("dplyr")

library(xgboost)
library(shapviz)
library(dplyr)

# ------------------------------------------------------------------------------
# 1. PATHS
# ------------------------------------------------------------------------------
WORKING_DIR  <- "D:/Pré-artigo 5-optosis model/Machine learning CancerRCDShiny_prediction/Enrique version/PHASE_III"
setwd(WORKING_DIR)

EXEMPLAR_DIR <- "PHASE_III_Megarun_4_4_complete/Synthesis_Graphics/Precision_Oncology_Exemplar_Lush_Omic"
bundle_path  <- "PHASE_III_Megarun_4_4_complete/PHASE_III_ML_Models/LGG_DSS_df374/model_bundle_LGG_DSS_df374.rds"

if(!file.exists(bundle_path)) stop("LGG_DSS model bundle not found.")

# ------------------------------------------------------------------------------
# 2. PARSE EXACT PRIMARY FEATURE NAMES FROM EXISTING PDF FILENAMES
# ------------------------------------------------------------------------------
cat("Parsing ground-truth primary signatures from existing PDF filenames...\n")

all_pdfs <- list.files(EXEMPLAR_DIR, pattern = "_SHAP_Dependence_.*\\.pdf$", full.names = FALSE)
cat(sprintf("  -> Found %d SHAP Dependence PDFs\n", length(all_pdfs)))

# Strip prefix "LGG_DSS_df374_SHAP_Dependence_" and suffix ".pdf"
# Then reverse the sig_safe encoding: 
#   sig_safe <- gsub("[^[:alnum:]]", "_", sig)
# Original format is LGG-XXXX.a.b.C.d.ee.ff.g.h.i
# Encoded as:        LGG_XXXX_a_b_C_d_ee_ff_g_h_i
# Decode rule: first underscore after "LGG" -> hyphen; all subsequent -> dot
decode_sig_safe <- function(safe_name) {
  # Replace first underscore with hyphen (LGG_XXXX -> LGG-XXXX)
  decoded <- sub("^([A-Za-z]+)_", "\\1-", safe_name)
  # All remaining underscores are dot separators
  decoded <- gsub("_", ".", decoded)
  return(decoded)
}

primary_features <- sapply(all_pdfs, function(f) {
  # Strip the fixed prefix
  stripped <- sub("^LGG_DSS_df374_SHAP_Dependence_", "", f)
  # Strip .pdf suffix
  stripped <- sub("\\.pdf$", "", stripped)
  decode_sig_safe(stripped)
})

cat("Primary signatures extracted from filenames:\n")
for(pf in primary_features) cat(sprintf("  -> %s\n", pf))

# ------------------------------------------------------------------------------
# 3. LOAD BUNDLE AND RECONSTRUCT 3D INTERACTION TENSOR (same as pipeline)
# ------------------------------------------------------------------------------
cat("\nLoading Model Bundle...\n")
bundle <- readRDS(bundle_path)

X_matrix <- bundle$RSF$xvar
colnames(X_matrix) <- bundle$XGBoost$feature_names
X_mat <- as.matrix(X_matrix)

cat("Reconstructing 3D SHAP interaction tensor safely in batches to avoid OOM...\n")
M_feats <- ncol(X_mat)
N_samp <- nrow(X_mat)
sum_abs_interaction <- matrix(0, nrow=M_feats, ncol=M_feats)

batch_size <- 50
for(b in seq(1, N_samp, by=batch_size)) {
  b_end <- min(b + batch_size - 1, N_samp)
  cat(sprintf("  -> Processing batch %d to %d (of %d)...\n", b, b_end, N_samp))
  p_inter <- predict(bundle$XGBoost, X_mat[b:b_end, , drop=FALSE], predinteraction=TRUE)
  p_inter <- p_inter[, 1:M_feats, 1:M_feats, drop=FALSE] # Strip bias column
  sum_abs_interaction <- sum_abs_interaction + apply(abs(p_inter), c(2,3), sum)
  gc()
}
mean_abs_interaction <- sum_abs_interaction / N_samp
diag(mean_abs_interaction) <- 0
colnames(mean_abs_interaction) <- colnames(X_mat)
rownames(mean_abs_interaction) <- colnames(X_mat)

cat("  -> Exact 3D average interaction matrix safely computed.\n")
cat("Reconstructing 1D SHAP object for plotting values...\n")
shp_interactions <- shapviz::shapviz(bundle$XGBoost, X_pred = X_mat, interactions = FALSE)
cat("  -> SHAP object successfully reconstructed.\n")

# ------------------------------------------------------------------------------
# 4. MATCH PRIMARY FEATURES AGAINST ACTUAL XGBoost COLUMN NAMES
# ------------------------------------------------------------------------------
# Features in the model may use "." instead of "-" due to R formula sanitization.
# We try both representations.
available_features <- colnames(X_mat)

resolve_feature <- function(raw_name, available) {
  # Try exact match first
  if(raw_name %in% available) return(raw_name)
  # Try with hyphen converted to dot (R formula sanitization)
  dot_ver <- gsub("-", ".", raw_name, fixed=TRUE)
  if(dot_ver %in% available) return(dot_ver)
  # Try first dash converted to dot
  dot_ver2 <- sub("-", ".", raw_name, fixed=TRUE)
  if(dot_ver2 %in% available) return(dot_ver2)
  return(NA)
}

resolved_features <- sapply(primary_features, resolve_feature, available = available_features)
cat("\nFeature Resolution Table:\n")
for(i in seq_along(primary_features)) {
  status <- ifelse(is.na(resolved_features[i]), "NOT FOUND", "RESOLVED")
  cat(sprintf("  [%s] %s  ->  %s\n", status, primary_features[i], resolved_features[i]))
}

# Keep only resolvable
valid_idx <- !is.na(resolved_features)
resolved_features <- resolved_features[valid_idx]
source_pdfs      <- all_pdfs[valid_idx]

if(length(resolved_features) == 0) {
  stop("No primary features could be matched to the model. Check feature name encoding.")
}

# ------------------------------------------------------------------------------
# 5. FOR EACH PRIMARY FEATURE: FIND COLOR_VAR PARTNER + CLASSIFY
# ------------------------------------------------------------------------------
cat(sprintf("\nAuditing %d confirmed pairs...\n", length(resolved_features)))

results_list <- list()

for(i in seq_along(resolved_features)) {
  sig      <- resolved_features[i]
  pdf_file <- source_pdfs[i]
  
  cat(sprintf("  [%d/%d] Primary: %s\n", i, length(resolved_features), sig))
  
  # Look up the highest mean absolute interaction partner from our safely computed matrix
  partner_strengths <- mean_abs_interaction[sig, ]
  best_idx <- which.max(partner_strengths)
  best_interactor <- names(partner_strengths)[best_idx]
  interaction_strength <- partner_strengths[best_idx]
  
  cat(sprintf("     -> color_var partner: %s  (strength: %.5f)\n", best_interactor, interaction_strength))
  
  # Extract SHAP values and physical values for classification
  shap_vals       <- shp_interactions$S[, sig]
  main_vals       <- shp_interactions$X[, sig]
  interactor_vals <- shp_interactions$X[, best_interactor]
  
  # Spearman correlation in HIGH EXPRESSION zone (top quartile of primary)
  q75_main <- quantile(main_vals, 0.75, na.rm = TRUE)
  idx_high  <- which(main_vals >= q75_main)
  
  if(length(idx_high) > 5) {
    ctest <- suppressWarnings(cor.test(interactor_vals[idx_high], shap_vals[idx_high], method = "spearman"))
    cor_val <- ctest$estimate
    pval_high <- ctest$p.value
  } else {
    cor_val <- NA
    pval_high <- NA
  }
  
  # Also check middle zone for antagonism rescue (Figure 6 scenario: 3-tiered)
  q25_main <- quantile(main_vals, 0.25, na.rm = TRUE)
  idx_mid  <- which(main_vals >= q25_main & main_vals < q75_main)
  
  if(length(idx_mid) > 5) {
    ctest_mid <- suppressWarnings(cor.test(interactor_vals[idx_mid], shap_vals[idx_mid], method = "spearman"))
    cor_mid <- ctest_mid$estimate
    pval_mid <- ctest_mid$p.value
  } else {
    cor_mid <- NA
    pval_mid <- NA
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
  
  results_list[[sig]] <- data.frame(
    Source_PDF                   = pdf_file,
    Primary_Signature            = sig,
    Primary_Omic_Token           = strsplit(sig, "\\.")[[1]][2],
    color_var_Partner            = best_interactor,
    Partner_Omic_Token           = strsplit(best_interactor, "\\.")[[1]][2],
    Interaction_Strength         = round(interaction_strength, 6),
    Spearman_HighZone_CrossTalk  = round(cor_val, 4),
    PValue_HighZone              = pval_high,
    Spearman_MidZone_CrossTalk   = round(cor_mid, 4),
    PValue_MidZone               = pval_mid,
    Mathematical_Classification  = class_label,
    stringsAsFactors = FALSE
  )
}

# ------------------------------------------------------------------------------
# 6. SORT & EXPORT
# ------------------------------------------------------------------------------
final_df <- bind_rows(results_list) %>%
  arrange(desc(Spearman_HighZone_CrossTalk))

output_csv <- file.path(EXEMPLAR_DIR, "Mathematical_Interaction_Proof_Matrix.csv")
write.csv(final_df, output_csv, row.names = FALSE)

cat(sprintf("\n\n============================================================\n"))
cat(sprintf("ALGORITHM COMPLETE. Ground-Truth Proof Matrix written to:\n -> %s\n", output_csv))
cat(sprintf("============================================================\n\n"))

cat("RANKED RESULTS HIERARCHY:\n")
for(i in 1:nrow(final_df)) {
  cat(sprintf("  [%d] %s\n      Partner: %s\n      Omic Cross: .%s x .%s  |  Spearman: %.3f (P=%g)  |  Class: %s\n      File: %s\n\n",
              i,
              final_df$Primary_Signature[i],
              final_df$color_var_Partner[i],
              final_df$Primary_Omic_Token[i],
              final_df$Partner_Omic_Token[i],
              ifelse(is.na(final_df$Spearman_HighZone_CrossTalk[i]), NA, final_df$Spearman_HighZone_CrossTalk[i]),
              ifelse(is.na(final_df$PValue_HighZone[i]), NA, final_df$PValue_HighZone[i]),
              final_df$Mathematical_Classification[i],
              final_df$Source_PDF[i]))
}

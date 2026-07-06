# ==============================================================================
# RScript_ZIMA_Global_Ranker_and_Extractor.R
# ==============================================================================
# Purpose: 
# 1. Dynamically read the 30 GB output's Master ZIMA Proof Matrix.
# 2. Mathematically sweep and rank all 37,890 interaction pairs globally.
# 3. Append explicit mathematical ranking scores (Top Synergy, Top Antagonism).
# 4. Automatically identify the #1 Absolute Supreme Exemplars based purely on the data.
# 5. Dynamically extract their exact PDF and TIFF topographies.
# ==============================================================================

library(dplyr)

# ------------------------------------------------------------------------------
# 1. PATH CONFIGURATION (ADJUST THIS TO POINT TO THE 30GB FOLDER ON ZIMACUBE)
# ------------------------------------------------------------------------------
ZIMA_OCEAN_DIR <- "D:/Pré-artigo 5-optosis model/Machine learning CancerRCDShiny_prediction/Enrique version/PHASE_III/PHASE_III_Megarun_4_4_complete"
MASTER_MATRIX_FILE <- file.path(ZIMA_OCEAN_DIR, "Master_ZIMA_Mathematical_Interaction_Proof_Matrix.csv")
OUTPUT_DIR <- file.path(ZIMA_OCEAN_DIR, "Synthesis_Graphics", "ZIMA_Supreme_Exemplars")

if(!dir.exists(OUTPUT_DIR)) dir.create(OUTPUT_DIR, recursive = TRUE)

cat("============================================================\n")
cat("📊 INITIATING GLOBAL MATHEMATICAL ZIMA RANKING ENGINE\n")
cat("============================================================\n\n")

if(!file.exists(MASTER_MATRIX_FILE)) {
  stop("FATAL ERROR: Master_ZIMA_Mathematical_Interaction_Proof_Matrix.csv not found!")
}

# Read the global interaction matrix
df_zima <- read.csv(MASTER_MATRIX_FILE, stringsAsFactors = FALSE)

# ------------------------------------------------------------------------------
# 2. MATHEMATICAL RANKING (SYNERGY, ANTAGONISM, BIFURCATION)
# ------------------------------------------------------------------------------
cat("-> Sweeping and Ranking Global Synergies...\n")
df_synergy <- df_zima %>%
  filter(grepl("SYNERGY", Mathematical_Classification)) %>%
  # Rank by lowest FDR first, then highest positive correlation
  arrange(FDR_HighZone, desc(Spearman_HighZone_CrossTalk)) %>%
  mutate(Global_Rank = row_number())

cat("-> Sweeping and Ranking Global Antagonisms...\n")
df_antagonism <- df_zima %>%
  filter(grepl("ANTAGONISM", Mathematical_Classification)) %>%
  # Rank by lowest FDR first, then strongest negative correlation
  arrange(FDR_HighZone, Spearman_HighZone_CrossTalk) %>%
  mutate(Global_Rank = row_number())

cat("-> Sweeping and Ranking Global Bifurcations...\n")
df_bifurcation <- df_zima %>%
  filter(grepl("BIFURCATION", Mathematical_Classification)) %>%
  # Rank by lowest HighZone FDR, then highest absolute HighZone correlation
  arrange(FDR_HighZone, desc(abs(Spearman_HighZone_CrossTalk))) %>%
  mutate(Global_Rank = row_number())

# Recombine and save the explicitly ranked matrix
df_ranked_master <- bind_rows(
  df_synergy,
  df_antagonism,
  df_bifurcation,
  df_zima %>% filter(Mathematical_Classification == "NOT SIGNIFICANT") %>%
    mutate(Global_Rank = NA)
)

ranked_output_file <- file.path(OUTPUT_DIR, "Master_ZIMA_Ranked_Proof_Matrix.csv")
write.csv(df_ranked_master, ranked_output_file, row.names = FALSE)
cat(sprintf("   [✓] Globally Ranked Matrix Saved to: %s\n\n", ranked_output_file))

# ------------------------------------------------------------------------------
# 3. DYNAMIC SUPREME EXEMPLAR ISOLATION
# ------------------------------------------------------------------------------
# Extract the mathematically proven #1 Exemplars
top_synergy     <- df_synergy[1, ]
top_antagonism  <- df_antagonism[1, ]
top_bifurcation <- df_bifurcation[1, ]

cat("============================================================\n")
cat("👑 DYNAMIC MATHEMATICAL CROWNING OF SUPREME EXEMPLARS\n")
cat("============================================================\n")
cat(sprintf("-> #1 SYNERGY    | Cohort: %s | Primary: %s | FDR: %.2e | rho: %+.4f\n", 
            top_synergy$Cohort, top_synergy$Primary_Signature, top_synergy$FDR_HighZone, top_synergy$Spearman_HighZone_CrossTalk))
cat(sprintf("-> #1 ANTAGONISM | Cohort: %s | Primary: %s | FDR: %.2e | rho: %+.4f\n", 
            top_antagonism$Cohort, top_antagonism$Primary_Signature, top_antagonism$FDR_HighZone, top_antagonism$Spearman_HighZone_CrossTalk))
cat(sprintf("-> #1 BIFURCATION| Cohort: %s | Primary: %s | FDR: %.2e | rho: %+.4f\n", 
            top_bifurcation$Cohort, top_bifurcation$Primary_Signature, top_bifurcation$FDR_HighZone, top_bifurcation$Spearman_HighZone_CrossTalk))
cat("============================================================\n\n")

# ------------------------------------------------------------------------------
# 4. TOPOGRAPHY EXTRACTION
# ------------------------------------------------------------------------------
target_exemplars <- bind_rows(
  top_synergy %>% mutate(Classification = "Supreme_Synergy"),
  top_antagonism %>% mutate(Classification = "Supreme_Antagonism"),
  top_bifurcation %>% mutate(Classification = "Supreme_Bifurcation")
)

for(i in 1:nrow(target_exemplars)) {
  cohort <- target_exemplars$Cohort[i]
  feature <- target_exemplars$Primary_Signature[i]
  class_label <- target_exemplars$Classification[i]
  
  # Mimic ZIMA core naming
  sig_safe <- gsub("[^[:alnum:]]", "_", feature)
  
  zima_dir <- file.path(ZIMA_OCEAN_DIR, cohort, "ZIMA_Exhaustive_SHAP_C_Suite")
  
  pdf_source  <- file.path(zima_dir, paste0(cohort, "_SHAP_Dependence_", sig_safe, ".pdf"))
  tiff_source <- file.path(zima_dir, paste0(cohort, "_SHAP_Dependence_", sig_safe, ".tiff"))
  
  pdf_target  <- file.path(OUTPUT_DIR, paste0(class_label, "_", cohort, "_", sig_safe, ".pdf"))
  tiff_target <- file.path(OUTPUT_DIR, paste0(class_label, "_", cohort, "_", sig_safe, ".tiff"))
  
  cat(sprintf("-> Extracting %s topography from %s...\n", class_label, cohort))
  
  if(file.exists(pdf_source)) {
    file.copy(pdf_source, pdf_target, overwrite = TRUE)
    cat("   [✓] PDF Extracted.\n")
  } else {
    cat(sprintf("   [!] PDF Not Found at: %s\n", pdf_source))
  }
  
  if(file.exists(tiff_source)) {
    file.copy(tiff_source, tiff_target, overwrite = TRUE)
    cat("   [✓] TIFF Extracted.\n")
  } else {
    cat(sprintf("   [!] TIFF Not Found at: %s\n", tiff_source))
  }
  cat("\n")
}

cat("============================================================\n")
cat("🏁 PIPELINE COMPLETE. Check Synthesis_Graphics/ZIMA_Supreme_Exemplars.\n")
cat("============================================================\n")

# ==============================================================================
# CANCERRCDPREDICTOR PHASE IV: ZIMA DEPLOYMENT PRE-FLIGHT AUDIT
# ==============================================================================
# Run this script on the ZIMA Server BEFORE launching app.R to verify that 
# all structural dependencies, datasets, and images are correctly positioned.

ZIMA_ROOT <- "~/students/aluno0549-6/PHASE_III/PHASE_III_Megarun_5_0_complete_ZIMA_Suit_Generator_final"

cat("\n============================================================\n")
cat("🚀 COMMENCING PHASE IV PRE-FLIGHT AUDIT...\n")
cat("============================================================\n")

all_clear <- TRUE

check_exists <- function(path, type, name) {
  if (type == "dir" && dir.exists(path)) {
    cat(sprintf("[OK] Directory Found: %s\n", name))
    return(TRUE)
  } else if (type == "file" && file.exists(path)) {
    cat(sprintf("[OK] File Found: %s\n", name))
    return(TRUE)
  } else {
    cat(sprintf("[ERROR] Missing %s: %s\n      Expected at: %s\n", type, name, path))
    return(FALSE)
  }
}

# 1. Root Directory
if(!check_exists(ZIMA_ROOT, "dir", "ZIMA_ROOT")) all_clear <- FALSE

# 2. Master Table S15
if(!check_exists(file.path(ZIMA_ROOT, "Table_S15_Master_ZIMA_Strict_Mathematical_Classification.csv"), "file", "Master Matrix (Table S15)")) all_clear <- FALSE

# 3. Models Directory
if(!check_exists(file.path(ZIMA_ROOT, "PHASE_III_ML_Models"), "dir", "PHASE_III_ML_Models Folder")) all_clear <- FALSE

# 4. Figures Directory & Assets
figs_dir <- file.path(ZIMA_ROOT, "Figures")
if(!check_exists(figs_dir, "dir", "Figures Folder")) {
  all_clear <- FALSE
} else {
  required_figs <- c(
    "Figure_4_LGG_DSS_df374_SHAP_Overall_Beeswarm.tiff",
    "Figure_S8_Sup_READ_OS_df160_SHAP_Overall_Beeswarm.tiff",
    "Figure_8_Master_Composite_600DPI.tiff",
    "Figure_9_Native_Dual_TimeROC.tiff",
    "Figure_5_LGG_DSS_df374_SHAP_Decision_Lethal_Trajectory_TCGA-HT-7616-01.tiff",
    "Figure_6_LGG_DSS_df374_SHAP_Decision_Protective_Trajectory_TCGA-DU-7008-01.tiff"
  )
  for (fig in required_figs) {
    if(!check_exists(file.path(figs_dir, fig), "file", fig)) all_clear <- FALSE
  }
}

# 5. App Directory & Logo
# Assuming the app is placed in ZIMA_ROOT/PHASE_IV_CancerRCDShiny
app_dir <- file.path(ZIMA_ROOT, "PHASE_IV_CancerRCDShiny")
if(!check_exists(app_dir, "dir", "App Directory (PHASE_IV_CancerRCDShiny)")) {
  all_clear <- FALSE
} else {
  if(!check_exists(file.path(app_dir, "app.R"), "file", "app.R script")) all_clear <- FALSE
  if(!check_exists(file.path(app_dir, "www", "cancerrcdpredictor_logo_bloodorange.png"), "file", "App Logo (www folder)")) all_clear <- FALSE
}

cat("\n============================================================\n")
if (all_clear) {
  cat("✅ ALL SYSTEMS GO! The server architecture is perfect. You are cleared to launch the app!\n")
} else {
  cat("❌ PRE-FLIGHT FAILED. Please review the missing files above and upload them to the correct paths before launching.\n")
}
cat("============================================================\n")

###############################################################################
# CPTAC External Validation — Quadripartite Pre-flight Check
# ============================================================================
# Verifies that all CPTAC validation assets are in place on ZIMA before
# running CPTAC_Validation_Quadripartite.R.
# Checks: CPTAC matrices, TCGA model bundles, performance tables,
#         MVL weights, training matrices, feature alignment, packages.
###############################################################################

setwd("~/students/aluno0549-6/PHASE_III/PHASE_III_Megarun_5_0_complete_ZIMA_Suit_Generator_final")

MODELS_DIR  <- "PHASE_III_ML_Models"
CPTAC_DIR   <- "CPTAC_validation_matrices"
DF_ROOT     <- "~/students/aluno0549-6/dfXXX_series"
OUT_DIR     <- "CPTAC_Quadripartite_Results"

cancers <- c("BRCA", "COAD", "GBM", "HNSC", "KIRC", "LUAD", "LUSC", "OV", "PAAD", "UCEC")
issues  <- 0

# Ensure output directory exists before sinking
if (!dir.exists(OUT_DIR)) dir.create(OUT_DIR, recursive = TRUE)

sink(file.path(OUT_DIR, "preflight_cptac_quadripartite.log"))
cat("CPTAC Quadripartite Validation — Pre-flight Check\n")
cat(sprintf("Timestamp: %s\n", Sys.time()))
cat(sprintf("Working directory: %s\n", getwd()))

# ════════════════════════════════════════════════════════════════════════════
# 1. OUTPUT DIRECTORY
# ════════════════════════════════════════════════════════════════════════════

cat("========================================\n")
cat("1. OUTPUT DIRECTORY\n")
cat("========================================\n")

if (!dir.exists(OUT_DIR)) {
  dir.create(OUT_DIR, recursive = TRUE)
  cat(sprintf("  %s: CREATED\n", OUT_DIR))
} else {
  # Check writable
  test_file <- file.path(OUT_DIR, ".write_test")
  write_success <- tryCatch({ writeLines("test", test_file); file.remove(test_file); TRUE }, error=function(e) FALSE)
  cat(sprintf("  %s: %s\n", OUT_DIR, if(write_success) "WRITABLE" else "NOT WRITABLE"))
  if (!write_success) issues <- issues + 1
}

# ════════════════════════════════════════════════════════════════════════════
# 2. CPTAC VALIDATION MATRICES
# ════════════════════════════════════════════════════════════════════════════

cat("\n========================================\n")
cat("2. CPTAC VALIDATION MATRICES\n")
cat("========================================\n")

if (!dir.exists(CPTAC_DIR)) {
  cat(sprintf("  %s: DIRECTORY MISSING\n", CPTAC_DIR))
  issues <- issues + 1
} else {
  for (dfv in c("df008", "df017", "df147", "df161", "df368", "df377")) {
    path <- file.path(CPTAC_DIR, paste0(dfv, "_validation.rds"))
    if (file.exists(path)) {
      df <- readRDS(path)
      n_rows <- nrow(df)
      n_cols <- ncol(df)
      types <- sort(unique(df$type))
      has_os <- "OS" %in% names(df)
      has_os_time <- "OS.time" %in% names(df)
      cat(sprintf("  %s_validation: %d x %d, types=%s, OS=%s, OS.time=%s\n",
                  dfv, n_rows, n_cols, paste(types, collapse=","),
                  if(has_os) "OK" else "MISSING",
                  if(has_os_time) "OK" else "MISSING"))
      if (!has_os || !has_os_time) issues <- issues + 1
    } else {
      cat(sprintf("  %s_validation: MISSING\n", dfv))
      issues <- issues + 1
    }
  }
}

# ════════════════════════════════════════════════════════════════════════════
# 3. TCGA MODEL BUNDLES
# ════════════════════════════════════════════════════════════════════════════

cat("\n========================================\n")
cat("3. TCGA MODEL BUNDLES\n")
cat("========================================\n")

all_folders <- list.dirs(MODELS_DIR, full.names = FALSE, recursive = FALSE)
all_folders <- all_folders[grepl("_df[0-9]+$", all_folders)]

for (cancer in cancers) {
  pattern <- paste0("^", cancer, "_OS_")
  matches <- all_folders[grepl(pattern, all_folders)]
  if (length(matches) > 0) {
    for (m in matches) {
      bf <- file.path(MODELS_DIR, m, paste0("model_bundle_", m, ".rds"))
      wf <- file.path(MODELS_DIR, m, "MVL_Synthesis", paste0(m, "_MVL_Algorithm_Weights.tsv"))
      pf <- file.path(MODELS_DIR, m, paste0(m, "_Global_Performance.tsv"))
      
      bundle_ok <- file.exists(bf)
      weights_ok <- file.exists(wf)
      perf_ok    <- file.exists(pf)
      
      if (!bundle_ok) { issues <- issues + 1; cat(sprintf("  %s: bundle MISSING\n", m)) }
      if (!weights_ok) { issues <- issues + 1; cat(sprintf("  %s: MVL weights MISSING\n", m)) }
      if (!perf_ok)    { issues <- issues + 1; cat(sprintf("  %s: Global_Performance.tsv MISSING\n", m)) }
      
      if (bundle_ok) {
        bundle <- readRDS(bf)
        has_rsf  <- !is.null(bundle$RSF)
        has_xgb  <- !is.null(bundle$XGBoost)
        has_mtlr <- !is.null(bundle$MTLR)
        has_bor  <- !is.null(bundle$Boruta)
        n_obs <- if(has_rsf) nrow(as.data.frame(bundle$RSF$yvar)) else NA
        n_ev  <- if(has_rsf) sum(as.data.frame(bundle$RSF$yvar)[,2]==1) else NA
        n_feats <- if(has_xgb) length(bundle$XGBoost$feature_names) else NA
        cat(sprintf("  %s: OK (RSF=%s,XGB=%s,MTLR=%s,Bor=%s) n=%d,ev=%d,feats=%d\n",
                    m, if(has_rsf) "+" else "-", if(has_xgb) "+" else "-",
                    if(has_mtlr) "+" else "-", if(has_bor) "+" else "-",
                    if(is.na(n_obs)) 0 else n_obs, if(is.na(n_ev)) 0 else n_ev,
                    if(is.na(n_feats)) 0 else n_feats))
      }
      
      if (weights_ok) {
        w <- read.delim(wf, stringsAsFactors=FALSE)
        learner_cols <- w$Dimension[w$Dimension %in% c("RSF","XGBoost","MTLR","Boruta")]
        cat(sprintf("    Weights: %d learners (%s)\n", length(learner_cols), paste(learner_cols, collapse=",")))
      }
      
      if (perf_ok) {
        perf <- read.delim(pf, stringsAsFactors=FALSE)
        models_in_perf <- unique(perf$Model)
        cat(sprintf("    Performance: %d rows, models=%s\n", nrow(perf), paste(models_in_perf, collapse=",")))
        
        # Model names in performance table: RSF_Out_of_Bag, XGBoost_Apparent, MTLR_Apparent, Boruta_Independent, MVL_ElasticNet_SuperLearner
        if (!"MVL_ElasticNet_SuperLearner" %in% perf$Model) {
          cat(sprintf("    WARNING: MVL_ElasticNet_SuperLearner missing from performance table\n"))
          issues <- issues + 1
        }
      }
    }
  } else {
    cat(sprintf("  %s_OS: NO MODEL FOLDER\n", cancer))
    issues <- issues + 1
  }
}

# ════════════════════════════════════════════════════════════════════════════
# 4. TRAINING dfXXX MATRICES
# ════════════════════════════════════════════════════════════════════════════

cat("\n========================================\n")
cat("4. TRAINING dfXXX MATRICES\n")
cat("========================================\n")

for (dfv in c("df008", "df017", "df147", "df161", "df368", "df377")) {
  path <- file.path(DF_ROOT, paste0(dfv, ".rds"))
  if (file.exists(path)) {
    cat(sprintf("  %s: EXISTS\n", dfv))
  } else {
    cat(sprintf("  %s: MISSING\n", dfv))
    issues <- issues + 1
  }
}

# ════════════════════════════════════════════════════════════════════════════
# 5. FEATURE ALIGNMENT (XGBoost)
# ════════════════════════════════════════════════════════════════════════════

cat("\n========================================\n")
cat("5. FEATURE ALIGNMENT (XGBoost)\n")
cat("========================================\n")

for (dfv in c("df008", "df017", "df147", "df161", "df368", "df377")) {
  cptac_path <- file.path(CPTAC_DIR, paste0(dfv, "_validation.rds"))
  if (!file.exists(cptac_path)) next
  
  cptac_df <- readRDS(cptac_path)
  cptac_cols <- names(cptac_df)
  
  os_folders <- all_folders[grepl("_OS_", all_folders) & grepl(paste0("_", dfv, "$"), all_folders)]
  
  for (m in os_folders) {
    cancer <- strsplit(m, "_")[[1]][1]
    if (!cancer %in% cancers) next
    bf <- file.path(MODELS_DIR, m, paste0("model_bundle_", m, ".rds"))
    if (!file.exists(bf)) next
    bundle <- readRDS(bf)
    feat_names <- bundle$XGBoost$feature_names
    
    cptac_feats <- intersect(feat_names, cptac_cols)
    missing_feats <- setdiff(feat_names, cptac_cols)
    
    cat(sprintf("  %s: %d/%d XGB features in CPTAC (missing %d)\n",
                cancer, length(cptac_feats), length(feat_names), length(missing_feats)))
    if (length(cptac_feats) < 10) {
      cat(sprintf("    WARNING: <10 aligned features\n"))
      issues <- issues + 1
    }
  }
}

# ════════════════════════════════════════════════════════════════════════════
# 6. REQUIRED R PACKAGES
# ════════════════════════════════════════════════════════════════════════════

cat("\n========================================\n")
cat("6. REQUIRED R PACKAGES\n")
cat("========================================\n")

for (pkg in c("survival", "randomForestSRC", "xgboost", "glmnet", "missForest", "dplyr", "survivalROC")) {
  if (requireNamespace(pkg, quietly = TRUE)) {
    cat(sprintf("  %s: INSTALLED (v%s)\n", pkg, as.character(packageVersion(pkg))))
  } else {
    cat(sprintf("  %s: NOT INSTALLED (will auto-install during validation)\n", pkg))
  }
}

# ════════════════════════════════════════════════════════════════════════════
# 7. SUMMARY
# ════════════════════════════════════════════════════════════════════════════

cat("\n========================================\n")
cat("7. CPTAC COHORT SUMMARY (expected)\n")
cat("========================================\n")

for (dfv in c("df008", "df017", "df147", "df161", "df368", "df377")) {
  path <- file.path(CPTAC_DIR, paste0(dfv, "_validation.rds"))
  if (!file.exists(path)) next
  df <- readRDS(path)
  df_cancers <- sort(unique(df$type))
  for (ca in intersect(df_cancers, cancers)) {
    sub <- df[df$type == ca, ]
    cat(sprintf("  %-6s (%s): n=%d, events=%d\n", ca, dfv, nrow(sub), sum(sub$OS==1, na.rm=TRUE)))
  }
}

cat("\n========================================\n")
if (issues == 0) {
  cat("ALL CHECKS PASSED. Ready for CPTAC Quadripartite validation.\n")
} else {
  cat(sprintf("%d ISSUE(S) FOUND. Resolve before running CPTAC_Validation_Quadripartite.R\n", issues))
}

cat(sprintf("\nOutput directory: %s/%s\n", getwd(), OUT_DIR))
sink()
cat(sprintf("Preflight log saved to %s/preflight_cptac_quadripartite.log\n", OUT_DIR))

###############################################################################
# CPTAC External Validation — Pre-flight Check
# ============================================================================
# Verifies that all CPTAC validation assets are in place on ZIMA before
# running the prediction pipeline.
###############################################################################

setwd("~/students/aluno0549-6/PHASE_III/PHASE_III_Megarun_5_0_complete_ZIMA_Suit_Generator_final")

MODELS_DIR  <- "PHASE_III_ML_Models"
CPTAC_DIR   <- "CPTAC_validation_matrices"
DF_ROOT     <- "~/students/aluno0549-6/dfXXX_series"

cancers <- c("BRCA", "COAD", "GBM", "HNSC", "KIRC", "LUAD", "LUSC", "OV", "PAAD", "UCEC")
issues  <- 0

sink("preflight_cptac_validation.log")
cat("CPTAC External Validation — Pre-flight Check\n")

cat("========================================\n")
cat("1. CPTAC VALIDATION MATRICES\n")
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

cat("\n========================================\n")
cat("2. TCGA MODEL BUNDLES\n")
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
      
      if (!file.exists(bf)) { issues <- issues + 1; cat(sprintf("  %s: bundle MISSING\n", m)) }
      if (!file.exists(wf)) { issues <- issues + 1; cat(sprintf("  %s: weights MISSING\n", m)) }
      
      if (file.exists(bf)) {
        bundle <- readRDS(bf)
        has_rsf  <- !is.null(bundle$RSF)
        has_xgb  <- !is.null(bundle$XGBoost)
        has_mtlr <- !is.null(bundle$MTLR)
        has_bor  <- !is.null(bundle$Boruta)
        surv <- as.data.frame(bundle$RSF$yvar)
        n_feats <- length(bundle$XGBoost$feature_names)
        cat(sprintf("  %s: OK (RSF=%s,XGB=%s,MTLR=%s,Bor=%s) n=%d,ev=%d,feats=%d\n",
                    m, if(has_rsf) "+" else "-", if(has_xgb) "+" else "-",
                    if(has_mtlr) "+" else "-", if(has_bor) "+" else "-",
                    nrow(surv), sum(surv[,2]==1), n_feats))
      }
    }
  } else {
    cat(sprintf("  %s_OS: NO MODEL FOLDER\n", cancer))
    issues <- issues + 1
  }
}

cat("\n========================================\n")
cat("3. TRAINING dfXXX MATRICES\n")
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

cat("\n========================================\n")
cat("4. FEATURE ALIGNMENT (XGBoost)\n")
cat("========================================\n")

for (dfv in c("df008", "df017", "df147", "df161", "df368", "df377")) {
  cptac_path <- file.path(CPTAC_DIR, paste0(dfv, "_validation.rds"))
  if (!file.exists(cptac_path)) next
  
  cptac_df <- readRDS(cptac_path)
  cptac_cols <- names(cptac_df)
  
  # Find cancers using this df variant — only CPTAC ones
  os_folders <- all_folders[grepl("_OS_", all_folders) & grepl(paste0("_", dfv, "$"), all_folders)]
  
  for (m in os_folders) {
    cancer <- strsplit(m, "_")[[1]][1]
    if (!cancer %in% cancers) next  # skip non-CPTAC cancers
    bf <- file.path(MODELS_DIR, m, paste0("model_bundle_", m, ".rds"))
    if (!file.exists(bf)) next
    bundle <- readRDS(bf)
    feat_names <- bundle$XGBoost$feature_names
    
    cptac_feats <- intersect(feat_names, cptac_cols)
    missing_feats <- setdiff(feat_names, cptac_cols)
    
    cat(sprintf("  %s: %d/%d XGB features in CPTAC (missing %d)\n",
                cancer, length(cptac_feats), length(feat_names), length(missing_feats)))
  }
}

cat("\n========================================\n")
cat("5. REQUIRED R PACKAGES\n")
cat("========================================\n")

for (pkg in c("survivalROC")) {
  if (requireNamespace(pkg, quietly = TRUE)) {
    cat(sprintf("  %s: INSTALLED\n", pkg))
  } else {
    cat(sprintf("  %s: NOT INSTALLED (will auto-install during validation)\n", pkg))
  }
}

cat("\n========================================\n")
if (issues == 0) {
  cat("ALL CHECKS PASSED. Ready for CPTAC validation.\n")
} else {
  cat(sprintf("%d ISSUE(S) FOUND.\n", issues))
}
sink()
cat("Preflight log saved to preflight_cptac_validation.log\n")

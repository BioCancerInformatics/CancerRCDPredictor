###############################################################################
# R2.8c — Reproducibility Extraction (ZIMA — Phase III/IV)
# ============================================================================
# Extracts:
#   1. sessionInfo() → sessionInfo_ZIMA.txt
#   2. Per-stratum feature lists (XGBoost, RSF, Boruta, MTLR) → feature_lists.tsv
#   3. Per-stratum hyperparameters (all 4 models + MVL) → hyperparameters.tsv
#   4. Random seeds documentation → random_seeds.txt
###############################################################################

setwd("~/students/aluno0549-6/PHASE_III/PHASE_III_Megarun_5_0_complete_ZIMA_Suit_Generator_final")
OUT_DIR <- "Reproducibility_extraction_ZIMA"
MODELS_DIR <- "PHASE_III_ML_Models"

# ── 1. sessionInfo() ────────────────────────────────────────────────────────
cat("=== 1. sessionInfo() ===\n")
sink(file.path(OUT_DIR, "sessionInfo_ZIMA.txt"))
sessionInfo()
sink()
cat("Saved: sessionInfo_ZIMA.txt\n")

# ── 2. Feature lists & Hyperparameters ──────────────────────────────────────
cat("\n=== 2. Feature lists & Hyperparameters ===\n")

strata <- list.dirs(MODELS_DIR, full.names = FALSE, recursive = FALSE)
strata <- strata[grepl("_df[0-9]+$", strata)]
cat(sprintf("Found %d strata.\n", length(strata)))

features_out <- data.frame(
  cohort = character(), model = character(),
  n_features = integer(), features = character(),
  stringsAsFactors = FALSE
)
hyper_out <- data.frame(
  cohort = character(), model = character(),
  param = character(), value = character(),
  stringsAsFactors = FALSE
)

counter <- 0
for (s in strata) {
  counter <- counter + 1
  bf <- file.path(MODELS_DIR, s, paste0("model_bundle_", s, ".rds"))
  if (!file.exists(bf)) next
  bundle <- readRDS(bf)
  
  # ---- FEATURES ----
  # Boruta
  if (!is.null(bundle$Boruta$finalDecision)) {
    bf <- names(bundle$Boruta$finalDecision)
    bf <- bf[bundle$Boruta$finalDecision %in% c("Confirmed", "Tentative")]
    if (length(bf) > 0) features_out <- rbind(features_out, data.frame(
      cohort = s, model = "Boruta", n_features = length(bf),
      features = paste(bf, collapse = ";"), stringsAsFactors = FALSE))
  }
  # MTLR
  if (!is.null(bundle$MTLR$weight_matrix)) {
    mf <- rownames(bundle$MTLR$weight_matrix)
    if (length(mf) > 0) features_out <- rbind(features_out, data.frame(
      cohort = s, model = "MTLR", n_features = length(mf),
      features = paste(mf, collapse = ";"), stringsAsFactors = FALSE))
  }
  # XGBoost
  if (!is.null(bundle$XGBoost$feature_names)) {
    features_out <- rbind(features_out, data.frame(
      cohort = s, model = "XGBoost", n_features = length(bundle$XGBoost$feature_names),
      features = paste(bundle$XGBoost$feature_names, collapse = ";"), stringsAsFactors = FALSE))
  }
  # RSF
  if (!is.null(bundle$RSF$importance)) {
    rf <- names(bundle$RSF$importance)
    features_out <- rbind(features_out, data.frame(
      cohort = s, model = "RSF", n_features = length(rf),
      features = paste(rf, collapse = ";"), stringsAsFactors = FALSE))
  }
  
  # ---- HYPERPARAMETERS ----
  # XGBoost
  if (!is.null(bundle$XGBoost$params)) {
    for (pn in names(bundle$XGBoost$params))
      hyper_out <- rbind(hyper_out, data.frame(
        cohort = s, model = "XGBoost", param = pn,
        value = as.character(bundle$XGBoost$params[[pn]]), stringsAsFactors = FALSE))
    if (!is.null(bundle$XGBoost$niter))
      hyper_out <- rbind(hyper_out, data.frame(
        cohort = s, model = "XGBoost", param = "nrounds",
        value = as.character(bundle$XGBoost$niter), stringsAsFactors = FALSE))
  }
  # RSF
  if (!is.null(bundle$RSF$ntree)) {
    hyper_out <- rbind(hyper_out, data.frame(
      cohort = s, model = "RSF", param = "ntree",
      value = as.character(bundle$RSF$ntree), stringsAsFactors = FALSE))
    hyper_out <- rbind(hyper_out, data.frame(
      cohort = s, model = "RSF", param = "mtry",
      value = if(!is.null(bundle$RSF$mtry)) as.character(bundle$RSF$mtry) else "default",
      stringsAsFactors = FALSE))
    hyper_out <- rbind(hyper_out, data.frame(
      cohort = s, model = "RSF", param = "nodesize",
      value = if(!is.null(bundle$RSF$nodesize)) as.character(bundle$RSF$nodesize) else "default",
      stringsAsFactors = FALSE))
  }
  # MTLR
  if (!is.null(bundle$MTLR)) {
    hyper_out <- rbind(hyper_out, data.frame(
      cohort = s, model = "MTLR", param = "nintervals",
      value = if(!is.null(bundle$MTLR$nintervals)) as.character(bundle$MTLR$nintervals) else "default",
      stringsAsFactors = FALSE))
    hyper_out <- rbind(hyper_out, data.frame(
      cohort = s, model = "MTLR", param = "regularization",
      value = if(!is.null(bundle$MTLR$C1)) paste0("C1=", bundle$MTLR$C1) else "default",
      stringsAsFactors = FALSE))
  }
  # Boruta
  if (!is.null(bundle$Boruta$ntree))
    hyper_out <- rbind(hyper_out, data.frame(
      cohort = s, model = "Boruta", param = "ntree",
      value = as.character(bundle$Boruta$ntree), stringsAsFactors = FALSE))
  # MVL ElasticNet
  wf <- file.path(MODELS_DIR, s, "MVL_Synthesis", paste0(s, "_MVL_Algorithm_Weights.tsv"))
  if (file.exists(wf)) {
    w <- read.delim(wf, stringsAsFactors = FALSE)
    if("alpha" %in% names(w)) hyper_out <- rbind(hyper_out, data.frame(
      cohort = s, model = "MVL", param = "alpha",
      value = as.character(w$alpha[1]), stringsAsFactors = FALSE))
    if("lambda" %in% names(w)) hyper_out <- rbind(hyper_out, data.frame(
      cohort = s, model = "MVL", param = "lambda",
      value = as.character(w$lambda[1]), stringsAsFactors = FALSE))
    for (j in 1:nrow(w))
      hyper_out <- rbind(hyper_out, data.frame(
        cohort = s, model = "MVL", param = paste0("weight_", w$Dimension[j]),
        value = as.character(round(w$Elastic_Net_Weight[j], 6)), stringsAsFactors = FALSE))
  }
  
  if (counter %% 20 == 0 || counter == length(strata))
    cat(sprintf("  [%d/%d] %s\n", counter, length(strata), s))
}

write.table(features_out, file.path(OUT_DIR, "feature_lists.tsv"), sep = "\t", row.names = FALSE, quote = FALSE)
cat(sprintf("Saved: feature_lists.tsv (%d rows)\n", nrow(features_out)))
write.table(hyper_out, file.path(OUT_DIR, "hyperparameters.tsv"), sep = "\t", row.names = FALSE, quote = FALSE)
cat(sprintf("Saved: hyperparameters.tsv (%d rows)\n", nrow(hyper_out)))

# ── 3. Random seeds ──────────────────────────────────────────────────────────
cat("\n=== 3. Random seeds ===\n")
sink(file.path(OUT_DIR, "random_seeds.txt"))
cat("Random Seed Documentation — Phase III/IV (ZIMA)\n")
cat("===============================================\n\n")
cat("Global: set.seed(42) for all model training (RSF, XGBoost, Boruta, MTLR, MVL)\n")
cat("Bootstrap CIs: set.seed(123) per stratum\n")
cat("Permutation testing: set.seed(123) + stratum index offset\n")
cat("Calibration: no random components\n\n")
cat("Per-stratum XGBoost seeds (from bundle):\n")
for (s in strata) {
  bf <- file.path(MODELS_DIR, s, paste0("model_bundle_", s, ".rds"))
  if (!file.exists(bf)) next
  bundle <- readRDS(bf)
  sv <- if (!is.null(bundle$XGBoost$params$seed)) bundle$XGBoost$params$seed else 42
  cat(sprintf("  %s: seed = %s\n", s, as.character(sv)))
}
sink()
cat("Saved: random_seeds.txt\n")

cat("\n=== DONE ===\n")
cat("  sessionInfo_ZIMA.txt\n")
cat("  feature_lists.tsv\n")
cat("  hyperparameters.tsv\n")
cat("  random_seeds.txt\n")

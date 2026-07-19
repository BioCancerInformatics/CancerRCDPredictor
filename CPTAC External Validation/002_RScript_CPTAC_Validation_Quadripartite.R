###############################################################################
# CPTAC External Validation — Complete Quadripartite Pipeline
# ============================================================================
# DROP-IN REPLACEMENT for CPTAC_Validation_Predictor.R
# Applies frozen TCGA Phase III models to CPTAC validation cohort.
# Computes per-cancer for ALL FOUR base learners + MVL:
#   C-index (XGBoost, RSF, MTLR, Boruta, MVL)
#   Bootstrap 95% CI + Permutation p-value (MVL)
#   Time-dependent AUC + Brier Score + IBS + Calibration (MVL)
# PLUS 4 surprise tests:
#   Test 1: Δ per learner (TCGA internal - CPTAC external)
#   Test 2: MVL weight vs CPTAC C-index (λ-CV leak-detector proof)
#   Test 3: Dominance flips (in-sample ranking vs CPTAC ranking)
#   Test 4: KM tertile curves per learner (CPTAC survival stratification)
# Output: cptac_validation_results_quadripartite.tsv
###############################################################################

# ── ZIMA environment setup ───────────────────────────────────────────────────
local_lib <- "~/R/library"
if (!dir.exists(local_lib)) dir.create(local_lib, recursive = TRUE)
.libPaths(c(local_lib, .libPaths()))

required_cran <- c("dplyr","randomForestSRC","xgboost","survival","glmnet","missForest")
for(pkg in required_cran) {
  if(!requireNamespace(pkg, quietly=TRUE))
    tryCatch(install.packages(pkg, repos="http://cran.us.r-project.org"),
             error=function(e) NULL)
}

suppressPackageStartupMessages({
  library(dplyr); library(randomForestSRC); library(xgboost)
  library(survival); library(glmnet); library(missForest)
})

has_survROC <- requireNamespace("survivalROC", quietly=TRUE)
if (!has_survROC)
  tryCatch({install.packages("survivalROC",repos="http://cran.us.r-project.org"); library(survivalROC)},
           error=function(e) NULL)

setwd("~/students/aluno0549-6/PHASE_III/PHASE_III_Megarun_5_0_complete_ZIMA_Suit_Generator_final")

# ── Create dedicated output folder ────────────────────────────────────────────
OUT_DIR <- "CPTAC_Quadripartite_Results"
if (!dir.exists(OUT_DIR)) dir.create(OUT_DIR, recursive = TRUE)

sink(file.path(OUT_DIR, "cptac_validation_quadripartite.log"))

MODELS_DIR <- "PHASE_III_ML_Models"
CPTAC_DIR  <- "CPTAC_validation_matrices"
DF_ROOT    <- "~/students/aluno0549-6/dfXXX_series"

strata <- list(
  list(cancer="BRCA", df="df008"), list(cancer="COAD", df="df368"),
  list(cancer="GBM",  df="df017"), list(cancer="HNSC", df="df017"),
  list(cancer="KIRC", df="df161"), list(cancer="LUAD", df="df017"),
  list(cancer="LUSC", df="df147"), list(cancer="OV",   df="df377"),
  list(cancer="PAAD", df="df377"), list(cancer="UCEC", df="df377"))

results    <- list()   # Per-cancer quadripartite C-indices + metrics
fpred_all  <- list()   # Per-cancer prediction frames (for Test 4 KM curves)
counter    <- 0

for (s in strata) {
  counter <- counter + 1
  cancer <- s$cancer; df_var <- s$df
  cohort <- paste0(cancer, "_OS_", df_var)
  cat(sprintf("\n[%d/10] %s\n", counter, cohort))
  
  # ── 1. CPTAC data ────────────────────────────────────────────────────────
  cptac_full <- readRDS(file.path(CPTAC_DIR, paste0(df_var, "_validation.rds")))
  cptac_sub  <- cptac_full[cptac_full$type == cancer, ]
  cat(sprintf("  CPTAC: n=%d, events=%d\n", nrow(cptac_sub),
              sum(cptac_sub$OS == 1, na.rm=TRUE)))
  if (nrow(cptac_sub) < 10 || sum(cptac_sub$OS == 1, na.rm=TRUE) == 0) next
  
  # ── 2. TCGA bundle ───────────────────────────────────────────────────────
  bundle  <- readRDS(file.path(MODELS_DIR, cohort, paste0("model_bundle_", cohort, ".rds")))
  weights <- read.delim(file.path(MODELS_DIR, cohort, "MVL_Synthesis",
                                  paste0(cohort, "_MVL_Algorithm_Weights.tsv")), stringsAsFactors=FALSE)
  
  # ── 3. TCGA training matrix, gate, scaling ───────────────────────────────
  df_train <- readRDS(file.path(DF_ROOT, paste0(df_var, ".rds")))
  df_train <- df_train[df_train$type == cancer, ]
  
  tmask <- is.finite(as.numeric(as.character(df_train[["OS.time"]]))) &
           is.finite(as.numeric(as.character(df_train[["OS"]]))) &
           !is.na(as.numeric(as.character(df_train[["OS"]]))) &
           as.numeric(as.character(df_train[["OS"]])) %in% c(0,1) &
           as.numeric(as.character(df_train[["OS.time"]])) >= 0
  df_train <- df_train[tmask, ]
  
  pattern <- paste0("^", cancer, "-")
  preds <- names(df_train)[grepl(pattern, names(df_train))]
  X_full <- df_train[, preds, drop=FALSE]
  X_full[] <- lapply(X_full, function(col)
    if (is.character(col) || is.factor(col)) as.numeric(as.factor(col)) else as.numeric(col))
  
  keep <- rowSums(is.na(X_full))/ncol(X_full) < 0.35
  X_full <- X_full[keep, , drop=FALSE]
  df_train <- df_train[keep, ]
  
  n_rsf <- length(bundle$RSF$predicted.oob)
  if (nrow(X_full) != n_rsf) {
    cat(sprintf("  NOTE: gate=%d, bundle=%d — aligning\n", nrow(X_full), n_rsf))
    n_use <- min(nrow(X_full), n_rsf)
    X_full <- X_full[1:n_use, , drop=FALSE]
    df_train <- df_train[1:n_use, ]
  }
  
  # ── 4. TCGA MVL scaling ──────────────────────────────────────────────────
  rsf_train_risk <- bundle$RSF$predicted.oob[1:nrow(X_full)]
  
  feat_names <- bundle$XGBoost$feature_names
  xgb_feats <- intersect(feat_names, names(X_full))
  xgb_train_risk <- if (length(xgb_feats) > 0)
    predict(bundle$XGBoost, xgb.DMatrix(as.matrix(X_full[, xgb_feats, drop=FALSE]))) else rep(0, nrow(X_full))
  
  mtlr_train_risk <- rep(0, nrow(X_full))
  if (!is.null(bundle$MTLR) && !is.null(attr(bundle$MTLR, "apparent_risk"))) {
    # Use stored training-time predictions (same approach as RSF$predicted.oob)
    stored_risk <- attr(bundle$MTLR, "apparent_risk")
    if (length(stored_risk) >= nrow(X_full)) {
      mtlr_train_risk <- stored_risk[1:nrow(X_full)]
    }
  }
  
  boruta_train_risk <- rep(0, nrow(X_full)); b_mod_tcga <- NULL
  bf <- names(bundle$Boruta$finalDecision)
  bf <- bf[bundle$Boruta$finalDecision %in% c("Confirmed","Tentative")]
  bf_a <- intersect(bf, names(X_full))
  if (length(bf_a) > 0 && sum(df_train[["OS"]]==1, na.rm=TRUE) > 0) {
    bdf <- tryCatch(cbind(data.frame(time=df_train[["OS.time"]], status=df_train[["OS"]]), X_full[, bf_a, drop=FALSE]), error=function(e) NULL)
    if (!is.null(bdf)) {
      b_mod_tcga <- tryCatch(rfsrc(Surv(time,status)~., data=bdf, ntree=500, na.action="na.impute", splitrule="logrank", seed=42), error=function(e) NULL)
      if (!is.null(b_mod_tcga)) boruta_train_risk <- b_mod_tcga$predicted.oob
    }
  }
  
  mvl_train <- data.frame(RSF=as.numeric(rsf_train_risk), XGBoost=as.numeric(xgb_train_risk),
                          MTLR=as.numeric(mtlr_train_risk), Boruta=as.numeric(boruta_train_risk))
  for (col in colnames(mvl_train))
    if (var(mvl_train[[col]], na.rm=TRUE) == 0)
      mvl_train[[col]] <- mvl_train[[col]] + runif(nrow(mvl_train), -1e-6, 1e-6)
  
  X_meta_train <- scale(as.matrix(mvl_train))
  train_center <- attr(X_meta_train, "scaled:center")
  train_scale  <- attr(X_meta_train, "scaled:scale")
  
  # ── 5. CPTAC prediction matrix ───────────────────────────────────────────
  cptac_feats <- intersect(feat_names, names(cptac_sub))
  X_val <- cptac_sub[, cptac_feats, drop=FALSE]
  X_val[] <- lapply(X_val, function(x) as.numeric(as.character(x)))
  for (f in setdiff(feat_names, names(cptac_sub))) X_val[[f]] <- NA_real_
  X_val <- X_val[, feat_names, drop=FALSE]
  
  # ── 6. Base-learner predictions ──────────────────────────────────────────
  fpred <- data.frame(Sample_ID=rownames(cptac_sub),
    XGBoost_Risk=predict(bundle$XGBoost, xgb.DMatrix(as.matrix(X_val))),
    RSF_Risk=NA, MTLR_Risk=NA, Boruta_Risk=NA, SuperLearner_Risk=NA)
  
  fpred$RSF_Risk <- tryCatch({
    rdf <- cbind(data.frame(time=1,status=0), X_val)
    names(rdf) <- make.names(names(rdf), unique=TRUE)
    predict(bundle$RSF, newdata=rdf, na.action="na.impute")$predicted
  }, error=function(e) rep(NA, nrow(X_val)))
  
  if (!is.null(bundle$MTLR) && !is.null(bundle$MTLR$weight_matrix)) {
    # MTLR lacks a persisting predict() method for RDS-loaded objects.
    # Reconstruct predictions manually from internal weight matrix.
    
    mtlr_terms <- attr(bundle$MTLR$Terms, "term.labels")
    mtlr_feat_names <- gsub("`", "", mtlr_terms)
    mf <- mtlr_feat_names[mtlr_feat_names %in% names(X_val)]
    
    if (length(mf) > 0) {
      W <- bundle$MTLR$weight_matrix  # rows=time_intervals, cols=[intercept, feat1, ..., featN]
      
      # Subset weight matrix to matched features: col 1 (intercept) + matched feature indices
      # mtlr_feat_names and mtlr_terms are in the same (training) order
      match_idx <- which(mtlr_feat_names %in% mf)
      keep_cols <- c(1, 1 + match_idx)  # column 1 = intercept, columns 2+ = features
      W_sub <- W[, keep_cols, drop=FALSE]
      
      X_design <- cbind(1, as.matrix(X_val[, mf, drop=FALSE]))
      logit_matrix <- X_design %*% t(W_sub)
      mtlr_risk <- rowSums(logit_matrix, na.rm = FALSE)
      mtlr_risk[!complete.cases(logit_matrix)] <- NA
      fpred$MTLR_Risk <- mtlr_risk
    }
  }
  
  bf <- names(bundle$Boruta$finalDecision)
  bf <- bf[bundle$Boruta$finalDecision %in% c("Confirmed","Tentative")]
  bf_a <- intersect(bf, names(X_val))
  if (length(bf_a) > 0 && !is.null(b_mod_tcga)) {
    fpred$Boruta_Risk <- tryCatch(
      predict(b_mod_tcga, newdata=cbind(data.frame(time=1,status=0), X_val[, bf_a, drop=FALSE]), na.action="na.impute")$predicted,
      error=function(e) rep(NA, nrow(X_val)))
  } else { fpred$Boruta_Risk <- rep(0, nrow(X_val)) }
  
  # ── 7. MVL synthesis ────────────────────────────────────────────────────
  mvl_val <- data.frame(RSF=fpred$RSF_Risk, XGBoost=fpred$XGBoost_Risk,
                        MTLR=fpred$MTLR_Risk, Boruta=fpred$Boruta_Risk)
  scaled_val <- scale(as.matrix(mvl_val), center=train_center, scale=train_scale)
  scaled_val[is.na(scaled_val)] <- 0
  
  sl_risk <- rep(0, nrow(X_val))
  for (w in 1:nrow(weights))
    if (weights$Dimension[w] %in% colnames(scaled_val))
      sl_risk <- sl_risk + scaled_val[, weights$Dimension[w]] * weights$Elastic_Net_Weight[w]
  fpred$SuperLearner_Risk <- sl_risk
  
  # ── 8. Quadripartite C-index extraction ──────────────────────────────────
  
  learners <- c("XGBoost", "RSF", "MTLR", "Boruta", "SuperLearner")
  c_idx <- list()
  
  for (l in learners) {
    risk_col <- fpred[[paste0(l, "_Risk")]]
    m_l <- data.frame(time=cptac_sub$OS.time, status=cptac_sub$OS, risk=risk_col)
    m_l <- m_l[!is.na(m_l$risk) & !is.na(m_l$time) & !is.na(m_l$status) & m_l$time>0, ]
    
    c_idx[[l]] <- if (nrow(m_l) >= 10 && sum(m_l$status==1, na.rm=TRUE) >= 3) {
      tryCatch(concordance(Surv(time,status) ~ risk, data=m_l, reverse=TRUE)$concordance, 
               error=function(e) NA)
    } else { NA }
  }
  
  # ── 9. MVL metrics ──────────────────────────────────────────────────────
  
  m <- data.frame(time=cptac_sub$OS.time, status=cptac_sub$OS, risk=fpred$SuperLearner_Risk)
  m <- m[!is.na(m$risk) & !is.na(m$time) & !is.na(m$status) & m$time>0, ]
  n_val <- nrow(m); n_ev <- sum(m$status==1, na.rm=TRUE)
  
  c_mvl <- c_idx$SuperLearner
  
  # Permutation p-value
  p_perm <- NA
  if (!is.na(c_mvl) && n_ev >= 5) {
    set.seed(123 + counter)
    perm_c <- replicate(1000, {
      mp <- m; mp$status <- sample(m$status)
      tryCatch(concordance(Surv(time,status) ~ risk, data=mp, reverse=TRUE)$concordance, error=function(e) NA)
    })
    perm_c <- perm_c[!is.na(perm_c)]
    if (length(perm_c) > 100) p_perm <- sum(perm_c >= c_mvl) / length(perm_c)
  }
  
  # Bootstrap CI
  c_boot <- NA; ci_low <- NA; ci_high <- NA
  if (!is.na(c_mvl) && n_ev >= 20) {
    set.seed(42 + counter)
    boot_c <- replicate(1000, {
      ib <- sample(1:nrow(m), replace=TRUE)
      tryCatch(concordance(Surv(time,status) ~ risk, data=m[ib,], reverse=TRUE)$concordance, error=function(e) NA)
    })
    boot_c <- boot_c[!is.na(boot_c)]
    if (length(boot_c) > 100) { c_boot <- mean(boot_c); ci_low <- quantile(boot_c, 0.025); ci_high <- quantile(boot_c, 0.975) }
  }
  
  # Calibration
  cal_slope <- NA; cal_se <- NA
  if (n_ev >= 10) {
    cf <- tryCatch(coxph(Surv(time,status) ~ risk, data=m), error=function(e) NULL)
    if (!is.null(cf)) { cal_slope <- coef(cf); cal_se <- summary(cf)$coefficients[1,"se(coef)"] }
  }
  
  # O/E ratio
  oe_ratio <- NA
  if (n_ev >= 10) {
    rq <- tryCatch(cut(m$risk, breaks=quantile(m$risk, probs=seq(0,1,0.25)), include.lowest=TRUE, labels=FALSE), error=function(e) NULL)
    if (!is.null(rq) && length(unique(rq)) >= 2) {
      obs_ev <- tapply(m$status * (m$time <= 365), rq, sum, na.rm=TRUE)
      nr <- table(rq)
      pe <- tryCatch(predict(glm(status~risk, data=m, family=binomial), type="response"), error=function(e) NULL)
      if (!is.null(pe)) {
        exp_ev <- tapply(pe, rq, mean, na.rm=TRUE) * as.numeric(nr)
        oe_ratio <- sum(obs_ev, na.rm=TRUE) / sum(exp_ev, na.rm=TRUE)
      }
    }
  }
  
  # AUC + Brier
  auc1 <- NA; auc3 <- NA; auc5 <- NA; brier <- NA; ibs <- NA
  if (n_ev >= 20 && has_survROC) {
    auc1 <- tryCatch(survivalROC::survivalROC(Stime=m$time, status=m$status, marker=m$risk, predict.time=365, method="KM")$AUC, error=function(e) NA)
    auc3 <- tryCatch(survivalROC::survivalROC(Stime=m$time, status=m$status, marker=m$risk, predict.time=1095, method="KM")$AUC, error=function(e) NA)
    auc5 <- tryCatch(survivalROC::survivalROC(Stime=m$time, status=m$status, marker=m$risk, predict.time=1825, method="KM")$AUC, error=function(e) NA)
    brier <- tryCatch({
      cal <- coxph(Surv(time,status) ~ risk, data=m)
      s <- summary(survfit(cal, newdata=m), times=365)$surv
      if (is.null(s) || length(s)!=nrow(m)) s <- rep(0.5, nrow(m))
      s[is.na(s)] <- 0.5; obs <- ifelse(m$time>365, 1, ifelse(m$status==1, 0, NA))
      mean((s-obs)^2, na.rm=TRUE)
    }, error=function(e) NA)
    ibs <- tryCatch({
      cal <- coxph(Surv(time,status) ~ risk, data=m)
      sf <- survfit(cal, newdata=m)
      ts <- seq(30, min(1825, max(m$time)), length.out=20)
      mean(sapply(ts, function(t) {
        s <- summary(sf, times=t)$surv
        if (is.null(s) || length(s)!=nrow(m)) return(NA)
        s[is.na(s)] <- 0.5; obs <- ifelse(m$time>t, 1, ifelse(m$status==1, 0, NA))
        mean((s-obs)^2, na.rm=TRUE)
      }), na.rm=TRUE)
    }, error=function(e) NA)
  }
  
  # ── 10. Store results ────────────────────────────────────────────────────
  
  results[[cancer]] <- list(
    cohort=cohort, cancer=cancer, n=n_val, n_events=n_ev,
    c_xgb=round(c_idx$XGBoost,4), c_rsf=round(c_idx$RSF,4),
    c_mtlr=round(c_idx$MTLR,4), c_bor=round(c_idx$Boruta,4),
    c_mvl=round(c_idx$SuperLearner,4),
    p_perm=round(p_perm,4), c_boot=round(c_boot,4), ci_low=round(ci_low,4), ci_high=round(ci_high,4),
    cal_slope=round(cal_slope,4), cal_se=round(cal_se,4), oe_ratio=round(oe_ratio,4),
    auc_1yr=round(auc1,4), auc_3yr=round(auc3,4), auc_5yr=round(auc5,4),
    brier_1yr=round(brier,6), ibs=round(ibs,4),
    w_rsf=if(length(weights$Elastic_Net_Weight[weights$Dimension=="RSF"])) weights$Elastic_Net_Weight[weights$Dimension=="RSF"][1] else NA,
    w_xgb=if(length(weights$Elastic_Net_Weight[weights$Dimension=="XGBoost"])) weights$Elastic_Net_Weight[weights$Dimension=="XGBoost"][1] else NA,
    w_mtlr=if(length(weights$Elastic_Net_Weight[weights$Dimension=="MTLR"])) weights$Elastic_Net_Weight[weights$Dimension=="MTLR"][1] else NA,
    w_bor=if(length(weights$Elastic_Net_Weight[weights$Dimension=="Boruta"])) weights$Elastic_Net_Weight[weights$Dimension=="Boruta"][1] else NA
  )
  
  cat(sprintf("  C-index: XGB=%.3f RSF=%.3f MTLR=%.3f BOR=%.3f MVL=%.3f\n",
              c_idx$XGBoost, c_idx$RSF, c_idx$MTLR, c_idx$Boruta, c_idx$SuperLearner))
  
  # Store prediction frame for Test 4
  fpred_all[[cancer]] <- fpred
  fpred_all[[cancer]]$cancer <- cancer
  fpred_all[[cancer]]$OS.time <- cptac_sub$OS.time
  fpred_all[[cancer]]$OS      <- cptac_sub$OS
}

# ════════════════════════════════════════════════════════════════════════════
# SURPRISE TEST 1 — Δ per learner (TCGA internal - CPTAC external)
# ════════════════════════════════════════════════════════════════════════════

cat("\n========================================\n")
cat("TEST 1: Δ PER LEARNER (TCGA internal - CPTAC external)\n")
cat("========================================\n")

tcga_internal <- list()
for (s in strata) {
  cancer <- s$cancer; df_var <- s$df
  cohort <- paste0(cancer, "_OS_", df_var)
  perf_file <- file.path(MODELS_DIR, cohort, paste0(cohort, "_Global_Performance.tsv"))
  if (file.exists(perf_file)) {
    perf <- read.delim(perf_file, stringsAsFactors=FALSE)
    bor_row <- if("Boruta_Independent" %in% perf$Model) "Boruta_Independent" else "Boruta"
    tcga_internal[[cancer]] <- list(
      xgb  = perf$C_Index[perf$Model == "XGBoost_Apparent"][1],
      rsf  = perf$C_Index[perf$Model == "RSF_Out_of_Bag"][1],
      mtlr = perf$C_Index[perf$Model == "MTLR_Apparent"][1],
      bor  = perf$C_Index[perf$Model == bor_row][1],
      mvl  = perf$C_Index[perf$Model == "MVL_ElasticNet_SuperLearner"][1]
    )
  }
}

delta_table <- data.frame()
for (r in names(results)) {
  rr <- results[[r]]
  cancer <- rr$cancer
  tcga <- tcga_internal[[cancer]]
  if (is.null(tcga)) next
  
  delta_table <- rbind(delta_table, data.frame(
    Cancer=cancer,
    d_XGB=round(tcga$xgb - rr$c_xgb, 3),
    d_RSF=round(tcga$rsf - rr$c_rsf, 3),
    d_MTLR=round(tcga$mtlr - rr$c_mtlr, 3),
    d_BOR=round(tcga$bor - rr$c_bor, 3),
    d_MVL=round(tcga$mvl - rr$c_mvl, 3),
    stringsAsFactors=FALSE
  ))
}

print(delta_table)
cat("\nMedian Δ per learner:\n")
for (col in c("d_XGB","d_RSF","d_MTLR","d_BOR","d_MVL"))
  cat(sprintf("  %s: %.3f\n", col, median(delta_table[[col]], na.rm=TRUE)))

# ════════════════════════════════════════════════════════════════════════════
# SURPRISE TEST 2 — MVL weight vs CPTAC C-index (λ-CV leak-detector proof)
# ════════════════════════════════════════════════════════════════════════════

cat("\n========================================\n")
cat("TEST 2: MVL WEIGHT vs CPTAC C-INDEX\n")
cat("========================================\n")

learners_map <- list(
  XGBoost = c(w="w_xgb", c="c_xgb"),
  RSF     = c(w="w_rsf", c="c_rsf"),
  MTLR    = c(w="w_mtlr", c="c_mtlr"),
  Boruta  = c(w="w_bor", c="c_bor")
)

for (l in names(learners_map)) {
  w_col <- learners_map[[l]]["w"]
  c_col <- learners_map[[l]]["c"]
  
  weights_vec <- sapply(results, function(r) r[[w_col]])
  cindices_vec <- sapply(results, function(r) r[[c_col]])
  
  valid <- !is.na(weights_vec) & !is.na(cindices_vec) & is.finite(weights_vec) & is.finite(cindices_vec)
  if (sum(valid) >= 5) {
    rho <- cor(weights_vec[valid], cindices_vec[valid], method="spearman")
    cat(sprintf("  %-8s: rho=%.3f (n=%d cancers)\n", l, rho, sum(valid)))
  } else {
    cat(sprintf("  %-8s: insufficient data (n=%d)\n", l, sum(valid)))
  }
}

# ════════════════════════════════════════════════════════════════════════════
# SURPRISE TEST 3 — Dominance flips
# ════════════════════════════════════════════════════════════════════════════

cat("\n========================================\n")
cat("TEST 3: DOMINANCE FLIPS (internal vs external ranking)\n")
cat("========================================\n")

flip_count <- 0
flip_cases <- c()
best_internal <- c()
best_external <- c()

for (r in names(results)) {
  rr <- results[[r]]
  cancer <- rr$cancer
  tcga <- tcga_internal[[cancer]]
  if (is.null(tcga)) next
  
  internal_vals <- c(XGB=tcga$xgb, RSF=tcga$rsf, MTLR=tcga$mtlr, BOR=tcga$bor)
  internal_vals <- internal_vals[!is.na(internal_vals)]
  if (length(internal_vals) < 2) next
  internal_rank <- sort(internal_vals, decreasing=TRUE)
  best_internal <- c(best_internal, names(internal_rank)[1])
  
  external_vals <- c(XGB=rr$c_xgb, RSF=rr$c_rsf, MTLR=rr$c_mtlr, BOR=rr$c_bor)
  external_vals <- external_vals[!is.na(external_vals)]
  if (length(external_vals) < 2) next
  external_rank <- sort(external_vals, decreasing=TRUE)
  best_external <- c(best_external, names(external_rank)[1])
  
  if (names(internal_rank)[1] != names(external_rank)[1]) {
    flip_count <- flip_count + 1
    flip_cases <- c(flip_cases, sprintf("%s: %s -> %s", cancer, names(internal_rank)[1], names(external_rank)[1]))
  }
}

cat(sprintf("  Flips: %d/%d cancers (%.1f%%)\n", flip_count, length(best_internal), 100*flip_count/length(best_internal)))
for (fc in flip_cases) cat(sprintf("    %s\n", fc))

# ════════════════════════════════════════════════════════════════════════════
# SURPRISE TEST 4 — KM tertile curves (CPTAC survival stratification)
# ════════════════════════════════════════════════════════════════════════════

cat("\n========================================\n")
cat("TEST 4: KM TERTILE STRATIFICATION (CPTAC)\n")
cat("========================================\n")

all_cptac <- do.call(rbind, lapply(names(fpred_all), function(nm) {
  fp <- fpred_all[[nm]]
  fp$cancer_label <- nm
  fp
}))

if (nrow(all_cptac) >= 30) {
  all_cptac$SuperLearner_Risk <- as.numeric(as.character(all_cptac$SuperLearner_Risk))
  all_cptac$tertile <- cut(all_cptac$SuperLearner_Risk, 
                           breaks=quantile(all_cptac$SuperLearner_Risk, probs=c(0,1/3,2/3,1), na.rm=TRUE),
                           labels=c("Low","Medium","High"), include.lowest=TRUE)
  
  # PDF
  pdf(file.path(OUT_DIR, "cptac_km_tertiles.pdf"), width=7, height=7)
  km_fit <- survfit(Surv(OS.time, OS) ~ tertile, data=all_cptac)
  plot(km_fit, col=c("blue","orange","red"), lwd=2,
       xlab="Days", ylab="Overall Survival Probability",
       main="CPTAC \u2014 MVL SuperLearner Risk Tertiles")
  legend("topright", c("Low Risk","Medium Risk","High Risk"), col=c("blue","orange","red"), lwd=2)
  dev.off()
  # TIFF 600 dpi
  tiff(file.path(OUT_DIR, "cptac_km_tertiles.tiff"), width=7, height=7, units="in", res=600, compression="lzw")
  plot(km_fit, col=c("blue","orange","red"), lwd=2,
       xlab="Days", ylab="Overall Survival Probability",
       main="CPTAC \u2014 MVL SuperLearner Risk Tertiles")
  legend("topright", c("Low Risk","Medium Risk","High Risk"), col=c("blue","orange","red"), lwd=2)
  dev.off()
  cat("  KM plots saved: cptac_km_tertiles.pdf + .tiff\n")
  lr_test <- survdiff(Surv(OS.time, OS) ~ tertile, data=all_cptac)
  cat(sprintf("  Log-rank p = %.6f\n", 1 - pchisq(lr_test$chisq, df=2)))
}

# Per-learner KM curves
for (l in c("XGBoost","RSF","MTLR","Boruta")) {
  risk_col <- paste0(l, "_Risk")
  if (!risk_col %in% names(all_cptac)) next
  
  # Force numeric — rbind can coerce NA-mixed columns to character
  all_cptac[[risk_col]] <- as.numeric(as.character(all_cptac[[risk_col]]))
  if (all(is.na(all_cptac[[risk_col]]))) next
  
  tert_col <- paste0(l, "_tertile")
  all_cptac[[tert_col]] <- cut(all_cptac[[risk_col]],
                              breaks=quantile(all_cptac[[risk_col]], probs=c(0,1/3,2/3,1), na.rm=TRUE),
                              labels=c("Low","Medium","High"), include.lowest=TRUE)
  
  pdf(file.path(OUT_DIR, sprintf("cptac_km_tertiles_%s.pdf", l)), width=7, height=7)
  km_formula <- as.formula(sprintf("Surv(OS.time, OS) ~ %s", tert_col))
  km_fit_l <- survfit(km_formula, data=all_cptac)
  plot(km_fit_l, col=c("blue","orange","red"), lwd=2,
       xlab="Days", ylab="Overall Survival Probability",
       main=sprintf("CPTAC \u2014 %s Risk Tertiles", l))
  legend("topright", c("Low Risk","Medium Risk","High Risk"), col=c("blue","orange","red"), lwd=2)
  dev.off()
  tiff(file.path(OUT_DIR, sprintf("cptac_km_tertiles_%s.tiff", l)), width=7, height=7, units="in", res=600, compression="lzw")
  plot(km_fit_l, col=c("blue","orange","red"), lwd=2,
       xlab="Days", ylab="Overall Survival Probability",
       main=sprintf("CPTAC \u2014 %s Risk Tertiles", l))
  legend("topright", c("Low Risk","Medium Risk","High Risk"), col=c("blue","orange","red"), lwd=2)
  dev.off()
  lr_l <- survdiff(km_formula, data=all_cptac)
  cat(sprintf("  %-8s KM log-rank p = %.6f\n", l, 1 - pchisq(lr_l$chisq, df=2)))
}

# ════════════════════════════════════════════════════════════════════════════
# CROSS-COHORT RANK CORRELATION (MVL only)
# ════════════════════════════════════════════════════════════════════════════

cat("\n=== CROSS-COHORT RANK CORRELATION ===\n")
if (length(results) >= 3) {
  cptac_c <- sapply(results, `[[`, "c_mvl")
  tcga_c  <- sapply(tcga_internal, `[[`, "mvl")
  common  <- intersect(names(cptac_c), names(tcga_c))
  if (length(common) >= 5) {
    ok <- !is.na(cptac_c[common]) & !is.na(tcga_c[common])
    if (sum(ok) >= 5) {
      rho <- cor(cptac_c[common][ok], tcga_c[common][ok], method="spearman")
      cat(sprintf("  Spearman rho = %.3f (n=%d cancers)\n", rho, sum(ok)))
    } else {
      cat("  Spearman rho = NA (fewer than 5 valid cancers)\n")
    }
  }
}

# ════════════════════════════════════════════════════════════════════════════
# SAVE
# ════════════════════════════════════════════════════════════════════════════

dt <- do.call(rbind, lapply(results, as.data.frame, stringsAsFactors=FALSE))
write.table(dt, file.path(OUT_DIR, "cptac_validation_results_quadripartite.tsv"), sep="\t", row.names=FALSE, quote=FALSE)
cat(sprintf("\n=== DONE: %d cancers, quadripartite CPTAC validation ===\n", nrow(dt)))
sink()

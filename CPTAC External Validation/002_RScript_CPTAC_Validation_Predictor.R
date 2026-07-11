###############################################################################
# CPTAC External Validation — Complete Prediction Pipeline
# ============================================================================
# Applies frozen TCGA Phase III models to CPTAC validation cohort.
# Computes per-cancer:
#   C-index + Bootstrap 95% CI + Permutation p-value
#   Time-dependent AUC (1/3/5yr, survivalROC, >=20 events)
#   Brier Score + IBS (Cox-based, >=20 events)
#   Calibration slope + O/E ratio (>=10 events)
#   Cross-cohort rank correlation (TCGA vs CPTAC)
# Output: cptac_validation_results.tsv
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

# survivalROC for AUC
has_survROC <- requireNamespace("survivalROC", quietly=TRUE)
if (!has_survROC)
  tryCatch({install.packages("survivalROC",repos="http://cran.us.r-project.org"); library(survivalROC)},
           error=function(e) NULL)

setwd("~/students/aluno0549-6/PHASE_III/PHASE_III_Megarun_5_0_complete_ZIMA_Suit_Generator_final")
sink("cptac_validation.log")

MODELS_DIR <- "PHASE_III_ML_Models"
CPTAC_DIR  <- "CPTAC_validation_matrices"
DF_ROOT    <- "~/students/aluno0549-6/dfXXX_series"

strata <- list(
  list(cancer="BRCA", df="df008"), list(cancer="COAD", df="df368"),
  list(cancer="GBM",  df="df017"), list(cancer="HNSC", df="df017"),
  list(cancer="KIRC", df="df161"), list(cancer="LUAD", df="df017"),
  list(cancer="LUSC", df="df147"), list(cancer="OV",   df="df377"),
  list(cancer="PAAD", df="df377"), list(cancer="UCEC", df="df377"))

results <- list()
counter  <- 0

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
  
  # Exact survival filter (matching training)
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
  
  # 35% gate
  keep <- rowSums(is.na(X_full))/ncol(X_full) < 0.35
  X_full <- X_full[keep, , drop=FALSE]
  df_train <- df_train[keep, ]
  
  # Align to bundle
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
  if (!is.null(bundle$MTLR)) {
    X_m <- as.data.frame(X_full); names(X_m) <- make.names(names(X_m), unique=TRUE)
    mf <- intersect(names(X_m), rownames(bundle$MTLR$weight_matrix))
    if (length(mf) > 0) {
      Xs <- X_m[, mf, drop=FALSE]
      if (anyNA(Xs)) Xs <- tryCatch(missForest::missForest(Xs, maxiter=5, ntree=50, verbose=FALSE)$ximp, error=function(e) Xs)
      pm <- tryCatch(predict(bundle$MTLR, cbind(time=1, status=0, Xs)), error=function(e) NULL)
      if (!is.null(pm))
        mtlr_train_risk <- if (ncol(pm)==nrow(X_full)) -colSums(pm) else -rowSums(pm)
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
  
  if (!is.null(bundle$MTLR)) {
    Xm <- as.data.frame(X_val); names(Xm) <- make.names(names(Xm), unique=TRUE)
    mf <- intersect(names(Xm), rownames(bundle$MTLR$weight_matrix))
    if (length(mf) > 0) {
      Xs <- Xm[, mf, drop=FALSE]
      if (anyNA(Xs)) Xs <- tryCatch(missForest::missForest(Xs, maxiter=5, ntree=50, verbose=FALSE)$ximp, error=function(e) Xs)
      pm <- tryCatch(predict(bundle$MTLR, cbind(time=1,status=0,Xs)), error=function(e) NULL)
      if (!is.null(pm)) fpred$MTLR_Risk <- if (ncol(pm)==nrow(X_val)) -colSums(pm) else -rowSums(pm)
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
  
  # ── 8. Metrics ───────────────────────────────────────────────────────────
  m <- data.frame(time=cptac_sub$OS.time, status=cptac_sub$OS, risk=sl_risk)
  m <- m[!is.na(m$risk) & !is.na(m$time) & !is.na(m$status) & m$time>0, ]
  n_val <- nrow(m); n_ev <- sum(m$status==1, na.rm=TRUE)
  
  # C-index
  c_idx <- tryCatch(concordance(Surv(time,status) ~ risk, data=m, reverse=TRUE)$concordance, error=function(e) NA)
  
  # Permutation p-value
  p_perm <- NA
  if (!is.na(c_idx) && n_ev >= 5) {
    set.seed(123 + counter)
    perm_c <- replicate(1000, {
      mp <- m; mp$status <- sample(m$status)
      tryCatch(concordance(Surv(time,status) ~ risk, data=mp, reverse=TRUE)$concordance, error=function(e) NA)
    })
    perm_c <- perm_c[!is.na(perm_c)]
    if (length(perm_c) > 100) p_perm <- sum(perm_c >= c_idx) / length(perm_c)
  }
  
  # Bootstrap CI
  c_boot <- NA; ci_low <- NA; ci_high <- NA
  if (!is.na(c_idx) && n_ev >= 20) {
    set.seed(42 + counter)
    boot_c <- replicate(1000, {
      ib <- sample(1:nrow(m), replace=TRUE)
      tryCatch(concordance(Surv(time,status) ~ risk, data=m[ib,], reverse=TRUE)$concordance, error=function(e) NA)
    })
    boot_c <- boot_c[!is.na(boot_c)]
    if (length(boot_c) > 100) { c_boot <- mean(boot_c); ci_low <- quantile(boot_c, 0.025); ci_high <- quantile(boot_c, 0.975) }
  }
  
  # Calibration slope
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
  
  # AUC (>=20 events)
  auc1 <- NA; auc3 <- NA; auc5 <- NA; brier <- NA; ibs <- NA
  if (n_ev >= 20 && has_survROC) {
    auc1 <- tryCatch(survivalROC::survivalROC(Stime=m$time, status=m$status, marker=m$risk, predict.time=365, method="KM")$AUC, error=function(e) NA)
    auc3 <- tryCatch(survivalROC::survivalROC(Stime=m$time, status=m$status, marker=m$risk, predict.time=1095, method="KM")$AUC, error=function(e) NA)
    auc5 <- tryCatch(survivalROC::survivalROC(Stime=m$time, status=m$status, marker=m$risk, predict.time=1825, method="KM")$AUC, error=function(e) NA)
    brier <- tryCatch({
      cal <- coxph(Surv(time,status) ~ risk, data=m)
      s <- summary(survfit(cal, newdata=m), times=365)$surv
      if (is.null(s) || length(s)!=nrow(m)) s <- rep(0.5, nrow(m))
      s[is.na(s)] <- 0.5
      obs <- ifelse(m$time>365, 1, ifelse(m$status==1, 0, NA))
      mean((s-obs)^2, na.rm=TRUE)
    }, error=function(e) NA)
    ibs <- tryCatch({
      cal <- coxph(Surv(time,status) ~ risk, data=m)
      sf <- survfit(cal, newdata=m)
      ts <- seq(30, min(1825, max(m$time)), length.out=20)
      mean(sapply(ts, function(t) {
        s <- summary(sf, times=t)$surv
        if (is.null(s) || length(s)!=nrow(m)) return(NA)
        s[is.na(s)] <- 0.5
        obs <- ifelse(m$time>t, 1, ifelse(m$status==1, 0, NA))
        mean((s-obs)^2, na.rm=TRUE)
      }), na.rm=TRUE)
    }, error=function(e) NA)
  }
  
  cat(sprintf("  C=%.4f, p=%.3f, CI=[%.3f-%.3f], slope=%.3f, AUC1=%.3f\n",
              c_idx, if(is.na(p_perm)) 0 else p_perm,
              if(is.na(ci_low)) 0 else ci_low, if(is.na(ci_high)) 0 else ci_high,
              if(is.na(cal_slope)) 0 else cal_slope,
              if(is.na(auc1)) 0 else auc1))
  
  results[[cancer]] <- list(
    cohort=cohort, cancer=cancer, n=n_val, n_events=n_ev,
    c_index=round(c_idx,4), p_perm=round(p_perm,4),
    c_boot=round(c_boot,4), ci_low=round(ci_low,4), ci_high=round(ci_high,4),
    cal_slope=round(cal_slope,4), cal_se=round(cal_se,4), oe_ratio=round(oe_ratio,4),
    auc_1yr=round(auc1,4), auc_3yr=round(auc3,4), auc_5yr=round(auc5,4),
    brier_1yr=round(brier,6), ibs=round(ibs,4))
}

# ── 9. Cross-cohort rank correlation ────────────────────────────────────────
cat("\n=== CROSS-COHORT RANK CORRELATION ===\n")
if (length(results) >= 3) {
  cptac_c <- sapply(results, `[[`, "c_index")
  # TCGA C-indices from manuscript Table S10
  tcga_c <- c(BRCA=0.759, COAD=0.656, GBM=0.752, HNSC=0.764, KIRC=0.686,
              LUAD=0.708, LUSC=0.610, OV=0.755, PAAD=0.664, UCEC=0.843)
  common <- intersect(names(cptac_c), names(tcga_c))
  if (length(common) >= 5) {
    rho <- cor(cptac_c[common], tcga_c[common], method="spearman")
    cat(sprintf("  Spearman rho = %.3f (n=%d cancers)\n", rho, length(common)))
  }
}

# ── 10. Event-stratified summary ─────────────────────────────────────────────
cat("\n=== EVENT-STRATIFIED SUMMARY ===\n")
hi <- sapply(results, function(r) if (!is.na(r$c_index) && r$n_events >= 20) r$c_index else NA)
lo <- sapply(results, function(r) if (!is.na(r$c_index) && r$n_events < 20) r$c_index else NA)
hi <- hi[!is.na(hi)]; lo <- lo[!is.na(lo)]
if (length(hi) > 0) cat(sprintf("  >=20 events (n=%d): median C=%.3f\n", length(hi), median(hi)))
if (length(lo) > 0) cat(sprintf("  <20 events  (n=%d): median C=%.3f\n", length(lo), median(lo)))

# ── Save ────────────────────────────────────────────────────────────────────
dt <- do.call(rbind, lapply(results, as.data.frame, stringsAsFactors=FALSE))
write.table(dt, "cptac_validation_results.tsv", sep="\t", row.names=FALSE, quote=FALSE)
cat(sprintf("\n=== DONE: %d cancers ===\n", nrow(dt)))
sink()

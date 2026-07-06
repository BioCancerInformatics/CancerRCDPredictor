# =========================================================================
# ZIMA PROBABILITY EXTRACTOR (TASK 3)
# Purpose: Convert continuous validation Z-scores into absolute clinical
#          probabilities (1, 3, 5 Years) for CancerRCDShiny using the
#          Phase III Baseline Hazard.
# =========================================================================

local_lib <- "~/R/library"
if (!dir.exists(local_lib)) dir.create(local_lib, recursive = TRUE)
.libPaths(c(local_lib, .libPaths()))

suppressPackageStartupMessages({
  library(dplyr)
  library(randomForestSRC)
  library(xgboost)
  library(MTLR)
  library(data.table)
  library(doParallel)
  library(foreach)
  library(survival)
  library(glmnet)
})

ZIMA_ROOT <- "~/students/aluno0549-6/PHASE_III/PHASE_III_Megarun_5_0_complete_ZIMA_Suit_Generator_final/ML_Internal_validation_dataset"
VAL_DIR <- ZIMA_ROOT
DF_ROOT <- "~/students/aluno0549-6/dfXXX_series"
MODELS_DIR <- "~/students/aluno0549-6/PHASE_III/PHASE_III_Megarun_5_0_complete_ZIMA_Suit_Generator_final/PHASE_III_ML_Models"

PRED_DIR <- file.path(VAL_DIR, "Blind_Predictions")
OUTPUT_DIR <- file.path(VAL_DIR, "CancerRCDShiny_Blind_Clinical_Probabilities")
if(!dir.exists(OUTPUT_DIR)) dir.create(OUTPUT_DIR, recursive=TRUE)

setwd(VAL_DIR)

cat("\n[Phase III Anchoring] Booting Strict Dual-Track Probability Extractor...\n")

# Detect successful blind prediction files
pred_files <- list.files(PRED_DIR, pattern = "_Blind_Predictions\\.tsv$", full.names = TRUE)
if(length(pred_files) == 0) {
    stop("CRITICAL ERROR: No blind predictions found in ", PRED_DIR)
}

cat(sprintf("-> Discovered %d validated ML cohorts ready for extraction.\n", length(pred_files)))

num_cores <- parallel::detectCores() - 1
if (num_cores < 1) num_cores <- 1
cat(sprintf("-> Initializing Parallel Mapping with %d cores...\n", num_cores))
cl <- parallel::makeCluster(num_cores, outfile = "")
parallel::clusterEvalQ(cl, {
  local_lib <- "~/R/library"
  .libPaths(c(local_lib, .libPaths()))
})
doParallel::registerDoParallel(cl)

execution_log <- foreach(p_file = pred_files, .packages = c("dplyr", "survival", "xgboost", "randomForestSRC", "MTLR"), .combine=rbind) %dopar% {
    
    # Extract unit_id from filename (e.g. ACC_DSS_df377_Blind_Predictions.tsv)
    filename <- basename(p_file)
    unit_id <- gsub("_Blind_Predictions\\.tsv$", "", filename)
    
    parts <- strsplit(unit_id, "_")[[1]]
    cancer_type <- parts[1]
    m <- parts[2] # metric: OS, DSS, DFI, PFI
    
    m_folder <- file.path(MODELS_DIR, unit_id)
    bundle_path <- file.path(m_folder, paste0("model_bundle_", unit_id, ".rds"))
    weights_path <- file.path(m_folder, "MVL_Synthesis", paste0(unit_id, "_MVL_Algorithm_Weights.tsv"))
    
    if(!file.exists(bundle_path) || !file.exists(weights_path)) {
        return(data.frame(Unit = unit_id, Status = "FAILED: Missing Phase III Bundle", stringsAsFactors=FALSE))
    }
    
    bundle <- readRDS(bundle_path)
    weights <- read.delim(weights_path, stringsAsFactors = FALSE)
    
    row_def <- bundle$meta
    d <- row_def$df
    clean_d <- ifelse(grepl("\\.rds$", d, ignore.case = TRUE), d, paste0(d, ".rds"))
    df_train_path <- file.path(DF_ROOT, clean_d)
    
    if(!file.exists(df_train_path)) {
        return(data.frame(Unit = unit_id, Status = "FAILED: Missing Phase III matrix", stringsAsFactors=FALSE))
    }
    
    # 1. LOAD AND PREPARE PHASE III DERIVATION COHORT (ANCHOR)
    df_train_raw <- readRDS(df_train_path)
    df_train_cancer <- df_train_raw[df_train_raw$type == cancer_type, , drop = FALSE]
    
    keep_surv <- !is.na(df_train_cancer[[m]]) & 
                 !is.na(df_train_cancer[[paste0(m, ".time")]]) &
                 df_train_cancer[[paste0(m, ".time")]] >= 0
    df_train_cancer <- df_train_cancer[keep_surv, , drop=FALSE]
    
    pattern <- paste0("^", cancer_type, "-")
    preds <- names(df_train_cancer)[grepl(pattern, names(df_train_cancer))]
    X_train <- df_train_cancer[, preds, drop = FALSE]
    X_train[] <- lapply(X_train, function(col) {
      if (is.character(col) || is.factor(col)) { as.numeric(as.factor(col)) } else { as.numeric(col) }
    })
    
    na_props <- rowSums(is.na(X_train)) / ncol(X_train)
    exclude_idx <- which(na_props >= 0.35)
    if (length(exclude_idx) > 0) {
        X_train <- X_train[-exclude_idx, , drop = FALSE]
        df_train_cancer <- df_train_cancer[-exclude_idx, , drop = FALSE]
    }
    
    # Extract exact Phase III times and events
    y_time <- as.numeric(df_train_cancer[[paste0(m, ".time")]])
    y_event <- as.numeric(df_train_cancer[[m]])
    
    # Safe Zero-Time Anchor to prevent Coxph internal failure without dropping Phase III rows
    y_time[y_time <= 0] <- 0.001
    
    # 2. RECONSTRUCT PHASE III TRAINING RISKS TO FIT BASELINE HAZARD
    model_xgb <- bundle$XGBoost
    dxgb_train <- xgb.DMatrix(data = as.matrix(X_train))
    xgb_train_risk <- as.numeric(predict(model_xgb, dxgb_train))
    
    # Get other component risks to synthesize SL
    rsf_train_risk <- as.numeric(bundle$RSF$predicted.oob)
    mtlr_train_risk <- tryCatch(as.numeric(attr(bundle$MTLR, "apparent_risk")), error = function(e) rep(0, nrow(X_train)))
    if(is.null(mtlr_train_risk)) mtlr_train_risk <- rep(0, nrow(X_train))
    
    boruta_train_risk <- rep(0, nrow(X_train))
    if (!is.null(bundle$Boruta)) {
      boruta_decision <- bundle$Boruta$finalDecision
      boruta_features <- names(boruta_decision)[boruta_decision %in% c("Confirmed", "Tentative")]
      if(length(boruta_features) > 0) {
         boruta_x <- as.data.frame(X_train)[, boruta_features, drop=FALSE]
         boruta_df <- cbind(data.frame(time = y_time, status = y_event), boruta_x)
         b_mod <- tryCatch(randomForestSRC::rfsrc(Surv(time, status) ~ ., data = boruta_df, ntree = 500, na.action = "na.impute", splitrule = "logrank", seed = 42), error = function(e) NULL)
         if(!is.null(b_mod)) boruta_train_risk <- as.numeric(b_mod$predicted.oob)
      }
    }
    
    mvl_train_matrix <- data.frame(RSF = rsf_train_risk, XGBoost = xgb_train_risk, MTLR = mtlr_train_risk, Boruta = boruta_train_risk)
    for(col in colnames(mvl_train_matrix)) {
       if(var(mvl_train_matrix[[col]], na.rm=TRUE) == 0 || is.na(var(mvl_train_matrix[[col]], na.rm=TRUE))) {
          mvl_train_matrix[[col]] <- mvl_train_matrix[[col]] + runif(nrow(mvl_train_matrix), -1e-6, 1e-6)
       }
    }
    
    X_meta_train <- scale(as.matrix(mvl_train_matrix))
    X_meta_train[is.na(X_meta_train)] <- 0
    sl_train_risk <- rep(0, nrow(X_meta_train))
    for(w_idx in 1:nrow(weights)) {
        algo <- weights$Dimension[w_idx]
        if(algo %in% colnames(X_meta_train)) {
            sl_train_risk <- sl_train_risk + (X_meta_train[, algo] * weights$Elastic_Net_Weight[w_idx])
        }
    }
    
    # INJECT REGULARIZING NOISE TO BREAK PERFECT SEPARATION AND FORCE COX CONVERGENCE
    # This prevents the 'Ran out of iterations' and 'infinite coefficients' warnings
    # which silently produce NaN (NA) survival extrapolations for extreme validation risks.
    set.seed(42)
    sl_train_risk <- sl_train_risk + runif(length(sl_train_risk), -0.01, 0.01)
    xgb_train_risk <- xgb_train_risk + runif(length(xgb_train_risk), -0.01, 0.01)
    
    # 3. FIT EXACT PHASE III COX BASELINE MODELS
    cox_sl <- tryCatch(coxph(Surv(y_time, y_event) ~ risk, data=data.frame(y_time=y_time, y_event=y_event, risk=sl_train_risk), x=TRUE, y=TRUE), error=function(e) NULL)
    cox_xgb <- tryCatch(coxph(Surv(y_time, y_event) ~ risk, data=data.frame(y_time=y_time, y_event=y_event, risk=xgb_train_risk), x=TRUE, y=TRUE), error=function(e) NULL)
    
    # BASELINE CONVERGENCE AUDIT: Check for perfect separation (diverging coefficients, singular matrices, or C-engine crashes)
    check_divergence <- function(cfit, test_risks) {
        if(is.null(cfit)) return("FAILED: Model Aborted (NULL)")
        if(is.na(cfit$coefficients[1]) || abs(cfit$coefficients[1]) > 10) return("DIVERGED: Perfect Separation (Inf Coef)")
        if(is.na(cfit$var[1,1]) || is.nan(cfit$var[1,1])) return("DIVERGED: Singular Variance Matrix")
        
        valid_risks <- test_risks[!is.na(test_risks) & is.finite(test_risks)]
        if(length(valid_risks) > 0) {
            c_crash <- tryCatch({
                invisible(survfit(cfit, newdata=data.frame(risk=valid_risks[1])))
                FALSE
            }, error=function(e) TRUE)
            
            if(c_crash) return("DIVERGED: C-Engine Mapping Crash (Hidden Topology)")
        }
        
        return("CONVERGED")
    }
    # 4. LOAD BLIND VALIDATION PREDICTIONS
    val_preds <- read.delim(p_file, stringsAsFactors = FALSE)
    N <- nrow(val_preds)
    
    audit_sl <- check_divergence(cox_sl, as.numeric(val_preds$SuperLearner_Risk))
    audit_xgb <- check_divergence(cox_xgb, as.numeric(val_preds$XGBoost_Risk))
    
    if(audit_sl != "CONVERGED" || audit_xgb != "CONVERGED") {
        audit_file <- file.path(OUTPUT_DIR, "ZIMA_Baseline_Convergence_Audit.tsv")
        audit_line <- data.frame(Unit=unit_id, Metric=m, Cancer=cancer_type, SuperLearner_Status=audit_sl, XGBoost_Status=audit_xgb, stringsAsFactors=FALSE)
        write.table(audit_line, file=audit_file, sep="\t", append=TRUE, col.names=!file.exists(audit_file), row.names=FALSE, quote=FALSE)
    }
    
    if(is.null(cox_sl) || is.null(cox_xgb)) {
        return(data.frame(Unit = unit_id, Status = "FAILED: Cox Baseline Mapping Aborted", stringsAsFactors=FALSE))
    }
    
    final_prob_df <- data.frame(
       Sample_ID = val_preds$Sample_ID,
       Inference_Path = rep("NA", N),
       Clinical_Z_Score = rep(NA, N),
       Prob_1Yr = rep(NA, N),
       Prob_3Yr = rep(NA, N),
       Prob_5Yr = rep(NA, N),
       stringsAsFactors = FALSE
    )
    
    # Extract Probabilities
    get_prob <- function(cfit, new_risk, time_target) {
        if(is.na(new_risk) || !is.finite(new_risk)) return(NA)
        tryCatch({
            sfit <- survfit(cfit, newdata = data.frame(risk = new_risk))
            s_sum <- summary(sfit, times = time_target, extend = TRUE)
            if(length(s_sum$surv) == 0) return(1.0) # Handle times before first event
            as.numeric(s_sum$surv)
        }, error = function(e) {
            cat(sprintf("\n[SURVFIT FATAL ERROR] Time: %s | Risk: %s | Error: %s\n", time_target, new_risk, e$message))
            return(NA)
        })
    }
    
    target_times <- c(365, 1095, 1825)
    
    # Apply DUAL-TRACK routing
    for(i in 1:N) {
        # FORCE PURE NUMERIC CAST TO PREVENT SURVFIT TEXT REJECTION
        sl_r <- as.numeric(val_preds$SuperLearner_Risk[i])
        xgb_r <- as.numeric(val_preds$XGBoost_Risk[i])
        
        if(!is.na(sl_r)) {
            # Path A
            final_prob_df$Inference_Path[i] <- "SuperLearner (Path A)"
            final_prob_df$Clinical_Z_Score[i] <- sl_r
            
            p1 <- get_prob(cox_sl, sl_r, target_times[1])
            p3 <- get_prob(cox_sl, sl_r, target_times[2])
            p5 <- get_prob(cox_sl, sl_r, target_times[3])
            
        } else {
            # Path B
            final_prob_df$Inference_Path[i] <- "XGBoost Fallback (Path B)"
            final_prob_df$Clinical_Z_Score[i] <- xgb_r
            
            p1 <- get_prob(cox_xgb, xgb_r, target_times[1])
            p3 <- get_prob(cox_xgb, xgb_r, target_times[2])
            p5 <- get_prob(cox_xgb, xgb_r, target_times[3])
        }
        
        # Clinical Conversion: OS/DSS = Survival Prob, DFI/PFI = Event Prob (1 - S(t))
        if(m %in% c("DFI", "PFI")) {
            final_prob_df$Prob_1Yr[i] <- ifelse(!is.na(p1), 1 - p1, NA)
            final_prob_df$Prob_3Yr[i] <- ifelse(!is.na(p3), 1 - p3, NA)
            final_prob_df$Prob_5Yr[i] <- ifelse(!is.na(p5), 1 - p5, NA)
        } else {
            final_prob_df$Prob_1Yr[i] <- p1
            final_prob_df$Prob_3Yr[i] <- p3
            final_prob_df$Prob_5Yr[i] <- p5
        }
    }
    
    # Clean up bounds just in case of numeric artifacting
    final_prob_df$Prob_1Yr <- pmin(pmax(final_prob_df$Prob_1Yr, 0), 1)
    final_prob_df$Prob_3Yr <- pmin(pmax(final_prob_df$Prob_3Yr, 0), 1)
    final_prob_df$Prob_5Yr <- pmin(pmax(final_prob_df$Prob_5Yr, 0), 1)
    
    write.table(final_prob_df, file.path(OUTPUT_DIR, paste0(unit_id, "_Clinical_Probabilities.tsv")), sep="\t", row.names=FALSE, quote=FALSE)
    
    return(data.frame(Unit = unit_id, Status = "SUCCESS", stringsAsFactors=FALSE))
}

parallel::stopCluster(cl)

log_file <- file.path(VAL_DIR, "ZIMA_Probability_Extraction_Log.tsv")
write.table(execution_log, log_file, sep="\t", row.names=FALSE, quote=FALSE)
cat(sprintf("\n✅ PROBABILITY EXTRACTION COMPLETED. Matrices saved to: %s\n", OUTPUT_DIR))


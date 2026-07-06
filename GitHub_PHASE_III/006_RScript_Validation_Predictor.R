# =========================================================================
# ZIMA VALIDATION PREDICTOR (DUAL-TRACK INFERENCE ENGINE)
# Purpose: Generate Blind Prognostic Risks for Rejected Internal Validation Cohorts
# =========================================================================

# --- BYPASS ZIMA SERVER PERMISSION ERRORS ---
local_lib <- "~/R/library"
if (!dir.exists(local_lib)) dir.create(local_lib, recursive = TRUE)
.libPaths(c(local_lib, .libPaths()))


# Auto-Install Missing Dependencies
required_cran <- c("dplyr", "randomForestSRC", "xgboost", "survival", "glmnet", "missForest", "remotes", "MTLR", "doParallel", "foreach", "data.table")
for(pkg in required_cran) {
  if(!requireNamespace(pkg, quietly = TRUE)) {
    cat(sprintf("Installing missing package: %s\n", pkg))
    install.packages(pkg, repos = "http://cran.us.r-project.org")
  }
}

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
  library(missForest)
})

ZIMA_ROOT <- "~/students/aluno0549-6/PHASE_III/PHASE_III_Megarun_5_0_complete_ZIMA_Suit_Generator_final/ML_Internal_validation_dataset"

VAL_DIR <- ZIMA_ROOT
DF_ROOT <- "~/students/aluno0549-6/dfXXX_series"
MODELS_DIR <- "~/students/aluno0549-6/PHASE_III/PHASE_III_Megarun_5_0_complete_ZIMA_Suit_Generator_final/PHASE_III_ML_Models"
OUTPUT_DIR <- file.path(VAL_DIR, "Blind_Predictions")
if(!dir.exists(OUTPUT_DIR)) dir.create(OUTPUT_DIR, recursive=TRUE)

setwd(VAL_DIR)

cat("\n[Gating Pipeline] Loading Master PRISTINE Dataset...\n")
master_pristine_path <- file.path(VAL_DIR, "ML_internal_validation_dataset_PRISTINE.rds")
master_pristine_df <- readRDS(master_pristine_path)
cancer_types <- unique(master_pristine_df$type)
cancer_types <- cancer_types[!is.na(cancer_types) & cancer_types != ""]

cat(sprintf("-> Detected %d unique cancer types covering %d pristine samples.\n", length(cancer_types), nrow(master_pristine_df)))

cat("\n🚀 STARTING ZIMA BLIND PREDICTIONS (PARALLEL DUAL-TRACK)...\n")

num_cores <- parallel::detectCores() - 1
if (num_cores < 1) num_cores <- 1
cat(sprintf("-> Initializing Parallel Cluster with %d cores...\n", num_cores))
cl <- parallel::makeCluster(num_cores, outfile = "")

# Force all parallel workers to recognize the local personal library bypass!
parallel::clusterEvalQ(cl, {
  local_lib <- "~/R/library"
  .libPaths(c(local_lib, .libPaths()))
})

doParallel::registerDoParallel(cl)

execution_log <- foreach(c_type = cancer_types, .packages = required_cran, .combine=rbind) %dopar% {
  cancer_type <- c_type
  cat(sprintf("\n=== Processing Validation Cohort: %s ===\n", cancer_type))
  
  # Gate samples directly from the master pristine dataframe
  df_val <- master_pristine_df[master_pristine_df$type == cancer_type, , drop = FALSE]
  if(nrow(df_val) == 0) return(NULL)
  
  model_folders <- list.dirs(MODELS_DIR, recursive = FALSE, full.names = TRUE)
  cohort_models <- model_folders[grepl(paste0("^", cancer_type, "_"), basename(model_folders))]
  
  if(length(cohort_models) == 0) {
    cat("No Phase III models found for", cancer_type, "\n")
    return(NULL)
  }
  
  cohort_log <- data.frame()
  
    for(m_folder in cohort_models) {
    unit_id <- basename(m_folder)
    cat(sprintf(" -> Evaluating Endpoint Model: %s\n", unit_id))
    
    mtlr_err_msg <- "None"
    boruta_err_msg <- "None"
    
    endpoint_log <- tryCatch({
    
    bundle_path <- file.path(m_folder, paste0("model_bundle_", unit_id, ".rds"))
    weights_path <- file.path(m_folder, "MVL_Synthesis", paste0(unit_id, "_MVL_Algorithm_Weights.tsv"))
    
    if(!file.exists(bundle_path) || !file.exists(weights_path)) {
      cat("    Missing bundle or weights, skipping.\n")
      next
    }
    
    bundle <- readRDS(bundle_path)
    weights <- read.delim(weights_path, stringsAsFactors = FALSE)
    
    row_def <- bundle$meta
    d <- row_def$df
    clean_d <- ifelse(grepl("\\.rds$", d, ignore.case = TRUE), d, paste0(d, ".rds"))
    df_train_path <- file.path(DF_ROOT, clean_d)
    
    if(!file.exists(df_train_path)) {
      cat("    Cannot find original training matrix", clean_d, "skipping.\n")
      next
    }
    
    # =========================================================================
    # 2. ON-THE-FLY RECONSTRUCTION (B_MOD & SCALING)
    # =========================================================================
    df_train_raw <- readRDS(df_train_path)
    df_train_cancer <- df_train_raw[df_train_raw$type == cancer_type, , drop = FALSE]
    
    # Extract exact Metric (e.g., DSS, OS) from unit_id (e.g., ACC_DSS_df377)
    parts <- strsplit(unit_id, "_")[[1]]
    m <- parts[2]
    
    # [Megarun 5.0 Match]: 1. Strict Survival Masking
    keep_surv <- !is.na(df_train_cancer[[m]]) & 
                 !is.na(df_train_cancer[[paste0(m, ".time")]]) &
                 df_train_cancer[[paste0(m, ".time")]] >= 0
    df_train_cancer <- df_train_cancer[keep_surv, , drop=FALSE]
    
    pattern <- paste0("^", cancer_type, "-")
    preds <- names(df_train_cancer)[grepl(pattern, names(df_train_cancer))]
    X_train <- df_train_cancer[, preds, drop = FALSE]
    X_train[] <- lapply(X_train, function(col) {
      if (is.character(col) || is.factor(col)) {
        as.numeric(as.factor(col))
      } else {
        as.numeric(col)
      }
    })
    
    # [Megarun 5.0 Match]: 2. Geometric Patient Exclusion (>=35% NAs)
    na_props <- rowSums(is.na(X_train)) / ncol(X_train)
    exclude_idx <- which(na_props >= 0.35)
    if (length(exclude_idx) > 0) {
        X_train <- X_train[-exclude_idx, , drop = FALSE]
        df_train_cancer <- df_train_cancer[-exclude_idx, , drop = FALSE]
    }
    
    model_xgb <- bundle$XGBoost
    model_rsf <- bundle$RSF
    model_mtlr <- bundle$MTLR
    model_boruta <- bundle$Boruta
    
    xgb_features <- model_xgb$feature_names
    
    # Extract Boruta Features
    boruta_features <- NULL
    if (!is.null(model_boruta)) {
      boruta_decision <- model_boruta$finalDecision
      boruta_features <- names(boruta_decision)[boruta_decision %in% c("Confirmed", "Tentative")]
    }
    
    b_mod <- NULL
    if(length(boruta_features) > 0) {
       boruta_x <- as.data.frame(X_train)[, boruta_features, drop=FALSE]
       # Reconstruct Boruta Proxy using the TRUE Phase III time and status!
       boruta_df <- cbind(data.frame(time = as.numeric(df_train_cancer[[paste0(m, ".time")]]), 
                                     status = as.numeric(df_train_cancer[[m]])), 
                          boruta_x)
       b_mod <- tryCatch({
          randomForestSRC::rfsrc(Surv(time, status) ~ ., 
                                 data = boruta_df, 
                                 ntree = 500,
                                 na.action = "na.impute",
                                 splitrule = "logrank",
                                 seed = 42)
       }, error = function(e) NULL)
    }
    
    # Re-extract the exact training risks to calculate perfect SuperLearner scaling parameters!
    dxgb_train <- xgb.DMatrix(data = as.matrix(X_train))
    xgb_train_risk <- predict(model_xgb, dxgb_train)
    
    rsf_train_risk <- model_rsf$predicted.oob
    
    mtlr_train_risk <- tryCatch(attr(model_mtlr, "apparent_risk"), error = function(e) rep(0, nrow(X_train)))
    if(is.null(mtlr_train_risk)) mtlr_train_risk <- rep(0, nrow(X_train))
    
    boruta_train_risk <- rep(0, nrow(X_train))
    if(!is.null(b_mod)) {
       boruta_train_risk <- b_mod$predicted.oob
    }
    
    # Extract Training Medians to anchor 100% missing validation columns
    train_medians <- apply(X_train, 2, median, na.rm=TRUE)
    
    mvl_train_matrix <- data.frame(
      RSF = as.numeric(rsf_train_risk),
      XGBoost = as.numeric(xgb_train_risk),
      MTLR = as.numeric(mtlr_train_risk),
      Boruta = as.numeric(boruta_train_risk)
    )
    
    for(col in colnames(mvl_train_matrix)) {
       if(var(mvl_train_matrix[[col]], na.rm=TRUE) == 0 || is.na(var(mvl_train_matrix[[col]], na.rm=TRUE))) {
          mvl_train_matrix[[col]] <- mvl_train_matrix[[col]] + runif(nrow(mvl_train_matrix), -1e-6, 1e-6)
       }
    }
    
    # 100% PERFECT TRAINING SCALING PARAMETERS
    X_meta_train <- scale(as.matrix(mvl_train_matrix))
    train_center <- attr(X_meta_train, "scaled:center")
    train_scale <- attr(X_meta_train, "scaled:scale")
    
    # =========================================================================
    # 3. FULL SUPERLEARNER VALIDATION INFERENCE (EXACT PHASE III REFLECTION)
    # =========================================================================
    missing_feats <- setdiff(xgb_features, colnames(df_val))
    if(length(missing_feats) > 0) {
      for(f in missing_feats) df_val[[f]] <- NA
    }
    
    X_val <- df_val[, xgb_features, drop=FALSE]
    X_val[] <- lapply(X_val, function(col) {
      if (is.character(col) || is.factor(col)) {
        as.numeric(as.factor(col))
      } else {
        as.numeric(col)
      }
    })
    rownames(X_val) <- df_val$sample
    
    # Pre-Emptive Apocalyptic Anchor: Inject Training Medians for 100% Missing Columns!
    X_val_anchored <- X_val
    for (col in colnames(X_val_anchored)) {
        if(all(is.na(X_val_anchored[[col]]))) {
            if(col %in% names(train_medians)) {
                X_val_anchored[[col]] <- train_medians[[col]]
            }
        }
    }
    
    final_preds <- data.frame(Sample_ID = df_val$sample, 
                              RSF_Risk = NA,
                              XGBoost_Risk = NA, 
                              Boruta_Risk = NA,
                              MTLR_Risk = NA,
                              SuperLearner_Risk = NA)
    
    # ---------------------------------------------------------
    # XGBoost Prediction (Native NA Handling)
    # ---------------------------------------------------------
    dxgb <- xgb.DMatrix(data = as.matrix(X_val))
    final_preds$XGBoost_Risk <- predict(model_xgb, dxgb)
    
    # ---------------------------------------------------------
    # RSF Prediction (Native Topological Imputation)
    # ---------------------------------------------------------
    rsf_val_df <- data.frame(time = 1, status = 0, as.data.frame(X_val_anchored))
    colnames(rsf_val_df) <- make.names(colnames(rsf_val_df), unique = TRUE)
    pt_rsf_risk <- tryCatch({
        predict(model_rsf, newdata = rsf_val_df, na.action = "na.impute")$predicted
    }, error = function(e) rep(NA, nrow(X_val)))
    final_preds$RSF_Risk <- pt_rsf_risk
    
    # ---------------------------------------------------------
    # MTLR Prediction (Hierarchical Imputation EXACTLY as Megarun 5.0)
    # ---------------------------------------------------------
    if(!is.null(model_mtlr) && inherits(model_mtlr, "mtlr")) {
        X_mtlr <- X_val_anchored
        
        # [Topological Fix]: Do NOT subset X_mtlr! 
        # MTLR's internal formula evaluator (model.frame) demands the EXACT original 
        # training signatures. If MTLR internally dropped features from its weight matrix, 
        # subsetting them here will starve the formula parser and trigger 'object not found'.
        # Passing the full superset allows model.frame to cleanly ignore unused features.
        X_sub <- X_mtlr
        
        orig_names <- colnames(X_sub)
            if (anyNA(X_sub)) {
                X_sub <- tryCatch({
                    mf_obj <- missForest::missForest(X_sub, maxiter = 5, ntree = 50, verbose = FALSE)
                    mf_obj$ximp
                }, error = function(e1) {
                    tryCatch({
                        if(requireNamespace("VIM", quietly = TRUE)) {
                            res <- VIM::kNN(X_sub, k = 5, imp_var = FALSE)
                            res[, 1:ncol(X_sub), drop=FALSE]
                        } else { stop("VIM not installed") }
                    }, error = function(e2) {
                        as.data.frame(lapply(X_sub, function(col) {
                            if(is.numeric(col)) { col[is.na(col)] <- median(col, na.rm=TRUE) }
                            col
                        }), check.names = FALSE)
                    })
                })
            }
            # Final Safety Net: If kNN succeeded but STILL left partial NAs intact
            if (anyNA(X_sub)) {
                X_sub <- as.data.frame(lapply(X_sub, function(col) {
                    if(is.numeric(col)) { col[is.na(col)] <- median(col, na.rm=TRUE) }
                    col
                }), check.names = FALSE)
            }
            colnames(X_sub) <- orig_names
            
            mtlr_val_df <- cbind(time = 1, status = 0, X_sub)
            preds_mtlr <- tryCatch(predict(model_mtlr, mtlr_val_df), error = function(e) {
                # [ESCA_PFI Anomaly Resolution]: If rigorous hyphens fail, Phase III training 
                # likely fell into the Layer 3 fallback and locked onto mangled dots. 
                # Re-attempt with dots to synchronize with the anomalous model geometry.
                mtlr_val_df_mangled <- mtlr_val_df
                colnames(mtlr_val_df_mangled) <- make.names(colnames(mtlr_val_df_mangled), unique=TRUE)
                tryCatch(predict(model_mtlr, mtlr_val_df_mangled), error = function(e2) {
                    mtlr_err_msg <<- e$message # Preserve original hyphen error if both fail
                    NULL
                })
            })
            if(!is.null(preds_mtlr)) {
                if (ncol(preds_mtlr) == nrow(X_val)) {
                    final_preds$MTLR_Risk <- -colSums(preds_mtlr)
                } else if (ncol(preds_mtlr) == (nrow(X_val) + 1)) {
                    final_preds$MTLR_Risk <- -colSums(preds_mtlr[, -1, drop = FALSE])
                } else {
                    final_preds$MTLR_Risk <- -rowSums(preds_mtlr)
                }
            }
    }
    
    # ---------------------------------------------------------
    # Boruta Prediction (Topological Imputation EXACTLY as Megarun 5.0)
    # ---------------------------------------------------------
    if(!is.null(b_mod)) {
        b_df <- cbind(data.frame(time=1, status=0), X_val_anchored[, boruta_features, drop=FALSE])
        # Removed make.names() here to perfectly match b_mod training geometry (hyphens intact)
        final_preds$Boruta_Risk <- tryCatch(predict(b_mod, newdata = b_df, na.action="na.impute")$predicted, error=function(e) {
            boruta_err_msg <<- e$message
            rep(NA, nrow(X_val))
        })
    }
    
    # ---------------------------------------------------------
    # SuperLearner Synthesis (Mathematically Scaled via Training Anchors)
    # ---------------------------------------------------------
    mvl_val <- data.frame(RSF = final_preds$RSF_Risk, XGBoost = final_preds$XGBoost_Risk, MTLR = final_preds$MTLR_Risk, Boruta = final_preds$Boruta_Risk)
    
    # Apply exact training scaling parameters!
    scaled_val <- scale(as.matrix(mvl_val), center = train_center, scale = train_scale)
    scaled_val[is.na(scaled_val)] <- 0
    
    sl_risk_vec <- rep(0, nrow(X_val))
    for(w_idx in 1:nrow(weights)) {
        algo <- weights$Dimension[w_idx]
        if(algo %in% colnames(scaled_val)) {
            sl_risk_vec <- sl_risk_vec + (scaled_val[, algo] * weights$Elastic_Net_Weight[w_idx])
        }
    }
    
    # Only require non-NA risks for algorithms that were ACTUALLY used in the Phase III ensemble!
    valid_mask <- rep(TRUE, nrow(X_val))
    if ("RSF" %in% weights$Dimension) valid_mask <- valid_mask & !is.na(final_preds$RSF_Risk)
    if ("XGBoost" %in% weights$Dimension) valid_mask <- valid_mask & !is.na(final_preds$XGBoost_Risk)
    if ("MTLR" %in% weights$Dimension) valid_mask <- valid_mask & !is.na(final_preds$MTLR_Risk)
    if ("Boruta" %in% weights$Dimension) valid_mask <- valid_mask & !is.na(final_preds$Boruta_Risk)
    
    final_preds$SuperLearner_Risk[valid_mask] <- sl_risk_vec[valid_mask]
    
    write.table(final_preds, file.path(OUTPUT_DIR, paste0(unit_id, "_Blind_Predictions.tsv")), sep="\t", row.names=FALSE, quote=FALSE)
    
    data.frame(Cohort = cancer_type, Endpoint = unit_id, Status = "SUCCESS", 
               MTLR_NAs = sum(is.na(final_preds$MTLR_Risk)),
               Boruta_NAs = sum(is.na(final_preds$Boruta_Risk)),
               SL_NAs = sum(is.na(final_preds$SuperLearner_Risk)),
               Error_Msg = paste("MTLR:", mtlr_err_msg, "| Boruta:", boruta_err_msg), 
               stringsAsFactors=FALSE)
               
    }, error = function(e) {
        data.frame(Cohort = cancer_type, Endpoint = unit_id, Status = "FAILED", 
                   MTLR_NAs = NA, Boruta_NAs = NA, SL_NAs = NA,
                   Error_Msg = e$message, stringsAsFactors=FALSE)
    })
    
    cohort_log <- rbind(cohort_log, endpoint_log)
  }
  return(cohort_log)
}

parallel::stopCluster(cl)

log_file <- file.path(VAL_DIR, "ZIMA_Execution_Log.tsv")
write.table(execution_log, log_file, sep="\t", row.names=FALSE, quote=FALSE)
cat(sprintf("\n✅ ZIMA BLIND PREDICTIONS COMPLETED. Log saved to: %s\n", log_file))

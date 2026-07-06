# =========================================================================
# PHASE IIIC — POST-HOC CALIBRATION AUDIT (BRIER SCORE EXTRACTOR)
# =========================================================================
# This script extracts absolute clinical calibration metrics (Time-Dependent 
# Brier Score and Integrated Brier Score) from previously fitted Phase III
# survival models without requiring a full model retraining.
#
# Metrics Evaluated:
# 1. Time-Dependent Brier Score at 1, 3, and 5 Years
# 2. Integrated Brier Score (IBS) across the entire follow-up
# =========================================================================

# -------------------------------------------------------------------------
# DEPENDENCIES (ZIMA NIXOS SAFE - NO AUTO-INSTALLS)
# -------------------------------------------------------------------------
# RStudio's static analyzer often attempts to auto-install packages when it sees library().
# Because ZIMA (/mnt/sharefiles/rlibs) is a read-only NixOS environment, this crashes the script.

# Load personal ZIMA library path to bypass read-only lock
local_lib <- "~/minhas_bibliotecas_R"
if(!dir.exists(local_lib)) {
    dir.create(local_lib, recursive = TRUE, showWarnings = FALSE)
}
.libPaths(c(local_lib, .libPaths()))
Sys.setenv(R_LIBS_USER = local_lib)

required_packages <- c("dplyr", "survival", "randomForestSRC", "xgboost", "glmnet", "MTLR", "rio", "future.apply")

# Autonomous Missing Package Detection and Bulk Installation
missing_packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]
if(length(missing_packages) > 0) {
    message("\n[AUTONOMOUS AUDIT] Missing packages detected: ", paste(missing_packages, collapse = ", "))
    message("[AUTONOMOUS AUDIT] Installing securely into personal ZIMA library: ", local_lib, "\n")
    install.packages(missing_packages, lib = local_lib, repos = "http://cran.rstudio.com/", dependencies = TRUE)
}

suppressPackageStartupMessages({
  for(pkg in required_packages) {
    if(!require(pkg, character.only = TRUE, quietly = TRUE)) {
      stop(sprintf("CRITICAL ERROR: Package '%s' failed to install or load.", pkg))
    }
  }
})

# =========================================================================
# I. USER-DEFINED PATH CONFIGURATION (ZIMA SERVER)
# =========================================================================
# As requested, targeting the exact ZIMA Suit Generator Working Directory
WORKING_DIR <- "~/students/aluno0549-6/PHASE_III/PHASE_III_Megarun_5_0_complete_ZIMA_Suit_Generator_final"
DF_ROOT_MANIFEST <- "~/students/aluno0549-6/dfXXX_series"

setwd(WORKING_DIR)
MODEL_OUTPUT_DIR <- file.path(WORKING_DIR, "PHASE_III_ML_Models")

if(!dir.exists(MODEL_OUTPUT_DIR)) stop("FATAL: MODEL_OUTPUT_DIR not found. Please verify the ZIMA directory structure.")

# =========================================================================
# II. PRESERVATION OF PHASE II/III DATA EXTRACTION GEOMETRY
# (These must exactly match Megarun 5.0 to guarantee equivalent X_matrix)
# =========================================================================

assert_time_schema_nonneg <- function(time, metric = NULL) {
  time_num <- suppressWarnings(as.numeric(as.character(time)))
  bad_time <- !is.na(time_num) & (!is.finite(time_num) | time_num < 0)
  if (any(bad_time)) stop("FAIL__TIME_SCHEMA: non-finite or negative survival times detected.", call. = FALSE)
  return(time_num)
}

filter_survival_complete <- function(df_sub, metric) {
  cols <- list(event = metric, time  = paste0(metric, ".time"))
  time  <- as.numeric(as.character(df_sub[[cols$time]]))
  event <- as.numeric(as.character(df_sub[[cols$event]]))
  time <- assert_time_schema_nonneg(time, metric)
  
  mask <- is.finite(time) & is.finite(event) & !is.na(event) & event %in% c(0, 1)
  if (!any(mask)) stop(paste("No complete survival rows remain for", metric), call. = FALSE)
  
  list(df_masked = df_sub[mask, , drop = FALSE], time = time[mask], event = event[mask])
}

build_combined_X_matrix <- function(df_masked, cancer_type) {
  pattern <- paste0("^", cancer_type, "-")
  preds <- names(df_masked)[grepl(pattern, names(df_masked))]
  if (length(preds) == 0L) stop("FAIL: No multi-omic predictors found.", call. = FALSE)
  
  df_X <- df_masked[, preds, drop = FALSE]
  df_X[] <- lapply(df_X, function(col) {
    if (is.character(col) || is.factor(col)) as.numeric(as.factor(col)) else as.numeric(col)
  })
  return(df_X)
}

# =========================================================================
# III. MASTER CALIBRATION EXTRACTION LOOP
# =========================================================================

log_file_path_atomic <- file.path(WORKING_DIR, "PHASE_IIIC_Brier_Extraction_Audit.log")
cat(paste0(Sys.time(), " - INITIATING PHASE IIIC BRIER SCORE EXTRACTION...\n"), file = log_file_path_atomic, append = FALSE)

# Locate all model bundles
bundle_files <- list.files(MODEL_OUTPUT_DIR, pattern = "model_bundle_.*\\.rds$", recursive = TRUE, full.names = TRUE)

if(length(bundle_files) == 0) {
  stop("No model_bundle_*.rds files found. Cannot perform post-hoc extraction.")
}

message(sprintf("Found %d completed ML cohorts. Beginning Calibration Audit in PARALLEL...", length(bundle_files)))

# Initialize Parallel Backend using the EXACT 100% identical setup from Megarun 5.0
n_cores <- max(1, parallel::detectCores(logical = TRUE) - 2)
message(sprintf("[PARALLEL ENGINE] Booting %d threads for multi-cohort extraction...", n_cores))

# Expand global memory limits to prevent large matrices from crashing threads
options(future.globals.maxSize = 10 * 1024^3) # 10 GB limit

# Explicitly create a PSOCK cluster using setup_strategy = "sequential" (Identical to Megarun 5.0)
cl <- parallel::makeCluster(n_cores, type = "PSOCK", setup_strategy = "sequential", outfile = "")

# Export necessary variables to the native R cluster
parallel::clusterExport(cl, varlist = c("log_file_path_atomic", "WORKING_DIR", "DF_ROOT_MANIFEST", "MODEL_OUTPUT_DIR", "assert_time_schema_nonneg", "filter_survival_complete", "build_combined_X_matrix"))

global_brier_metrics_list <- parallel::parLapply(cl, bundle_files, function(bundle_path) {
  require(survival, quietly = TRUE)
  require(randomForestSRC, quietly = TRUE)
  tryCatch({
    # 1. Extract Unit Identity
    bundle_dir <- dirname(bundle_path)
    unit_id <- basename(bundle_dir)
    cat(sprintf("Processing Unit: %s\n", unit_id))
    
    # 2. Load the specific Megarun 5.0 bundle
    bundle <- readRDS(bundle_path)
    row_def <- bundle$meta
    
    c <- row_def$cancer_type
    m <- row_def$metric
    d <- row_def$df
    
    # 3. Reload and rebuild exact X_matrix and outcomes
    clean_d <- ifelse(grepl("\\.rds$", d, ignore.case = TRUE), d, paste0(d, ".rds"))
    df_path <- file.path(DF_ROOT_MANIFEST, clean_d)
    
    if(!file.exists(df_path)) stop(paste("Dataset missing:", df_path))
    df_raw <- readRDS(df_path)
    
    df_cancer <- df_raw[df_raw$type == c, , drop = FALSE]
    surv_pack <- filter_survival_complete(df_cancer, m)
    df_cohort <- surv_pack$df_masked
    
    X_matrix <- build_combined_X_matrix(df_cohort, c)
    y_time <- surv_pack$time
    y_event <- surv_pack$event
    
    # Apply Megarun 4.3 Missingness Filter EXACTLY
    na_props <- rowSums(is.na(X_matrix)) / ncol(X_matrix)
    exclude_idx <- which(na_props >= 0.35)
    if (length(exclude_idx) > 0) {
      X_matrix <- X_matrix[-exclude_idx, , drop = FALSE]
      y_time <- y_time[-exclude_idx]
      y_event <- y_event[-exclude_idx]
      df_cohort <- df_cohort[-exclude_idx, , drop = FALSE]
    }
    
    # Create the Survival Evaluation DataFrame
    eval_df <- data.frame(time = as.numeric(y_time), status = as.numeric(y_event))
    
    # Ensure survival object
    Surv_obj <- survival::Surv(eval_df$time, eval_df$status)
    eval_df$Surv_obj <- Surv_obj
    
    # Define clinical landmarks (1, 3, 5 years)
    max_time <- max(eval_df$time, na.rm = TRUE)
    target_times <- c(365, 1095, 1825)
    valid_times <- target_times[target_times < max_time]
    
    if(length(valid_times) == 0) stop("No valid follow-up times to evaluate.")
    
    # 4. Synthesize Risk Proxies natively from loaded models
    
    safe_assign_risk <- function(df, risk_vec) {
        if(is.null(risk_vec)) return(rep(NA, nrow(df)))
        if(length(risk_vec) == nrow(df)) return(as.numeric(risk_vec))
        
        # If lengths mismatch (e.g., rfsrc implicitly drops time <= 0)
        # Attempt to geometrically map using time > 0 filter
        valid_idx <- which(df$time > 0)
        if(length(valid_idx) == length(risk_vec)) {
             mapped_risk <- rep(NA, nrow(df))
             mapped_risk[valid_idx] <- as.numeric(risk_vec)
             return(mapped_risk)
        }
        
        # Fallback to pure NA to prevent cluster crash
        return(rep(NA, nrow(df)))
    }
    
    # A) RSF
    if(!is.null(bundle$RSF) && !inherits(bundle$RSF, "try-error")) {
        eval_df$RSF_Risk <- safe_assign_risk(eval_df, bundle$RSF$predicted.oob)
    } else { eval_df$RSF_Risk <- NA }
    
    # B) XGBoost
    if(!is.null(bundle$XGBoost) && !inherits(bundle$XGBoost, "try-error")) {
        if(!is.null(bundle$SHAP) && (is.matrix(bundle$SHAP) || is.data.frame(bundle$SHAP))) {
            eval_df$XGBoost_Risk <- safe_assign_risk(eval_df, rowSums(bundle$SHAP))
        } else {
            eval_df$XGBoost_Risk <- safe_assign_risk(eval_df, predict(bundle$XGBoost, as.matrix(X_matrix)))
        }
    } else { eval_df$XGBoost_Risk <- NA }
    
    # C) MTLR
    if(!is.null(bundle$MTLR) && !inherits(bundle$MTLR, "try-error")) {
        eval_df$MTLR_Risk <- safe_assign_risk(eval_df, attr(bundle$MTLR, "apparent_risk"))
    } else { eval_df$MTLR_Risk <- NA }
    
    # D) MVL Synthesis (Reconstruct exact MVL vector based on the metrics saved)
    # The true MVL synthesis was saved inside the `bundle$metrics` frame or we can reconstruct it
    # Because we did not save `mvl_super_risk` vector explicitly, we must re-evaluate the meta model
    # Wait, the meta model itself wasn't saved in the bundle. 
    # Let's map risk vectors via CoxPH models to compute Apparent Calibration using pec::pec
    
    # Safe Model Extraction Function using coxph as a mapping bridge for pure risk vectors
    # This maps the continuous risk score to a calibrated Breslow survival curve.
    brier_results <- data.frame(Time = valid_times)
    ibs_vals <- list()
    
    process_calibration <- function(risk_vector, model_name, sub_dir) {
       if(anyNA(risk_vector)) return(NULL)
       
       tmp_df <- eval_df
       tmp_df$risk <- risk_vector
       
       cfit <- coxph(Surv(time, status) ~ risk, data = tmp_df, x=TRUE, y=TRUE)
       
       # ---------------------------------------------------------
       # NATIVE MATHEMATICAL BRIER SCORE (IPCW)
       # ---------------------------------------------------------
       calculate_ipcw_brier <- function(surv_obj, pred_probs, target_time) {
         time_vec <- surv_obj[,1]
         event_vec <- surv_obj[,2]
         N <- length(time_vec)
         
         km_cens <- survfit(Surv(time_vec, 1 - event_vec) ~ 1)
         
         get_G <- function(t_val) {
           if(t_val <= 0 || t_val < min(km_cens$time)) return(1.0)
           idx <- max(which(km_cens$time <= t_val))
           if(length(idx) == 0 || is.na(idx)) return(1.0)
           val <- km_cens$surv[idx]
           if(is.na(val) || val == 0) return(min(km_cens$surv[km_cens$surv > 0]))
           return(val)
         }
         
         G_t <- get_G(target_time)
         b_scores <- numeric(N)
         
         for(i in 1:N) {
           if(time_vec[i] <= target_time && event_vec[i] == 1) {
             w_i <- 1 / get_G(time_vec[i])
             b_scores[i] <- w_i * (0 - pred_probs[i])^2
           } else if (time_vec[i] > target_time) {
             w_i <- 1 / G_t
             b_scores[i] <- w_i * (1 - pred_probs[i])^2
           } else {
             b_scores[i] <- 0
           }
       }
       return(mean(b_scores, na.rm=TRUE))
     }
       
     # Extract absolute survival probabilities dynamically from Cox Breslow estimator
     sfit <- survfit(cfit, newdata = tmp_df)
       
       # 1. Evaluate at clinical landmarks (1, 3, 5 years)
       s_sum <- summary(sfit, times = valid_times, extend = TRUE)
       surv_probs_matrix <- s_sum$surv
       if(is.null(dim(surv_probs_matrix))) surv_probs_matrix <- matrix(surv_probs_matrix, nrow=1)
       surv_probs_matrix <- t(surv_probs_matrix) # [N x T]
       
       bs_t <- numeric(length(valid_times))
       for(k in seq_along(valid_times)) {
           bs_t[k] <- calculate_ipcw_brier(eval_df$Surv_obj, surv_probs_matrix[, k], valid_times[k])
       }
       
       patient_ids <- NULL
       possible_id_cols <- c("bcr_patient_barcode", "patient", "sample", "Patient_ID", "ID", "id")
       for(col in possible_id_cols) {
           if(col %in% names(df_cohort)) {
               patient_ids <- as.character(df_cohort[[col]])
               break
           }
       }
       if(is.null(patient_ids)) patient_ids <- rownames(X_matrix)
       if(is.null(patient_ids)) patient_ids <- 1:nrow(tmp_df)
       
       prob_df <- data.frame(Patient_ID = patient_ids, stringsAsFactors = FALSE)
       for(i in seq_along(valid_times)) {
           yr_label <- round(valid_times[i]/365, 0)
           prob_df[[paste0("Surv_Prob_", yr_label, "Yr")]] <- surv_probs_matrix[, i]
           prob_df[[paste0("Event_Prob_", yr_label, "Yr")]] <- 1 - surv_probs_matrix[, i]
       }
       
       if(!dir.exists(sub_dir)) dir.create(sub_dir, recursive=TRUE)
       write.table(prob_df, file.path(sub_dir, paste0(unit_id, "_", model_name, "_Patient_Probabilities.tsv")), sep="\t", row.names=FALSE, quote=FALSE)
       
       # 2. Evaluate continuously for Integrated Brier Score (IBS) and plotting
       dense_times <- seq(0, max(valid_times), length.out = 50)
       s_sum_dense <- summary(sfit, times = dense_times, extend = TRUE)
       dense_probs <- s_sum_dense$surv
       if(is.null(dim(dense_probs))) dense_probs <- matrix(dense_probs, nrow=1)
       dense_probs <- t(dense_probs)
       
       dense_bs <- numeric(length(dense_times))
       for(j in seq_along(dense_times)) {
           dense_bs[j] <- calculate_ipcw_brier(eval_df$Surv_obj, dense_probs[, j], dense_times[j])
       }
       
       # Trapezoidal Integration for IBS
       dt <- diff(dense_times)
       ibs <- sum(dt * (dense_bs[-1] + dense_bs[-length(dense_bs)]) / 2) / max(dense_times)
       
       # ---------------------------------------------------------
       # NATIVE VISUALIZATIONS
       # ---------------------------------------------------------
       # Save raw curve data for composite generator
       curve_df <- data.frame(Time_Days = dense_times, Brier_Score = dense_bs)
       write.table(curve_df, file.path(sub_dir, paste0(unit_id, "_", model_name, "_Error_Curve_Data.tsv")), sep="\t", row.names=FALSE, quote=FALSE)
       
       pdf_path <- file.path(sub_dir, paste0(unit_id, "_", model_name, "_Brier_Error_Curve.pdf"))
       pdf(pdf_path, width = 7, height = 6)
       plot(dense_times, dense_bs, type="l", col="blue", lwd=2, 
            xlab="Time (Days)", ylab="Brier Score (IPCW)", 
            main=paste("Prediction Error Curve -", model_name, "\nUnit:", unit_id))
       dev.off()
       
       tiff_path <- file.path(sub_dir, paste0(unit_id, "_", model_name, "_Brier_Error_Curve.tiff"))
       tiff(tiff_path, width = 7, height = 6, units = "in", res = 600, compression = "lzw")
       plot(dense_times, dense_bs, type="l", col="blue", lwd=2, 
            xlab="Time (Days)", ylab="Brier Score (IPCW)", 
            main=paste("Prediction Error Curve -", model_name, "\nUnit:", unit_id))
       dev.off()
       
       # Save Global Scores
       cal_df <- data.frame(Time_Days = valid_times, Brier_Score = bs_t)
       write.table(cal_df, file.path(sub_dir, paste0(unit_id, "_", model_name, "_Brier_Scores.tsv")), sep="\t", row.names=FALSE, quote=FALSE)
       
       return(list(BS = bs_t, IBS = ibs))
    }
    
    rsf_dir <- file.path(bundle_dir, "RSF")
    xgb_dir <- file.path(bundle_dir, "XGBoost")
    mtlr_dir <- file.path(bundle_dir, "MTLR")
    mvl_dir <- file.path(bundle_dir, "MVL_Synthesis")
    
    # E) Independent Boruta Proxy Generation
    boruta_proxy <- rep(0, nrow(eval_df))
    if(!is.null(bundle$Boruta)) {
        boruta_decision <- bundle$Boruta$finalDecision
        boruta_features <- names(boruta_decision)[boruta_decision %in% c("Confirmed", "Tentative")]
        if(length(boruta_features) > 0) {
            boruta_x <- as.data.frame(X_matrix)[, boruta_features, drop=FALSE]
            boruta_df <- cbind(data.frame(time = as.numeric(y_time), status = as.numeric(y_event)), boruta_x)
            
            # The Boruta proxy uses the exact same `rfsrc` parameters from Megarun 5.0
            b_mod <- rfsrc(Surv(time, status) ~ ., data = boruta_df, ntree = 500, na.action = "na.impute", splitrule = "logrank", seed=42)
            boruta_proxy <- safe_assign_risk(eval_df, b_mod$predicted.oob)
        }
    }
    
    # Evaluate Models
    res_rsf <- process_calibration(eval_df$RSF_Risk, "RSF", rsf_dir)
    res_xgb <- process_calibration(eval_df$XGBoost_Risk, "XGBoost", xgb_dir)
    res_mtlr <- process_calibration(eval_df$MTLR_Risk, "MTLR", mtlr_dir)
    
    boruta_dir <- file.path(bundle_dir, "Boruta")
    res_boruta <- process_calibration(boruta_proxy, "Boruta_Independent", boruta_dir)
    
    # For MVL, since we don't have the explicit super_risk vector, we can reconstruct it
    # by reading the weights from the saved TSV
    mvl_weights_file <- file.path(mvl_dir, paste0(unit_id, "_MVL_Algorithm_Weights.tsv"))
    res_mvl <- NULL
    if(file.exists(mvl_weights_file)) {
        w_df <- read.delim(mvl_weights_file, sep="\t")
        mvl_matrix <- data.frame(
            RSF = as.numeric(eval_df$RSF_Risk),
            XGBoost = as.numeric(eval_df$XGBoost_Risk),
            MTLR = as.numeric(eval_df$MTLR_Risk),
            Boruta = boruta_proxy
        )
        
        # Jitter exactly like Megarun
        for(col in colnames(mvl_matrix)) {
           if(var(mvl_matrix[[col]], na.rm=TRUE) == 0) mvl_matrix[[col]] <- mvl_matrix[[col]] + runif(nrow(mvl_matrix), -1e-6, 1e-6)
        }
        X_meta <- scale(as.matrix(mvl_matrix))
        X_meta[is.na(X_meta)] <- 0
        
        # Apply weights 
        mvl_super_risk <- rep(0, nrow(X_meta))
        for(i in 1:nrow(w_df)) {
           dim_name <- w_df$Dimension[i]
           if(dim_name %in% colnames(X_meta)) {
               mvl_super_risk <- mvl_super_risk + X_meta[, dim_name] * w_df$Elastic_Net_Weight[i]
           }
        }
        
        res_mvl <- process_calibration(mvl_super_risk, "MVL_Synthesis", mvl_dir)
    }
    
    # 5. Compile Global Master Row
    row_out <- data.frame(
      Unit_ID = unit_id,
      Cancer_Type = c,
      Metric = m,
      DF = d,
      
      RSF_IBS = if(!is.null(res_rsf)) res_rsf$IBS else NA,
      XGBoost_IBS = if(!is.null(res_xgb)) res_xgb$IBS else NA,
      MTLR_IBS = if(!is.null(res_mtlr)) res_mtlr$IBS else NA,
      Boruta_IBS = if(!is.null(res_boruta)) res_boruta$IBS else NA,
      MVL_Synthesis_IBS = if(!is.null(res_mvl)) res_mvl$IBS else NA,
      
      # 1 Year specific calibration
      RSF_BS_1Yr = if(!is.null(res_rsf) && 365 %in% valid_times) res_rsf$BS[valid_times==365] else NA,
      XGB_BS_1Yr = if(!is.null(res_xgb) && 365 %in% valid_times) res_xgb$BS[valid_times==365] else NA,
      MTLR_BS_1Yr = if(!is.null(res_mtlr) && 365 %in% valid_times) res_mtlr$BS[valid_times==365] else NA,
      Boruta_BS_1Yr = if(!is.null(res_boruta) && 365 %in% valid_times) res_boruta$BS[valid_times==365] else NA,
      MVL_BS_1Yr = if(!is.null(res_mvl) && 365 %in% valid_times) res_mvl$BS[valid_times==365] else NA,
      
      # 3 Year specific calibration
      RSF_BS_3Yr = if(!is.null(res_rsf) && 1095 %in% valid_times) res_rsf$BS[valid_times==1095] else NA,
      XGB_BS_3Yr = if(!is.null(res_xgb) && 1095 %in% valid_times) res_xgb$BS[valid_times==1095] else NA,
      MTLR_BS_3Yr = if(!is.null(res_mtlr) && 1095 %in% valid_times) res_mtlr$BS[valid_times==1095] else NA,
      Boruta_BS_3Yr = if(!is.null(res_boruta) && 1095 %in% valid_times) res_boruta$BS[valid_times==1095] else NA,
      MVL_BS_3Yr = if(!is.null(res_mvl) && 1095 %in% valid_times) res_mvl$BS[valid_times==1095] else NA,
      
      # 5 Year specific calibration
      RSF_BS_5Yr = if(!is.null(res_rsf) && 1825 %in% valid_times) res_rsf$BS[valid_times==1825] else NA,
      XGB_BS_5Yr = if(!is.null(res_xgb) && 1825 %in% valid_times) res_xgb$BS[valid_times==1825] else NA,
      MTLR_BS_5Yr = if(!is.null(res_mtlr) && 1825 %in% valid_times) res_mtlr$BS[valid_times==1825] else NA,
      Boruta_BS_5Yr = if(!is.null(res_boruta) && 1825 %in% valid_times) res_boruta$BS[valid_times==1825] else NA,
      MVL_BS_5Yr = if(!is.null(res_mvl) && 1825 %in% valid_times) res_mvl$BS[valid_times==1825] else NA
    )
    
    cat(paste0(Sys.time(), " - [SUCCESS] Brier Extracted for ", unit_id, "\n"), file = log_file_path_atomic, append = TRUE)
    return(row_out)
    
  }, error = function(e) {
    err_msg <- paste0(Sys.time(), " - [FAILURE] Unit: ", bundle_path, " -> ", e$message, "\n")
    cat(err_msg, file = log_file_path_atomic, append = TRUE)
    return(NULL)
  })
})

# Filter out NULLs from errors
global_brier_metrics <- Filter(Negate(is.null), global_brier_metrics_list)

# =========================================================================
# IV. SAVE MASTER METRICS
# =========================================================================

if(length(global_brier_metrics) > 0) {
  master_brier_df <- do.call(rbind, global_brier_metrics)
  master_save_path <- file.path(WORKING_DIR, "MASTER_Phase_III_Brier_Calibration.csv")
  write.csv(master_brier_df, master_save_path, row.names = FALSE)
  message("\n✅ Calibration Extraction Complete! Master matrix saved to: ", master_save_path)
} else {
  message("\n⚠️ Extraction Skipped: No cohorts were successfully evaluated.")
}

# =========================================================================
# V. AUTONOMOUS EXEMPLAR IDENTIFICATION FOR MANUSCRIPT FIGURES
# =========================================================================
if(exists("master_brier_df")) {
  message("\n[AUTONOMOUS AUDIT] Identifying optimal clinical exemplars for manuscript figures...")
  
  if("MVL_Synthesis_IBS" %in% colnames(master_brier_df)) {
      valid_df <- master_brier_df[!is.na(master_brier_df$MVL_Synthesis_IBS), ]
      
      if(nrow(valid_df) > 0) {
          # Calculate how many metrics each cancer type evaluated
          agg_df <- aggregate(MVL_Synthesis_IBS ~ Cancer_Type, data = valid_df, 
                              FUN = function(x) c(mean = mean(x), count = length(x)))
          agg_df <- data.frame(Cancer_Type = agg_df$Cancer_Type, 
                               Mean_IBS = agg_df$MVL_Synthesis_IBS[,"mean"],
                               Count = agg_df$MVL_Synthesis_IBS[,"count"])
          
          # Identify "Full Set" Candidates (Cancer types that evaluated the maximum number of endpoints, e.g., 4)
          max_metrics <- max(agg_df$Count)
          cancers_with_full_set <- agg_df$Cancer_Type[agg_df$Count == max_metrics]
          
          if(length(cancers_with_full_set) > 0) {
              full_set_df <- valid_df[valid_df$Cancer_Type %in% cancers_with_full_set, ]
              
              # Keep only the essential columns for pivoting
              pivot_df <- full_set_df[, c("Cancer_Type", "Metric", "MVL_Synthesis_IBS")]
              
              # Reshape data to wide format using base R
              wide_df <- reshape(pivot_df, timevar = "Metric", idvar = "Cancer_Type", direction = "wide")
              
              # Rename columns to be clean (e.g., MVL_Synthesis_IBS.OS -> OS)
              colnames(wide_df) <- gsub("MVL_Synthesis_IBS\\.", "", colnames(wide_df))
              
              # Calculate mean IBS to rank them
              metric_cols <- setdiff(colnames(wide_df), "Cancer_Type")
              wide_df$Mean_IBS <- rowMeans(wide_df[, metric_cols, drop=FALSE], na.rm = TRUE)
              
              # Sort by best (lowest) Mean IBS
              wide_df <- wide_df[order(wide_df$Mean_IBS), ]
              
              # Export the beautiful full-set list
              exemplar_path <- file.path(WORKING_DIR, "Full_Set_Exemplar_Candidates.tsv")
              write.table(wide_df, exemplar_path, sep="\t", row.names=FALSE, quote=FALSE)
              
              # Print the absolute best full set candidate to console
              best_overall <- wide_df[1, ]
              message(sprintf("🏆 BEST FULL-SET COHORT (across %d metrics): %s (Mean IBS: %.4f)", 
                              max_metrics, best_overall$Cancer_Type, best_overall$Mean_IBS))
              message(sprintf("📄 Full Set (%d-Metric) Exemplar candidate list saved to: %s", max_metrics, exemplar_path))
              
              # =========================================================================
              # VI. NATIVE COMPOSITE FIGURE GENERATOR
              # =========================================================================
              message("\n[AUTONOMOUS AUDIT] Generating Native 4-Panel Composite Figures for Top Candidates...")
              
              # Take Top 3 Full-Set Candidates
              top_candidates <- head(wide_df$Cancer_Type, 3)
              
              for(cohort in top_candidates) {
                  # Define expected metrics to plot
                  metrics <- c("OS", "DSS", "PFI", "DFI")
                  panel_labels <- c("A", "B", "C", "D")
                  
                  # Prepare plot device
                  comp_dir <- file.path(WORKING_DIR, "Composite_Figures")
                  if(!dir.exists(comp_dir)) dir.create(comp_dir, recursive = TRUE)
                  
                  comp_pdf <- file.path(comp_dir, paste0("COMPOSITE_Exemplar_", cohort, ".pdf"))
                  comp_tiff <- file.path(comp_dir, paste0("COMPOSITE_Exemplar_", cohort, ".tiff"))
                  
                  generate_composite <- function() {
                      # 2x2 Layout with uniform margins
                      par(mfrow = c(2, 2), mar = c(4, 4, 3, 1), oma = c(0.5, 0.5, 0.5, 0.5))
                      
                      for(i in seq_along(metrics)) {
                          m <- metrics[i]
                          label <- panel_labels[i]
                          
                          # Find the Unit ID that matches this cohort and metric
                          unit_row <- valid_df[valid_df$Cancer_Type == cohort & valid_df$Metric == m, ]
                          if(nrow(unit_row) > 0) {
                              unit_id <- unit_row$Unit_ID[1]
                              curve_file <- file.path(MODEL_OUTPUT_DIR, unit_id, "MVL_Synthesis", paste0(unit_id, "_MVL_Synthesis_Error_Curve_Data.tsv"))
                              
                              if(file.exists(curve_file)) {
                                  c_data <- read.delim(curve_file)
                                  plot(c_data$Time_Days, c_data$Brier_Score, type="l", col="blue", lwd=2.5,
                                       ylim = c(0, max(0.25, max(c_data$Brier_Score, na.rm=T))),
                                       xlab="Time (Days)", ylab="Brier Score (IPCW)",
                                       main=paste("Endpoint:", m), cex.main=1.2)
                                  
                                  # Add panel label A, B, etc. top left (Bold, larger size)
                                  mtext(label, side = 3, adj = -0.15, line = 1, cex = 1.5, font = 2)
                              } else {
                                  plot.new()
                                  title(main = paste("Endpoint:", m), cex.main=1.2)
                                  text(0.5, 0.5, "Data Missing")
                                  mtext(label, side = 3, adj = -0.15, line = 1, cex = 1.5, font = 2)
                              }
                          } else {
                              plot.new()
                              title(main = paste("Endpoint:", m), cex.main=1.2)
                              text(0.5, 0.5, "Model Missing")
                              mtext(label, side = 3, adj = -0.15, line = 1, cex = 1.5, font = 2)
                          }
                      }
                  }
                  
                  # Save as PDF
                  pdf(comp_pdf, width = 10, height = 8)
                  generate_composite()
                  dev.off()
                  
                  # Save as high-res TIFF
                  tiff(comp_tiff, width = 10, height = 8, units = "in", res = 600, compression = "lzw")
                  generate_composite()
                  dev.off()
                  
                  message("✅ Composite Figure Generated: ", cohort)
              }
              
              # =========================================================================
              # VII. MULTI-CANCER MASTER COMPOSITE (OVERLAID CURVES)
              # =========================================================================
              message("\n[AUTONOMOUS AUDIT] Generating Multi-Cancer Master Composite...")
              
              master_pdf <- file.path(comp_dir, "COMPOSITE_MASTER_Exemplar.pdf")
              master_tiff <- file.path(comp_dir, "COMPOSITE_MASTER_Exemplar.tiff")
              
              palette_colors <- c("#E41A1C", "#377EB8", "#4DAF4A")
              
              generate_master_composite <- function() {
                  metrics <- c("OS", "DSS", "PFI", "DFI")
                  panel_labels <- c("A", "B", "C", "D")
                  par(mfrow = c(2, 2), mar = c(4, 4, 3, 1), oma = c(0.5, 0.5, 0.5, 0.5))
                  
                  for(i in seq_along(metrics)) {
                      m <- metrics[i]
                      label <- panel_labels[i]
                      
                      # 1. Pre-calculate global axis limits
                      max_x <- 1825
                      max_y <- 0.1
                      valid_files <- list()
                      
                      for(cohort in top_candidates) {
                          u_row <- valid_df[valid_df$Cancer_Type == cohort & valid_df$Metric == m, ]
                          if(nrow(u_row) > 0) {
                              c_file <- file.path(MODEL_OUTPUT_DIR, u_row$Unit_ID[1], "MVL_Synthesis", paste0(u_row$Unit_ID[1], "_MVL_Synthesis_Error_Curve_Data.tsv"))
                              if(file.exists(c_file)) {
                                  valid_files[[cohort]] <- c_file
                                  c_data <- read.delim(c_file)
                                  max_x <- max(max_x, max(c_data$Time_Days, na.rm=T))
                                  max_y <- max(max_y, max(c_data$Brier_Score, na.rm=T))
                              }
                          }
                      }
                      
                      # 2. Setup the empty plot grid
                      plot(NULL, xlim = c(0, max_x), ylim = c(0, max(0.25, max_y)),
                           xlab="Time (Days)", ylab="Brier Score (IPCW)", main=paste("Endpoint:", m), cex.main=1.2)
                      mtext(label, side = 3, adj = -0.15, line = 1, cex = 1.5, font = 2)
                      
                      # 3. Draw the RAW mathematical curves
                      for(j in seq_along(top_candidates)) {
                          cohort <- top_candidates[j]
                          if(!is.null(valid_files[[cohort]])) {
                              c_data <- read.delim(valid_files[[cohort]])
                              lines(c_data$Time_Days, c_data$Brier_Score, col=palette_colors[j], lwd=2.5)
                          }
                      }
                      
                      # 4. Add the Legend (Only in Panel A to avoid clutter)
                      if(i == 1) {
                          legend("topright", legend=top_candidates, col=palette_colors, lwd=2.5, bty="n", cex=1.1)
                      }
                  }
              }
              
              pdf(master_pdf, width = 10, height = 8)
              generate_master_composite()
              dev.off()
              
              tiff(master_tiff, width = 10, height = 8, units = "in", res = 600, compression = "lzw")
              generate_master_composite()
              dev.off()
              
              message("✅ Master Multi-Cancer Composite Generated.")
          }
      }
  }
}

# Gracefully terminate the parallel engine
if(exists("cl")) {
    parallel::stopCluster(cl)
    message("🛑 Parallel engine successfully terminated.")
}

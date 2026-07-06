# =========================================================================
# ZIMA PHASE III CLINICAL PROBABILITY EXTRACTOR (CLEAN SLATE VERSION)
# Purpose: Extract 1-Year, 3-Year, and 5-Year individualized clinical 
#          probabilities directly from the Phase III Reference models.
#
# Physics Core (Brier Mirror):
# - 100% pure Brier internal logic for native Risk array assignment.
# - Reconstructs MVL exactly as calibrated via ElasticNet.
# - Outputs probabilities for ALL models (RSF, XGBoost, MTLR, Boruta, MVL).
# - Bifurcation: OS/DSS -> S(t), DFI/PFI -> 1-S(t).
# =========================================================================

local_lib <- "~/minhas_bibliotecas_R"
if(!dir.exists(local_lib)) dir.create(local_lib, recursive = TRUE, showWarnings = FALSE)
.libPaths(c(local_lib, .libPaths()))
Sys.setenv(R_LIBS_USER = local_lib)

required_packages <- c("dplyr", "survival", "randomForestSRC", "xgboost", "glmnet", "MTLR", "rio")

suppressPackageStartupMessages({
  for(pkg in required_packages) {
    if(!require(pkg, character.only = TRUE, quietly = TRUE)) {
      cat(sprintf("Installing %s...\n", pkg))
      install.packages(pkg, lib = local_lib, repos = "http://cran.rstudio.com/")
      library(pkg, character.only = TRUE)
    }
  }
})

# =========================================================================
# I. ENVIRONMENT & PARALLEL SETUP
# =========================================================================
ZIMA_ROOT <- "~/students/aluno0549-6/PHASE_III/PHASE_III_Megarun_5_0_complete_ZIMA_Suit_Generator_final"
DF_ROOT <- "~/students/aluno0549-6/dfXXX_series"
MODEL_OUTPUT_DIR <- file.path(ZIMA_ROOT, "PHASE_III_ML_Models")

OUTPUT_DIR <- file.path(ZIMA_ROOT, "CancerRCDShiny_Phase_III_Clinical_Probabilities")
if(!dir.exists(OUTPUT_DIR)) dir.create(OUTPUT_DIR, recursive=TRUE)

LOG_FILE <- file.path(ZIMA_ROOT, "Phase_III_Probability_Extraction.log")
cat(paste0("--- BATCH RUN: ", Sys.time(), " ---\n"), file = LOG_FILE, append = FALSE)

# -------------------------------------------------------------------------
# GEOMETRY PRESERVATION FUNCTIONS (Exact Brier Match)
# -------------------------------------------------------------------------
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

bundle_files <- list.files(MODEL_OUTPUT_DIR, pattern = "model_bundle_.*\\.rds$", recursive = TRUE, full.names = TRUE)
if(length(bundle_files) == 0) stop("CRITICAL ERROR: No Phase III bundles found!")

cat(sprintf("\n[ZIMA EXTRACTOR] Booting Pure Brier Phase III Engine for %d Cohorts...\n", length(bundle_files)))

n_cores <- max(1, parallel::detectCores(logical = TRUE) - 2)
cl <- parallel::makeCluster(n_cores, type = "PSOCK", setup_strategy = "sequential", outfile = "")
parallel::clusterExport(cl, varlist = c("ZIMA_ROOT", "DF_ROOT", "OUTPUT_DIR", "LOG_FILE", "assert_time_schema_nonneg", "filter_survival_complete", "build_combined_X_matrix"))

# =========================================================================
# II. PARALLEL EXTRACTION ENGINE
# =========================================================================
results_list <- parallel::parLapply(cl, bundle_files, function(bundle_path) {
    require(survival, quietly = TRUE)
    require(xgboost, quietly = TRUE)
    require(randomForestSRC, quietly = TRUE)
    
    tryCatch({
        bundle_dir <- dirname(bundle_path)
        unit_id <- basename(bundle_dir)
        cat(sprintf("[%s] Starting Extraction\n", unit_id), file = LOG_FILE, append = TRUE)
        
        bundle <- readRDS(bundle_path)
        row_def <- bundle$meta
        c <- row_def$cancer_type
        m <- row_def$metric
        d <- row_def$df
        
        # Load Dataset
        clean_d <- ifelse(grepl("\\.rds$", d, ignore.case = TRUE), d, paste0(d, ".rds"))
        df_path <- file.path(DF_ROOT, clean_d)
        if(!file.exists(df_path)) {
            cat(sprintf("[%s] ERROR: Dataset not found %s\n", unit_id, df_path), file = LOG_FILE, append = TRUE)
            return(NULL)
        }
        
        df_raw <- readRDS(df_path)
        df_cancer <- df_raw[df_raw$type == c, , drop = FALSE]
        
        # 1. Apply Brier exact survival filtering
        surv_pack <- filter_survival_complete(df_cancer, m)
        df_cohort <- surv_pack$df_masked
        y_time <- surv_pack$time
        y_event <- surv_pack$event
        
        # 2. Apply Brier exact X_matrix regex extractor
        X_matrix <- build_combined_X_matrix(df_cohort, c)
        
        # Apply Megarun 4.3 Missingness Filter EXACTLY (from Brier)
        na_props <- rowSums(is.na(X_matrix)) / ncol(X_matrix)
        exclude_idx <- which(na_props >= 0.35)
        if (length(exclude_idx) > 0) {
            X_matrix <- X_matrix[-exclude_idx, , drop = FALSE]
            y_time <- y_time[-exclude_idx]
            y_event <- y_event[-exclude_idx]
            df_cohort <- df_cohort[-exclude_idx, , drop = FALSE]
        }
        
        eval_df <- data.frame(time = y_time, status = y_event, sample = df_cohort$sample)
        
        if(nrow(eval_df) == 0) {
            cat(sprintf("[%s] SKIPPED: 0 patients remaining after Brier missingness filter.\n", unit_id), file = LOG_FILE, append = TRUE)
            return(NULL)
        }
        
        # -----------------------------------------------------------------
        # A. RISK ASSIGNMENT (100% Brier Parity)
        # -----------------------------------------------------------------
        # Exact `safe_assign_risk` function from Brier Code
        safe_assign_risk <- function(df, risk_vec) {
            if(is.null(risk_vec) || inherits(risk_vec, "try-error")) return(rep(NA, nrow(df)))
            if(length(risk_vec) == nrow(df)) return(as.numeric(risk_vec))
            
            # If lengths mismatch, use time > 0 filter logic from Brier
            valid_idx <- which(df$time > 0)
            if(length(valid_idx) == length(risk_vec)) {
                 mapped_risk <- rep(NA, nrow(df))
                 mapped_risk[valid_idx] <- as.numeric(risk_vec)
                 return(mapped_risk)
            }
            # Hard Fallback
            return(rep(NA, nrow(df)))
        }
        
        eval_df$RSF_Risk <- safe_assign_risk(eval_df, tryCatch(bundle$RSF$predicted.oob, error=function(e) NULL))
        eval_df$MTLR_Risk <- safe_assign_risk(eval_df, tryCatch(attr(bundle$MTLR, "apparent_risk"), error=function(e) NULL))
        
        if(!is.null(bundle$SHAP) && (is.matrix(bundle$SHAP) || is.data.frame(bundle$SHAP))) {
            eval_df$XGBoost_Risk <- safe_assign_risk(eval_df, rowSums(bundle$SHAP))
        } else {
            eval_df$XGBoost_Risk <- NA
        }
        
        boruta_proxy <- rep(NA, nrow(eval_df))
        if(!is.null(bundle$Boruta) && !inherits(bundle$Boruta, "try-error")) {
            boruta_decision <- bundle$Boruta$finalDecision
            if(!is.null(boruta_decision)) {
                boruta_features <- names(boruta_decision)[boruta_decision %in% c("Confirmed", "Tentative")]
                if(length(boruta_features) > 0) {
                    boruta_x <- as.data.frame(X_matrix)[, boruta_features, drop=FALSE]
                    boruta_df <- cbind(data.frame(time = y_time, status = y_event), boruta_x)
                    b_mod <- tryCatch(randomForestSRC::rfsrc(Surv(time, status) ~ ., data = boruta_df, ntree = 500, na.action = "na.impute", splitrule = "logrank", seed=42), error = function(e) NULL)
                    if(!is.null(b_mod)) {
                        boruta_proxy <- safe_assign_risk(eval_df, b_mod$predicted.oob)
                    }
                }
            }
        }
        eval_df$Boruta_Risk <- boruta_proxy
        
        # -----------------------------------------------------------------
        # B. SUPERLEARNER SYNTHESIS (MVL Matrix Recombination)
        # -----------------------------------------------------------------
        mvl_matrix <- data.frame(
            RSF = as.numeric(eval_df$RSF_Risk),
            XGBoost = as.numeric(eval_df$XGBoost_Risk),
            MTLR = as.numeric(eval_df$MTLR_Risk),
            Boruta = as.numeric(eval_df$Boruta_Risk)
        )
        
        for(col in colnames(mvl_matrix)) {
           if(var(mvl_matrix[[col]], na.rm=TRUE) == 0 || is.na(var(mvl_matrix[[col]], na.rm=TRUE))) {
               mvl_matrix[[col]] <- mvl_matrix[[col]] + runif(nrow(mvl_matrix), -1e-6, 1e-6)
           }
        }
        
        X_meta <- scale(as.matrix(mvl_matrix))
        X_meta[is.na(X_meta)] <- 0
        
        mvl_super_risk <- rep(NA, nrow(X_meta))
        mvl_dir <- file.path(bundle_dir, "MVL_Synthesis")
        mvl_weights_file <- file.path(mvl_dir, paste0(unit_id, "_MVL_Algorithm_Weights.tsv"))
        
        if(file.exists(mvl_weights_file)) {
            w_df <- read.delim(mvl_weights_file, sep="\t")
            mvl_super_risk <- rep(0, nrow(X_meta))
            for(i in 1:nrow(w_df)) {
               dim_name <- w_df$Dimension[i]
               if(dim_name %in% colnames(X_meta)) {
                   mvl_super_risk <- mvl_super_risk + X_meta[, dim_name] * w_df$Elastic_Net_Weight[i]
               }
            }
        }
        eval_df$MVL_Risk <- mvl_super_risk
        
        # -----------------------------------------------------------------
        # C. CLINICAL PROBABILITY MAPPING (Cox Breslow Engine)
        # -----------------------------------------------------------------
        target_times <- c(365, 1095, 1825)
        
        extract_probs <- function(risk_vec) {
            if(is.null(risk_vec) || anyNA(risk_vec)) return(rep(NA, 3 * nrow(eval_df)))
            
            tmp_df <- eval_df
            tmp_df$risk <- risk_vec
            tmp_df$time[tmp_df$time <= 0] <- 0.001 # Native CoxPH anchor to prevent infinity mapping
            
            cfit <- tryCatch(coxph(Surv(time, status) ~ risk, data = tmp_df, x=TRUE, y=TRUE), error=function(e) NULL)
            if(is.null(cfit)) return(rep(NA, 3 * nrow(eval_df)))
            
            sfit <- tryCatch(survfit(cfit, newdata = tmp_df), error=function(e) NULL)
            if(is.null(sfit)) return(rep(NA, 3 * nrow(eval_df)))
            
            s_sum <- summary(sfit, times = target_times, extend = TRUE)
            p_mat <- s_sum$surv
            if(is.null(dim(p_mat))) {
                p_mat <- matrix(p_mat, ncol=length(target_times)) # [1_patient x 3_timepoints]
            } else {
                p_mat <- t(p_mat) # [N_patients x 3_timepoints]
            }
            
            # Apply Bifurcation
            if(m %in% c("DFI", "PFI")) {
                p_mat <- 1 - p_mat
            }
            
            # Flatten to 1D vector (1Yr_all, 3Yr_all, 5Yr_all) for easy df assignment
            return(c(p_mat[, 1], p_mat[, 2], p_mat[, 3]))
        }
        
        N <- nrow(eval_df)
        p_rsf <- extract_probs(eval_df$RSF_Risk)
        p_xgb <- extract_probs(eval_df$XGBoost_Risk)
        p_mtlr <- extract_probs(eval_df$MTLR_Risk)
        p_boruta <- extract_probs(eval_df$Boruta_Risk)
        p_mvl <- extract_probs(eval_df$MVL_Risk)
        
        # -----------------------------------------------------------------
        # D. MASTER DATAFRAME ASSEMBLY
        # -----------------------------------------------------------------
        final_df <- data.frame(
            Sample_ID = eval_df$sample,
            Cancer = c,
            Endpoint = m,
            
            RSF_Risk = eval_df$RSF_Risk,
            RSF_Prob_1Yr = p_rsf[1:N],
            RSF_Prob_3Yr = p_rsf[(N+1):(2*N)],
            RSF_Prob_5Yr = p_rsf[(2*N+1):(3*N)],
            
            XGBoost_Risk = eval_df$XGBoost_Risk,
            XGBoost_Prob_1Yr = p_xgb[1:N],
            XGBoost_Prob_3Yr = p_xgb[(N+1):(2*N)],
            XGBoost_Prob_5Yr = p_xgb[(2*N+1):(3*N)],
            
            MTLR_Risk = eval_df$MTLR_Risk,
            MTLR_Prob_1Yr = p_mtlr[1:N],
            MTLR_Prob_3Yr = p_mtlr[(N+1):(2*N)],
            MTLR_Prob_5Yr = p_mtlr[(2*N+1):(3*N)],
            
            Boruta_Risk = eval_df$Boruta_Risk,
            Boruta_Prob_1Yr = p_boruta[1:N],
            Boruta_Prob_3Yr = p_boruta[(N+1):(2*N)],
            Boruta_Prob_5Yr = p_boruta[(2*N+1):(3*N)],
            
            MVL_Risk = eval_df$MVL_Risk,
            MVL_Prob_1Yr = p_mvl[1:N],
            MVL_Prob_3Yr = p_mvl[(N+1):(2*N)],
            MVL_Prob_5Yr = p_mvl[(2*N+1):(3*N)],
            
            stringsAsFactors = FALSE
        )
        
        out_name <- paste0(unit_id, "_Phase_III_Probabilities.tsv")
        out_path <- file.path(OUTPUT_DIR, out_name)
        write.table(final_df, out_path, sep="\t", row.names=FALSE, quote=FALSE)
        
        cat(sprintf("[%s] SUCCESS. Matrix saved to %s\n", unit_id, out_name), file = LOG_FILE, append = TRUE)
        return(unit_id)
        
    }, error = function(e) {
        cat(sprintf("[CRASH] ERROR IN %s: %s\n", bundle_path, e$message), file = LOG_FILE, append = TRUE)
        return(NULL)
    })
})

parallel::stopCluster(cl)
cat(sprintf("\n✅ PURE PHASE III EXTRACTION COMPLETED.\nRaw data matrices exported directly to: %s\n", OUTPUT_DIR))

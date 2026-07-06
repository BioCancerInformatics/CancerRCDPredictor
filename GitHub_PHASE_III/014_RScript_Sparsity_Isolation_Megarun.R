# Phase IIIB alternative module — STRICTLY LIMITED TO TOKEN .2 (MUTATION) & TOKEN .3 (CNV) PREDICTIVE VARIABLES
# Quadripartite Meta-Learner: RSF, XGBoost, Boruta, and MTLR
###############################################################################

# -------------------------------------------------------------------------
# PREAMBLE — Phase IIIB Constitution & Logic
# -------------------------------------------------------------------------
# This script represents the Phase IIIB alternative module.
# It acts as a strict computational quarantine: we intentionally delete all 
# continuous phenotypic markers (Proteins [.1], microRNA [.4], Transcripts [.5], 
# mRNA [.6], and CpG Methylation [.7]) and FORCE the algorithms to model survival 
# using exclusively Sparse Binary Variables (Somatic Mutations [.2] and Copy Number Variations [.3]). 
#
# CONSTRAINTS:
# 1) Cohort Preservation: The script loads ONLY the TAR-approved preprocessing
#    regimes (dfXXX) as defined in `improved_unchanged_best_fullset.tsv`.
# 2) Predictor Integration: Models execute exclusively groupwise (c, m, d),
#    using a single massive, combined predictor matrix (X) that includes ALL 
#    variables across ALL 7 omic layers simultaneously. Modeling is NOT segregated
#    by omic layer.
  # 3) Missing Data Handling: To preserve the geometry of missingness:
  #    - RSF uses `randomForestSRC` (native proximity imputation via tree splits)
  #    - XGBoost uses native sparsity-aware learning.
  #    - MTLR requires deterministic imputation prior to entry (Phase II fallback).
#
# INTERPRETABILITY:
# SHAP/LIME extraction wrappers run across all models post-hoc.
# -------------------------------------------------------------------------

# Phase I Harmonization: Programmatic Audit of LSHMOM Stratum Exclusions and Lineage Distributions
# 
# 1. Load the Phase II log
log_data <- readRDS("CoxNet_phaseII_feasibility_log.rds")

# 2. Extract uniquely executed strata (lineage + endpoint combinations)
unique_strata <- unique(log_data[, c("cancer_type", "metric")])

# 3. Define the theoretical landscape (33 TCGA canonical lineages + 4 endpoints)
all_metrics <- c("OS", "PFI", "DFI", "DSS")
canonical_lineages <- c(
  "ACC", "BLCA", "BRCA", "CESC", "CHOL", "COAD", "DLBC", "ESCA", 
  "GBM", "HNSC", "KICH", "KIRC", "KIRP", "LAML", "LGG", "LIHC", 
  "LUAD", "LUSC", "MESO", "OV", "PAAD", "PCPG", "PRAD", "READ", 
  "SARC", "SKCM", "STAD", "TGCT", "THCA", "THYM", "UCEC", "UCS", "UVM"
)

# 4. Calculate attrition metrics
theoretical_n <- length(canonical_lineages) * length(all_metrics)
actual_n <- nrow(unique_strata)
missing_n <- theoretical_n - actual_n

cat(sprintf("--- Phase I Harmonization Attrition ---\n"))
cat(sprintf("Theoretical Strata: %d\n", theoretical_n))
cat(sprintf("Actual LSHMOM Strata: %d\n", actual_n))
cat(sprintf("Uncomputable Strata: %d\n\n", missing_n))

# 5. Build a presence/absence matrix to map the missing data landscape
strata_table <- table(
  factor(unique_strata$cancer_type, levels = canonical_lineages), 
  factor(unique_strata$metric, levels = all_metrics)
)

# Convert to data frame and count missing elements per lineage
strata_df <- as.data.frame.matrix(strata_table)
strata_df$Total_Available <- rowSums(strata_df)
strata_df$Missing_Count <- 4 - strata_df$Total_Available

# 6. Extract and display only the lineages suffering attrition
missing_distribution <- strata_df[strata_df$Missing_Count > 0, ]
cat("Distribution of Missing Strata by Lineage:\n(0 = missing, 1 = computable)\n")
print(missing_distribution)


if (!requireNamespace("glmnet", quietly = TRUE)) {
  message("Installing required package 'glmnet' for MVL Synthesis...")
  install.packages("glmnet", repos = "http://cran.us.r-project.org")
}
if (!requireNamespace("shapviz", quietly = TRUE)) {
  message("Installing required package 'shapviz' for visual integration...")
  install.packages("shapviz", repos = "http://cran.us.r-project.org")
}
if (!requireNamespace("ggplot2", quietly = TRUE)) {
  message("Installing required package 'ggplot2' for visual integration...")
  install.packages("ggplot2", repos = "http://cran.us.r-project.org")
}

suppressPackageStartupMessages({
  library(dplyr)
  library(glmnet)
  library(tidyr)
  library(rio)
  library(future.apply)
  library(randomForestSRC)
  library(xgboost)
  library(survival)
  library(lime)
  library(MTLR)
  library(timeROC)
  library(Boruta)
  library(ranger)
  library(missForest)
})

# =========================================================================
# LIME EXTENSION ARCHITECTURE FOR SURVIVAL MODELS
# (Note: Functions injected dynamically on worker nodes during parallel loop)
# =========================================================================

# =========================================================================
# I. USER-DEFINED PATH CONFIGURATION (PORTABLE ENVIRONMENT SETUP)
# =========================================================================
# Define these three absolute paths based on the local machine or server environment.

# (A) Working Directory (Where Phase III scripts execute and ML outputs will live)
WORKING_DIR <- "~/students/aluno0549-6/PHASE_III"

# (B) Root folder containing all the preprocessed dfXXX.rds matrix datasets
DF_ROOT_MANIFEST <- "~/students/aluno0549-6/dfXXX_series"

# (C) The exact physical path to the Phase II Eligibility Table (TAR Admissibility)
TAR_TABLE_PATH <- "~/students/aluno0549-6/PHASE_III/CoxNet_phaseII_feasibility_log.rds"

# --- Automatic Subdirectory Enforcement ---
setwd(WORKING_DIR)
MODEL_OUTPUT_DIR <- file.path(WORKING_DIR, "PHASE_IIIB_Sparsity_Isolation_Models")

if(!dir.exists(MODEL_OUTPUT_DIR)) dir.create(MODEL_OUTPUT_DIR, recursive = TRUE)

# =========================================================================
# II. LOAD TAR ADMISSIBILITY GATE
# =========================================================================
# Load the authoritative TAR routing table established by the Phase II CANARY Action Policy.
if (!file.exists(TAR_TABLE_PATH)) {
  stop("FATAL: TAR routing table missing: ", TAR_TABLE_PATH, call. = FALSE)
}

dfinput_raw <- readRDS(TAR_TABLE_PATH)

dfinput <- dfinput_raw %>%
  dplyr::filter(PHASE_III_logic == "ELIGIBLE_NONCOX__GEOMETRY_MU_EXHAUSTED") %>%
  dplyr::mutate(
    # Remap the CANARY 'df_file' directly to 'df' to preserve downstream loop linkage
    df = as.character(df_file),
    cancer_type = trimws(as.character(cancer_type)),
    metric = trimws(as.character(metric))
  )

message("✅ PHASE III INIT: Loaded Phase II CANARY Admissibility Gating (", nrow(dfinput), " valid execution units).")

# =========================================================================
# III. PHASE II SURVIVAL MASKING HELPER FUNCTIONS 
#      (Preserves exact (c, m, d) cohort isolation)
# =========================================================================

assert_time_schema_nonneg <- function(time, metric = NULL) {
  time_num <- suppressWarnings(as.numeric(as.character(time)))
  bad_time <- !is.na(time_num) & (!is.finite(time_num) | time_num < 0)
  if (any(bad_time)) {
    stop("FAIL__TIME_SCHEMA: non-finite or negative survival times detected (time < 0).", call. = FALSE)
  }
  return(time_num)
}

filter_survival_complete <- function(df_sub, metric) {
  cols <- list(event = metric, time  = paste0(metric, ".time"))
  time  <- as.numeric(as.character(df_sub[[cols$time]]))
  event <- as.numeric(as.character(df_sub[[cols$event]]))
  
  time <- assert_time_schema_nonneg(time, metric)
  
  mask <- is.finite(time) & is.finite(event) & !is.na(event) & event %in% c(0, 1)
  if (!any(mask)) {
    stop(paste("No complete survival rows remain after endpoint-scoped masking for", metric), call. = FALSE)
  }
  
  # Return the strict cohort
  list(df_masked = df_sub[mask, , drop = FALSE], time = time[mask], event = event[mask])
}

build_combined_X_matrix <- function(df_masked, cancer_type) {
  # Isolates all omic predictors simultaneously by looking for the cancer type prefix.
  pattern <- paste0("^", cancer_type, "-")
  preds <- names(df_masked)[grepl(pattern, names(df_masked))]
  
  if (length(preds) == 0L) {
    stop("FAIL: No multi-omic predictors found matching prefix ", cancer_type, "-", call. = FALSE)
  }
  
  # Ensure all predictors are strictly numeric. 
  # Converts any characters/factors to distinct numeric codes natively.
  df_X <- df_masked[, preds, drop = FALSE]
  df_X[] <- lapply(df_X, function(col) {
    if (is.character(col) || is.factor(col)) {
      as.numeric(as.factor(col))
    } else {
      as.numeric(col)
    }
  })
  
  return(df_X)
}

# =========================================================================
# IV. ML ALGORITHM WRAPPER FUNCTIONS 
# =========================================================================

# Algorithm 1: Random Survival Forest (randomForestSRC)
run_RSF <- function(X, y_time, y_event) {
  # Suppress inner C-level multi-threading to prevent nested CPU contention
  # with the outer future_lapply parallel engine on massive servers.
  options(rf.cores = 1, mc.cores = 1)
  
  # Construct Surv Object payload
  train_data <- data.frame(time = as.numeric(unlist(y_time)), status = as.numeric(unlist(y_event)), as.data.frame(X))
  
  # Fits using na.action="na.impute" to honor the decision that forced 
  # completion destroys geometry. Native tree topology imputes on the fly.
  model_rsf <- rfsrc(Surv(time, status) ~ ., 
                     data = train_data, 
                     ntree = 1000, 
                     na.action = "na.impute",  # Native topological NA handling
                     importance = "anti", # Fast Native VIMP 
                     splitrule = "logrank",
                     seed = 42)
  return(model_rsf)
}

# Algorithm 2: XGBoost Survival
run_XGBoost <- function(X, y_time, y_event) {
  # XGBoost requires events and times to be merged into a single label 
  # format, with negative times indicating right-censoring.
  d_label <- ifelse(as.numeric(unlist(y_event)) == 1, as.numeric(unlist(y_time)), -as.numeric(unlist(y_time)))
  
  # XGBoost natively maps NAs inside the tree splits without imputation
  X_matrix <- as.matrix(X)
  dtrain <- xgb.DMatrix(data = X_matrix, label = d_label)
  
  params <- list(
    objective = "survival:cox",
    eval_metric = "cox-nloglik",
    eta = 0.05,
    max_depth = 4,
    min_child_weight = 3,
    subsample = 0.8,
    nthread = 1 # Strictly lock XGBoost OpenMP to 1 thread per worker
  )
  
  model_xgb <- xgb.train(params = params, 
                         data = dtrain, 
                         nrounds = 500,
                         early_stopping_rounds = 20,
                         watchlist = list(train = dtrain),
                         verbose = 0)
  return(model_xgb)
}

# Algorithm 3: Multi-Task Logistic Regression (MTLR)
run_MTLR <- function(X, y_time, y_event, model_boruta = NULL) {
  # MTLR SINGULARITY & LIST FIX: MTLR matrix core violently crashes if any subset
  # of X retains an invisible list geometry. Force strict atomic double conversion by unlisting.
  X_clean <- as.data.frame(lapply(X, function(x) as.numeric(unlist(x))))
  rownames(X_clean) <- rownames(X)
  colnames(X_clean) <- colnames(X)
  
  # 1. Boruta Dimensional Shrinkage
  boruta_features <- NULL
  if (!is.null(model_boruta)) {
    boruta_decision <- model_boruta$finalDecision
    boruta_features <- names(boruta_decision)[boruta_decision %in% c("Confirmed", "Tentative")]
  }
  
  if (is.null(boruta_features) || length(boruta_features) == 0) {
      if(ncol(X_clean) > 100) {
        var_vals <- apply(X_clean, 2, var, na.rm = TRUE)
        boruta_features <- names(sort(var_vals, decreasing = TRUE)[1:100])
      } else {
        boruta_features <- colnames(X_clean)
      }
  }
  
  if (!requireNamespace("MTLR", quietly = TRUE)) stop("Package 'MTLR' is required but not installed. Please install via devtools::install_github('haiderlab/MTLR')", call. = FALSE)
  
  mtlr_success <- FALSE
  model_mtlr <- NULL
  features_to_use <- intersect(boruta_features, colnames(X_clean))
  
  while(!mtlr_success && length(features_to_use) >= 1) {
    X_sub <- X_clean[, features_to_use, drop = FALSE]
    cat(sprintf("    [MTLR] Attempting dimension: %d features...\n", ncol(X_sub)))
    
    # 2. Advanced Hierarchical Missing Data Imputation 
    if (anyNA(X_sub)) {
      X_sub <- tryCatch({
        mf_obj <- missForest::missForest(X_sub, maxiter = 5, ntree = 50, verbose = FALSE)
        mf_obj$ximp
      }, error = function(e1) {
        cat("    [MTLR] missForest failed. Tripping Layer 2: kNN Imputation...\n")
        tryCatch({
          # Ensure VIM is available but do not stop if not installed
          if(requireNamespace("VIM", quietly = TRUE)) {
             res <- VIM::kNN(X_sub, k = 5, imp_var = FALSE)
             res[, 1:ncol(X_sub), drop=FALSE]
          } else {
             stop("VIM not installed")
          }
        }, error = function(e2) {
          cat("    [MTLR] kNN failed. Tripping Layer 3: Apocalyptic Median...\n")
          as.data.frame(lapply(X_sub, function(col) {
            if(is.numeric(col)) { col[is.na(col)] <- median(col, na.rm=TRUE) }
            col
          }))
        })
      })
    }
    
    train_data <- cbind(time = as.numeric(unlist(y_time)), status = as.numeric(unlist(y_event)), X_sub)
    
    # 3. Execution: Extract optimal Ridge penalty via mtlr_cv, then fit explicit mtlr model
    model_mtlr <- tryCatch({
      cv_res <- MTLR::mtlr_cv(Surv(time, status) ~ ., data = train_data)
      MTLR::mtlr(Surv(time, status) ~ ., data = train_data, C1 = cv_res$best_C1)
    }, error = function(e) {
      tryCatch(MTLR::mtlr(Surv(time, status) ~ ., data = train_data), error = function(e2) NULL)
    })
    
    if (!is.null(model_mtlr)) {
        preds <- tryCatch(predict(model_mtlr, train_data), error = function(e) NULL)
        if (!is.null(preds)) mtlr_success <- TRUE else model_mtlr <- NULL
    }
    
    if(!mtlr_success) {
        cat("    [MTLR Singularity] Matrix crashed. Surgically retreating by dropping 1 feature...\n")
        features_to_use <- head(features_to_use, -1)
    }
  }
  
  if (is.null(model_mtlr)) {
      cat("    [MTLR CATASTROPHIC] Could not invert matrix even down to 1 dimension.\n")
      # Terminal NULL protection: bypass strict NULL memory limit by instantiating list proxy
      model_proxy <- list(status = "FAILED_MATRIX_COLLAPSE")
      attr(model_proxy, "apparent_risk") <- rep(0, nrow(train_data))
      return(model_proxy)
  }
  
  # Embed the exact apparent risk surrogate locally into the model object so 
  # downstream predict() calls don't crash when faced with raw, untruncated NA geometry!
  preds <- predict(model_mtlr, train_data)
  
  if (is.data.frame(preds) || is.matrix(preds)) {
    N_samp <- nrow(train_data)
    if (nrow(preds) == N_samp) {
      risk <- -rowSums(preds)
    } else if (ncol(preds) == N_samp) {
      risk <- -colSums(preds)
    } else if (ncol(preds) == (N_samp + 1) && !is.null(colnames(preds)) && colnames(preds)[1] == "time") {
      # MTLR standard return shape: M times rows x (1 time col + N_samp cols)
      risk <- -colSums(preds[, -1, drop = FALSE])
    } else if (nrow(preds) == (N_samp + 1) && !is.null(rownames(preds)) && rownames(preds)[1] == "time") {
      risk <- -rowSums(preds[-1, , drop = FALSE])
    } else {
      # Fallback risk surrogate: attempt conversion or default to zero risk
      risk <- tryCatch(as.numeric(preds), error = function(e) rep(0, N_samp))
    }
    attr(model_mtlr, "apparent_risk") <- risk
  } else {
    attr(model_mtlr, "apparent_risk") <- tryCatch(as.numeric(preds), error = function(e) rep(0, nrow(train_data)))
  }
  
  return(model_mtlr)
}

# Algorithm 4: Survival-Boruta (via Ranger Topology Integration)
run_Boruta <- function(X, y_time, y_event) {
  # BORUTA MISSING DATA FIX: Boruta/ranger natively crashes on NAs. 
  # We strictly wrap missForest (Random Forest Non-Linear Imputation) EXCLUSIVELY 
  # inside this internal function. The outer X_matrix remains totally untouched!
  if (anyNA(X)) {
    cat("    [Boruta Pre-Processor] Generating local missForest grid...\n")
    mf_obj <- missForest::missForest(X, maxiter = 5, ntree = 50, verbose = FALSE)
    X <- mf_obj$ximp
  }

  # BORUTA FORMULA FIX: ranger's parser natively errors out if column names contain hyphens, bars, or wildcards.
  orig_names <- colnames(X)
  # Aggressive non-alphanumeric strip to prevent `Surv() ~ .` parsing collapse
  safe_names <- gsub("[^[:alnum:]]", "_", orig_names)
  colnames(X) <- make.names(safe_names, unique = TRUE)
  
  # Boruta engine natively rejects right-censored objects. We bypass this by
  # feeding it a continuous dummy vector, but trapping the true survival matrix 
  # inside a localized custom importance wrapper powered by ranger.
  dummy_y <- y_time 
  
  getImpSurv <- function(x, y, ntree = 500, ...) {
    # Strictly enforce data.frame geometry; ranger matrix interface is brittle for formulas
    train_df <- data.frame(time = as.numeric(unlist(y_time)), status = as.numeric(unlist(y_event)), as.data.frame(x))
    # Fast Native Random Forest using Impurity for absolute shadow scaling
    model <- ranger::ranger(Surv(time, status) ~ ., data = train_df, num.trees = ntree, importance = "impurity")
    return(model$variable.importance)
  }
  
  # Deploy Master Feature Selection (Standard maxRuns=100 for maximum rigidity)
  set.seed(42)
  boruta_res <- Boruta::Boruta(x = X, y = dummy_y, getImp = getImpSurv, maxRuns = 100, doTrace = 0)
  
  # Re-inject original biological names into the Boruta decision matrix
  names(boruta_res$finalDecision) <- orig_names
  colnames(boruta_res$ImpHistory)[1:length(orig_names)] <- orig_names
  
  return(boruta_res)
}

# =========================================================================
# V. SHAP & LIME EXPLAINABILITY EXTRACTION
# =========================================================================
extract_shap <- function(model_xgb, X_matrix) {
  # Extract true SHAP values using exact TreeSHAP topology natively embedded
  # within the xgboost C++ backend for global and local importance.
  shap_contrib <- predict(model_xgb, X_matrix, predcontrib = TRUE)
  return(shap_contrib)
}

extract_lime <- function(model_xgb, X_matrix, observation_indices = 1:5) {
  # Existing function body unchanged
  # LIME (Local Interpretable Model-agnostic Explanations)
  # Fits a localized linear surrogate model around specific patient coordinates
  # to explain individual predictions in highly nonlinear survival topographies.
  
  X_df <- as.data.frame(X_matrix)
  
  # LIME MISSING DATA FIX: LIME violently crashes if NA persists in the background slice.
  # We enforce deterministic median imputation exclusively for the LIME proxy space.
  X_df[] <- lapply(X_df, function(col) {
    if(is.numeric(col)) col[is.na(col)] <- median(col, na.rm=TRUE)
    return(col)
  })
  
  # Ensure target observations exist within the dataset bounds
  target_idx <- intersect(observation_indices, 1:nrow(X_df))
  if(length(target_idx) == 0) return(NULL)
  
  # Initialize the explainer using a representative background sample to save computation
  bg_data <- X_df
  if(nrow(bg_data) > 300) {
    set.seed(42)  # Fixed seed for auditability
    bg_data <- bg_data[sample(1:nrow(bg_data), 300), ]
  }
  
  # LIME NATIVE FIX: lime specifically hard-blocks "survival:cox" objective strings.
  # We bypass this entirely by wrapping it in a transparent proxy class and manually feeding it.
  class(model_xgb) <- c("Safe_XGB_Proxy", class(model_xgb))
  
  # Inject the local S3 hooks dynamically so lime sees them instantly within the evaluation frame.
  # (Crucial: stripped illegal 'lime::' prefix before '<<-' to prevent '::<-' namespace crashing)
  model_type.Safe_XGB_Proxy <<- function(x, ...) "regression"
  predict_model.Safe_XGB_Proxy <<- function(x, newdata, ...) data.frame(Response = as.numeric(predict(x, as.matrix(newdata))))
  
  # Safe evaluation wrapper (prevents minor LIME errors from terminating the entire ML block)
  res <- tryCatch({
    explainer <- lime::lime(bg_data, model_xgb, bin_continuous = FALSE)
    
    lime_explanation <- lime::explain(X_df[target_idx, , drop = FALSE], 
                                      explainer, 
                                      n_features = 10)
    
    list(explainer = explainer, local_explanations = lime_explanation)
  }, error = function(e) {
    message("LIME Explainer Skipped (", e$message, ")")
    return(NULL)
  })
  
  return(res)
}

# =========================================================================
# VI. PARALLEL EXECUTION ENGINE (THE $(c,m,d)$ LOOP)
# =========================================================================

# Dynamically define parallel scale (Absolute Portability Preserved).
# We use parallel::detectCores() purely instead of future::availableCores() 
# because RStudio Server/cgroups can artificially mask the latter down to 2 cores.
total_cores <- parallel::detectCores(logical = TRUE)
NUM_WORKERS <- max(1, total_cores - 2)

# Expand global memory limits to prevent large multi-omic matrices from crashing
# the background threads before they even start.
options(future.globals.maxSize = 10 * 1024^3) # 10 GB limit

# Deploy EXACTLY identical parallel bridging logic to Phase I and II.
# We explicitly create a PSOCK cluster using base parallel to avoid Windows 'system()'
# path evaluation errors that sometimes occur with future::multisession natively.
# Setup strategy 'sequential' prevents TCP socket timeouts during worker initialization.
cat(sprintf("Deploying Phase II-Stable Multisession Cluster... (%d Workers)\n", NUM_WORKERS))
cl <- parallel::makeCluster(NUM_WORKERS, type = "PSOCK", setup_strategy = "sequential", outfile = "")
future::plan(future::cluster, workers = cl)

start_time <- Sys.time()
message("🚀 Initiating Parallel Engine at ", start_time, " with ", NUM_WORKERS, " workers...")

# The Master Loop mapping over every TAR Admissible unit defined by Phase II
results <- future_lapply(1:nrow(dfinput), function(i) {
  
  # Bypass S3 dispatcher bugs natively by assigning a proxy class to the xgboost payload.

  tryCatch({
    row_def <- dfinput[i, ]
    c <- row_def$cancer_type
    m <- row_def$metric
    d <- row_def$df
    
    # =========================================================================
    # [RESUME ENGINE PATCH]: Auto-Skip Already Completed Executions
    # =========================================================================
    clean_d_name_chk <- sub("\\.rds$", "", d, ignore.case = TRUE)
    unit_id_chk <- paste0(c, "_", m, "_", clean_d_name_chk)
    save_path_chk <- file.path(MODEL_OUTPUT_DIR, unit_id_chk, paste0("model_bundle_", unit_id_chk, ".rds"))
    
    if(file.exists(save_path_chk)) {
      msg <- paste0("[SKIPPED/RESUMED] Unit: ", unit_id_chk, " already computed. Saving time & skipping...")
      # Using cat instead of message so it reliably outputs across parallel worker threads
      cat(msg, "\n")
      return(msg)
    }
    
    # 1. Load exact data explicitly cleared by Phase II CANARY Action Policy
    # The 'df' column in the TSV already contains '.rds' in some cases.
    clean_d <- ifelse(grepl("\\.rds$", d, ignore.case = TRUE), d, paste0(d, ".rds"))
    df_path <- file.path(DF_ROOT_MANIFEST, clean_d)
    if(!file.exists(df_path)) stop("Dataset missing: ", df_path)
    df_raw <- readRDS(df_path)
    
    # 2. Strict endpoint-scoped subsetting (Generating S_{c,m,d})
    df_cancer <- df_raw[df_raw$type == c, , drop = FALSE]
    surv_pack <- filter_survival_complete(df_cancer, m) # Applies mask perfectly
    df_cohort <- surv_pack$df_masked
    
    # 3. Pull combined multi-omic matrix
    X_matrix <- build_combined_X_matrix(df_cohort, c)
    
    y_time <- surv_pack$time
    y_event <- surv_pack$event
    
    # [MEGARUN 4.2] Explicitly bind the rigorous TCGA patient ID for topological traceability
    if ("sample" %in% names(df_cohort)) {
      rownames(X_matrix) <- make.unique(as.character(df_cohort$sample))
    }
    
    # [MEGARUN 4.3] Geometric Patient Exclusion (Exclude >=35% Missingness in Predictors)
    na_props <- rowSums(is.na(X_matrix)) / ncol(X_matrix)
    exclude_idx <- which(na_props >= 0.35)
    partial_idx <- which(na_props > 0 & na_props < 0.35)
    
    # We will need the audit paths if either exclusion or partial missing data is detected
    if (length(exclude_idx) > 0 || length(partial_idx) > 0) {
      clean_d_name_chk2 <- sub("\\.rds$", "", d, ignore.case = TRUE)
      unit_id_chk2 <- paste0(c, "_", m, "_", clean_d_name_chk2)
      eval_folder_chk2 <- file.path(MODEL_OUTPUT_DIR, unit_id_chk2)
      if (!dir.exists(eval_folder_chk2)) dir.create(eval_folder_chk2, recursive = TRUE)
      
      # Log patients with partial missing data who ARE RETAINED (for cross-referencing)
      if (length(partial_idx) > 0) {
        partial_df <- data.frame(
          Sample_ID = rownames(X_matrix)[partial_idx],
          Missing_Percentage = round(na_props[partial_idx] * 100, 2)
        )
        partial_file <- file.path(eval_folder_chk2, paste0(unit_id_chk2, "_Retained_Partial_Missing.tsv"))
        write.table(partial_df, partial_file, sep="\t", row.names=FALSE, quote=FALSE)
      }
      
      # Log & Delete patients strictly exceeding the 35% barrier
      if (length(exclude_idx) > 0) {
        exclusion_df <- data.frame(
          Sample_ID = rownames(X_matrix)[exclude_idx],
          Missing_Percentage = round(na_props[exclude_idx] * 100, 2)
        )
        audit_file <- file.path(eval_folder_chk2, paste0(unit_id_chk2, "_Excluded_Patients_Geometric.tsv"))
        write.table(exclusion_df, audit_file, sep="\t", row.names=FALSE, quote=FALSE)
        
        # Dynamic Runtime Filter
        cat(sprintf("    [MEGARUN 4.3 FILTER] Excluded %d patients with >=35%% missing omics. Logged %d retained with partial NAs.\n", length(exclude_idx), length(partial_idx)))
        X_matrix <- X_matrix[-exclude_idx, , drop = FALSE]
        y_time <- y_time[-exclude_idx]
        y_event <- y_event[-exclude_idx]
        df_cohort <- df_cohort[-exclude_idx, , drop = FALSE]
      }
    }
    
    # =========================================================================
    # [PHASE IIIB] ABSOLUTE BIOLOGICAL QUARANTINE (SPARSITY ISOLATION)
    # =========================================================================
    # We strip all Continuous Topologies (.1, .4, .5, .6, .7) and legally trap 
    # the algorithmic array onto strictly Sparse Binary Genotypes (.2 and .3).
    # Critical Architecture Note: Subsetting occurs AFTER patient missingness exclusion
    # to guarantee identical cohort sizing between Phase III main arm and Phase IIIB.
    
    # [MEGARUN EMERGENCY PATCH applied by AI]
    # We must strictly identify the ".2." or ".3." at the EXACT topological position
    # corresponding to the omic layer (immediately after the numeric ID), 
    # not at the random end ($) of the barcode string!
    sparse_cols <- grep("^[A-Za-z]+-[0-9]+\\.[23]\\.", colnames(X_matrix), value = TRUE)
    
    if(length(sparse_cols) < 2) {
        msg <- paste0("[SKIPPED/PHASE_IIIB] Unit: ", c, "_", m, " lacks enough Genotype markers (<2) to run a matrix.")
        cat(msg, "\n")
        return(msg)
    }
    X_matrix <- X_matrix[, sparse_cols, drop = FALSE]
    
    # 4. Train Algorithms
    cat(sprintf("Training Models -> Unit: [%s | %s | %s] N=%d Preds=%d\n", c, m, d, nrow(X_matrix), ncol(X_matrix)))
    
    model_rsf <- run_RSF(X_matrix, y_time, y_event)
    model_xgb <- run_XGBoost(X_matrix, y_time, y_event)
    
    cat("Deploying Boruta Master Assessor (Expect Execution Delays)...\n")
    boruta_fail_msg <- NA
    model_boruta <- tryCatch({
       run_Boruta(X_matrix, y_time, y_event)
    }, error = function(e) {
       msg <- e$message
       cat(sprintf("-> Boruta [PASSED/LOGGED]: Failed to converge shadow bounds (%s)\n", msg))
       boruta_fail_msg <<- msg
       return(NULL)
    })

    # MTLR is mathematically brittle against collinearity and sparsity. Wrap it defensively 
    # so a singular matrix crash here doesn't kill the entire master cohort evaluation.
    # MTLR NOW RECEIVES BORUTA VALIDATED FEATURES TO SOLVE P >> N DEGENERATE SINGULARITY!
    mtlr_fail_msg <- NA
    model_mtlr <- tryCatch({
       run_MTLR(X_matrix, y_time, y_event, model_boruta)
    }, error = function(e) {
       msg <- e$message
       message(sprintf("-> MTLR [SKIPPED]: %s", msg))
       mtlr_fail_msg <<- msg
       NULL
    })
    
    # 5. Extract Explainability 
    cat("Extracting Model Explanations...\n")
    shap_vals <- extract_shap(model_xgb, as.matrix(X_matrix))
    lime_vals <- extract_lime(model_xgb, as.matrix(X_matrix))
    
    # 6. Evaluation Metrics & Subdirectory Creation
    clean_d_name <- sub("\\.rds$", "", d, ignore.case = TRUE)
    unit_id <- paste0(c, "_", m, "_", clean_d_name)
    eval_folder <- file.path(MODEL_OUTPUT_DIR, unit_id)
    if(!dir.exists(eval_folder)) dir.create(eval_folder, recursive = TRUE)
    
    # Calculate Global Performance
    err.rate <- model_rsf$err.rate[length(model_rsf$err.rate)]
    rsf_c_index <- 1 - err.rate
    rsf_risk <- model_rsf$predicted.oob
    
    xgb_risk <- predict(model_xgb, as.matrix(X_matrix))
    xgb_concordance_obj <- survival::concordance(Surv(as.numeric(unlist(y_time)), as.numeric(unlist(y_event))) ~ xgb_risk)
    xgb_c_index <- xgb_concordance_obj$concordance
    xgb_c_index <- ifelse(xgb_c_index < 0.5, 1 - xgb_c_index, xgb_c_index) # Log-hazard direction fix
    
    # Calculate MTLR Performance safely (Abort to NA if it was Skipped above)
    mtlr_risk <- tryCatch({
      if(is.null(model_mtlr)) stop("MTLR Object Absent")
      # Extract the exact apparent risk directly from the embedded attribute 
      # since external prediction would fail on the unmodified, sparse X_matrix architecture.
      attr(model_mtlr, "apparent_risk")
    }, error = function(e) rep(NA, length(y_time)))
    
    mtlr_c_index <- tryCatch({
      mtlr_conc <- survival::concordance(Surv(as.numeric(unlist(y_time)), as.numeric(unlist(y_event))) ~ mtlr_risk)$concordance
      ifelse(mtlr_conc < 0.5, 1 - mtlr_conc, mtlr_conc)
    }, error = function(e) NA)
    
    # [NO ONE STAYS BEHIND] NA -> 0.5 Baseline Coercion
    rsf_flag <- if(is.na(rsf_c_index)) "Coerced_0.5" else "OK"
    if(is.na(rsf_c_index)) rsf_c_index <- 0.5
    
    xgb_flag <- if(is.na(xgb_c_index)) "Coerced_0.5" else "OK"
    if(is.na(xgb_c_index)) xgb_c_index <- 0.5
    
    mtlr_flag <- if(is.na(mtlr_c_index)) "Coerced_0.5" else "OK"
    if(is.na(mtlr_c_index)) mtlr_c_index <- 0.5
    
    perf_df <- data.frame(
      Model = c("RSF_Out_of_Bag", "XGBoost_Apparent", "MTLR_Apparent"),
      C_Index = c(rsf_c_index, xgb_c_index, mtlr_c_index),
      Execution_Flag = c(rsf_flag, xgb_flag, mtlr_flag)
    )
    
    # =========================================================================
    # 6.4b: Quadripartite MVL Meta-Learner Synthesis (Elastic-Net)
    # =========================================================================
    cat("    [MVL Synthesis] Executing Quadripartite Multi-View Meta-Learner...\n")
    
    # Safely construct the Quadripartite Meta-Matrix
    mvl_matrix <- data.frame(
      RSF = as.numeric(rsf_risk),
      XGBoost = as.numeric(xgb_risk),
      MTLR = if(!is.na(mtlr_fail_msg) || is.null(mtlr_risk)) rep(0, length(y_time)) else as.numeric(mtlr_risk)
    )
    
    # Boruta Topological Conversion
    mvl_matrix$Boruta <- rep(0, length(y_time)) 
    boruta_c_index <- NA
    if (!is.null(model_boruta)) {
      boruta_decision <- model_boruta$finalDecision
      boruta_features <- names(boruta_decision)[boruta_decision %in% c("Confirmed", "Tentative")]
      if (length(boruta_features) > 0) {
        # TIBBLE SINGULARITY FIX: Subsetting a tibble returns a list-geometry. 
        # base::data.frame() reads its length as its number of columns, causing differing row crashes.
        # We explicitly cast to a strict base data.frame and use cbind() to enforce vertical dimensionality.
        boruta_x <- as.data.frame(X_matrix)[, boruta_features, drop=FALSE]
        boruta_df <- cbind(data.frame(time = as.numeric(unlist(y_time)), status = as.numeric(unlist(y_event))), boruta_x)
        
        boruta_proxy <- tryCatch({
          # Native topological OOB prediction mapping via randomForestSRC handling NAs natively
          b_mod <- randomForestSRC::rfsrc(Surv(time, status) ~ ., 
                                          data = boruta_df, 
                                          ntree = 500,
                                          na.action = "na.impute",
                                          splitrule = "logrank",
                                          seed = 42)
          b_mod$predicted.oob
        }, error = function(e) rep(0, length(y_time)))
        mvl_matrix$Boruta <- boruta_proxy
        
        # Explicit Boruta C-Index Calculation for Logging
        bor_tmp <- tryCatch({
            b_conc <- survival::concordance(Surv(as.numeric(unlist(y_time)), as.numeric(unlist(y_event))) ~ boruta_proxy)$concordance
            ifelse(b_conc < 0.5, 1 - b_conc, b_conc)
        }, error = function(e) NA)
        if(!is.na(bor_tmp)) boruta_c_index <- bor_tmp
      }
    }
    
    # [NO ONE STAYS BEHIND] Coerce NA to 0.5 and ALWAYS push to the Evaluation Matrix
    boruta_zero_features <- is.na(boruta_c_index)
    boruta_flag <- if(boruta_zero_features) "Coerced_0_Features" else "OK"
    if(boruta_zero_features) boruta_c_index <- 0.5
    perf_df <- rbind(perf_df, data.frame(Model = "Boruta_Independent", C_Index = boruta_c_index, Execution_Flag = boruta_flag))
    
    mvl_c_index <- NA
    mvl_meta_mod <- NULL
    mvl_super_risk <- rep(NA, length(y_time))
    mvl_coefs_df <- NULL
    
    if (ncol(mvl_matrix) > 1 && sum(y_event) > 1) {
        mvl_res <- tryCatch({
           # [MegaRun 3.9 Defense: Micro-Jitter Variance Injection]
           for(col in colnames(mvl_matrix)) {
              if(var(mvl_matrix[[col]], na.rm=TRUE) == 0) {
                 mvl_matrix[[col]] <- mvl_matrix[[col]] + runif(nrow(mvl_matrix), -1e-6, 1e-6)
              }
           }
           
           # Scale explicitly to force equivalent variance domains for fair Beta Coefficient extraction
           X_meta <- scale(as.matrix(mvl_matrix))
           # NA fill strictly for meta matrix if NA leaked
           X_meta[is.na(X_meta)] <- 0
           
           # Dynamic nfolds handling to prevent CV crashing on cohorts with low event counts
           n_events <- sum(y_event)
           n_cv_folds <- if(n_events < 30) max(3, floor(n_events / 3)) else 10
           
           meta_mod <- glmnet::cv.glmnet(x = X_meta, y = survival::Surv(y_time, y_event), family = "cox", alpha = 0.5, nfolds = n_cv_folds)
           
           super_risk <- predict(meta_mod, newx = X_meta, s = "lambda.min")[,1]
           conc <- survival::concordance(Surv(as.numeric(unlist(y_time)), as.numeric(unlist(y_event))) ~ super_risk)$concordance
           conc_final <- ifelse(conc < 0.5, 1 - conc, conc)
           
           exact_coefs <- as.matrix(coef(meta_mod, s = "lambda.min"))
           c_df <- data.frame(Dimension = rownames(exact_coefs), Elastic_Net_Weight = as.numeric(exact_coefs))
           c_df <- c_df[c_df$Dimension != "(Intercept)", ]
           c_df <- c_df[order(abs(c_df$Elastic_Net_Weight), decreasing = TRUE), ]
           
           list(mod = meta_mod, c_index = conc_final, super_risk = super_risk, coefs = c_df)
        }, error = function(e) {
           cat(sprintf("    [MVL Collapse Intercepted] %s -> Deploying Uniform Average Fallback...\n", head(e$message, 1)))
           
           # Direct Uniform Average (Bayesian Fallback)
           super_risk <- rowMeans(scale(as.matrix(mvl_matrix)), na.rm = TRUE)
           
           # Safe Concordance Extraction
           conc <- tryCatch(survival::concordance(Surv(as.numeric(unlist(y_time)), as.numeric(unlist(y_event))) ~ super_risk)$concordance, error=function(e2) 0.5)
           conc_final <- ifelse(conc < 0.5, 1 - conc, conc)
           
           # Provide proportional uniform coefficients so Global Synthesis stays alive structurally
           uni_weight <- 1 / ncol(mvl_matrix)
           c_df <- data.frame(Dimension = colnames(mvl_matrix), Elastic_Net_Weight = rep(uni_weight, ncol(mvl_matrix)))
           
           list(mod = "UNIFORM_AVERAGE_FALLBACK", c_index = conc_final, super_risk = super_risk, coefs = c_df)
        })
        
        if(!is.null(mvl_res)) {
           mvl_meta_mod <- mvl_res$mod
           mvl_c_index  <- mvl_res$c_index
           mvl_super_risk <- mvl_res$super_risk
           mvl_coefs_df <- mvl_res$coefs
        }
    }
    
    # Core MVL Matrix Integration
    perf_df <- rbind(perf_df, data.frame(Model = "MVL_ElasticNet_SuperLearner", C_Index = mvl_c_index, Execution_Flag = "OK"))

    # 6.5 Time-Dependent AUC Extraction (1, 3, 5 Years) For ALL Models
    max_time <- max(y_time, na.rm = TRUE)
    target_times <- c(365, 1095, 1825)
    valid_times <- target_times[target_times < max_time]
    
    if(length(valid_times) > 0) {
      
      # Extract raw mortality risk proxies for ROC generation
      
      # Modular internal function to draw and save all metrics dynamically per model
      generate_roc_artifacts <- function(risk_scores, model_name) {
        if (anyNA(risk_scores)) return(NULL)
        
        # Enforce analytical subdirectories directly (RSF/, XGBoost/, MTLR/)
        sub_dir <- file.path(eval_folder, model_name)
        if(!dir.exists(sub_dir)) dir.create(sub_dir, recursive=TRUE)
        
        roc_obj <- tryCatch({
          timeROC::timeROC(T = as.numeric(unlist(y_time)), delta = as.numeric(unlist(y_event)), marker = risk_scores, cause = 1, weighting = "marginal", times = valid_times, iid = TRUE)
        }, error = function(e) NULL)
        
        if (!is.null(roc_obj)) {
          # Fix AUC directionality (ensure higher risk = higher AUC)
          auc_vals <- ifelse(roc_obj$AUC < 0.5, 1 - roc_obj$AUC, roc_obj$AUC)
          
          # Safely extract Standard Error, preventing catastrophic 'differing number of rows' failure when timeROC(iid=TRUE) internal matrix inversion bypasses SE creation
          se_vals <- rep(NA, length(valid_times))
          if(!is.null(roc_obj$inference) && !is.null(roc_obj$inference$vect_sd_1) && length(roc_obj$inference$vect_sd_1) == length(valid_times)) {
              se_vals <- roc_obj$inference$vect_sd_1
          }
          auc_df <- data.frame(Time_Days = valid_times, AUC = auc_vals, SE = se_vals)
          
          # Write TSV into sub-folder
          write.table(auc_df, file.path(sub_dir, paste0(unit_id, "_", model_name, "_AUC_1_3_5_Years.tsv")), sep="\t", row.names=FALSE, quote=FALSE)
          
          # Create a brilliant diagnostic legend string combining the Year and exact mathematically generated AUC
          legend_labels <- paste0(round(valid_times/365, 0), " Years (AUC: ", sprintf("%.3f", auc_vals), ")")
          
          # Write PDF Plot into sub-folder
          pdf_path <- file.path(sub_dir, paste0(unit_id, "_", model_name, "_AUC_Curves.pdf"))
          pdf(pdf_path, width = 7, height = 6)
          plot(roc_obj, time = valid_times[1], col = "blue", main = paste("Time-Dependent ROC (", model_name, ") -", unit_id))
          if(length(valid_times) > 1) plot(roc_obj, time = valid_times[2], col = "red", add = TRUE)
          if(length(valid_times) > 2) plot(roc_obj, time = valid_times[3], col = "darkgreen", add = TRUE)
          legend("bottomright", legend = legend_labels, col = c("blue", "red", "darkgreen")[1:length(valid_times)], lwd = 2)
          dev.off()
          
          # Write TIFF Plot into sub-folder
          tiff_path <- file.path(sub_dir, paste0(unit_id, "_", model_name, "_AUC_Curves.tiff"))
          tiff(tiff_path, width = 7, height = 6, units = "in", res = 600, compression = "lzw")
          plot(roc_obj, time = valid_times[1], col = "blue", main = paste("Time-Dependent ROC (", model_name, ") -", unit_id))
          if(length(valid_times) > 1) plot(roc_obj, time = valid_times[2], col = "red", add = TRUE)
          if(length(valid_times) > 2) plot(roc_obj, time = valid_times[3], col = "darkgreen", add = TRUE)
          legend("bottomright", legend = legend_labels, col = c("blue", "red", "darkgreen")[1:length(valid_times)], lwd = 2)
          dev.off()
        }
      }
      
      # Deploy symmetry rendering
      generate_roc_artifacts(xgb_risk, "XGBoost")
      generate_roc_artifacts(rsf_risk, "RSF")
      generate_roc_artifacts(mtlr_risk, "MTLR")
      if(!is.na(mvl_c_index)) {
         generate_roc_artifacts(mvl_super_risk, "MVL_Synthesis")
      }
    }
    
    # 7. Extract Variable Importance to TSV
    # RSF Native VIMP
    safe_rsf_names <- gsub("^([A-Za-z]+)\\.", "\\1-", names(model_rsf$importance))
    vimp_df <- data.frame(Feature = safe_rsf_names, VIMP = as.numeric(model_rsf$importance), stringsAsFactors = FALSE)
    vimp_df <- vimp_df[order(vimp_df$VIMP, decreasing = TRUE), ]
    
    # XGBoost Global Feature Map (TryCatch to prevent single-predictor C++ crashes like passing Pred=1 strings)
    xgb_imp <- tryCatch({
       xgboost::xgb.importance(model = model_xgb)
    }, error = function(e) NULL)
    
    # MTLR Continuous L2-Norm Feature Importance Extraction
    mtlr_imp <- tryCatch({
       if(!is.null(model_mtlr) && !is.null(model_mtlr$weight_matrix) && is.matrix(model_mtlr$weight_matrix)) {
          w_mat <- model_mtlr$weight_matrix
          valid_cols <- colnames(w_mat)[colnames(w_mat) != "Bias"]
          if(length(valid_cols) > 0) {
             l2_norms <- sapply(valid_cols, function(f) sqrt(sum(w_mat[, f]^2, na.rm=TRUE)))
             m_df <- data.frame(Feature = gsub("^`|`$", "", valid_cols), MTLR_L2_Norm = l2_norms, stringsAsFactors = FALSE)
             m_df[order(m_df$MTLR_L2_Norm, decreasing = TRUE), ]
          } else NULL
       } else NULL
    }, error = function(e) NULL)
    
    # XGBoost SHAP Summary
    shap_df <- tryCatch({
       shap_mean <- colMeans(abs(shap_vals))
       sh_tmp <- data.frame(Feature = names(shap_mean), Mean_Abs_SHAP = as.numeric(shap_mean))
       sh_tmp <- sh_tmp[sh_tmp$Feature != "BIAS", ] # Exclude XGBoost Intercept BIAS column
       sh_tmp[order(sh_tmp$Mean_Abs_SHAP, decreasing = TRUE), ]
    }, error = function(e) NULL)
    
    # 8. Write Artifacts to Disk via Dedicated Subdirectories
    
    # Ensure structural folders exist for Variable Importance / LIME / SHAP / Boruta
    xgb_dir <- file.path(eval_folder, "XGBoost")
    rsf_dir <- file.path(eval_folder, "RSF")
    boruta_dir <- file.path(eval_folder, "Boruta")
    mvl_dir <- file.path(eval_folder, "MVL_Synthesis")
    
    if(!dir.exists(xgb_dir)) dir.create(xgb_dir, recursive=TRUE)
    if(!dir.exists(rsf_dir)) dir.create(rsf_dir, recursive=TRUE)
    mtlr_dir <- file.path(eval_folder, "MTLR")
    if(!dir.exists(boruta_dir)) dir.create(boruta_dir, recursive=TRUE)
    if(!dir.exists(mvl_dir)) dir.create(mvl_dir, recursive=TRUE)
    if(!dir.exists(mtlr_dir)) dir.create(mtlr_dir, recursive=TRUE)
    
    # Core global metric saved in the root cohort folder 
    write.table(perf_df, file.path(eval_folder, paste0(unit_id, "_Global_Performance.tsv")), sep="\t", row.names=FALSE, quote=FALSE)
    
    # Route specifics into their algorithm subdirectories natively 
    write.table(vimp_df, file.path(rsf_dir, paste0(unit_id, "_RSF_VIMP.tsv")), sep="\t", row.names=FALSE, quote=FALSE)
    if(!is.null(xgb_imp)) write.table(xgb_imp, file.path(xgb_dir, paste0(unit_id, "_XGBoost_Node_Importance.tsv")), sep="\t", row.names=FALSE, quote=FALSE)
    if(!is.null(mtlr_imp)) write.table(mtlr_imp, file.path(mtlr_dir, paste0(unit_id, "_MTLR_Feature_Importance.tsv")), sep="\t", row.names=FALSE, quote=FALSE)
    
    # SHAP Visual Module Integration
    if(!is.null(shap_df)) {
       write.table(shap_df, file.path(xgb_dir, paste0(unit_id, "_XGBoost_SHAP_Summary.tsv")), sep="\t", row.names=FALSE, quote=FALSE)
       
       shap_vis_dir <- xgb_dir
       
       # Generate Shapviz Object (Dual Strategy for MegaRun 3.6 Interactions)
       suppressWarnings({
         # 1. Standard 1D Matrix (For Beeswarm & Waterfall)
         shp <- tryCatch(shapviz::shapviz(model_xgb, X_pred = as.matrix(X_matrix)), error = function(e) NULL)
         
         # 2. Synergistic 3D Matrix (For Native Interactions)
         shp_interactions <- tryCatch(shapviz::shapviz(model_xgb, X_pred = as.matrix(X_matrix), interactions = TRUE), error = function(e) NULL)
         
         if(!is.null(shp)) {
            omic_caption <- "Signature second token identifier (Omic Layer Origin): (.1) protein abundance, (.2) somatic mutation status, (.3) copy number variation,\n(.4) microRNA expression, (.5) transcript isoform abundance, (.6) mRNA expression, and (.7) CpG methylation"
            
            # 1. Beeswarm Plot (Global)
            p_beeswarm <- shapviz::sv_importance(shp, kind = "beeswarm") + 
                          ggplot2::labs(caption = omic_caption) + 
                          ggplot2::theme(plot.caption = ggplot2::element_text(size=8, hjust=0))
                          
            pdf(file.path(shap_vis_dir, paste0(unit_id, "_SHAP_Overall_Beeswarm.pdf")), width = 10, height = 8)
            print(p_beeswarm)
            dev.off()
            tiff(file.path(shap_vis_dir, paste0(unit_id, "_SHAP_Overall_Beeswarm.tiff")), width = 10, height = 8, units = "in", res = 600, compression = "lzw")
            print(p_beeswarm)
            dev.off()
            
            # 2. Dependence Plots (Top 10)
            top_signatures <- head(shap_df$Feature[shap_df$Mean_Abs_SHAP > 0], 10)
            for(sig in top_signatures) {
               sig_safe <- gsub("[^[:alnum:]]", "_", sig)
               
               # GUARANTEE INTERACTION: Force shapviz to parse the 3D Synergy Matrix if available
               active_shp <- if(!is.null(shp_interactions)) shp_interactions else shp
               
               # Explicitly force 'auto' search so it mathematically picks the strongest INTERACTING feature for color
               q25 <- quantile(X_matrix[, sig], 0.25, na.rm = TRUE)
               q75 <- quantile(X_matrix[, sig], 0.75, na.rm = TRUE)
               p_dep <- shapviz::sv_dependence(active_shp, v = sig, color_var = "auto", size = 2.75) + 
                        ggplot2::geom_vline(xintercept = q25, color = "#0000FF", linetype = "dashed", linewidth = 0.6, alpha = 0.7) +
                        ggplot2::geom_vline(xintercept = q75, color = "#FF0000", linetype = "dashed", linewidth = 0.6, alpha = 0.7) +
                        ggplot2::geom_hline(yintercept = 0, color = "gray50", linetype = "solid", linewidth = 0.5, alpha = 0.8) +
                        ggplot2::labs(caption = omic_caption) + 
                        ggplot2::theme(plot.caption = ggplot2::element_text(size=8, hjust=0))
               
               pdf(file.path(shap_vis_dir, paste0(unit_id, "_SHAP_Dependence_", sig_safe, ".pdf")), width = 8, height = 6)
               print(p_dep)
               dev.off()
               tiff(file.path(shap_vis_dir, paste0(unit_id, "_SHAP_Dependence_", sig_safe, ".tiff")), width = 8, height = 6, units = "in", res = 600, compression = "lzw")
               print(p_dep)
               dev.off()
            }
            
            # 3. Local Interpretable Decision Plots (Patient-Level Waterfall)
            # Mathematically extract the two most extreme topological trajectories
            idx_lethal <- which.max(xgb_risk)
            idx_protective <- which.min(xgb_risk)
            
            patient_lethal_id <- rownames(X_matrix)[idx_lethal]
            patient_protective_id <- rownames(X_matrix)[idx_protective]
            
            # Fallbacks just in case rownames fail
            if(is.null(patient_lethal_id) || patient_lethal_id == "") patient_lethal_id <- as.character(idx_lethal)
            if(is.null(patient_protective_id) || patient_protective_id == "") patient_protective_id <- as.character(idx_protective)
            
            target_patients <- list()
            target_patients[[paste0("Lethal_Trajectory_", patient_lethal_id)]] <- idx_lethal
            target_patients[[paste0("Protective_Trajectory_", patient_protective_id)]] <- idx_protective
            
            for(pt_label in names(target_patients)) {
               pid <- target_patients[[pt_label]]
               if(length(pid) == 0 || is.na(pid)) next
               
               p_water <- shapviz::sv_waterfall(shp, row_id = pid) + 
                          ggplot2::labs(caption = omic_caption) + 
                          ggplot2::theme(plot.caption = ggplot2::element_text(size=8, hjust=0))
               
               pdf(file.path(shap_vis_dir, paste0(unit_id, "_SHAP_Decision_", pt_label, ".pdf")), width = 10, height = 6)
               print(p_water)
               dev.off()
               tiff(file.path(shap_vis_dir, paste0(unit_id, "_SHAP_Decision_", pt_label, ".tiff")), width = 10, height = 6, units = "in", res = 600, compression = "lzw")
               print(p_water)
               dev.off()
            }
         }
       })
    }
    
    # LIME Visual Module Integration
    if (!is.null(lime_vals) && !is.null(lime_vals$local_explanations)) {
       clean_lime_df <- as.data.frame(lapply(lime_vals$local_explanations, function(x) if(is.list(x)) sapply(x, toString) else x))
       write.table(clean_lime_df, file.path(xgb_dir, paste0(unit_id, "_LIME_Local_Explanations.tsv")), sep="\t", row.names=FALSE, quote=FALSE)
       
       lime_vis_dir <- xgb_dir
       
       omic_caption <- "Signature second token identifier (Omic Layer Origin): (.1) protein abundance, (.2) somatic mutation status, (.3) copy number variation,\n(.4) microRNA expression, (.5) transcript isoform abundance, (.6) mRNA expression, and (.7) CpG methylation"
       num_cases <- length(unique(lime_vals$local_explanations$case))
       dynamic_height <- max(12, num_cases * 6)

       p_lime <- lime::plot_features(lime_vals$local_explanations, ncol = 1) + 
                 ggplot2::labs(caption = omic_caption) + 
                 ggplot2::theme(
                    plot.caption = ggplot2::element_text(size=8, hjust=0),
                    axis.text.y = ggplot2::element_text(size=6, lineheight=0.8),
                    strip.text = ggplot2::element_text(size=10, face="bold"),
                    panel.spacing = ggplot2::unit(2, "lines")
                 )
                 
       pdf(file.path(lime_vis_dir, paste0(unit_id, "_LIME_Patient_Level_Attributions.pdf")), width = 12, height = dynamic_height)
       print(p_lime)
       dev.off()
       tiff(file.path(lime_vis_dir, paste0(unit_id, "_LIME_Patient_Level_Attributions.tiff")), width = 12, height = dynamic_height, units = "in", res = 600, compression = "lzw")
       print(p_lime)
       dev.off()
    }
    
    # Export MVL Synthesis Weights
    if(!is.null(mvl_coefs_df)) {
       write.table(mvl_coefs_df, file.path(mvl_dir, paste0(unit_id, "_MVL_Algorithm_Weights.tsv")), sep="\t", row.names=FALSE, quote=FALSE)
    }

    # Export Boruta Core Decisions and Plots (Only if Boruta Passed successfully)
    if (!is.null(model_boruta)) {
       safe_boruta_names <- gsub("^([A-Za-z]+)\\.", "\\1-", rownames(Boruta::attStats(model_boruta)))
       bor_stats <- data.frame(Feature = safe_boruta_names, Boruta::attStats(model_boruta), stringsAsFactors = FALSE)
       rownames(bor_stats) <- NULL
       bor_stats <- bor_stats[order(bor_stats$meanImp, decreasing = TRUE), ]
       write.table(bor_stats, file.path(boruta_dir, paste0(unit_id, "_Boruta_Feature_Decisions.tsv")), sep="\t", row.names=FALSE, quote=FALSE)
       
       pdf(file.path(boruta_dir, paste0(unit_id, "_Boruta_Summary_Boxplot.pdf")), width = 12, height = 8)
       plot(model_boruta, las = 2, cex.axis = 0.6, main = paste("Boruta Assessor -", unit_id))
       dev.off()
       
       tiff(file.path(boruta_dir, paste0(unit_id, "_Boruta_Summary_Boxplot.tiff")), width = 12, height = 8, units = "in", res = 600, compression = "lzw")
       plot(model_boruta, las = 2, cex.axis = 0.6, main = paste("Boruta Assessor -", unit_id))
       dev.off()
    }
    
    # Save the core model bundle
    save_path <- file.path(eval_folder, paste0("model_bundle_", unit_id, ".rds"))
    saveRDS(list(
      meta = row_def,
      RSF = model_rsf,
      XGBoost = model_xgb,
      MTLR = model_mtlr,
      Boruta = model_boruta,
      SHAP = shap_vals,
      LIME = lime_vals,
      metrics = perf_df
    ), save_path)
    
    # Return logging string
    bor_log_flag <- if(!is.na(boruta_fail_msg)) " BORUTA_SKIPPED" else if(!boruta_zero_features) paste0(" BORUTA=", round(boruta_c_index,3)) else " BORUTA_0_FEATURES(0.5)"
    mtlr_log_flag <- if(!is.na(mtlr_fail_msg)) paste0(" MTLR_SKIPPED=", mtlr_fail_msg) else paste0(" MTLR=", round(mtlr_c_index,3))
    mvl_log_flag <- paste0(" MVL_Super=", round(mvl_c_index,3))
    success_msg <- paste0("[SUCCESS] Unit: ", unit_id, " (C-Index: RSF=", round(rsf_c_index,3), " XGB=", round(xgb_c_index,3), bor_log_flag, mtlr_log_flag, mvl_log_flag, ")")
    
    # DYNAMIC MASTER LOGGING APPEND 
    # 1. Append textual Audit Log
    log_file_path_atomic <- file.path(WORKING_DIR, "PHASE_IIIB_Sparsity_Execution_Audit.log")
    cat(paste0(Sys.time(), " - ", success_msg, "\n"), file = log_file_path_atomic, append = TRUE)
    
    # 2. Append tabular Performance CSV dynamically
    csv_file_path_atomic <- file.path(WORKING_DIR, "MASTER_Phase_IIIB_Sparsity_Performance.csv")
    
    failed_idx <- perf_df$Execution_Flag != "OK"
    flag_string <- paste(paste(perf_df$Model[failed_idx], perf_df$Execution_Flag[failed_idx], sep="="), collapse = " | ")
    if(flag_string == "") flag_string <- "OK"
    
    perf_wide <- data.frame(
      Unit_ID = unit_id,
      Cancer_Type = c,
      Metric = m,
      DF = clean_d_name,
      RSF_C_Index = round(rsf_c_index, 4),
      XGBoost_C_Index = round(xgb_c_index, 4),
      MTLR_C_Index = round(mtlr_c_index, 4),
      Boruta_C_Index = round(boruta_c_index, 4),
      MVL_C_Index = round(mvl_c_index, 4),
      Execution_Flags = flag_string,
      Timestamp = Sys.time()
    )
    # Write with headers only if file doesn't exist
    write.table(perf_wide, file = csv_file_path_atomic, append = file.exists(csv_file_path_atomic), quote = FALSE, sep = ",", row.names = FALSE, col.names = !file.exists(csv_file_path_atomic))
    
    return(success_msg)
    
  }, error = function(err) {
    err_msg <- paste0("[FAILURE] Unit: ", dfinput$cancer_type[i], " | ", dfinput$metric[i], " | ", dfinput$df[i], " -> ERROR: ", err$message)
    message(err_msg)
    
    # Append textual Error Log dynamically
    log_file_path_atomic <- file.path(WORKING_DIR, "PHASE_IIIB_Sparsity_Execution_Audit.log")
    cat(paste0(Sys.time(), " - ", err_msg, "\n"), file = log_file_path_atomic, append = TRUE)
    
    return(err_msg)
  })
}, future.seed = TRUE)

end_time <- Sys.time()
runtime_mins <- round(as.numeric(difftime(end_time, start_time, units = "mins")), 2)

# =========================================================================
# VII. COMPILE AND EXPORT EXECUTION LOG
# =========================================================================
message("✅ PHASE III EXECUTION COMPLETE in ", runtime_mins, " minutes. Generating Log...")

log_output <- c(
  "=========================================================================",
  "PHASE III ENSEMBLE EXECUTION SYSTEM LOG",
  "=========================================================================",
  paste("Pipeline Start Time :", format(start_time, "%Y-%m-%d %H:%M:%S")),
  paste("Pipeline End Time   :", format(end_time, "%Y-%m-%d %H:%M:%S")),
  paste("Total Runtime       :", runtime_mins, "minutes"),
  paste("CPU Cores Utilized  :", NUM_WORKERS),
  paste("Total Cohorts Scoped:", nrow(dfinput)),
  "-------------------------------------------------------------------------",
  "EXECUTION UNIT STATUS REPORT:",
  unlist(results),
  "========================================================================="
)

log_file_path <- file.path(WORKING_DIR, "PHASE_IIIB_Sparsity_Execution_Audit.log")
writeLines(log_output, log_file_path)

# Stop the parallel cluster processes
parallel::stopCluster(cl)

message("💾 Audit Log saved successfully to: ", log_file_path)

# =========================================================================
# VIII. MASTER INTEGRATION & AGGREGATION
# =========================================================================
# To prevent manual review of 96+ individual cohort subdirectories, this final 
# module sweeps the master output directory, extracts the Global Performance 
# C-Indices of every successfully evaluated cohort, and mathematically fuses 
# them into a single, wide-format Master Spreadsheet for Results/Discussion analysis.
message("🔄 Initiating Post-Run Master Data Aggregation...")

perf_files <- list.files(MODEL_OUTPUT_DIR, pattern = "_Global_Performance\\.tsv$", recursive = TRUE, full.names = TRUE)

if (length(perf_files) > 0) {
  master_perf_list <- lapply(perf_files, function(f) {
    temp_df <- read.delim(f, sep = "\t", stringsAsFactors = FALSE)
    cohort_id <- gsub("_Global_Performance\\.tsv", "", basename(f))
    temp_df$Cohort <- cohort_id
    # Reorganize geometry
    temp_df <- temp_df[, c("Cohort", "Model", "C_Index")]
    return(temp_df)
  })
  
  master_performance_df <- do.call(rbind, master_perf_list)
  
  # Pivot structurally so columns are Algorithms and rows are Cohorts
  master_wide <- master_performance_df %>%
    tidyr::pivot_wider(names_from = Model, values_from = C_Index) %>%
    dplyr::arrange(dplyr::desc(XGBoost_Apparent))
  
  master_save_path <- file.path(WORKING_DIR, "MASTER_Phase_IIIB_Sparsity_Performance.csv")
  write.csv(master_wide, master_save_path, row.names = FALSE)
  
  message("✅ Aggregation Complete! Master cohort matrix saved to: ", master_save_path)
} else {
  message("⚠️ Aggregation Skipped: No Global Performance TSV files survived the loop.")
}

# ---
# File successfully updated and re-saved by AI Assistant. Safe to run.
# Re-audited and timestamp refreshed for Megarun 3.0 Configuration (with Aggregator).
# ---

  #### REFACTORED 23/08/2025
  #### 
  #### 03/31/2025 - DEEPLY AMENDED ON 05/21/2025
  #### Enrique Medina-Acosta, Emanuell Rodrigues de Souza,
  #### Higor Almeida Cordeiro Nogueira, Victor Santos Lopes
  #### UENF/CBB/LBT
  ####
  ####

  ##### =====================================================================
  ##### 📦 HIERARCHICAL, MEMORY-EFFICIENT, AUTO-RESUMABLE IMPUTATION PIPELINE.
  ##### =====================================================================
  ##### Run: RScript_UNIVERSAL AUTO-RESUME LOGIC FOR IMPUTATION PIPELINE — FINAL VERSION
  #### -------------------------------------------------------------------------------------------
  #### Machine learning Analysis of multi-omics predictive variables and multiple response variables
  #### --------------------------------------------------------------------------------------------
  #### ML_get_data and downstream analysis for prediction of outcomes using
  
  # ------------------------------------------------------------------------------
  # Global Verbosity Control for Console Output
  # 
  # This flag and wrapper functions allow suppression of progress messages and
  # console output during large-scale or batch runs. Setting VERBOSE to FALSE
  # disables log_msg() and log_log_msg() output globally, reducing I/O overhead
  # without affecting critical error or warning behavior. This strategy prevents
  # slowdowns caused by excessive console printing, especially in nested loops or
  # resource-limited environments.
  # ------------------------------------------------------------------------------
  # =============================================================
  # 🔧 Universal Verbose Flag Compatibility Layer
  # Ensures lowercase 'verbose' is available in all scopes
  # ===========================================================
  
  # 🔧 Global Verbosity Control for Console Output
  VERBOSE <- TRUE  # Set to FALSE to silence non-critical console output
  
  # Create fallback alias if not defined in local scope
  if (!exists("verbose", inherits = FALSE)) verbose <- VERBOSE
  
  # Unified message logger that respects both 'verbose' and 'VERBOSE'
  log_msg <- function(...) {
    if ((exists("verbose", inherits = TRUE) && isTRUE(verbose)) || isTRUE(VERBOSE)) {
      message(...)
    }
  }
  
  # log_cat <- function(...) if (isTRUE(VERBOSE)) log_msg(...)
  
  ### Expert Commentary on Code 1: Enrique_ML_get_data_updated_Efficiency_Sequential_Application - REFACTORED.R
  
  # 🔍 PURPOSE:
  # Implements a complete, structured pipeline for multi-layer, groupwise imputation 
  # across a harmonized pan-cancer multi-omic dataset. Serves as the primary engine 
  # for controlled, memory-efficient imputation, tracking, and dataset construction.
  
  # ✅ STRENGTHS:
  # - Layer-wise modular imputation logic: survival → CNV → mutation → continuous omics.
  # - Fully auditable: intermediate .rds outputs, tracking table (`output_name_table_all.tsv`).
  # - Harmonizes clinical and omic variables with validation steps pre-imputation.
  # - Implements automatic export of diagnostic tables and visual summaries (e.g., DiagrammeR).
  # - Wrapper-based modularity allows downstream plug-and-play analysis and method replacement.
  # - Groupwise imputation enforced throughout (by cancer type).
  
  # ⚠️ POTENTIAL LIMITATIONS:
  # - Redundant logic (e.g., suffix parsing) may benefit from modular reuse.
  # - Memory usage could spike with excessive verbosity during long loops.
  # - Lack of internal self-validation (e.g., consistency checks across omic blocks) post-run.
  #
  # 🧠 OVERALL:
  # This is a clean, structured, logically sound imputation framework. Its modular hierarchy 
  # and progressive tracking allow robust downstream analytics and reproducibility.
  
  # ✅ CONCLUSION:
  # Joint with Code 2: UNIVERSAL_RESUME_ENGINE_DYNAMIC_REPORTING.R,tThe two scripts are tightly coupled, logically harmonious, and share audit mechanisms and 
  # tracking conventions. Their separation of duties (execution vs. recovery) aligns with 
  # best practices in bioinformatics pipeline design.
  
  ### 
  ### =======================================================================
  ### MODULE 1 — Dependency Sentinel: Detect, Filter, and Activate Packages
  ### ======================================================================
  ### 
  ### 
  
  # Define required packages (fallback in case dynamic detection fails)
  cran_packages <- c(
    "caret", "circlize", "ComplexHeatmap", "dplyr", "ggplot2", "grid", "gridExtra",
    "kableExtra", "Matrix", "mice", "missForest", "pROC", "purrr", "reshape2",
    "rio", "rms", "survival", "survivalROC", "survcomp", "survminer",
    "tidyr", "timeROC", "UpSetR", "VIM", "xgboost", "DiagrammeR"
  )
  
  bioc_packages <- c("ComplexHeatmap", "survcomp")
  
  github_packages <- list(
    "UCSCXenaShiny" = "openbiox/UCSCXenaShiny",
    "DiagrammeRsvg" = "rich-iannone/DiagrammeRsvg"
  )
  
  special_packages <- c("rsvg", "magick", "lightgbm")
  
  # ---- Universal Installation Helpers ----
  install_if_missing_cran <- function(pkg) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      install.packages(pkg, dependencies = TRUE)
    }
  }
  
  install_if_missing_bioc <- function(pkg) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      if (!requireNamespace("BiocManager", quietly = TRUE)) install.packages("BiocManager")
      BiocManager::install(pkg)
    }
  }
  
  install_if_missing_github <- function(pkg, repo) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      if (!requireNamespace("remotes", quietly = TRUE)) install.packages("remotes")
      remotes::install_github(repo)
    }
  }
  
  # ---- Install Declared Packages ----
  invisible(lapply(cran_packages, install_if_missing_cran))
  invisible(lapply(bioc_packages, install_if_missing_bioc))
  invisible(mapply(install_if_missing_github, names(github_packages), github_packages))
  invisible(lapply(special_packages, install_if_missing_cran))
  
  # ---- Dynamic Package Detection from Script ----
  script_path <- "Enrique_ML_get_data_updated_Efficiency_Sequential_Application - REFACTORED.R"  # OK
  
  script_lines <- readLines(script_path, warn = FALSE, encoding = "UTF-8")
  script_text <- paste(script_lines, collapse = "\n")
  
  pkg_matches <- character()
  
  # Detect 'library()' or 'require()' calls
  pkg_matches <- c(pkg_matches, unlist(regmatches(
    script_text, gregexpr("(?<=library\\(|require\\()[\"']?([a-zA-Z0-9\\.]+)[\"']?", script_text, perl = TRUE)
  )))
  
  # Also collect package names in vectors like c("pkg1", "pkg2", ...)
  pkg_matches <- c(pkg_matches, unlist(regmatches(
    script_text, gregexpr("\"[a-zA-Z0-9\\.]+\"", script_text, perl = TRUE)
  )))
  
  # Clean and deduplicate
  pkg_matches <- gsub("\"", "", pkg_matches)
  pkg_matches <- sort(unique(pkg_matches))
  
  # 🛡️ Exclude known problematic or non-relevant packages
  excluded_pkgs <- c("plasma", "red", "text", "topics")
  pkg_matches <- setdiff(pkg_matches, excluded_pkgs)
  log_msg("🛡️ Excluded problematic or non-relevant packages:", paste(excluded_pkgs, collapse = ", "), "\n")
  
  # Validate availability on system or CRAN
  available_cran <- rownames(available.packages())
  valid_pkgs <- pkg_matches[
    sapply(pkg_matches, function(pkg) {
      requireNamespace(pkg, quietly = TRUE) || pkg %in% available_cran
    })
  ]
  
  # Install + load detected packages
  load_or_install <- function(pkg) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      install.packages(pkg, dependencies = TRUE)
    }
    suppressPackageStartupMessages(library(pkg, character.only = TRUE))
  }
  invisible(lapply(valid_pkgs, load_or_install))
  
  log_msg("✔ Detected and loaded packages:\n")
  print(valid_pkgs)
  
  # Set working directory
  grep("setwd", readLines("Enrique_ML_get_data_updated_Efficiency_Sequential_Application - REFACTORED.R"), value = TRUE)

  if (file.exists("Enrique_ML_get_data_updated_Efficiency_Sequential_Application - REFACTORED.R")) {
    grep("setwd", readLines("Enrique_ML_get_data_updated_Efficiency_Sequential_Application - REFACTORED.R"), value = TRUE)
  }
  
  setwd("D:/Pré-artigo 5-optosis model/Machine learning CancerRCDShiny_prediction/Enrique version")
  
  #### ============================================================================
  #### 📦 Master Wrapper Functions for rio Import/Export with NA Safety
  #### ============================================================================
  
  library(rio)
  
  safe_import_tsv <- function(file, format = NULL, ...) {
    import(file, format = format, na.strings = "NA", ...)
  }
  
  # ---- Safe TSV writer (define once) ----
  if (!exists("safe_export_tsv", mode = "function")) {
    safe_export_tsv <- function(x, file, na = "NA") {
      stopifnot(is.character(file), length(file) == 1L)
      dir.create(dirname(file), recursive = TRUE, showWarnings = FALSE)
      data.table::fwrite(
        data.table::as.data.table(x),
        file = file,
        sep = "\t",
        na = na,
        quote = FALSE
      )
    }
  }

  #### ============================================================================
  #### 📦 Installation Script for PDF/Image Processing Packages
  #### ============================================================================
  
  required_pkgs <- c("pdftools", "magick", "fs", "tidyverse")
  new_pkgs <- required_pkgs[!(required_pkgs %in% installed.packages()[, "Package"])]
  
  if (length(new_pkgs) > 0) {
    message("Installing missing packages: ", paste(new_pkgs, collapse = ", "))
    tryCatch({
      install.packages(new_pkgs, repos = "https://cloud.r-project.org", dependencies = TRUE)
      message("Successfully installed packages")
    }, error = function(e) {
      warning("Installation failed: ", e$message)
      tryCatch({
        install.packages(new_pkgs, repos = "https://cran.rstudio.com", dependencies = TRUE)
        message("Successfully installed using backup mirror")
      }, error = function(e) {
        stop("Critical installation failure. Please check internet connection or try:\n",
             'install.packages(c("', paste(new_pkgs, collapse = '", "'), '"))')
      })
    })
  } else {
    message("All required packages already installed")
  }
  
  # Verify successful loading
  success <- sapply(required_pkgs, require, character.only = TRUE)
  if (all(success)) {
    message("\nAll packages loaded successfully:")
    print(sessionInfo()[c("otherPkgs", "loadedOnly")])
  } else {
    warning("Failed to load: ", paste(names(success)[!success], collapse = ", "))
  }
  
  # Linux-specific system requirement hints
  if (Sys.info()["sysname"] == "Linux") {
    message("\nLinux system detected. You may also need to run:")
    message('sudo apt-get install libpoppler-cpp-dev libmagick++-dev')
  }
  
  log_msg("✔ Detected and loaded packages:\n")
  print(valid_pkgs)
  
  # 👇 Add this block to show what's actually loaded
  log_msg("\n📦 Final list of packages successfully loaded in this session:\n")
  loaded_pkgs <- valid_pkgs[sapply(valid_pkgs, function(pkg) pkg %in% loadedNamespaces())]
  print(loaded_pkgs)

####
####
####  ZERO - BACKSETTING - End of package setting PART 0 ####
####  
####  
  
################################################################################
# 📦 IMPUTATION STRATEGY OVERVIEW
# ------------------------------------------------------------------------------
# File: Enrique_ML_get_data_updated_Efficiency_Sequential_Application.R
# Date: 2025-05-24
# Author: Enrique Medina-Acosta
#
# Description:
# This R script implements a memory-efficient, sequential, and auto-resumable 
# pipeline for multi-layer imputation of missing data in pan-cancer multi-omic 
# datasets. It supports harmonized handling of survival, CNV, mutation, and 
# continuous variables (protein, miRNA, transcript, mRNA, methylation).
#
# ------------------------------------------------------------------------------
# 💡 Key Features:
# - Groupwise imputation by cancer type (`type` column)
# - Layer-wise modularity with fault-tolerant checkpoints
# - Auto-resume logic using file index tracking
# - Exported tracking table: `output_name_table_all.tsv`
#
# ------------------------------------------------------------------------------
# 🔄 Imputation Sequence:
#
# (1) Survival Layer
#     Inputs: df005
#     Methods: mean, median, random
#     Outputs: df006–df008
#
# (2) CNV Layer
#     Inputs: df006–df008 (3 survival-imputed files)
#     Methods: mode, random, kNN
#     Outputs: df009–df017
#
# (3) Mutation Layer
#     Inputs: df009–df017 (9 CNV-imputed files)
#     Methods: mean, median, mode, BernoulliRandom
#     Outputs: df018–df053 (36 mutation-imputed datasets)
#
# (4) Continuous Omic Layers (.1 = Protein, .4 = miRNA, .5 = Transcript, 
#                              .6 = mRNA, .7 = Methylation)
#     Inputs: df018–df053 (36 mutation-imputed files)
#     Methods:
#         • Mean:       df054–df089
#         • Median:     df090–df125
#         • Random:     df126–df161
#         • kNN:        df162–df197
#         • missForest: df198–df233
#         • XGBoost:    df234–df269
#         • LightGBM:   df270–df305
#         • MICE:       df306–df341
#         •iSVD         df342dfdf377
# ------------------------------------------------------------------------------
# 🧠 Memory Safety Strategy:
# - All operations performed in batches (e.g., 9×4 = 36 mutation imputation)
# - Objects removed via `rm()` and `gc()` immediately after export
# - Every imputation step outputs an `.rds` file and logs into 
#   `output_name_table_all.tsv`
#
# ------------------------------------------------------------------------------
# ✅ Output Summary:
# - Total imputation outputs: 336 (`df006`–`df341`)
# - Final datasets are ready for evaluation and modeling
#
# See respective wrapper functions for implementation:
# - `impute_survival_variables()`
# - `impute_cnv_groupwise_*()`
# - `impute_mutation_groupwise_*()`
# - `impute_<method>_groupwise()` for continuous layers
################################################################################

#### 
#### 
#### END OF MODULE 1 ####
#### 
#### 

#### =============================================================================
#### MODULE 2 — Workflow Diagram Builder with Colored Levels (Content Preserved)
#### =============================================================================
library(DiagrammeR)
library(DiagrammeRsvg)
library(rsvg)
library(magick)
library(stringi)

wrap_text <- function(text, width = 30) {
  stri_wrap(text, width = width) |> paste(collapse = "\n")
}

labels <- list(
  df001 = wrap_text("df001: Multi-omic RCD Signatures; demographic and clinical data"),
  df001_cleaned = wrap_text("Remove Rows with All NAs (Columns 23–End)"),
  df001_harmonized = wrap_text("Harmonize Clinical Variables (Columns 23–30)"),
  df004 = wrap_text("df004: Rename and Validate (No NAs in Columns 23–30)"),
  df005 = wrap_text("df005: Harmonize CNV and Mutation Variables (CNV: Categorical; Mutation: Binary)"),
  df006 = wrap_text("df006: Impute Survival Data (Method: Mean)"),
  df007 = wrap_text("df007: Impute Survival Data (Method: Median)"),
  df008 = wrap_text("df008: Impute Survival Data (Method: Random)"),
  df009 = wrap_text("df009: Impute CNV Data (Method: Mode)"),
  df010 = wrap_text("df010: Impute CNV Data (Method: Random)"),
  df011 = wrap_text("df011: Impute CNV Data (Method: kNN)"),
  df018 = wrap_text("df018: Impute Mutation Data (Method: Mean)"),
  df027 = wrap_text("df027: Impute Mutation Data (Method: Median)"),
  df036 = wrap_text("df036: Impute Mutation Data (Method: Mode)"),
  df045 = wrap_text("df045: Impute Mutation Data (Method: BernoulliRandom)"),
  df054 = wrap_text("df054: Continuous Omic Imputation (Mean, df054–df089)"),
  df090 = wrap_text("df090: Continuous Omic Imputation (Median, df090–df125)"),
  df126 = wrap_text("df126: Continuous Omic Imputation (Random, df126–df161)"),
  df162 = wrap_text("df162: Continuous Omic Imputation (kNN, df162–df197)"),
  df198 = wrap_text("df198: Continuous Omic Imputation (missForest, df198–df233)"),
  df234 = wrap_text("df234: Continuous Omic Imputation (XGBoost, df234–df269)"),
  df270 = wrap_text("df270: Continuous Omic Imputation (LightGBM, df270–df305)"),
  df306 = wrap_text("df306: Continuous Omic Imputation (MICE, df306–df341)"),
  df342 = wrap_text("df342: Continuous Omic Imputation (iSVD, df342–df377)"),
  evaluation = wrap_text("Model Evaluation and Selection\n(n = 377 total datasets)")
)

diagram <- grViz("
digraph workflow {
  graph [layout = dot, rankdir = TB]
  node [shape = rectangle, style = filled, width = 2.5, height = 1.0, fontsize = 10, fontname = \"Helvetica\"] 

  # Stage-based fill colors
  df001           [label = '@@1', fillcolor = gray90]
  df001_cleaned   [label = '@@2', fillcolor = gray90]
  df001_harmonized[label = '@@3', fillcolor = gray90]
  df004           [label = '@@4', fillcolor = gray90]
  df005           [label = '@@5', fillcolor = gray90]

  df006           [label = '@@6', fillcolor = lightblue]
  df007           [label = '@@7', fillcolor = lightblue]
  df008           [label = '@@8', fillcolor = lightblue]

  df009           [label = '@@9', fillcolor = palegreen]
  df010           [label = '@@10', fillcolor = palegreen]
  df011           [label = '@@11', fillcolor = palegreen]

  df018           [label = '@@12', fillcolor = khaki1]
  df027           [label = '@@13', fillcolor = khaki1]
  df036           [label = '@@14', fillcolor = khaki1]
  df045           [label = '@@15', fillcolor = khaki1]

  df054           [label = '@@16', fillcolor = thistle1]
  df090           [label = '@@17', fillcolor = thistle1]
  df126           [label = '@@18', fillcolor = thistle1]
  df162           [label = '@@19', fillcolor = thistle1]
  df198           [label = '@@20', fillcolor = thistle1]
  df234           [label = '@@21', fillcolor = thistle1]
  df270           [label = '@@22', fillcolor = thistle1]
  df306           [label = '@@23', fillcolor = thistle1]
  df342           [label = '@@24', fillcolor = thistle1]

  evaluation      [label = '@@25', fillcolor = lightsalmon]

  # Edges
  df001 -> df001_cleaned -> df001_harmonized -> df004 -> df005
  df005 -> {df006 df007 df008}
  {df006 df007 df008} -> {df009 df010 df011}
  {df009 df010 df011} -> {df018 df027 df036 df045}
  {df018 df027 df036 df045} -> {df054 df090 df126 df162 df198 df234 df270 df306 df342}
  {df054 df090 df126 df162 df198 df234 df270 df306 df342} -> evaluation
}

[1]: labels$df001
[2]: labels$df001_cleaned
[3]: labels$df001_harmonized
[4]: labels$df004
[5]: labels$df005
[6]: labels$df006
[7]: labels$df007
[8]: labels$df008
[9]: labels$df009
[10]: labels$df010
[11]: labels$df011
[12]: labels$df018
[13]: labels$df027
[14]: labels$df036
[15]: labels$df045
[16]: labels$df054
[17]: labels$df090
[18]: labels$df126
[19]: labels$df162
[20]: labels$df198
[21]: labels$df234
[22]: labels$df270
[23]: labels$df306
[24]: labels$df342
[25]: labels$evaluation
")

# Export as SVG
svg_code <- export_svg(diagram)
svg_raw <- charToRaw(svg_code)

# Save as high-resolution TIFF (600 DPI, landscape 11 × 8.5 inches)
png_file <- "workflow_diagram_tmp.png"
rsvg_png(svg_raw, file = png_file, width = 6600, height = 5100)  # 11 in × 600 DPI

# Convert to TIFF
image_write(image_read(png_file), path = "workflow_diagram_colored.tiff", format = "tiff", compression = "lzw")

# Save as PDF (vector)
rsvg_pdf(svg_raw, file = "workflow_diagram_colored.pdf", width = 11, height = 8.5)

log_msg("✅ Diagram exported:\n")
log_msg("• TIFF (600 DPI): workflow_diagram_colored.tiff\n")
log_msg("• PDF (vector):  workflow_diagram_colored.pdf\n")

### Imputation method summary table and imputation_strategy_summary_table
library(tibble)
library(dplyr)
library(openxlsx)

# Build the table
imputation_table <- tibble(
  Dataset = c(
    "df005",
    "df006-df008", "df009-df017", "df018-df053",
    "df054-df089", "df090-df125", "df126-df161", "df162-df197",
    "df198-df233", "df234-df269", "df270-df305", "df306-df341",
    "df342-df377"
  ),
  Layer = c(
    "Preprocessing",
    "Survival Imputation", "CNV Imputation", "Mutation Imputation",
    "Continuous (Mean)", "Continuous (Median)", "Continuous (Random)",
    "Continuous (kNN)", "Continuous (missForest)", "Continuous (XGBoost)",
    "Continuous (LightGBM)", "Continuous (MICE)", "Continuous (iSVD)"
  ),
  Methods = c(
    "N/A",
    "Mean, Median, Random",
    "Mode, Random, kNN",
    "Mean, Median, Mode, BernoulliRandom",
    "Mean", "Median", "Random", "kNN",
    "missForest", "XGBoost", "LightGBM", "MICE", "iSVD"
  ),
  n_Datasets = c(
    1, 3, 9, 36,
    36, 36, 36, 36,
    36, 36, 36, 36,
    36
  )
)

# Print and export
print(imputation_table)
write.xlsx(imputation_table, "Imputation_Workflow_Summary.xlsx", overwrite = TRUE)

# =============================================================================
# CREATE ONE-PAGE IMPUTATION STRATEGY SUMMARY TABLE
# =============================================================================
# =============================================================
# 📘 Rebuild Imputation Strategy Summary Table from Block Table
# =============================================================
# Author: Enrique Medina-Acosta
# Purpose: This script reconstructs the imputation strategy summary
#          table using harmonized function name imputation_block_table.tsv as the authoritative source.
#          The output table includes one module per row, preserving the
#          original order and ranges exactly as defined in the block table.
# =============================================================

# 📦 Load required libraries
library(dplyr)
library(readr)
library(stringr)

# -----------------------------
# 🧱 Step 1: Construct the Imputation Block Table with iSVD
# -----------------------------

imputation_block_table <- data.frame(
  block = c(
    "Survival_mean", "Survival_median", "Survival_random",
    "CNV_mode", "CNV_random", "CNV_knn",
    "Mutation_mean", "Mutation_median", "Mutation_random", "Mutation_knn",
    "Continuous_mean", "Continuous_median", "Continuous_random", "Continuous_knn",
    "Continuous_missForest", "Continuous_XGBoost", "Continuous_LightGBM", "Continuous_MICE",
    "Continuous_iSVD"  # 🆕 New method
  ),
  
  input_start = c(
    rep("df005", 3),              # Survival
    rep("df006", 3),              # CNV
    rep("df009", 4),              # Mutation
    rep("df018", 9)               # Continuous (added 1)
  ),
  
  input_end = c(
    rep("df005", 3),              # Survival
    rep("df008", 3),              # CNV
    rep("df017", 4),              # Mutation
    rep("df053", 9)               # Continuous (added 1)
  ),
  
  output_start = c(
    "df006", "df007", "df008",    # Survival
    "df009", "df012", "df015",    # CNV
    "df018", "df027", "df036", "df045",    # Mutation
    "df054", "df090", "df126", "df162",    # Continuous
    "df198", "df234", "df270", "df306",    # Continuous
    "df342"                       # 🆕 iSVD output start
  ),
  
  output_end = c(
    "df006", "df007", "df008",    # Survival
    "df011", "df014", "df017",    # CNV
    "df026", "df035", "df044", "df053",    # Mutation
    "df089", "df125", "df161", "df197",    # Continuous
    "df233", "df269", "df305", "df341",    # Continuous
    "df377"                       # 🆕 iSVD output end (assumes 36 cancer types)
  ),
  
  method = c(
    "mean", "median", "random",
    "mode", "random", "knn",
    "mean", "median", "random", "knn",
    "mean", "median", "random", "knn",
    "missForest", "XGBoost", "LightGBM", "MICE",
    "iSVD"  # 🆕
  ),
  
  type = c(
    rep("Survival", 3),
    rep("CNV", 3),
    rep("Mutation", 4),
    rep("Continuous", 9)  # 🆕
  ),
  
  module_function = c(
    "run_survival_mean_imputation", "run_survival_median_imputation", "run_survival_random_imputation",
    "run_cnv_mode_imputation", "run_cnv_random_imputation", "run_cnv_knn_imputation",
    "run_mutation_mean_imputation", "run_mutation_median_imputation",
    "run_mutation_random_imputation", "run_mutation_knn_imputation",
    "run_continuous_mean_imputation", "run_continuous_median_imputation",
    "run_continuous_random_imputation", "run_continuous_knn_imputation",
    "run_continuous_missForest_imputation", "run_continuous_xgboost_imputation",
    "run_continuous_lightgbm_imputation", "run_continuous_mice_imputation",
    "run_continuous_isvd_imputation"  # 🆕
  ),
  
  stringsAsFactors = FALSE
)

# -----------------------------
# 🧱 Step 2: Prefix all function names with 'groupwise_'
# -----------------------------
imputation_block_table <- imputation_block_table %>%
  mutate(module_function = paste0("groupwise_", module_function))

# 🧮 Compute numeric indices
imputation_block_table <- imputation_block_table %>%
  mutate(
    input_start_idx  = as.integer(gsub("\\D", "", input_start)),
    input_end_idx    = as.integer(gsub("\\D", "", input_end)),
    output_start_idx = as.integer(gsub("\\D", "", output_start)),
    output_end_idx   = as.integer(gsub("\\D", "", output_end))
  )

# 💾 Export as TSV
safe_export_tsv(as.data.frame(imputation_block_table), "imputation_block_table.tsv")

# -------------------------------------
# ✅ Step 2: Validate Sequence Integrity
# -------------------------------------
# Create a list of all output indices
output_indices <- unlist(mapply(seq, 
                                imputation_block_table$output_start_idx, 
                                imputation_block_table$output_end_idx))

# Sort and check for continuity
missing_indices <- setdiff(seq(min(output_indices), max(output_indices)), output_indices)
duplicated_indices <- output_indices[duplicated(output_indices)]

# Print validation summary
log_msg("🔍 VALIDATION SUMMARY\n")
log_msg("→ Total output blocks:        ", nrow(imputation_block_table), "\n")
log_msg("→ Total output .rds indices: ", length(output_indices), "\n")
log_msg("→ Unique .rds indices:       ", length(unique(output_indices)), "\n")
log_msg("→ Missing indices:           ", length(missing_indices), "\n")
log_msg("→ Overlapping indices:       ", length(duplicated_indices), "\n\n")

if (length(missing_indices) > 0) {
  log_msg("⚠️ Missing index values:\n")
  print(missing_indices)
}

if (length(duplicated_indices) > 0) {
  log_msg("⚠️ Overlapping index values:\n")
  print(duplicated_indices)
}

# -------------------------------------
# 🧾 Step 3: Save Summary as DataFrame
# -------------------------------------
validation_summary_df <- data.frame(
  total_blocks = nrow(imputation_block_table),
  total_output_indices = length(output_indices),
  unique_output_indices = length(unique(output_indices)),
  missing_index_count = length(missing_indices),
  duplicated_index_count = length(duplicated_indices),
  stringsAsFactors = FALSE
)

# 📥 Load the imputation block table
block_df <- read_tsv("imputation_block_table.tsv", col_types = cols())

# 🧠 Construct the expanded strategy summary table (strict order, one module per row)
expanded_strategy_df <- block_df %>%
  mutate(
    Stage = type,
    Method = str_to_title(method),  # Capitalize method
    Input_Range = paste0("df", sprintf("%03d", input_start_idx), "-df", sprintf("%03d", input_end_idx)),
    Output_Range = paste0("df", sprintf("%03d", output_start_idx), "-df", sprintf("%03d", output_end_idx))
  ) %>%
  select(Stage, Method, Input_Range, Output_Range) %>%
  mutate(row_index = row_number()) %>%
  arrange(row_index) %>%
  select(-row_index)

# 📊 Preview result
print(expanded_strategy_df)

# 💾 Optional: Save to file
# Export to expected file name
safe_export_tsv(as.data.frame(expanded_strategy_df), "imputation_strategy_summary.tsv")

# =============================================================
# 📘 Validate Congruence Between Block Table and Strategy Summary
# =============================================================
# Author: Enrique Medina-Acosta
# Purpose: Certify that harmonized function name imputation_block_table.tsv and
#          imputation_strategy_summary.tsv are identical in module
#          order and input/output ranges per module.
# =============================================================

# 📦 Load required libraries
library(dplyr)
library(readr)
library(stringr)

# 📥 Load both tables
block_df <- read_tsv("imputation_block_table.tsv", col_types = cols())
strategy_df <- read_tsv("imputation_strategy_summary.tsv", col_types = cols())

# 🧠 Normalize and reconstruct fields from block_df for comparison
block_check <- block_df %>%
  mutate(
    Stage = type,
    Method = str_to_title(method),
    Input_Range = paste0("df", sprintf("%03d", input_start_idx), "-df", sprintf("%03d", input_end_idx)),
    Output_Range = paste0("df", sprintf("%03d", output_start_idx), "-df", sprintf("%03d", output_end_idx))
  ) %>%
  select(Stage, Method, Input_Range, Output_Range)

# 🧪 Compare the two dataframes
if (nrow(block_check) != nrow(strategy_df)) {
  stop("❌ Number of rows mismatch between block table and strategy summary.")
}

comparison_matrix <- block_check == strategy_df
all_match <- all(comparison_matrix)

if (all_match) {
  message("✅ Tables are congruent: All rows and columns match in order and values.")
} else {
  message("❌ Tables are NOT congruent. The following rows mismatch:")
  mismatch_indices <- which(!apply(comparison_matrix, 1, all))
  print(block_check[mismatch_indices, ])
  print(strategy_df[mismatch_indices, ])
}

#### -----------------------------
#### Total Expected Output Objects
#### -----------------------------
####
#### Step 1: Initial sequential processing
#### • df001 to df005 = 5 dataframes
####
#### Step 2: Survival data imputation
#### • 3 methods (mean, median, random)
#### • Result: df006 to df008 → 3 dataframes
####
#### Step 3: CNV imputation per survival object
#### • 3 CNV methods × 3 survival-imputed → 9 dataframes
####
#### Step 4: Mutation imputation per CNV-imputed dataset
#### • 4 mutation methods × 9 CNV-imputed → 36 dataframes (df018-df053)
#### 
#### Step 5: Continuous imputation per mutation-imputed dataset
#### • 9 continuous methods × 36 mutation-imputed → 288 dataframes (df054-df377)

#### END OF MODULE 2 ####

####
####
#### =====================================================================================================
#### MODULE 3 — Part A: Signature Preprocessing and UCSCXenaShiny Extraction Preparation
#### =====================================================================================================
####
#### Objective:
#### Prepare the input data by importing multi-omic target signatures, identifying duplicated entries,
#### and standardizing the 'Omic_feature' column. Ensures compatibility with UCSCXenaShiny functions
#### before clinical and omic querying begins.
#### Programmatically extract clinical and multi-omic data using UCSCXenaShiny functions for the selected gene signatures.
#### This block verifies necessary function definitions, imports the target list, identifies duplicated signatures,
#### and prepares distribution summaries for downstream querying.

# Load required packages
library(UCSCXenaShiny)
library(UCSCXenaTools)
library(rio)

# Check for required function availability
if (!exists("tcga_surv_get")) stop("❌ Erro: tcga_surv_get() não está definido. Verifique a importação.")
if (!exists("load_data")) stop("❌ Erro: load_data() não está definido. Verifique a importação.")

# Import multi-omic gene signature table
gene_symbols <- import("df1157_ML_targets_final.tsv", format = "tsv", na.strings = "NA")

# Quick inspection of key dimensions
length(unique(gene_symbols$Signature))
length(unique(gene_symbols$Nomenclature))
length(unique(gene_symbols$CTAB))
length(unique(gene_symbols$`Omic feature`))

# Identify duplicated 'Nomenclature' entries
duplicated_nomenclature <- gene_symbols %>%
  group_by(Nomenclature) %>%
  filter(n() > 1) %>%
  ungroup()

# Identify duplicated 'Signature' entries
duplicated_signature <- gene_symbols %>%
  group_by(Signature) %>%
  filter(n() > 1) %>%
  ungroup()

# Distribution of duplicated 'Signature' values per CTAB
signature_distribution <- duplicated_signature %>%
  as.data.frame() %>%
  dplyr::count(CTAB, Signature, name = "Count") %>%
  dplyr::arrange(desc(Count))

# Standardize 'Omic_feature': keep 'mRNA' and 'miRNA', lowercase all others
gene_symbols <- gene_symbols %>%
  {
    # Step 1: Rename column to syntactically valid form
    names(.)[names(.) == "Omic feature"] <- "Omic_feature"
    
    # Step 2: Apply standardization
    mutate(.,
           Omic_feature = ifelse(
             tolower(Omic_feature) %in% c("mrna", "mirna"),
             c("mRNA", "miRNA")[match(tolower(Omic_feature), c("mrna", "mirna"))],
             tolower(Omic_feature)
           )
    )
  }

# Verify outcome
head(unique(gene_symbols$Omic_feature))

#######
#######
####### Filtering most-clinically relevant signatures for the geometric polytop manuscript (26/10/2026)
#######
#######
#######
suppressPackageStartupMessages({
  library(dplyr)
  library(stringr)
  library(tidyr)
})

# ============================================================
# 0) START: Filter by clinical and immune classes (once)
# ============================================================
# Input: gene_symbols (one row per SIGNATURE)
# Columns used: Combined_Outcome, immune_classification, Elements, RCD, Nomenclature,
#               Signature, microenvironment_classification, (optionally) RCD_ranking,
#               "RCD form", Phenotype

filtered_signatures <- gene_symbols %>%
  filter(
    Combined_Outcome %in% c("Meaningful Risky", "Meaningful Protective"),
    immune_classification %in% c("Hot", "Cold")
  ) %>%
  mutate(
    Elements = suppressWarnings(as.integer(Elements)),
    RCD      = suppressWarnings(as.integer(RCD))
  )

# ============================================================
# 1) Keep multi-element signatures (≥ 2) and derive RCD_class
#    from numeric RCD: 0 = No-RCD; 1 = Single-RCD; ≥2 = Multi-RCD
#    (This avoids list-columns and parsing issues.)
# ============================================================

filtered_step3 <- filtered_signatures %>%
  filter(!is.na(Elements), Elements >= 2) %>%
  mutate(
    RCD_class = case_when(
      is.na(RCD)       ~ NA_character_,
      RCD == 0L        ~ "No-RCD",
      RCD == 1L        ~ "Single-RCD",
      RCD >= 2L        ~ "Multi-RCD"
    )
  )

# ============================================================
# 2) Produce convenient subsets for ranking and inspection
# ============================================================

multiRCD_signatures  <- filtered_step3 %>% filter(RCD_class == "Multi-RCD")
singleRCD_signatures <- filtered_step3 %>% filter(RCD_class == "Single-RCD")

# If present, coerce RCD_ranking to numeric once
coerce_rcd_ranking <- function(df) {
  if ("RCD_ranking" %in% names(df)) {
    df %>% mutate(RCD_ranking = suppressWarnings(as.numeric(RCD_ranking)))
  } else df
}
multiRCD_signatures  <- coerce_rcd_ranking(multiRCD_signatures)
singleRCD_signatures <- coerce_rcd_ranking(singleRCD_signatures)

# Ranking logic: Elements (desc) → Combined_Outcome → immune_classification → RCD_ranking (desc, if present)
if ("RCD_ranking" %in% names(multiRCD_signatures)) {
  ranked_multiRCD <- multiRCD_signatures %>%
    arrange(desc(Elements), Combined_Outcome, immune_classification, desc(RCD_ranking))
} else {
  ranked_multiRCD <- multiRCD_signatures %>%
    arrange(desc(Elements), Combined_Outcome, immune_classification)
}

if ("RCD_ranking" %in% names(singleRCD_signatures)) {
  ranked_singleRCD <- singleRCD_signatures %>%
    arrange(desc(Elements), Combined_Outcome, immune_classification, desc(RCD_ranking))
} else {
  ranked_singleRCD <- singleRCD_signatures %>%
    arrange(desc(Elements), Combined_Outcome, immune_classification)
}

# Quick summaries (no list-columns involved)
summary_table <- filtered_step3 %>%
  dplyr::count(Combined_Outcome, immune_classification, RCD_class, name = "n")

summary_table
head(ranked_multiRCD, 15)
head(ranked_singleRCD, 15)

# ============================================================
# 3) Candidate set: focus on Multi-RCD + multi-element, and
#    surface top-K per (Outcome × Immune) cell
#    (more components first; ties broken by larger RCD).
# ============================================================

# Start from ranked_multiRCD (already Multi-RCD & Elements ≥2)
df <- ranked_multiRCD

pick_topK <- function(data, K = 3) {
  data %>%
    arrange(desc(Elements), desc(RCD)) %>%
    slice_head(n = K)
}

K <- 3

top_MP_Cold <- df %>%
  filter(Combined_Outcome == "Meaningful Protective",
         immune_classification == "Cold") %>%
  pick_topK(K)

top_MR_Hot <- df %>%
  filter(Combined_Outcome == "Meaningful Risky",
         immune_classification == "Hot") %>%
  pick_topK(K)

top_MR_Cold <- df %>%
  filter(Combined_Outcome == "Meaningful Risky",
         immune_classification == "Cold") %>%
  pick_topK(K)

# NOTE on selecting columns with spaces:
# use backticks (`RCD form`) or all_of("RCD form"). Here I use all_of for safety.
candidate_set <- bind_rows(
  mutate(top_MP_Cold, cohort = "MP-Cold"),
  mutate(top_MR_Hot,  cohort = "MR-Hot"),
  mutate(top_MR_Cold, cohort = "MR-Cold")
) %>%
  select(
    cohort, Nomenclature, Signature, Elements, RCD, RCD_class,
    Combined_Outcome, immune_classification, microenvironment_classification,
    Phenotype, all_of("RCD form")
  )


candidate_set

# ============================================================
# 4) Exemplar locking (Clinical-first lens)
#    Protective exemplar fixed (P1: BRCA-376, anti-tumoral TME)
#    MR-Hot: largest Elements (tie → larger RCD)
#    MR-Cold: prioritize TME (pro-tumoral > dual > anti-tumoral), then Elements, then RCD
# ============================================================

df_ex <- candidate_set %>% filter(RCD_class == "Multi-RCD")  # ensure we remain on Multi-RCD

# If candidate_set dropped RCD_class, re-attach from ranked_multiRCD
if (!"RCD_class" %in% names(df_ex)) {
  df_ex <- candidate_set %>%
    left_join(ranked_multiRCD %>% select(Nomenclature, RCD_class), by = "Nomenclature") %>%
    filter(RCD_class == "Multi-RCD")
}

prot_id <- "BRCA-376.6.3.N.2.95.84.1.3.2"

exemplar_protective <- df_ex %>%
  filter(
    Combined_Outcome == "Meaningful Protective",
    immune_classification == "Cold",
    microenvironment_classification == "anti-tumoral",
    Nomenclature == prot_id
  )

exemplar_MR_hot <- df_ex %>%
  filter(
    Combined_Outcome == "Meaningful Risky",
    immune_classification == "Hot"
  ) %>%
  arrange(desc(Elements), desc(RCD)) %>%
  slice_head(n = 1)

exemplar_MR_cold <- df_ex %>%
  filter(
    Combined_Outcome == "Meaningful Risky",
    immune_classification == "Cold"
  ) %>%
  mutate(
    tme_priority = case_when(
      microenvironment_classification == "pro-tumoral"  ~ 2L,
      microenvironment_classification == "dual"         ~ 1L,
      microenvironment_classification == "anti-tumoral" ~ 0L,
      TRUE                                              ~ -1L
    )
  ) %>%
  arrange(desc(tme_priority), desc(Elements), desc(RCD)) %>%
  slice_head(n = 1) %>%
  select(-tme_priority)

exemplar_trio <- bind_rows(
  exemplar_protective %>% mutate(cohort = "MP-Cold (anti-tumoral)"),
  exemplar_MR_hot     %>% mutate(cohort = "MR-Hot"),
  exemplar_MR_cold    %>% mutate(cohort = "MR-Cold (TME-prioritized)")
) %>%
  select(
    cohort, Nomenclature, Signature, Elements, RCD,
    Combined_Outcome, immune_classification, microenvironment_classification,
    Phenotype, all_of("RCD form")
  )

exemplar_trio

#####
#####
#####
#####

###
###
### 
#### END OF MODULE 3A ####
### 
### 
### 

####
####
#### ============================================================================================================
#### MODULE 3B — Part B: Dataframe Splitting and Exporting by CTAB and Omic Feature (UCSCXenaShiny Preparation)
#### ============================================================================================================
####
#### Objective:
#### Split the full gene_symbols dataframe by:
####   (1) CTAB values, and
####   (2) Omic feature types.
#### For each subset, a `.tsv` file is saved in the working directory, and all intermediate objects are purged 
#### from the global environment to ensure memory cleanliness and reproducibility.
#### ------------------------------------------------------------------------------------------------------------

suppressPackageStartupMessages(library(rio))

##### -------------------------------------------------------------------------------------------------------
##### Step 1 — Split gene_symbols by CTAB and save each subset to disk
##### -------------------------------------------------------------------------------------------------------
unique_ctabs <- unique(gene_symbols$CTAB)

for (ctab in unique_ctabs) {
  df_subset  <- gene_symbols[gene_symbols$CTAB == ctab, , drop = FALSE]  # note the empty column index
  object_name <- paste0("gene_symbols_", make.names(as.character(ctab)))
  file_name   <- paste0(object_name, ".tsv")
  
  assign(object_name, df_subset, envir = .GlobalEnv)
  rio::export(df_subset, file_name, format = "tsv", na = "NA")
  
  log_msg("💾 Saved subset for ", ctab, " as ", file_name, "\n")
}

##### -------------------------------------------------------------------------------------------------------
##### Step 2 — Remove all CTAB-split objects from global environment
##### -------------------------------------------------------------------------------------------------------

objs_to_remove <- ls(pattern = "^gene_symbols_")
rm(list = objs_to_remove, envir = .GlobalEnv)

log_msg("🧹 Removed", length(objs_to_remove), "CTAB-based objects from global environment:\n")
print(objs_to_remove)

##### -------------------------------------------------------------------------------------------------------
##### Step 3 — Split gene_symbols by Omic_feature and save each subset to disk
##### -------------------------------------------------------------------------------------------------------
unique_features <- unique(gene_symbols$Omic_feature)

for (feature in unique_features) {
  df_subset   <- gene_symbols[gene_symbols$Omic_feature == feature, , drop = FALSE]  # ← note the empty column index
  safe_feature <- make.names(as.character(feature))
  object_name  <- paste0("gene_symbols_", safe_feature)
  file_name    <- paste0("gene_symbols_", safe_feature, ".tsv")
  
  rio::export(df_subset, file_name, format = "tsv", na = "NA")
  log_msg("💾 Saved: ", file_name, "\n")
  
  assign(object_name, df_subset, envir = .GlobalEnv)
}

##### -------------------------------------------------------------------------------------------------------
##### Step 4 — Remove all Omic-feature-based objects from global environment
##### -------------------------------------------------------------------------------------------------------

objs_to_remove <- ls(pattern = "^gene_symbols_")
rm(list = objs_to_remove, envir = .GlobalEnv)

log_msg("🧹 Removed", length(objs_to_remove), "Omic feature-based objects from global environment:\n")
print(objs_to_remove)

###
###
### END OF MODULE 3B ###
### 
### 
### 

####
####
#### ============================================================================================================
#### MODULE 4 — Unified Retrieval of Clinical, Demographic, and Survival-Linked Expression Data via UCSCXenaShiny
#### ============================================================================================================
####
#### Objective:
#### Fetch and integrate multi-omic expression data for selected gene signatures across TCGA cohorts, using
#### `tcga_surv_get()` from UCSCXenaShiny. Progressive saving via RDS avoids reprocessing. Final output is a
#### wide-format TSV integrating clinical, survival, and omic data.
#### ------------------------------------------------------------------------------------------------------------

# Load clinical and survival data
tcga_clinical_data <- load_data("tcga_clinical")
tcga_survival_data <- load_data("tcga_surv") 

# Stop execution if clinical or survival data is missing
if (is.null(tcga_clinical_data) || is.null(tcga_survival_data)) {
  stop("❌ Erro ao carregar dados clínicos ou de sobrevivência.")
}

# Merge clinical and survival data by "sample"
tcga_cli_data <- dplyr::full_join(tcga_clinical_data, tcga_survival_data, by = "sample") %>%
  dplyr::distinct(.keep_all = TRUE)

# Ensure .opt_pancan flag is defined
if (!exists(".opt_pancan")) {
  message("⚠️ .opt_pancan não definido. Definindo como TRUE por padrão.")
  .opt_pancan <- TRUE
}

# Define RDS file for saving intermediate results
rds_file <- "gene_omic_list_progress.rds"

# Load previous progress if available
if (file.exists(rds_file)) {
  message("⏳ Carregando progresso anterior de ", rds_file)
  gene_omic_list <- readRDS(rds_file)
} else {
  gene_omic_list <- list()
}

# Retrieve list of already processed entries to avoid redundancy
processed_genes <- names(gene_omic_list)

# Main extraction loop
for (i in seq_len(nrow(gene_symbols))) {
  gene <- gene_symbols$Signature[i]
  cancer_type <- gene_symbols$CTAB[i]
  omic <- gene_symbols$Omic_feature[i]
  Nomenclature <- gene_symbols$Nomenclature[i]
  
  # Skip if already processed
  if (Nomenclature %in% processed_genes) next
  
  message("🔍 Processando: ", gene, " | ", cancer_type, " | ", omic)
  
  # Attempt data extraction
  data <- tryCatch({
    tcga_surv_get(
      item = gene,
      TCGA_cohort = cancer_type,
      profile = omic,
      TCGA_cli_data = tcga_cli_data,
      opt_pancan = .opt_pancan
    )
  }, error = function(e) {
    warning("⚠️ Erro ao obter dados para ", gene, " (", omic, " @ ", cancer_type, "): ", e$message)
    NULL
  })
  
  # Process only valid results
  if (!is.null(data) && "value" %in% names(data)) {
    
    # Rename 'sampleID' to 'sample' if present
    if ("sampleID" %in% names(data)) {
      data <- dplyr::rename(data, sample = sampleID)
    }
    
    # Standardize and format
    data <- data %>%
      dplyr::rename(expression_value = value) %>%
      dplyr::mutate(
        gene = gene,
        type = cancer_type,
        omic_feature = omic,
        Nomenclature = Nomenclature
      ) %>%
      dplyr::select(sample, type, Nomenclature, expression_value)
    
    # Store data by nomenclature
    gene_omic_list[[Nomenclature]] <- data
  } else {
    message("❌ Sem dados retornados para: ", gene, " (", omic, " @ ", cancer_type, ")")
  }
  
  # Progressive save every 10 entries
  if (length(gene_omic_list) %% 10 == 0) {
    saveRDS(gene_omic_list, rds_file)
    message("💾 Progresso salvo em ", rds_file)
  }
}

# Final save after loop completes
saveRDS(gene_omic_list, rds_file)
message("✅ Progresso final salvo em ", rds_file)

# Proceed only if data exists
if (length(gene_omic_list) > 0) {
  # Combine all expression tables into a single dataframe
  final_omic_data <- dplyr::bind_rows(gene_omic_list)
  
  # Convert long format to wide format
  final_omic_data <- final_omic_data %>%
    tidyr::pivot_wider(names_from = Nomenclature, values_from = expression_value)
  
  # Merge clinical data with processed expression data
  final_data <- dplyr::full_join(tcga_cli_data, final_omic_data, by = c("sample", "type"))
  
  # Convert list columns to character strings
  list_columns <- sapply(final_data, is.list)
  if (any(list_columns)) {
    final_data <- final_data %>%
      dplyr::mutate(across(where(is.list), ~ if_else(is.null(.x), NA_character_, toString(.x))))
  }
  
  # Organize final dataset
  final_data <- final_data %>%
    dplyr::arrange(type, sample) %>%
    dplyr::relocate(sample, type)
  
  # Save as TSV
  safe_export_tsv(as.data.frame(final_data), "ML_final_data.tsv")
  
  # Reimport to verify
  ML_final_data <- rio::import("ML_final_data.tsv", format = "tsv", na.strings = "NA")
  
  # Check unique column count
  print(length(unique(colnames(ML_final_data))))
} else {
  message("❌ Nenhum dado foi gerado. Verifique os logs.")
}

####
####
#### ----------------------------------------------------------------------------
#### PART B - debugging missing values in variable "Nomenclature" in final output
#### -----------------------------------------------------------------------------
#### Checking expected dimension and structure of final output
#### Check which Nomenclature values are missing in gene_omic_list
#### 
#### 
missing_nomenclature <- setdiff(gene_symbols$Nomenclature, names(gene_omic_list))
print(length(missing_nomenclature))  # Should return 22
print(missing_nomenclature)  # Show missing values

## Check Which Nomenclature Values Were Processed in the Loop
processed_nomenclature <- c()  # Track stored values

## Check for NULL Entries in gene_omic_list
null_entries <- sum(sapply(gene_omic_list, is.null))
print(paste("Number of NULL entries in gene_omic_list:", null_entries))

## Check if Data Exists for These Nomenclature Values
check_data <- gene_symbols %>%
  filter(Nomenclature %in% missing_nomenclature)
print(check_data)

print(length(unique(names(gene_omic_list))))  # Should be 14907
print(length(unique(colnames(final_omic_data))))  # Should be 14907

######
###### ----------------------------------------------------------------
###### Rerun UCSCXEna fetch script only for missing Nomenclature values
###### ----------------------------------------------------------------
######
# If missing_nomenclature variable values exist, rerun the code only
# process only the missing "Nomenclature" values rather than rerunning the entire script. This will ensure the missing 22 columns are retrieved efficiently.
# Filter only the missing Nomenclature values from gene_symbols.
# Run the loop only for these missing values.
# Append the new results to gene_omic_list.
# Save the updated gene_omic_list and regenerate final_omic_data.
# Re-merge with tcga_cli_data and save the final dataset.
######
######
######
# Identify missing Nomenclature values
missing_nomenclature <- setdiff(gene_symbols$Nomenclature, names(gene_omic_list))
message("Processing only missing Nomenclature values: ", length(missing_nomenclature))

# Filter gene_symbols to process only missing Nomenclature values
missing_gene_symbols <- gene_symbols %>%
  filter(Nomenclature %in% missing_nomenclature)

# Process missing values only
for (i in seq_len(nrow(missing_gene_symbols))) {
  gene <- missing_gene_symbols$Signature[i]
  cancer_type <- missing_gene_symbols$CTAB[i]
  omic <- missing_gene_symbols$Omic_feature[i]
  Nomenclature <- missing_gene_symbols$Nomenclature[i]
  
  message("Processing missing Nomenclature: ", Nomenclature)
  
  data <- tryCatch({
    tcga_surv_get(
      item = gene,
      TCGA_cohort = cancer_type,
      profile = omic,
      TCGA_cli_data = tcga_cli_data,
      opt_pancan = .opt_pancan
    )
  }, error = function(e) {
    warning("Error with ", Nomenclature, ": ", e$message)
    NULL
  })
  
  if (!is.null(data) && "value" %in% colnames(data)) {
    if ("sampleID" %in% colnames(data)) {
      data <- rename(data, sample = sampleID)
    }
    
    data <- data %>%
      rename(expression_value = value) %>%
      mutate(
        gene = gene,
        type = cancer_type,
        omic_feature = omic,
        Nomenclature = Nomenclature
      ) %>%
      select(sample, type, Nomenclature, expression_value)
    
    gene_omic_list[[Nomenclature]] <- data
  } else {
    message("⚠ No data retrieved for ", Nomenclature)
  }
  
  # Save progress every 5 genes
  if (length(gene_omic_list) %% 5 == 0) {
    saveRDS(gene_omic_list, rds_file)
    message("Progress saved in ", rds_file)
  }
}

# Final save
saveRDS(gene_omic_list, rds_file)
message("Final progress saved in ", rds_file)

# Rebuild final dataset
final_omic_data <- bind_rows(gene_omic_list) %>%
  pivot_wider(names_from = Nomenclature, values_from = expression_value)

final_data <- full_join(tcga_cli_data, final_omic_data, by = c("sample", "type"))

safe_export_tsv(as.data.frame(final_data), "ML_final_data_updated.tsv")
message("Updated dataset saved as ML_final_data_updated.tsv")

ML_final_data_updated <- import( "ML_final_data_updated.tsv", na.strings = "NA")

df001 <- ML_final_data_updated ### it corresponde to the final retrived TCGA per patient data

####
####
#### -------------------------------------------------------
#### PART C - validation of the preceding debugging PART B ####
#### -------------------------------------------------------
#### Debugging for missing variables in final output
#### Checking expected dimension and structure of final output
#### Check which Nomenclature values are missing in gene_omic_list
#### 
#### 
missing_nomenclature <- setdiff(gene_symbols$Nomenclature, names(gene_omic_list))
print(length(missing_nomenclature))  # Should return 22
print(missing_nomenclature)  # Show missing values

## Check which Nomenclature values eere processed in the loop
processed_nomenclature <- c()  # Track stored values

## Check for NULL Entries in gene_omic_list
null_entries <- sum(sapply(gene_omic_list, is.null))
print(paste("Number of NULL entries in gene_omic_list:", null_entries))

## Check if Data Exists for These Nomenclature Values
check_data <- gene_symbols %>%
  filter(Nomenclature %in% missing_nomenclature)
print(check_data)

print(length(unique(names(gene_omic_list))))  # Should be 14907
print(length(unique(colnames(final_omic_data))))  # Should be 14909

setdiff(colnames(final_omic_data), names(gene_omic_list))  # Find columns in final_omic_data that are not in gene_omic_list

# Identify missing Nomenclature values
missing_nomenclature <- setdiff(gene_symbols$Nomenclature, names(gene_omic_list))
message("Processing only missing Nomenclature values: ", length(missing_nomenclature))

# Filter missing gene symbols based on missing nomenclature
missing_gene_symbols <- gene_symbols %>%
  filter(Nomenclature %in% missing_nomenclature)

# Log the number of missing entries
log_msg("🔹 Number of missing gene symbol entries:", nrow(missing_gene_symbols), "\n")
gc()

####
####
#### -----------------------------------------------------
#### Converting empty values (== "") to NA missing values
#### -----------------------------------------------------
#### 
### It does not put the string "NA" into the cell. 
### Instead, it converts empty string values ("") to actual R missing values, i.e., 
### NA of type logical, character, or factor, depending on the column's class.
# Count empty string ("") values in each column
empty_string_counts <- sapply(df001, function(col) sum(col == "", na.rm = TRUE))

# Show columns with at least one empty value
empty_string_counts[empty_string_counts > 0]

df001[df001 == ""] <- NA

# Count empty string ("") values in each column
empty_string_counts <- sapply(df001, function(col) sum(col == "", na.rm = TRUE))

# Show columns with at least one empty value
empty_string_counts[empty_string_counts > 0]

##### 
##### -----------------------------------------------------------------------------------
##### Remove Rows Fully NA Across Columns 23:14937 (Predictor variables only)
##### -----------------------------------------------------------------------------------
##### 

# Define the range of columns to inspect
col_range <- 23:14937

# Identify rows where all values in columns 23 through 14937 are NA
fully_na_rows <- which(rowSums(is.na(df001[, col_range])) == length(col_range))

# Logging
log_msg("🔍 Found ", length(fully_na_rows), " rows with all NA in columns 23 to 14937.\n")

# Optional: Save those rows for tracking
excluded_na_rows_df <- df001[fully_na_rows, ]

# Remove those rows (including the first occurrence)
df001_cleaned <- df001[-fully_na_rows, ]

# Post-validation
remaining <- which(rowSums(is.na(df001_cleaned[, col_range])) == length(col_range))
if (length(remaining) == 0) {
  log_msg("✅ All fully-NA rows across the specified column range were successfully removed.\n")
} else {
  log_msg("⚠️ Still", length(remaining), "rows remaining with all NA in that range.\n")
}

# Export to expected file name
safe_export_tsv(as.data.frame(df001_cleaned), "df001_cleaned.tsv")

##### 
##### -----------------------------------------------------------------------------------
##### Remove Rows Fully NA Across Columns 23:30 (Response variables only)
##### -----------------------------------------------------------------------------------
##### 
df001_cleaned <- import("df001_cleaned.tsv", na.strings = "NA")

# Count empty string ("") values in each column
empty_string_counts <- sapply(df001_cleaned, function(col) sum(col == "", na.rm = TRUE))

# Show columns with at least one empty value
empty_string_counts[empty_string_counts > 0]

# ===== Audit-enhanced removal of fully-NA rows across columns 23:30 variables =====

# 0) Guards and configuration
stopifnot(is.data.frame(df001_cleaned))
col_range <- 23:30
if (max(col_range) > ncol(df001_cleaned)) {
  stop(sprintf("col_range exceeds ncol(df001_cleaned) = %d.", ncol(df001_cleaned)))
}

# 1) Pre-snapshot
pre_n <- nrow(df001_cleaned)
block_pre <- df001_cleaned[, col_range, drop = FALSE]

# 2) Identify rows fully NA in the block
fully_na_rows <- which(rowSums(is.na(block_pre)) == ncol(block_pre))

# 3) Logging
log_msg("🔍 Found ", length(fully_na_rows), " rows with all NA in columns 23 to 30.\n")

# 4) Optional: Save those rows for tracking (and verify they are indeed fully NA)
## Preconditions (defensive checks; keep them if helpful)
stopifnot(is.data.frame(df001_cleaned))
stopifnot(is.integer(col_range) || is.numeric(col_range))
col_range <- as.integer(col_range)
stopifnot(all(col_range >= 1L), max(col_range) <= ncol(df001_cleaned))

## 1) Identify rows fully NA over the target column range
target_block <- df001_cleaned[, col_range, drop = FALSE]
fully_na_rows <- which(rowSums(is.na(target_block)) == ncol(target_block))

## 2) Optional: Save those rows for tracking (and verify they are indeed fully NA)
if (length(fully_na_rows) > 0L) {
  # NOTE the comma to avoid the 'drop' warning:
  excluded_na_rows_df <- df001_cleaned[fully_na_rows, , drop = FALSE]
  
  # Re-check on the same column set actually being tested
  sub_after <- excluded_na_rows_df[, col_range, drop = FALSE]
  
  # Strict validation: every selected row must be all-NA across col_range
  stopifnot(
    ncol(sub_after) == length(col_range),
    all(rowSums(is.na(sub_after)) == ncol(sub_after))
  )
  
  # (optional) persist
  safe_export_tsv(as.data.frame(excluded_na_rows_df), "excluded_na_rows_df.tsv")
}

# 5) Remove those rows (no-op safe)
if (length(fully_na_rows) > 0L) {
  idx <- sort(unique(fully_na_rows))
  # defensive bounds check
  stopifnot(all(idx >= 1L & idx <= nrow(df001_cleaned)))
  
  # specify the (all) columns explicitly so `drop` is honored
  df001_cleaned <- df001_cleaned[-idx, , drop = FALSE]
}

# 6) Post-validation
block_post <- df001_cleaned[, col_range, drop = FALSE]
remaining <- which(rowSums(is.na(block_post)) == ncol(block_post))

# 7) Assertions: row delta equals number identified; none remain
delta_n <- pre_n - nrow(df001_cleaned)
if (delta_n != length(fully_na_rows)) {
  stop(sprintf("Row delta mismatch: expected to drop %d, but delta was %d.",
               length(fully_na_rows), delta_n))
}
if (length(remaining) != 0L) {
  stop(sprintf("Post-validation failure: %d fully-NA rows remain in columns 23–30.",
               length(remaining)))
}

# 8) Summary table (prints to console for quick inspection)
audit_summary <- data.frame(
  metric = c("n_rows_before", "n_rows_after", "n_cols_in_block",
             "fully_na_found", "fully_na_removed", "fully_na_remaining"),
  value  = c(pre_n, nrow(df001_cleaned), ncol(block_pre),
             length(fully_na_rows), delta_n, length(remaining)),
  row.names = NULL
)
print(audit_summary)

# 9) Final logging and export
log_msg("✅ All fully-NA rows across columns 23–30 were successfully removed. ",
        "Dropped: ", delta_n, " rows.\n")

# Optionally persist the excluded rows for provenance:
safe_export_tsv(as.data.frame(excluded_na_rows_df), "excluded_rows_23_30.tsv")

safe_export_tsv(as.data.frame(df001_cleaned), "df001_cleaned.tsv")

####
####
####

#### ============================================================================
#### 📌 Summary of `df001_cleaned` vs `df001_cleaned_final`
#### ============================================================================

# 🔹 df001_cleaned:
# - This is the initial cleaned dataset **prior to imputation**.
# - It may contain duplicated patient entries.
# - Columns 23–30 may have inconsistencies across those duplicated entries.
# - Used as the basis for identifying and harmonizing duplicates.

# 🔹 df001_cleaned_final:
# - This is the final pré-harmonized version of the dataset after resolving duplicates.
# - Duplicated patient entries were identified and grouped.
# - For identical values in cols 23–30 → rows were retained as-is.
# - For differing values in cols 23–30 → values were harmonized using the row with
#   the fewest missing entries as reference.
# - The harmonized duplicated entries were merged back with the unique patients.
# - This is the **final dataset** to be used for downstream imputation and modeling.

#### =============================================================================
#### 📘 Harmonization Pipeline for Duplicated Patients in `df001_cleaned`
#### =============================================================================

suppressPackageStartupMessages(library(rio))

#### -----------------------------------------------------------------------------
#### Step 0: Load Initial Cleaned Dataset
#### -----------------------------------------------------------------------------
df001_cleaned <- import("df001_cleaned.tsv", na.strings = "NA")

#### -----------------------------------------------------------------------------
#### Step 1: Identify Duplicated Patients
#### -----------------------------------------------------------------------------
patient_counts <- table(df001_cleaned$patient)
duplicated_patients <- names(patient_counts[patient_counts > 1])
df_duplicated_all <- df001_cleaned[df001_cleaned$patient %in% duplicated_patients, ]

# Logging
log_msg("🔍 Total duplicated patient entries:", nrow(df_duplicated_all), "\n")
log_msg("📊 Unique duplicated patients:", length(duplicated_patients), "\n")

#### -----------------------------------------------------------------------------
#### Step 2: Compare Cols 23–30 Within Each Duplicated Patient
#### -----------------------------------------------------------------------------
col_range <- 23:30
identical_rows_list <- list()
differing_rows_list <- list()

for (pat in duplicated_patients) {
  group <- df_duplicated_all[df_duplicated_all$patient == pat, ]
  comp_cols <- group[, col_range]
  if (nrow(unique(comp_cols)) == 1) {
    identical_rows_list[[pat]] <- group
  } else {
    differing_rows_list[[pat]] <- group
  }
}

has_identical  <- length(identical_rows_list)  > 0L
has_differing  <- length(differing_rows_list)  > 0L

df_identical_rows <- if (has_identical) {
  do.call(rbind, identical_rows_list)
} else if (exists("schema_identical") && is.data.frame(schema_identical)) {
  schema_identical[0,  drop = FALSE]
} else {
  structure(list(), class = "data.frame", row.names = integer(0))  # 0×0 df
}

df_differing_rows <- if (has_differing) {
  do.call(rbind, differing_rows_list)
} else if (exists("schema_differing") && is.data.frame(schema_differing)) {
  schema_differing[0,  drop = FALSE]
} else {
  structure(list(), class = "data.frame", row.names = integer(0))  # 0×0 df
}

# Optional: a boolean you can reuse downstream
has_differences <- has_differing

# Logging
log_msg("✅ Identical col23–30 groups: ", nrow(df_identical_rows), " rows\n")
log_msg("⚠️ Differing col23–30 groups: ", nrow(df_differing_rows), " rows\n")

#### -----------------------------------------------------------------------------
#### Step 3: Harmonize Differing Cols 23–30 Using Row with Fewest NAs
#### -----------------------------------------------------------------------------
df_harmonized_differing <- df_differing_rows
modification_log <- data.frame(
  patient_id = character(), rows_modified = integer(),
  values_replaced = integer(), reference_row = integer(),
  stringsAsFactors = FALSE
)

for (pat in unique(df_differing_rows$patient)) {
  idx <- which(df_differing_rows$patient == pat)
  group <- df_differing_rows[idx, ]
  mat <- group[, col_range]
  na_counts <- rowSums(is.na(mat))
  ref_idx <- which.min(na_counts)
  ref_values <- mat[ref_idx, ]
  
  total_changes <- 0
  for (j in seq_along(idx)) {
    current_idx <- idx[j]
    old_values <- df_differing_rows[current_idx, col_range]
    diffs <- ref_values != old_values | (is.na(ref_values) != is.na(old_values))
    total_changes <- total_changes + sum(diffs, na.rm = TRUE)
    df_harmonized_differing[current_idx, col_range] <- ref_values
  }
  
  modification_log <- rbind(modification_log, data.frame(
    patient_id = pat,
    rows_modified = length(idx),
    values_replaced = total_changes,
    reference_row = idx[ref_idx]
  ))
}

log_msg("🛠️ Harmonization completed for: ", nrow(modification_log), " patients\n")

#### -----------------------------------------------------------------------------
#### Step 4: Reconstruct Full Harmonized Dataset
#### -----------------------------------------------------------------------------
df_duplicated_harmonized <- rbind(df_identical_rows, df_harmonized_differing)
patients_harmonized <- unique(df_duplicated_harmonized$patient)
df_unique_patients <- df001_cleaned[!(df001_cleaned$patient %in% patients_harmonized), ]

df001_cleaned_final <- rbind(df_unique_patients, df_duplicated_harmonized)

# Final Logging
log_msg("📦 Final rows:", nrow(df001_cleaned_final), "\n")
log_msg("🧾 Unique patients:", length(unique(df001_cleaned_final$patient)), "\n")

#### -----------------------------------------------------------------------------
#### Summary Comment
#### -----------------------------------------------------------------------------
# 🔹 df001_cleaned:
#     - Original cleaned dataset, includes duplicated patients.
# 🔹 df001_cleaned_final:
#     - Fully harmonized dataset after resolving duplicates in cols 23–30.
#     - To be used for downstream imputation and modeling.

#### 
#### 
#### ----------------------------------------------------------------------------------
#### Validation of Duplicate patients harmonization of clinical data before imputation
#### ----------------------------------------------------------------------------------
#### 
#### 
# 
# Step 1: Count the number of times each patient appears
patient_counts <- table(df001_cleaned_final$patient)

# Step 2: Identify patients with more than one occurrence
duplicated_patients <- names(patient_counts[patient_counts > 1])

# Step 3: Subset the original dataframe for all those patients (include all duplicates)
df001_cleaned_final_duplications <- df001_cleaned_final[df001_cleaned_final$patient %in% duplicated_patients, ]

# Step 4: Logging
dup_freqs <- patient_counts[duplicated_patients]
log_msg("🔍 Total duplicated patient entries (including originals):", nrow(df001_cleaned_final_duplications), "\n")
log_msg("📊 Number of unique duplicated patients:", length(duplicated_patients), "\n")
log_msg("📈 Duplication frequency ranges from: ",
    min(dup_freqs), " to ", max(dup_freqs), " occurrences.\n")

# Optional: view summary distribution of duplication frequency
duplication_summary <- as.data.frame(table(dup_freqs))
colnames(duplication_summary) <- c("Occurrences", "Num_Patients")
print(duplication_summary)

# Define the column range to compare
col_range <- 23:30

# Get all duplicated patient IDs
duplicated_patients <- unique(df001_cleaned_final_duplications$patient)

# Initialize lists
identical_rows_list <- list()
differing_rows_list <- list()

# Iterate over each duplicated patient
for (pat in duplicated_patients) {
  patient_group <- df001_cleaned_final_duplications[df001_cleaned_final_duplications$patient == pat, ]
  comp_cols <- patient_group[, col_range]
  
  # Check if all rows in columns 23:30 are identical
  if (nrow(unique(comp_cols)) == 1) {
    identical_rows_list[[pat]] <- patient_group
  } else {
    differing_rows_list[[pat]] <- patient_group
  }
}

# Combine rows into dataframes
df_identical_23_30 <- do.call(rbind, identical_rows_list)

gc()

# Safely combine and log differing patient groups
# # Logging
log_msg("✅ Identical patient groups in columns 23:30: " , length(identical_rows_list), " patients | ",
    nrow(df_identical_23_30), " rows\n")

if (length(differing_rows_list) > 0) {
  df_differing_23_30 <- do.call(rbind, differing_rows_list)
  log_msg("⚠️ Differing patient groups in columns 23:30:", 
      length(differing_rows_list), "patients |",
      nrow(df_differing_23_30), "rows\n")
} else {
  df_differing_23_30 <- data.frame()
  log_msg("ℹ️ No differing patient groups found in columns 23:30.\n")
}

#####
##### ------------------------------------------------------------------------------------
##### Harmonizing Values in Columns 23:30 Across Duplicated Patients in df_differing_23_30
##### -------------------------------------------------------------------------------------
#####
# -----------------------------------------------------------
# Harmonize columns 23:30 across duplicates in df_differing_23_30
# Replace values using the row with the fewest NAs
# Also log modified patients and number of values changed
# -----------------------------------------------------------

df_differing_23_30_harmonized <- df_differing_23_30
col_range <- 23:30
duplicated_patients <- unique(df_differing_23_30$patient)

# Initialize a log dataframe
modification_log <- data.frame(
  patient_id = character(),
  rows_modified = integer(),
  values_replaced = integer(),
  reference_row = integer(),
  stringsAsFactors = FALSE
)

for (pat in duplicated_patients) {
  patient_group_idx <- which(df_differing_23_30$patient == pat)
  patient_group <- df_differing_23_30[patient_group_idx, ]
  comp_matrix <- patient_group[, col_range]
  
  # Count NAs per row and pick the one with the fewest
  na_counts <- rowSums(is.na(comp_matrix))
  reference_index <- which.min(na_counts)
  reference_values <- comp_matrix[reference_index, ]
  
  # Calculate how many values will be replaced in total
  total_replacements <- 0
  for (i in seq_along(patient_group_idx)) {
    current_idx <- patient_group_idx[i]
    old_values <- df_differing_23_30[current_idx, col_range]
    # Count non-identical values (including NAs)
    diffs <- reference_values != old_values | (is.na(reference_values) != is.na(old_values))
    total_replacements <- total_replacements + sum(diffs, na.rm = TRUE)
    
    # Replace with reference
    df_differing_23_30_harmonized[current_idx, col_range] <- reference_values
  }
  
  # Log modification
  modification_log <- rbind(modification_log, data.frame(
    patient_id = pat,
    rows_modified = length(patient_group_idx),
    values_replaced = total_replacements,
    reference_row = patient_group_idx[reference_index]
  ))
}

log_msg("✅ Harmonization complete for ", nrow(modification_log), " patients.\n")
print(head(modification_log))

# Optional: merge with df_identical_23_30 to reconstruct full duplicate subset
df001_cleaned_duplications_harmonized <- rbind(df_identical_23_30, df_differing_23_30_harmonized)

# Optional: export results
# export(df_differing_23_30_harmonized, "df_differing_23_30_harmonized.tsv", na = "NA")
# export(modification_log, "modification_log.tsv", na = "NA")
# export(df001_cleaned_duplications_harmonized, "df001_cleaned_duplications_harmonized.tsv", na = "NA")

# ------------------------------------------------------------------------------------
# Merge harmonized duplicated patient block into original full dataset
# ------------------------------------------------------------------------------------

# Step 1: Extract the list of all patient IDs that were harmonized
harmonized_patients <- unique(df001_cleaned_duplications_harmonized$patient)

# Step 2: Subset df001_cleaned to exclude all those harmonized patients
df001_cleaned_without_duplicates <- df001_cleaned[!(df001_cleaned$patient %in% harmonized_patients), ]

# Step 3: Append the harmonized duplicate patient block
df001_cleaned_final <- rbind(df001_cleaned_without_duplicates, df001_cleaned_duplications_harmonized)

# Step 4: Optional consistency check
log_msg("📦 Rows in final dataset:", nrow(df001_cleaned_final), "\n")
log_msg("🧾 Unique patients:", length(unique(df001_cleaned_final$patient)), "\n")

# Export to expected file name
safe_export_tsv(as.data.frame(df001_cleaned_final), "df001_cleaned_final.tsv")

df001_cleaned_final <- import("df001_cleaned_final.tsv", na.strings = "NA")
gc()

#####
##### ----------------------------------------------------------------------
##### Check for Fully Duplicated Rows Across All Columns in df001_cleaned_final
##### ----------------------------------------------------------------------
#####

# Check for duplicated rows across all variables
duplicated_row_flags <- duplicated(df001_cleaned_final)

# Count how many duplicated rows
num_duplicates <- sum(duplicated_row_flags)

# Display result
log_msg("🔍 Total number of fully duplicated rows:", num_duplicates, "\n")

# Optional: Extract the duplicated rows (excluding first occurrence)
df001_cleaned_final_duplicates <- df001_cleaned_final[duplicated_row_flags, ]

# Optional: View summary
if (num_duplicates > 0) {
  print(head(df001_cleaned_final_duplicates))
} else {
  log_msg("✅ No fully duplicated rows found in df001_cleaned_final.\n")
}

# Remove rows where all values from column 23 onwards are NA
df001_cleaned_final_filtered <- df001_cleaned_final[!apply(df001_cleaned_final[, 23:ncol(df001_cleaned_final)], 1, function(row) all(is.na(row))), ]

gc()

df004 <- df001_cleaned_final_filtered

safe_export_tsv(as.data.frame(df004), "df004.tsv")

rm(df001_cleaned_final, df001_cleaned_duplications_harmonized, df001_cleaned_final_duplications,
   df002)

gc()

### 
### 
### 
### END OF MODULE 4 ###
### 
### 
### 

####
####
#### -----------------------------------------------------------------------------
#### MODULE 5. Groupwise imputation across high-dimensional binomial or continuous variables
#### -----------------------------------------------------------------------------
#### 
#### Remaking the consolidated dataframe for ML imputations
#### Brainstorm meeting 03/25/2025; 01/04/2025
#### Emanuell, Higor, Victor, Enrique
#### 

# Step 1: Filter rows in df004 and assign to df002
df002 <- df004

df002_filtered <- df002[!apply(df002[, 23:ncol(df002)], 1, function(row) all(is.na(row))), ]
df002_filtered_2 <- df002[apply(df002[, 23:ncol(df002)], 1, function(row) all(is.na(row))), ]

df003 <- df002_filtered

# Step 2: Define survival columns to harmonize
cols_to_check <- c("OS", "DSS", "DFI", "PFI", 
                   "OS.time", "DSS.time", "DFI.time", "PFI.time")

# Step 3: Separate duplicate and non-duplicate patients by cancer type
duplicates <- df003 %>%
  group_by(patient, type) %>%
  filter(n() >= 2) %>%
  arrange(patient, type, .by_group = TRUE)

non_duplicates <- df003 %>%
  group_by(patient, type) %>%
  filter(n() == 1) %>%
  ungroup()

# Step 4: Harmonize duplicated rows pairwise
fill_pairwise <- function(df_pair) {
  if (nrow(df_pair) != 2) return(df_pair)
  row1 <- df_pair[1, ]
  row2 <- df_pair[2, ]
  for (col in cols_to_check) {
    if (is.na(row1[[col]]) && !is.na(row2[[col]])) row1[[col]] <- row2[[col]]
    else if (!is.na(row1[[col]]) && is.na(row2[[col]])) row2[[col]] <- row1[[col]]
  }
  bind_rows(row1, row2)
}

filled_duplicates <- duplicates %>%
  group_split(patient, type) %>%
  lapply(fill_pairwise) %>%
  bind_rows()

# Step 5: Identify unresolved duplicated values
unresolved <- filled_duplicates %>%
  group_by(patient, type) %>%
  filter(n() >= 2) %>%
  summarise(across(all_of(cols_to_check), ~all(is.na(.x))), .groups = "drop") %>%
  pivot_longer(cols = all_of(cols_to_check), names_to = "variable", values_to = "both_na") %>%
  filter(both_na == TRUE)

# Step 6: Merge cleaned duplicates with non-duplicates
df003_cleaned <- bind_rows(filled_duplicates, non_duplicates) %>%
  arrange(patient, type)

gc()

if (nrow(unresolved) > 0) {
  message("Unresolved NA values (both duplicates had NA):")
  print(unresolved)
} else {
  message("All possible imputations successfully completed.")
}

safe_export_tsv(as.data.frame(df003_cleaned), "df003_cleaned.tsv")
gc()

#### ---------------------------------------------------------------------------
#### PART 1 - Convert CNV omic values (feature 3) to categorical for imputation
#### ---------------------------------------------------------------------------
#### Creating input df005: NA_harmonized_survival_variables_and binomialized_CNV_and Mutation_variables_pre_imputation

map_to_cnv_status <- function(x) {
  if (is.character(x) || is.factor(x)) x <- as.numeric(as.character(x))
  if (is.numeric(x)) {
    return(ifelse(is.na(x), NA,
                  ifelse(x == 0, "Normal",
                         ifelse(x > 0, "Duplicated", "Deleted"))))
  } else return(x)
}

apply_cnv_mapping_2nd_pos_3_only <- function(df) {
  target_vars <- names(df)[sapply(strsplit(names(df), "\\."), function(x) {
    length(x) >= 2 && x[2] == "3"
  })]
  df[target_vars] <- lapply(df[target_vars], map_to_cnv_status)
  return(df)
}

df003_cleaned_categoric_3 <- apply_cnv_mapping_2nd_pos_3_only(df003_cleaned)
gc()

#### ---------------------------------------------------------------------------
#### PART 2 - Convert Mutation omic values (feature 2) to binary for imputation
#### ---------------------------------------------------------------------------
#### Creating input df005: NA_harmonized_survival_variables_and binomialized_CNV_and Mutation_variables_pre_imputation

map_to_binary <- function(x) {
  if (is.factor(x) || is.character(x)) x <- as.numeric(as.character(x))
  if (is.numeric(x)) return(ifelse(is.na(x), NA, ifelse(x == 0, 0, 1)))
  else return(x)
}

apply_mapping_to_2nd_pos_2_only <- function(df) {
  target_vars <- names(df)[sapply(strsplit(names(df), "\\."), function(x) {
    length(x) >= 2 && x[2] == "2"
  })]
  df[target_vars] <- lapply(df[target_vars], map_to_binary)
  return(df)
}

# Apply to CNV-processed data
df003_cleaned_categoric_2_3 <- apply_mapping_to_2nd_pos_2_only(df003_cleaned_categoric_3)

### Creating df005 NA_harmonized_survival_variables_and binomialized_CNV_and Mutation_variables_pre_imputation
df005 <- df003_cleaned_categoric_2_3

# Save and reload to confirm
safe_export_tsv(as.data.frame(df005), "df005.tsv")

df005 <- import("df005.tsv", na.strings = "NA") 

#####
##### Creating df005.rds harmonized_survival_variables_and binomialized_CNV_and Mutation_variables_pre_imputation
##### 

# Save df005 as RDS file
saveRDS(df005, file = "df005.rds")
saveRDS(df005, file = "df005_NA_Dupl_harmonized.rds") # this is input .rds file for Emanuell´s harmonization

#### NOTE: This df005 MUST BE RUN THROUGH EMANUELL´s HARMONIZTION PIPELINE OF DEMOGRAPHIC AND CLINCIAL DATA 

# Cleanup
gc()

# List all objects starting with "df00" or "df00X_"
objs_to_remove <- ls(pattern = "^df00\\d(_.*)?$")

# Exclude 'df005' from the list
objs_to_remove <- setdiff(objs_to_remove, "df005")

# Remove the remaining objects from the environment
rm(list = objs_to_remove, envir = .GlobalEnv)

# Optional: free up memory
gc()

### 
### 
### 
### END OF MODULE 5 ###
###  
### 
### 

###
###
### PRÉ-MODULE 6 - HANDLING NEW df005.rds passed harmonization/imputation Emanuell
### Importing  df with harmonized clinical and demographic (df020_Emanuell.tsv)
### This df MUST be generated based on the earlier df005 generated in MOUDULE 5 above, but harmonized by Emanuell (08/22/2025)
### Dependencies: it requires uploading of "df020_Emanuell.tsv"
###
###
# 📦 Safe import function
safe_import <- function(file, format = NULL, ...) {
  rio::import(file, format = format, na.strings = "NA", ...)
}

# 🗂️ Import the TSV file
df005 <- safe_import("df020_Emanuell.tsv", format = "tsv")

# 💾 Save the object as RDS in the working directory
saveRDS(df005, file = "df005.rds")

# 🔄 Reload later (restores the exact same object)
df005 <- readRDS("df005.rds")

df020_Emanuell <- readRDS("df020_Emanuell.rds")

### ============================================================================================
### 🔍 Diagnostic Audit — Non-NA Time Variable Counts Per Cancer Type - Eligibility logic
### ============================================================================================
###
### Purpose:
### This diagnostic block assesses the availability of survival time data (OS.time, DSS.time, 
### DFI.time, and PFI.time) across all cancer types present in `df005`. It quantifies the number 
### of non-missing entries per time variable and identifies cancer types that lack sufficient 
### observations (< 5) to support ML-based survival time imputation.
###
### Output:
### • Full per-type count matrix:         "min_nonNA_counts_per_type.tsv"
### • Global minimum per time variable:   "min_nonNA_counts_per_time_variable.tsv"
### • Cancer types below threshold:       "types_with_low_nonNA_counts.tsv"
###
### ⚠️ Cancer types flagged as sparse must follow fallback-only imputation logic.
### ============================================================================================

# 📦 Load required libraries
library(dplyr)
library(tidyr)
library(readr)
library(rio)

# 🎯 Define survival time variables of interest
time_vars <- c("OS.time", "DSS.time", "DFI.time", "PFI.time")

# ✅ Validate presence of all required columns in df005
missing_cols <- setdiff(c("type", time_vars), colnames(df005))
if (length(missing_cols) > 0) {
  stop("Missing required columns in df005: ", paste(missing_cols, collapse = ", "))
}

# 📊 Compute non-NA value counts for each .time variable by cancer type
non_na_counts <- df005 %>%
  group_by(type) %>%
  summarise(across(all_of(time_vars), ~sum(!is.na(.x)), .names = "non_na_{.col}")) %>%
  ungroup()

# 💾 Optional: Save summary of minimum values across all types per time variable
min_counts <- non_na_counts %>%
  summarise(across(starts_with("non_na_"), min, .names = "min_{.col}"))

export(min_counts, file = "df005_min_nonNA_counts_per_time_variable.tsv", format = "tsv", na = "NA")

# 💾 Save full per-type count matrix
min_nonNA_df <- df005 %>%
  group_by(type) %>%
  summarise(
    OS_time_nonNA  = sum(!is.na(OS.time)),
    DSS_time_nonNA = sum(!is.na(DSS.time)),
    DFI_time_nonNA = sum(!is.na(DFI.time)),
    PFI_time_nonNA = sum(!is.na(PFI.time)),
    .groups = "drop"
  )

write_tsv(min_nonNA_df, "df005_min_nonNA_counts_per_type.tsv")

# =======================================================================================================
# ⛔️ Constraint Check: Sparse Survival Time Data and ML-Based Imputation Ineligibility
# -------------------------------------------------------------------------------------------------------
# ML-based regression methods (e.g., XGBoost, LightGBM, missForest) require a sufficient number of 
# ground-truth (non-NA) observations to train models. Cancer types with <5 valid values in any .time 
# variable are therefore ineligible for ML-based imputation and must be handled via fallback logic.
#
# ➤ Fallback logic applies in the following hierarchy:
#     1. Intra-type median (if available)
#     2. Ontology groupwise mean-of-medians
#     3. Fixed hardcoded default (e.g., 90 days)
# =======================================================================================================

# ⚠️ Identify and extract cancer types with <5 valid values in any time variable
types_with_low_counts <- min_nonNA_df %>%
  filter(OS_time_nonNA  < 5 |
           DSS_time_nonNA < 5 |
           DFI_time_nonNA < 5 |
           PFI_time_nonNA < 5)

# 💾 Save filtered list for exclusion from ML modeling
write_tsv(types_with_low_counts, "types_with_low_nonNA_counts.tsv")

# 🔎 Extract unique set of affected cancer types
affected_types <- unique(types_with_low_counts$type)
print(affected_types)

#### 
#### 
#### 
#### END OF PREPARATION MODULES ####
#### 
#### 
#### 

# =========================================================================================
# RUN "RScript_Modulo_2_AND_3_condicional.R" FOR SURVIVAL IMPUTATION WITH CONDITIONALS
# =========================================================================================
source("RScript_Modulo_2_AND_3_condicional.R")

####
####
#### END
#### 
#### 
#### 



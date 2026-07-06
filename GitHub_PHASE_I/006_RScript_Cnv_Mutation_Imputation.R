#### Imputation sequence I/O input and output file
#### REFACTORED 02/11/2025
#### 
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

# ============================================================
# Summary Table of Expected Indexed Dataframes (df001–df377)
# ============================================================

# Load required libraries
suppressPackageStartupMessages({
  library(dplyr)
  library(tibble)
  library(openxlsx)   # for Excel export (optional)
  library(readr)      # for TSV export (optional)
})

# Construct the summary table
summary_table <- tibble(
  Category = c("Pre-processing", "Survival", "CNV", "Mutation", "Continuous", "Total Indexed"),
  Count = c(5, 3, 9, 36, 288, 377),
  Index_Range = c("df001–df005", "df006–df008", "df009–df017", "df018–df053", "df054–df377", "df001–df377")
)

# Print table to console
print(summary_table)

# -----------------------------
# Optional: Export to Excel
# -----------------------------
# Uncomment to export
# write.xlsx(summary_table, "Imputation_Index_Summary.xlsx", overwrite = TRUE)

# -----------------------------
# Optional: Export to TSV
# -----------------------------
# Uncomment to export
# write_tsv(summary_table, "Imputation_Index_Summary.tsv")

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


### STARTS HERE!!
##### -------------------------------------------------------------------------
##### CNV Imputation Eligibility & Non-Removal Policy (Dimension Preservation)
##### -------------------------------------------------------------------------
# Invariants for this stage:
# • The dataframe schema (set and order of columns) MUST be preserved across all steps.
# • No column/variable is ever removed, renamed, or re-ordered during CNV imputation.
# • Imputation is applied ONLY where the per-(variable × type) NA rate ≤ THRESHOLD (0.35).
# • Variables failing the NA ≤ THRESHOLD criterion remain UNTOUCHED (values are carried
#   forward exactly as in the input, including NAs), ensuring identical dimensions.
#
# Configuration:
# • DECIMALS  = 4  → number of decimal places used to report NA rates.
# • THRESHOLD = 0.35 → per-(variable × type) maximal NA rate eligible for imputation.
#
# Definitions (evaluated per variable × type):
# • na_rate(var, type) = fraction of NA entries in that subgroup (reported to DECIMALS).
# • eligible_target(var, type) = (na_rate(var, type) ≤ THRESHOLD).
#
# Target rule:
# • If eligible_target == TRUE → apply CNV imputation (Mode / Random / kNN) to NA cells ONLY.
# • If eligible_target == FALSE → DO NOT IMPUTE this variable for this type; copy values as-is.
#
# Predictor mask (algorithmic only; NOT a schema change):
# • For distance-based methods (kNN), construct the distance using ONLY predictor columns that
#   (a) are CNV variables, and (b) are eligible_target == TRUE in the same type.
# • Ineligible variables remain present in the dataframe but are EXCLUDED from distance
#   calculations internally. This does NOT remove columns.
#
# Pass-through guarantee:
# • All variables, including those with na_rate > THRESHOLD, must be present in the output with
#   identical names and positions. For ineligible targets, the output equals the input.
#
# Logging (per output object):
# • For each (variable, type): record NA_Rate (rounded to DECIMALS), Eligible (TRUE/FALSE),
#   Method (CNV_mode/CNV_random/CNV_knn/None), n_imputed, and Predictor_Masked (kNN only).
#
# Edge cases:
# • Variables with 100% NA in a type remain 100% NA in that type (no defaults, no borrowing).
# • Tie-breaking and seeding apply ONLY to eligible targets (deterministic behavior).
#
# Outcome:
# • The output dataframe retains identical dimensions and column order.
# • Only eligible NA cells are imputed; ineligible variables are passed through unchanged.
##### -------------------------------------------------------------------------

##### -------------------------------------------------------------------------
##### CNV Micro-Missingness Policy (Very Few NAs per variable × type)
##### -------------------------------------------------------------------------
# Scope:
# • Applies when the target CNV variable is nominal {Deleted, Normal, Duplicated}
#   and eligible for imputation (NA_rate ≤ THRESHOLD) within a given 'type'.
# • "Few NAs" may be defined as n_NA ∈ {1,2,3} or NA_rate ≤ 0.05 (advisory).
#
# Method selection (priority order):
# a) n_NA ≤ 2 AND skew ≥ 0.70 → MODE (deterministic tie-break: Deleted < Normal < Duplicated).
# b) n_NA ≤ 3 AND 0.40 ≤ skew < 0.70 → RANDOM (empirical probabilities; fixed seed).
# c) n_NA ≤ 3 with informative predictors → kNN (within type), majority vote; fixed seed.
##### -------------------------------------------------------------------------


##### ================================================================
##### CNV Target Variable Selector + Strict Token Validation Check
##### ================================================================
# Purpose:
#   Identify CNV variables from a harmonized multi-omic dataframe based on
#   ontology-encoded column names. CNV variables are defined as those whose
#   second dot-delimited token equals "3". This selector is used throughout
#   the CNV imputation pipeline to guarantee that only true CNV predictors
#   are included in MODE, RANDOM, and kNN imputation steps.
#
# Requirements:
#   • Variable naming ontology must be enforced upstream: <Prefix>.3.<Suffix>
#   • Second token must equal "3" EXACTLY (no partial matches allowed)
#
# Guarantees:
#   • Returns only CNV variables (strict token = "3")
#   • Excludes variables such as: token = "13", "03", "3p", "3A", "30", etc.
#   • Assertion checks enforce correctness and fail early if ontology breaks
#
# Usage:
#   cnv_vars <- .select_cnv_targets(df)
#   Used as the canonical CNV variable discovery method for the imputation pipeline.
##### ================================================================

# Helper function to extract CNV-encoded variables
.select_cnv_targets <- function(df) {
  names(df)[sapply(strsplit(names(df), "\\."),
                   function(x) length(x) >= 2 && x[2] == "3")]
}

# --- Strong ontology validation for a given dataframe (e.g., df006) ---

df006 <- readRDS("df006.rds")

cnv_vars <- .select_cnv_targets(df006)
stopifnot(length(cnv_vars) > 0)  # Must detect at least one CNV variable

# Re-validate that all selected variables satisfy strict token == "3"
ok_token <- sapply(strsplit(cnv_vars, "\\."),
                   function(tok) length(tok) >= 2 && identical(tok[2], "3"))
stopifnot(all(ok_token))  # Fails if any selected var violates the definition

##### ======================================================================
##### MODULE CNV Imputation — End-to-End Amended Pipeline (MODE, RANDOM, kNN)
##### ======================================================================

suppressPackageStartupMessages({
  library(rio)   # import/export
  library(VIM)   # kNN for categorical data
})

# ------------------------------
# Global configuration
# ------------------------------
DECIMALS  <- 4
THRESHOLD <- 0.35

# Deterministic tie-break order for MODE / vote ties
CNV_STATE_ORDER <- c("Deleted", "Normal", "Duplicated")

# ------------------------------
# Shared helpers
# ------------------------------

na_rate_by_var_type <- function(x, type_vec, digits = DECIMALS) {
  stopifnot(length(x) == length(type_vec))
  rates <- tapply(is.na(x), type_vec, function(z) mean(z), simplify = TRUE)
  # Round for reporting (do not change underlying gating comparisons)
  round(rates, digits = digits)
}

# Gate: impute this (variable × type) only if NA-rate ≤ THRESHOLD
should_impute_target <- function(x, type_vec, target_type, thr = THRESHOLD) {
  raw_rate <- tapply(is.na(x), type_vec, function(z) mean(z), simplify = TRUE)
  rate     <- raw_rate[[as.character(target_type)]]
  isTRUE(!is.na(rate) && rate <= thr)
}

# Deterministic mode with explicit state-order tie-break
deterministic_mode <- function(v, state_order = CNV_STATE_ORDER) {
  v <- v[!is.na(v)]
  if (!length(v)) return(NA_character_)
  tab  <- table(v)
  maxc <- max(tab)
  tied <- names(tab)[tab == maxc]
  pick <- intersect(state_order, tied)
  if (length(pick)) pick[1] else sort(tied)[1]
}

# Random sampling over LEVELS using empirical probabilities (optionally smoothed)
sample_empirical_levels <- function(v, n, seed = NULL, smoothing = 0L) {
  if (!is.null(seed)) set.seed(seed)
  v <- v[!is.na(v)]
  if (!length(v)) return(rep(NA_character_, n))
  counts <- table(v)
  if (smoothing > 0L) counts[] <- counts + smoothing
  probs  <- as.numeric(counts) / sum(counts)
  levs   <- names(counts)
  sample(levs, size = n, replace = TRUE, prob = probs)
}

# CNV target selection by naming rule (2nd token == "3") — naming guaranteed upstream
.select_cnv_targets <- function(df) {
  names(df)[sapply(strsplit(names(df), "\\."), function(x) length(x) >= 2 && x[2] == "3")]
}

# ---------------------------
# CNV_mode (groupwise by type)
# ---------------------------

impute_mode_groupwise_by_type <- function(df, type_col = "type", verbose = FALSE) {
  METHOD_ID   <- "CNV_mode"
  df_out      <- df
  target_vars <- .select_cnv_targets(df)
  
  imputation_log <- data.frame(
    Variable        = character(),
    Type            = character(),
    NA_Rate         = numeric(),
    Eligible        = logical(),
    NA_Before       = integer(),
    NA_After        = integer(),
    n_imputed       = integer(),
    Method          = character(),
    Predictor_Masked= logical(),
    stringsAsFactors = FALSE
  )
  
  for (var in target_vars) {
    prefix    <- strsplit(var, "-")[[1]][1]
    all_rows  <- which(df[[type_col]] == prefix)
    if (length(all_rows) == 0L) next
    
    # Eligibility & reporting rate (rounded)
    eligible <- should_impute_target(df[[var]], df[[type_col]], target_type = prefix, thr = THRESHOLD)
    rates    <- na_rate_by_var_type(df[[var]], df[[type_col]], digits = DECIMALS)
    na_rate  <- as.numeric(rates[[as.character(prefix)]])
    if (!eligible) {
      if (verbose) message("[", METHOD_ID, "] Pass-through (ineligible, NA_rate=",
                           format(na_rate, nsmall = DECIMALS), ") for ", var, " in type ", prefix)
      # Even if pass-through, we record a 'None' line for audit clarity
      imputation_log <- rbind(imputation_log, data.frame(
        Variable = var, Type = prefix, NA_Rate = na_rate, Eligible = FALSE,
        NA_Before = sum(is.na(df[[var]][all_rows])), NA_After = sum(is.na(df[[var]][all_rows])),
        n_imputed = 0L, Method = "None", Predictor_Masked = FALSE, stringsAsFactors = FALSE
      ))
      next
    }
    
    na_rows <- which(df[[type_col]] == prefix & is.na(df[[var]]))
    if (length(na_rows) == 0L) {
      # Eligible but nothing to impute; still log
      imputation_log <- rbind(imputation_log, data.frame(
        Variable = var, Type = prefix, NA_Rate = na_rate, Eligible = TRUE,
        NA_Before = 0L, NA_After = 0L, n_imputed = 0L, Method = METHOD_ID,
        Predictor_Masked = FALSE, stringsAsFactors = FALSE
      ))
      next
    }
    
    na_before  <- sum(is.na(df[[var]][all_rows]))
    group_vals <- df[[var]][all_rows][!is.na(df[[var]][all_rows])]
    if (!length(group_vals)) next
    
    mode_val <- deterministic_mode(group_vals, state_order = CNV_STATE_ORDER)
    df_out[[var]][na_rows] <- mode_val
    
    na_after <- sum(is.na(df_out[[var]][all_rows]))
    n_imp    <- length(na_rows)
    
    if (verbose) message("[", METHOD_ID, "] Imputed ", n_imp, " cells for ", var,
                         " (type ", prefix, "; NA_rate=", format(na_rate, nsmall = DECIMALS),
                         ") with '", mode_val, "'")
    
    imputation_log <- rbind(imputation_log, data.frame(
      Variable = var, Type = prefix, NA_Rate = na_rate, Eligible = TRUE,
      NA_Before = na_before, NA_After = na_after,
      n_imputed = n_imp, Method = METHOD_ID,
      Predictor_Masked = FALSE, stringsAsFactors = FALSE
    ))
  }
  
  assign("imputation_log", imputation_log, envir = .GlobalEnv)
  df_out
}

# ------------------------------------
# CNV_random (proportional over levels)
# ------------------------------------

impute_random_groupwise_by_type <- function(df, type_col = "type", seed = 123, smoothing = 0L, verbose = FALSE) {
  METHOD_ID   <- "CNV_random"
  df_out      <- df
  target_vars <- .select_cnv_targets(df)
  
  imputation_log <- data.frame(
    Variable        = character(),
    Type            = character(),
    NA_Rate         = numeric(),
    Eligible        = logical(),
    NA_Before       = integer(),
    NA_After        = integer(),
    n_imputed       = integer(),
    Method          = character(),
    Predictor_Masked= logical(),
    stringsAsFactors = FALSE
  )
  
  for (var in target_vars) {
    prefix    <- strsplit(var, "-")[[1]][1]
    all_rows  <- which(df[[type_col]] == prefix)
    if (length(all_rows) == 0L) next
    
    eligible <- should_impute_target(df[[var]], df[[type_col]], target_type = prefix, thr = THRESHOLD)
    rates    <- na_rate_by_var_type(df[[var]], df[[type_col]], digits = DECIMALS)
    na_rate  <- as.numeric(rates[[as.character(prefix)]])
    if (!eligible) {
      if (verbose) message("[", METHOD_ID, "] Pass-through (ineligible, NA_rate=",
                           format(na_rate, nsmall = DECIMALS), ") for ", var, " in type ", prefix)
      imputation_log <- rbind(imputation_log, data.frame(
        Variable = var, Type = prefix, NA_Rate = na_rate, Eligible = FALSE,
        NA_Before = sum(is.na(df[[var]][all_rows])), NA_After = sum(is.na(df[[var]][all_rows])),
        n_imputed = 0L, Method = "None", Predictor_Masked = FALSE, stringsAsFactors = FALSE
      ))
      next
    }
    
    na_mask   <- is.na(df[[var]]) & df[[type_col]] == prefix
    n_na      <- sum(na_mask)
    if (n_na == 0L) {
      imputation_log <- rbind(imputation_log, data.frame(
        Variable = var, Type = prefix, NA_Rate = na_rate, Eligible = TRUE,
        NA_Before = 0L, NA_After = 0L, n_imputed = 0L, Method = METHOD_ID,
        Predictor_Masked = FALSE, stringsAsFactors = FALSE
      ))
      next
    }
    
    na_before  <- sum(is.na(df[[var]][all_rows]))
    group_vals <- df[[var]][all_rows][!is.na(df[[var]][all_rows])]
    
    sampled_vals <- sample_empirical_levels(group_vals, n = n_na, seed = seed, smoothing = smoothing)
    df_out[[var]][na_mask] <- sampled_vals
    
    na_after <- sum(is.na(df_out[[var]][all_rows]))
    if (verbose) message("[", METHOD_ID, "] Imputed ", n_na, " cells for ", var,
                         " (type ", prefix, "; NA_rate=", format(na_rate, nsmall = DECIMALS),
                         ") using proportional sampling")
    
    imputation_log <- rbind(imputation_log, data.frame(
      Variable = var, Type = prefix, NA_Rate = na_rate, Eligible = TRUE,
      NA_Before = na_before, NA_After = na_after,
      n_imputed = n_na, Method = METHOD_ID,
      Predictor_Masked = FALSE, stringsAsFactors = FALSE
    ))
  }
  
  assign("imputation_log", imputation_log, envir = .GlobalEnv)
  df_out
}

# -----------------------------
# CNV_knn (groupwise by 'type')
# -----------------------------

impute_knn_groupwise_by_type <- function(df, type_col = "type", k = 5, verbose = FALSE) {
  METHOD_ID   <- "CNV_knn"
  df_out      <- df
  target_vars <- .select_cnv_targets(df)
  
  imputation_log <- data.frame(
    Variable        = character(),
    Type            = character(),
    NA_Rate         = numeric(),
    Eligible        = logical(),
    NA_Before       = integer(),
    NA_After        = integer(),
    n_imputed       = integer(),
    Method          = character(),
    Predictor_Masked= logical(),
    stringsAsFactors = FALSE
  )
  
  for (var in target_vars) {
    prefix     <- strsplit(var, "-")[[1]][1]
    group_rows <- which(df[[type_col]] == prefix)
    if (length(group_rows) == 0L) next
    
    # Eligibility for TARGET
    eligible <- should_impute_target(df[[var]], df[[type_col]], target_type = prefix, thr = THRESHOLD)
    rates    <- na_rate_by_var_type(df[[var]], df[[type_col]], digits = DECIMALS)
    na_rate  <- as.numeric(rates[[as.character(prefix)]])
    if (!eligible) {
      if (verbose) message("[", METHOD_ID, "] Pass-through (ineligible target, NA_rate=",
                           format(na_rate, nsmall = DECIMALS), ") for ", var, " in type ", prefix)
      imputation_log <- rbind(imputation_log, data.frame(
        Variable = var, Type = prefix, NA_Rate = na_rate, Eligible = FALSE,
        NA_Before = sum(is.na(df[[var]][group_rows])), NA_After = sum(is.na(df[[var]][group_rows])),
        n_imputed = 0L, Method = "None", Predictor_Masked = FALSE, stringsAsFactors = FALSE
      ))
      next
    }
    
    # Predictor set: eligible CNV predictors in same type (exclude target)
    cnv_cols <- .select_cnv_targets(df)
    cnv_cols <- setdiff(cnv_cols, var)
    eligible_preds <- vapply(cnv_cols, function(cv) {
      should_impute_target(df[[cv]], df[[type_col]], target_type = prefix, thr = THRESHOLD)
    }, logical(1))
    pred_cols <- cnv_cols[eligible_preds]
    pred_masked_flag <- length(pred_cols) > 0L
    
    # kNN feasibility checks
    if (length(pred_cols) == 0L || length(group_rows) <= k) {
      if (verbose) message("[", METHOD_ID, "] Skipped (no feasible predictor set / rows) for ",
                           var, " in type ", prefix, " → left untouched")
      imputation_log <- rbind(imputation_log, data.frame(
        Variable = var, Type = prefix, NA_Rate = na_rate, Eligible = TRUE,
        NA_Before = sum(is.na(df[[var]][group_rows])), NA_After = sum(is.na(df[[var]][group_rows])),
        n_imputed = 0L, Method = "None", Predictor_Masked = pred_masked_flag,
        stringsAsFactors = FALSE
      ))
      next
    }
    
    df_sub <- df[group_rows, c(var, pred_cols), drop = FALSE]
    if (!is.factor(df_sub[[var]])) df_sub[[var]] <- as.factor(df_sub[[var]])
    
    original_na <- is.na(df_sub[[var]])
    if (!any(original_na)) {
      imputation_log <- rbind(imputation_log, data.frame(
        Variable = var, Type = prefix, NA_Rate = na_rate, Eligible = TRUE,
        NA_Before = 0L, NA_After = 0L, n_imputed = 0L, Method = "None",
        Predictor_Masked = pred_masked_flag, stringsAsFactors = FALSE
      ))
      next
    }
    na_before <- sum(original_na)
    
    # Restrict imputation to TARGET column
    df_knn <- tryCatch(
      suppressWarnings(VIM::kNN(df_sub, variable = var, k = k, imp_var = FALSE)),
      error = function(e) NULL
    )
    if (is.null(df_knn)) {
      if (verbose) message("[", METHOD_ID, "] Error during kNN for ", var, " in type ", prefix, " → left untouched")
      imputation_log <- rbind(imputation_log, data.frame(
        Variable = var, Type = prefix, NA_Rate = na_rate, Eligible = TRUE,
        NA_Before = na_before, NA_After = na_before, n_imputed = 0L, Method = "None",
        Predictor_Masked = pred_masked_flag, stringsAsFactors = FALSE
      ))
      next
    }
    
    imputed_mask <- original_na & !is.na(df_knn[[var]])
    n_imp <- sum(imputed_mask)
    if (n_imp > 0L) {
      df_out[group_rows, var] <- df_knn[[var]]
      na_after <- sum(is.na(df_out[group_rows, var]))
      
      if (verbose) message("[", METHOD_ID, "] Imputed ", n_imp, " values for ", var,
                           " in type ", prefix, " (k=", k, "; NA_rate=", format(na_rate, nsmall = DECIMALS), ")")
      
      imputation_log <- rbind(imputation_log, data.frame(
        Variable = var, Type = prefix, NA_Rate = na_rate, Eligible = TRUE,
        NA_Before = na_before, NA_After = na_after, n_imputed = n_imp, Method = METHOD_ID,
        Predictor_Masked = pred_masked_flag, stringsAsFactors = FALSE
      ))
    } else {
      if (verbose) message("[", METHOD_ID, "] No imputations performed for ", var, " in type ", prefix, " → left untouched")
      imputation_log <- rbind(imputation_log, data.frame(
        Variable = var, Type = prefix, NA_Rate = na_rate, Eligible = TRUE,
        NA_Before = na_before, NA_After = na_before, n_imputed = 0L, Method = "None",
        Predictor_Masked = pred_masked_flag, stringsAsFactors = FALSE
      ))
    }
  }
  
  assign("imputation_log", imputation_log, envir = .GlobalEnv)
  df_out
}

# ===========================================
# End-to-end execution and output registration
# ===========================================

# Initialize global output name table if absent
if (!exists("output_name_table_all")) {
  output_name_table_all <- data.frame(
    Step = character(),
    Input_File = character(),
    Output_Object = character(),
    Saved_As = character(),
    stringsAsFactors = FALSE
  )
}

# -------------------------
# CNV_mode → df009–df011
# -------------------------
input_files_mode  <- c("df006.rds", "df007.rds", "df008.rds")
output_names_mode <- c("df009", "df010", "df011")

for (i in seq_along(input_files_mode)) {
  input_df   <- import(input_files_mode[i])
  imputed_df <- impute_mode_groupwise_by_type(input_df, verbose = TRUE)
  
  assign(output_names_mode[i], imputed_df)
  export(imputed_df, paste0(output_names_mode[i], ".rds"))
  
  output_name_table_all <- rbind(output_name_table_all, data.frame(
    Step = "CNV_mode",
    Input_File = input_files_mode[i],
    Output_Object = output_names_mode[i],
    Saved_As = paste0(output_names_mode[i], ".rds"),
    stringsAsFactors = FALSE
  ))
  
  rm(list = output_names_mode[i]); rm(imputed_df, input_df); gc()
}

# ----------------------------
# CNV_random → df012–df014
# ----------------------------
for (i in 6:8) {
  input_file <- sprintf("df%03d.rds", i)   # df006.rds, df007.rds, df008.rds
  df <- import(input_file)
  cat("✅ Loaded:", input_file, "\n")
  
  df_out <- impute_random_groupwise_by_type(df, seed = 123, smoothing = 0L, verbose = TRUE)
  
  output_index <- i + 6  # 12,13,14
  output_name  <- sprintf("df%03d", output_index)
  assign(output_name, df_out)
  saveRDS(df_out, sprintf("%s.rds", output_name))
  
  output_name_table_all <- rbind(output_name_table_all, data.frame(
    Step = "CNV_random",
    Input_File = input_file,
    Output_Object = output_name,
    Saved_As = sprintf("%s.rds", output_name),
    stringsAsFactors = FALSE
  ))
  
  rm(list = c("df", output_name, "df_out", "imputation_log")); gc()
}

# -------------------------
# CNV_knn → df015–df017
# -------------------------
for (i in 6:8) {
  input_file <- sprintf("df%03d.rds", i)
  df <- import(input_file)
  cat("✅ Loaded:", input_file, "\n")
  
  df_out <- impute_knn_groupwise_by_type(df, k = 5, verbose = TRUE)
  
  output_index <- i + 9  # 15,16,17
  output_name  <- sprintf("df%03d", output_index)
  assign(output_name, df_out)
  saveRDS(df_out, sprintf("%s.rds", output_name))
  
  output_name_table_all <- rbind(output_name_table_all, data.frame(
    Step = "CNV_knn",
    Input_File = input_file,
    Output_Object = output_name,
    Saved_As = sprintf("%s.rds", output_name),
    stringsAsFactors = FALSE
  ))
  
  rm(list = c("df", output_name, "df_out", "imputation_log")); gc()
}

# Persist/update the unified mapping table once at the end (idempotent)
write.table(
  output_name_table_all,
  file = "output_name_table_all.tsv",
  sep = "\t",
  row.names = FALSE,
  col.names = TRUE,
  quote = FALSE
)

print(output_name_table_all)

##### Expected CNV outputs created:
##### • CNV_mode:   df009, df010, df011
##### • CNV_random: df012, df013, df014
##### • CNV_knn:    df015, df016, df017


##### ===============================================================
##### Batch Validation — CNV Imputation (df006→df009–df017)
##### ===============================================================

suppressPackageStartupMessages({
  library(rio)
})

# ---- Configuration (match pipeline) ----
DECIMALS  <- 4
THRESHOLD <- 0.35

# ---- Helpers (same ontology as pipeline) ----
.select_cnv_targets <- function(df) {
  names(df)[sapply(strsplit(names(df), "\\."), function(x) length(x) >= 2 && x[2] == "3")]
}

na_rate_by_var_type_raw <- function(x, type_vec) {
  stopifnot(length(x) == length(type_vec))
  tapply(is.na(x), type_vec, function(z) mean(z), simplify = TRUE)
}

should_impute_target_raw <- function(x, type_vec, target_type, thr = THRESHOLD) {
  raw_rate <- na_rate_by_var_type_raw(x, type_vec)
  rate     <- raw_rate[[as.character(target_type)]]
  isTRUE(!is.na(rate) && rate <= thr)
}

# Token strictness: 2nd token must be exactly "3"
validate_token_strict <- function(vars) {
  if (length(vars) == 0L) return(FALSE)
  all(sapply(strsplit(vars, "\\."), function(tok) length(tok) >= 2 && identical(tok[2], "3")))
}

# Groupwise confinement: any NA→non-NA change must lie within the expected 'type'
check_groupwise_confinement <- function(df_in, df_out, cnv_vars) {
  for (var in cnv_vars) {
    changed_idx <- which(is.na(df_in[[var]]) & !is.na(df_out[[var]]))
    if (!length(changed_idx)) next
    expected_type <- sub("-.*$", "", var)
    if (!all(df_out$type[changed_idx] == expected_type)) return(FALSE)
  }
  TRUE
}

# Non-NA stability: existing non-NA entries must not be altered
check_nonNA_stability <- function(df_in, df_out, cnv_vars) {
  for (var in cnv_vars) {
    idx <- which(!is.na(df_in[[var]]))
    if (!length(idx)) next
    if (!identical(df_in[[var]][idx], df_out[[var]][idx])) return(FALSE)
  }
  TRUE
}

# Any eligible imputation occurred? (NA→non-NA)
find_any_imputed_example <- function(df_in, df_out, cnv_vars) {
  for (var in cnv_vars) {
    idx <- which(is.na(df_in[[var]]) & !is.na(df_out[[var]]))
    if (length(idx) > 0L) return(TRUE)
  }
  FALSE
}

# One example of ineligible (> THRESHOLD) left untouched
find_one_ineligible_untouched <- function(df_in, df_out, cnv_vars, threshold = THRESHOLD) {
  for (var in cnv_vars) {
    for (tp in unique(df_in$type)) {
      mask <- df_in$type == tp
      rate <- mean(is.na(df_in[mask, var]))
      if (!is.na(rate) && rate > threshold) {
        before <- df_in[mask, var]
        after  <- df_out[mask, var]
        if (identical(before, after)) {
          return(list(found = TRUE, var = var, type = tp, rate = rate))
        }
      }
    }
  }
  list(found = FALSE)
}

# ---- Expected mapping (per design) ----
plan <- list(
  CNV_mode   = list(inputs = c(6,7,8), outputs = c(9,10,11)),
  CNV_random = list(inputs = c(6,7,8), outputs = c(12,13,14)),
  CNV_knn    = list(inputs = c(6,7,8), outputs = c(15,16,17))
)

# ---- Accumulators for summaries ----
summary_rows  <- list()
detail_issues <- list()

# ---- Batch over all 3×3 outputs ----
row_i <- 0L
for (method in names(plan)) {
  ins  <- plan[[method]]$inputs
  outs <- plan[[method]]$outputs
  
  for (j in seq_along(ins)) {
    in_id  <- ins[j]
    out_id <- outs[j]
    
    in_file  <- sprintf("df%03d.rds", in_id)
    out_file <- sprintf("df%03d.rds", out_id)
    
    if (!file.exists(in_file) || !file.exists(out_file)) {
      detail_issues[[length(detail_issues) + 1L]] <-
        sprintf("Missing file(s): %s or %s", in_file, out_file)
      next
    }
    
    df_in  <- import(in_file)
    df_out <- import(out_file)
    
    # (A) Global NA counts
    na_in  <- sum(is.na(df_in))
    na_out <- sum(is.na(df_out))
    
    # (B) Dimension invariance
    rows_ok <- nrow(df_in) == nrow(df_out)
    cols_ok <- ncol(df_in) == ncol(df_out)
    
    # (C) CNV token strictness
    cnv_vars <- .select_cnv_targets(df_in)
    token_ok <- validate_token_strict(cnv_vars)
    
    # (D) Groupwise confinement (prefix-matched type only)
    group_ok <- check_groupwise_confinement(df_in, df_out, cnv_vars)
    
    # (E) Non-NA stability (no overwriting of existing values)
    stable_ok <- check_nonNA_stability(df_in, df_out, cnv_vars)
    
    # (F) Ineligible untouched example
    ineligible_chk <- find_one_ineligible_untouched(df_in, df_out, cnv_vars, threshold = THRESHOLD)
    
    # (G) Any eligible imputation example occurred?
    any_imp <- find_any_imputed_example(df_in, df_out, cnv_vars)
    
    # Record summary row
    row_i <- row_i + 1L
    summary_rows[[row_i]] <- data.frame(
      Method                 = method,
      Input                  = sprintf("df%03d", in_id),
      Output                 = sprintf("df%03d", out_id),
      NA_in                  = na_in,
      NA_out                 = na_out,
      Rows_OK                = rows_ok,
      Cols_OK                = cols_ok,
      Token_OK               = token_ok,
      Groupwise_OK           = group_ok,
      NonNA_Stable           = stable_ok,
      Any_Imputed            = any_imp,
      Inelig_Example_Found   = ineligible_chk$found,
      Inelig_Var             = if (ineligible_chk$found) ineligible_chk$var  else NA_character_,
      Inelig_Type            = if (ineligible_chk$found) ineligible_chk$type else NA_character_,
      Inelig_Rate            = if (ineligible_chk$found) round(ineligible_chk$rate, DECIMALS) else NA_real_,
      stringsAsFactors = FALSE
    )
  }
}

cnv_validation_summary <- do.call(rbind, summary_rows)

# ---- Print and optionally export
print(cnv_validation_summary)

# Optional: write TSV for records/audit
try({
  write.table(cnv_validation_summary, file = "cnv_validation_summary.tsv",
              sep = "\t", row.names = FALSE, col.names = TRUE, quote = FALSE)
}, silent = TRUE)

# Report any structural issues encountered (e.g., missing files)
if (length(detail_issues)) {
  cat("\n--- Issues ---\n")
  cat(paste0("* ", unlist(detail_issues), collapse = "\n"), "\n")
} else {
  cat("\nNo structural issues detected.\n")
}


#####
#####
#####
#####
#####
#####
## ============================
## CNV storage audit: df009–df017
## ============================

suppressPackageStartupMessages({
  library(rio)
  library(dplyr)
  library(stringr)
  library(tibble)
})

# Strict CNV selector: second dot-delimited token equals "3"
select_cnv_vars <- function(df) {
  nms <- names(df)
  keep <- vapply(strsplit(nms, "\\."), function(tok) length(tok) >= 2 && tok[2] == "3", logical(1))
  nms[keep]
}

# Classify how a CNV column is stored
classify_cnv_storage <- function(x) {
  cls <- class(x)[1]
  if (is.factor(x)) {
    levs <- levels(x)
    # canonical textual levels present?
    has_text_levels <- all(c("Deleted","Normal","Duplicated") %in% levs)
    return(list(
      storage_class = paste0("factor(", length(levs), " levels)"),
      encoding = if (has_text_levels) "categorical_text_levels" else "factor_other_levels",
      levels = paste(levs, collapse = "|")
    ))
  }
  if (is.character(x)) {
    uniq <- unique(na.omit(x))
    has_text_values <- all(c("Deleted","Normal","Duplicated") %in% uniq)
    return(list(
      storage_class = "character",
      encoding = if (has_text_values) "categorical_text_values" else "character_other_values",
      levels = paste(utils::head(sort(uniq), 20), collapse = "|")
    ))
  }
  if (is.numeric(x) || is.integer(x)) {
    uniq_num <- sort(unique(na.omit(as.numeric(x))))
    # Check if subset of {1,2,3}
    subset_123 <- length(uniq_num) > 0 && all(uniq_num %in% c(1,2,3))
    return(list(
      storage_class = if (is.integer(x)) "integer" else "numeric",
      encoding = if (subset_123) "numeric_1_2_3" else "numeric_other",
      levels = paste(utils::head(uniq_num, 20), collapse = "|")
    ))
  }
  # Fallback
  uniq <- unique(na.omit(x))
  return(list(
    storage_class = paste(class(x), collapse = "+"),
    encoding = "other",
    levels = paste(utils::head(uniq, 20), collapse = "|")
  ))
}

audit_one_file <- function(path) {
  if (!file.exists(path)) return(NULL)
  df <- suppressWarnings(rio::import(path))
  cnv_vars <- select_cnv_vars(df)
  if (!length(cnv_vars)) {
    return(tibble(
      file = basename(path), variable = NA_character_, storage_class = NA_character_,
      encoding = "no_cnv_columns_detected", n_NA = NA_integer_, distinct_nonNA = NA_integer_,
      levels_or_values = NA_character_
    ))
  }
  rows <- lapply(cnv_vars, function(v) {
    info <- classify_cnv_storage(df[[v]])
    tibble(
      file = basename(path),
      variable = v,
      storage_class = info$storage_class,
      encoding = info$encoding,
      n_NA = sum(is.na(df[[v]])),
      distinct_nonNA = dplyr::n_distinct(df[[v]][!is.na(df[[v]])]),
      levels_or_values = info$levels
    )
  })
  bind_rows(rows)
}

files <- sprintf("df%03d.rds", 9:17)
res_list <- lapply(files, audit_one_file)
audit_tbl <- bind_rows(res_list)

# Print a compact summary
print(
  audit_tbl %>%
    arrange(file, variable) %>%
    select(file, variable, storage_class, encoding, n_NA, distinct_nonNA)
)

# Flag any suspicious numeric-encoded CNV columns
suspicious_numeric <- audit_tbl %>%
  filter(encoding %in% c("numeric_1_2_3", "numeric_other"))

cat("\n=== Summary by file (any CNV numeric?) ===\n")
print(
  audit_tbl %>%
    mutate(is_numeric_cnv = encoding %in% c("numeric_1_2_3","numeric_other")) %>%
    group_by(file) %>%
    summarize(any_numeric_cnv = any(is_numeric_cnv), .groups = "drop")
)

# Optional: export full audit
try({
  write.table(audit_tbl, file = "cnv_storage_audit_df009_df017.tsv",
              sep = "\t", row.names = FALSE, col.names = TRUE, quote = FALSE)
  message("\nWrote: cnv_storage_audit_df009_df017.tsv")
}, silent = TRUE)

# If anything looks coerced, show the first few offenders
if (nrow(suspicious_numeric)) {
  cat("\n>>> Potentially coerced CNV columns (numeric):\n")
  print(suspicious_numeric %>% arrange(file, variable) %>% head(50))
} else {
  cat("\nNo numeric-encoded CNV columns detected.\n")
}


##### ======================================================================
##### MODULE MUTATION LAYER IMPUTATION— Groupwise Binary Imputation (Mean, Median, Mode, Bernoulli)
##### Schema-preserving; Eligibility gate NA-rate ≤ 0.35; No fallbacks
##### Target selection: 2nd dot-delimited token == "2" (strict)
##### I/O mapping: df009–df017 → df018–df053 (9×4 = 36 outputs)
##### ======================================================================

suppressPackageStartupMessages({
  library(rio)
})

# -----------------------
# Global policy constants
# -----------------------
DECIMALS  <- 4
THRESHOLD <- 0.35

# -----------------------
# Lightweight shared utils
# -----------------------

# Strict selector: Mutation variables have second token == "2"
.select_mutation_targets <- function(df) {
  names(df)[sapply(strsplit(names(df), "\\."), function(x) length(x) >= 2 && x[2] == "2")]
}

# Per-(variable × type) NA-rate (raw), with optional rounded reporting
na_rate_by_var_type <- function(x, type_vec, digits = DECIMALS) {
  stopifnot(length(x) == length(type_vec))
  raw <- tapply(is.na(x), type_vec, function(z) mean(z), simplify = TRUE)
  # Return both for convenience
  list(raw = raw, rounded = round(raw, digits = digits))
}

# Gate: only impute when raw NA-rate ≤ THRESHOLD
should_impute_target <- function(x, type_vec, target_type, thr = THRESHOLD) {
  rates <- na_rate_by_var_type(x, type_vec)$raw
  rate  <- rates[[as.character(target_type)]]
  isTRUE(!is.na(rate) && rate <= thr)
}

# Coerce a target column to numeric {0,1,NA} if needed (no schema change)
.coerce_binary_numeric <- function(v) {
  if (is.numeric(v)) return(v)
  suppressWarnings(as.numeric(as.character(v)))
}

# Deterministic majority for binary {0,1}; tie-break fixed to 0 < 1
deterministic_majority_binary <- function(v) {
  v <- v[!is.na(v)]
  if (!length(v)) return(NA_real_)
  tab  <- table(v)
  maxc <- max(tab)
  tied <- as.numeric(names(tab)[tab == maxc])
  # tie-breaker: pick 0 if tied (0 < 1)
  if (length(tied) > 1) return(0)
  tied[1]
}

# Bernoulli sampler with fixed seed; returns 0/1 numeric
sample_bernoulli <- function(n, p, seed = NULL) {
  if (!is.null(seed)) set.seed(seed)
  as.numeric(runif(n) < p)  # 1 with prob p, else 0
}

# -----------------------
# Method 1: MEAN (binary)
# -----------------------
impute_mutation_mean_groupwise_binary <- function(df, type_col = "type", verbose = FALSE) {
  METHOD_ID  <- "Mutation_Mean"
  df_out     <- df
  targets    <- .select_mutation_targets(df)
  
  imputation_log <- data.frame(
    Variable = character(), Type = character(), NA_Before = integer(),
    NA_After = integer(), NA_Rate = numeric(), n_imputed = integer(),
    Method = character(), stringsAsFactors = FALSE
  )
  
  for (var in targets) {
    prefix <- strsplit(var, "-")[[1]][1]  # group key encoded upstream
    all_rows <- which(df[[type_col]] == prefix)
    if (length(all_rows) == 0L) next
    
    # Eligibility gate
    if (!should_impute_target(df[[var]], df[[type_col]], target_type = prefix, thr = THRESHOLD)) {
      if (verbose) message("[", METHOD_ID, "] Pass-through (ineligible) for ", var, " @type=", prefix)
      next
    }
    
    # Prepare values
    x <- .coerce_binary_numeric(df[[var]])
    na_rows   <- all_rows[is.na(x[all_rows])]
    if (!length(na_rows)) next
    
    obs_vals  <- x[all_rows][!is.na(x[all_rows])]
    if (!length(obs_vals)) {
      # No fallback by policy; leave untouched
      if (verbose) message("[", METHOD_ID, "] No observed values for ", var, " @type=", prefix, " → untouched")
      next
    }
    
    # Bernoulli mean prevalence
    p_hat <- mean(obs_vals) # in [0,1]
    # Mean (rounded) with deterministic tie-break at 0.5 → 0
    binary_val <- if (p_hat > 0.5) 1 else if (p_hat < 0.5) 0 else 0
    
    na_before <- sum(is.na(x[all_rows]))
    df_out[[var]][na_rows] <- binary_val
    na_after  <- sum(is.na(df_out[[var]][all_rows]))
    rate_now  <- round(na_rate_by_var_type(df[[var]], df[[type_col]])$raw[[as.character(prefix)]], DECIMALS)
    
    if (verbose) {
      message("[", METHOD_ID, "] ", var, " @type=", prefix,
              " | p̂=", signif(p_hat, 4), " → ", binary_val,
              " | imputed=", length(na_rows))
    }
    
    imputation_log <- rbind(imputation_log, data.frame(
      Variable = var, Type = prefix, NA_Before = na_before, NA_After = na_after,
      NA_Rate = rate_now, n_imputed = length(na_rows), Method = METHOD_ID,
      stringsAsFactors = FALSE
    ))
  }
  
  assign("imputation_log", imputation_log, envir = .GlobalEnv)
  df_out
}

# -------------------------
# Method 2: MEDIAN (binary)
# -------------------------
impute_mutation_median_groupwise_binary <- function(df, type_col = "type", verbose = FALSE) {
  METHOD_ID  <- "Mutation_Median"
  df_out     <- df
  targets    <- .select_mutation_targets(df)
  
  imputation_log <- data.frame(
    Variable = character(), Type = character(), NA_Before = integer(),
    NA_After = integer(), NA_Rate = numeric(), n_imputed = integer(),
    Method = character(), stringsAsFactors = FALSE
  )
  
  for (var in targets) {
    prefix <- strsplit(var, "-")[[1]][1]
    all_rows <- which(df[[type_col]] == prefix)
    if (length(all_rows) == 0L) next
    
    if (!should_impute_target(df[[var]], df[[type_col]], target_type = prefix, thr = THRESHOLD)) {
      if (verbose) message("[", METHOD_ID, "] Pass-through (ineligible) for ", var, " @type=", prefix)
      next
    }
    
    x <- .coerce_binary_numeric(df[[var]])
    na_rows  <- all_rows[is.na(x[all_rows])]
    if (!length(na_rows)) next
    
    obs_vals <- x[all_rows][!is.na(x[all_rows])]
    if (!length(obs_vals)) {
      if (verbose) message("[", METHOD_ID, "] No observed values for ", var, " @type=", prefix, " → untouched")
      next
    }
    
    med_val   <- stats::median(obs_vals)  # numeric 0/1 or .5 (rare if non-binary creep)
    binary_val <- if (med_val > 0.5) 1 else if (med_val < 0.5) 0 else 0
    
    na_before <- sum(is.na(x[all_rows]))
    df_out[[var]][na_rows] <- binary_val
    na_after  <- sum(is.na(df_out[[var]][all_rows]))
    rate_now  <- round(na_rate_by_var_type(df[[var]], df[[type_col]])$raw[[as.character(prefix)]], DECIMALS)
    
    if (verbose) {
      message("[", METHOD_ID, "] ", var, " @type=", prefix,
              " | median=", signif(med_val, 4), " → ", binary_val,
              " | imputed=", length(na_rows))
    }
    
    imputation_log <- rbind(imputation_log, data.frame(
      Variable = var, Type = prefix, NA_Before = na_before, NA_After = na_after,
      NA_Rate = rate_now, n_imputed = length(na_rows), Method = METHOD_ID,
      stringsAsFactors = FALSE
    ))
  }
  
  assign("imputation_log", imputation_log, envir = .GlobalEnv)
  df_out
}

# -----------------------
# Method 3: MODE (binary)
# -----------------------
impute_mutation_mode_groupwise_binary <- function(df, type_col = "type", verbose = FALSE) {
  METHOD_ID  <- "Mutation_Mode"
  df_out     <- df
  targets    <- .select_mutation_targets(df)
  
  imputation_log <- data.frame(
    Variable = character(), Type = character(), NA_Before = integer(),
    NA_After = integer(), NA_Rate = numeric(), n_imputed = integer(),
    Method = character(), stringsAsFactors = FALSE
  )
  
  for (var in targets) {
    prefix <- strsplit(var, "-")[[1]][1]
    all_rows <- which(df[[type_col]] == prefix)
    if (length(all_rows) == 0L) next
    
    if (!should_impute_target(df[[var]], df[[type_col]], target_type = prefix, thr = THRESHOLD)) {
      if (verbose) message("[", METHOD_ID, "] Pass-through (ineligible) for ", var, " @type=", prefix)
      next
    }
    
    x <- .coerce_binary_numeric(df[[var]])
    na_rows  <- all_rows[is.na(x[all_rows])]
    if (!length(na_rows)) next
    
    obs_vals <- x[all_rows][!is.na(x[all_rows])]
    if (!length(obs_vals)) {
      if (verbose) message("[", METHOD_ID, "] No observed values for ", var, " @type=", prefix, " → untouched")
      next
    }
    
    m <- deterministic_majority_binary(obs_vals)
    
    na_before <- sum(is.na(x[all_rows]))
    df_out[[var]][na_rows] <- m
    na_after  <- sum(is.na(df_out[[var]][all_rows]))
    rate_now  <- round(na_rate_by_var_type(df[[var]], df[[type_col]])$raw[[as.character(prefix)]], DECIMALS)
    
    if (verbose) {
      message("[", METHOD_ID, "] ", var, " @type=", prefix,
              " | mode=", m, " | imputed=", length(na_rows))
    }
    
    imputation_log <- rbind(imputation_log, data.frame(
      Variable = var, Type = prefix, NA_Before = na_before, NA_After = na_after,
      NA_Rate = rate_now, n_imputed = length(na_rows), Method = METHOD_ID,
      stringsAsFactors = FALSE
    ))
  }
  
  assign("imputation_log", imputation_log, envir = .GlobalEnv)
  df_out
}

# --------------------------------
# Method 4: BernoulliRandom (binary)
# --------------------------------
# Draws NA cells from Bernoulli(p̂), where p̂ is within-type prevalence of 1’s
# Deterministic via seed; no fallback if no observed values in the group
impute_mutation_bernoulli_groupwise_binary <- function(df, type_col = "type",
                                                       seed = 123, verbose = FALSE) {
  METHOD_ID  <- "Mutation_BernoulliRandom"
  df_out     <- df
  targets    <- .select_mutation_targets(df)
  
  imputation_log <- data.frame(
    Variable = character(), Type = character(), NA_Before = integer(),
    NA_After = integer(), NA_Rate = numeric(), n_imputed = integer(),
    Method = character(), stringsAsFactors = FALSE
  )
  
  for (var in targets) {
    prefix <- strsplit(var, "-")[[1]][1]
    all_rows <- which(df[[type_col]] == prefix)
    if (length(all_rows) == 0L) next
    
    if (!should_impute_target(df[[var]], df[[type_col]], target_type = prefix, thr = THRESHOLD)) {
      if (verbose) message("[", METHOD_ID, "] Pass-through (ineligible) for ", var, " @type=", prefix)
      next
    }
    
    x <- .coerce_binary_numeric(df[[var]])
    na_rows  <- all_rows[is.na(x[all_rows])]
    if (!length(na_rows)) next
    
    obs_vals <- x[all_rows][!is.na(x[all_rows])]
    if (!length(obs_vals)) {
      if (verbose) message("[", METHOD_ID, "] No observed values for ", var, " @type=", prefix, " → untouched")
      next
    }
    
    p_hat <- mean(obs_vals) # prevalence of 1’s
    draws <- sample_bernoulli(n = length(na_rows), p = p_hat, seed = seed)
    
    na_before <- sum(is.na(x[all_rows]))
    df_out[[var]][na_rows] <- draws
    na_after  <- sum(is.na(df_out[[var]][all_rows]))
    rate_now  <- round(na_rate_by_var_type(df[[var]], df[[type_col]])$raw[[as.character(prefix)]], DECIMALS)
    
    if (verbose) {
      message("[", METHOD_ID, "] ", var, " @type=", prefix,
              " | p̂=", signif(p_hat, 4), " | imputed=", length(na_rows))
    }
    
    imputation_log <- rbind(imputation_log, data.frame(
      Variable = var, Type = prefix, NA_Before = na_before, NA_After = na_after,
      NA_Rate = rate_now, n_imputed = length(na_rows), Method = METHOD_ID,
      stringsAsFactors = FALSE
    ))
  }
  
  assign("imputation_log", imputation_log, envir = .GlobalEnv)
  df_out
}

# ===========================================
# End-to-end execution and output registration
# Inputs: df009–df017
# Outputs:
#   Mean      → df018–df026
#   Median    → df027–df035
#   Mode      → df036–df044
#   Bernoulli → df045–df053
# ===========================================

if (!exists("output_name_table_all")) {
  output_name_table_all <- data.frame(
    Step = character(), Input_File = character(),
    Output_Object = character(), Saved_As = character(),
    stringsAsFactors = FALSE
  )
}

# ---- Mean → df018–df026
for (i in 9:17) {
  input_file <- sprintf("df%03d.rds", i)        # df009–df017
  df <- import(input_file)
  cat("✅ Loaded:", input_file, "\n")
  
  df_out <- impute_mutation_mean_groupwise_binary(df, verbose = TRUE)
  
  output_index <- i + 9                           # 9→18, …, 17→26
  output_name  <- sprintf("df%03d", output_index)
  assign(output_name, df_out)
  saveRDS(df_out, sprintf("%s.rds", output_name))
  
  output_name_table_all <- rbind(output_name_table_all, data.frame(
    Step = "Mutation_Mean", Input_File = input_file,
    Output_Object = output_name, Saved_As = sprintf("%s.rds", output_name),
    stringsAsFactors = FALSE
  ))
  
  rm(list = c("df", output_name, "df_out", "imputation_log")); gc()
}

# ---- Median → df027–df035
for (i in 9:17) {
  input_file <- sprintf("df%03d.rds", i)
  df <- import(input_file)
  cat("✅ Loaded:", input_file, "\n")
  
  df_out <- impute_mutation_median_groupwise_binary(df, verbose = TRUE)
  
  output_index <- i + 18                          # 9→27, …, 17→35
  output_name  <- sprintf("df%03d", output_index)
  assign(output_name, df_out)
  saveRDS(df_out, sprintf("%s.rds", output_name))
  
  output_name_table_all <- rbind(output_name_table_all, data.frame(
    Step = "Mutation_Median", Input_File = input_file,
    Output_Object = output_name, Saved_As = sprintf("%s.rds", output_name),
    stringsAsFactors = FALSE
  ))
  
  rm(list = c("df", output_name, "df_out", "imputation_log")); gc()
}

# ---- Mode → df036–df044
for (i in 9:17) {
  input_file <- sprintf("df%03d.rds", i)
  df <- import(input_file)
  cat("✅ Loaded:", input_file, "\n")
  
  df_out <- impute_mutation_mode_groupwise_binary(df, verbose = TRUE)
  
  output_index <- i + 27                          # 9→36, …, 17→44
  output_name  <- sprintf("df%03d", output_index)
  assign(output_name, df_out)
  saveRDS(df_out, sprintf("%s.rds", output_name))
  
  output_name_table_all <- rbind(output_name_table_all, data.frame(
    Step = "Mutation_Mode", Input_File = input_file,
    Output_Object = output_name, Saved_As = sprintf("%s.rds", output_name),
    stringsAsFactors = FALSE
  ))
  
  rm(list = c("df", output_name, "df_out", "imputation_log")); gc()
}

# ---- BernoulliRandom → df045–df053
for (i in 9:17) {
  input_file <- sprintf("df%03d.rds", i)
  df <- import(input_file)
  cat("✅ Loaded:", input_file, "\n")
  
  df_out <- impute_mutation_bernoulli_groupwise_binary(df, seed = 123, verbose = TRUE)
  
  output_index <- i + 36                          # 9→45, …, 17→53
  output_name  <- sprintf("df%03d", output_index)
  assign(output_name, df_out)
  saveRDS(df_out, sprintf("%s.rds", output_name))
  
  output_name_table_all <- rbind(output_name_table_all, data.frame(
    Step = "Mutation_BernoulliRandom", Input_File = input_file,
    Output_Object = output_name, Saved_As = sprintf("%s.rds", output_name),
    stringsAsFactors = FALSE
  ))
  
  rm(list = c("df", output_name, "df_out", "imputation_log")); gc()
}

# Persist/refresh the unified registry once at the end (idempotent safe)
write.table(
  output_name_table_all,
  file = "output_name_table_all.tsv",
  sep = "\t", row.names = FALSE, col.names = TRUE, quote = FALSE
)

print(output_name_table_all)
##### ======================================================================

##### ===============================================================
##### Batch Validation — Mutation Imputation (df009→df018–df053)
##### Mirrors CNV validator; four methods: Mean, Median, Mode, Bernoulli
##### ===============================================================

suppressPackageStartupMessages({
  library(rio)
})

# ---- Configuration (match pipeline) ----
DECIMALS  <- 4
THRESHOLD <- 0.35

# ---- Helpers (consistent with Mutation pipeline) ----
.select_mutation_targets <- function(df) {
  names(df)[sapply(strsplit(names(df), "\\."), function(x) length(x) >= 2 && x[2] == "2")]
}

na_rate_by_var_type_raw <- function(x, type_vec) {
  stopifnot(length(x) == length(type_vec))
  tapply(is.na(x), type_vec, function(z) mean(z), simplify = TRUE)
}

should_impute_target_raw <- function(x, type_vec, target_type, thr = THRESHOLD) {
  raw_rate <- na_rate_by_var_type_raw(x, type_vec)
  rate     <- raw_rate[[as.character(target_type)]]
  isTRUE(!is.na(rate) && rate <= thr)
}

# Strict token == "2" for all selected mutation columns
validate_token_strict <- function(vars) {
  if (length(vars) == 0L) return(FALSE)
  all(sapply(strsplit(vars, "\\."), function(tok) length(tok) >= 2 && identical(tok[2], "2")))
}

# Changes must be confined to the correct type (prefix before the first '-')
check_groupwise_confinement <- function(df_in, df_out, mut_vars) {
  for (var in mut_vars) {
    changed_idx <- which(is.na(df_in[[var]]) & !is.na(df_out[[var]]))
    if (!length(changed_idx)) next
    expected_type <- sub("-.*$", "", var)
    if (!all(df_out$type[changed_idx] == expected_type)) return(FALSE)
  }
  TRUE
}

# Ensure non-NA values in input were NOT overwritten in output (only NA→non-NA allowed)
check_nonNA_stability <- function(df_in, df_out, mut_vars) {
  for (var in mut_vars) {
    idx <- which(!is.na(df_in[[var]]))
    if (!length(idx)) next
    if (!identical(df_in[[var]][idx], df_out[[var]][idx])) return(FALSE)
  }
  TRUE
}

# Any imputation occurred? (NA→non-NA for at least one mutation variable)
find_any_imputed_example <- function(df_in, df_out, mut_vars) {
  for (var in mut_vars) {
    idx <- which(is.na(df_in[[var]]) & !is.na(df_out[[var]]))
    if (length(idx) > 0L) return(TRUE)
  }
  FALSE
}

# Find one ineligible (> THRESHOLD) case that remained unchanged
find_one_ineligible_untouched <- function(df_in, df_out, mut_vars, threshold = THRESHOLD) {
  for (var in mut_vars) {
    for (tp in unique(df_in$type)) {
      mask <- df_in$type == tp
      rate <- mean(is.na(df_in[mask, var]))
      if (!is.na(rate) && rate > threshold) {
        before <- df_in[mask, var]
        after  <- df_out[mask, var]
        if (identical(before, after)) {
          return(list(found = TRUE, var = var, type = tp, rate = rate))
        }
      }
    }
  }
  list(found = FALSE)
}

# ---- Expected mapping (per design) ----
plan <- list(
  Mutation_Mean            = list(inputs = 9:17,  outputs = 18:26),
  Mutation_Median          = list(inputs = 9:17,  outputs = 27:35),
  Mutation_Mode            = list(inputs = 9:17,  outputs = 36:44),
  Mutation_BernoulliRandom = list(inputs = 9:17,  outputs = 45:53)
)

# ---- Accumulators ----
summary_rows  <- list()
detail_issues <- list()
row_i <- 0L

# ---- Batch over all 4×9 outputs ----
for (method in names(plan)) {
  ins  <- plan[[method]]$inputs
  outs <- plan[[method]]$outputs
  
  for (k in seq_along(ins)) {
    in_id  <- ins[k]
    out_id <- outs[k]
    
    in_file  <- sprintf("df%03d.rds", in_id)
    out_file <- sprintf("df%03d.rds", out_id)
    
    if (!file.exists(in_file) || !file.exists(out_file)) {
      detail_issues[[length(detail_issues) + 1L]] <-
        sprintf("Missing file(s): %s or %s", in_file, out_file)
      next
    }
    
    df_in  <- import(in_file)
    df_out <- import(out_file)
    
    # Targets and token strictness
    mut_vars <- .select_mutation_targets(df_in)
    token_ok <- validate_token_strict(mut_vars)
    
    # (1) NA counts (global)
    na_in  <- sum(is.na(df_in))
    na_out <- sum(is.na(df_out))
    
    # (2) Schema invariance
    rows_ok <- nrow(df_in) == nrow(df_out)
    cols_ok <- ncol(df_in) == ncol(df_out)
    
    # (3) Groupwise confinement (correct type only)
    group_ok <- check_groupwise_confinement(df_in, df_out, mut_vars)
    
    # (4) Non-NA stability (no overwriting)
    stable_ok <- check_nonNA_stability(df_in, df_out, mut_vars)
    
    # (5) Ineligible untouched example
    ineligible_chk <- find_one_ineligible_untouched(df_in, df_out, mut_vars, threshold = THRESHOLD)
    
    # (6) Any eligible imputation?
    any_imp <- find_any_imputed_example(df_in, df_out, mut_vars)
    
    # Record summary
    row_i <- row_i + 1L
    summary_rows[[row_i]] <- data.frame(
      Method        = method,
      Input         = sprintf("df%03d", in_id),
      Output        = sprintf("df%03d", out_id),
      NA_in         = na_in,
      NA_out        = na_out,
      Rows_OK       = rows_ok,
      Cols_OK       = cols_ok,
      Token_OK      = token_ok,
      Groupwise_OK  = group_ok,
      NonNA_Stable  = stable_ok,
      Any_Imputed   = any_imp,
      Inelig_Example_Found = ineligible_chk$found,
      Inelig_Var    = if (ineligible_chk$found) ineligible_chk$var  else NA_character_,
      Inelig_Type   = if (ineligible_chk$found) ineligible_chk$type else NA_character_,
      Inelig_Rate   = if (ineligible_chk$found) round(ineligible_chk$rate, DECIMALS) else NA_real_,
      stringsAsFactors = FALSE
    )
  }
}

mutation_validation_summary <- do.call(rbind, summary_rows)

# ---- Print and optionally export
print(mutation_validation_summary)

# Optional: write TSV for audit trail
try({
  write.table(mutation_validation_summary, file = "mutation_validation_summary.tsv",
              sep = "\t", row.names = FALSE, col.names = TRUE, quote = FALSE)
}, silent = TRUE)

# Report any structural issues encountered (e.g., missing files)
if (length(detail_issues)) {
  cat("\n--- Issues ---\n")
  cat(paste0("* ", unlist(detail_issues), collapse = "\n"), "\n")
} else {
  cat("\nNo structural issues detected.\n")
}

# Sanity checks
nrow(mutation_validation_summary)            # expected: 36 (4 methods × 9 inputs)
table(mutation_validation_summary$Method)    # each should show 9




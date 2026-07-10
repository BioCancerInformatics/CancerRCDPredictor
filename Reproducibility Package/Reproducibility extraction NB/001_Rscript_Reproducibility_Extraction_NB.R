###############################################################################
# R2.8c — Reproducibility Extraction (Notebook — Phase I/II)
# ============================================================================
# Run from: D:\Pré-artigo 5-optosis model\Machine learning CancerRCDShiny_prediction
# Extracts:
#   1. sessionInfo() → sessionInfo_NB.txt
#   2. Preprocessing parameters → preprocessing_params.txt
#   3. Phase I/II seed documentation → random_seeds_NB.txt
###############################################################################

setwd("d:/Pré-artigo 5-optosis model/Machine learning CancerRCDShiny_prediction")
OUT_DIR <- "R1/Reproducibility extraction NB"
if(!dir.exists(OUT_DIR)) dir.create(OUT_DIR, recursive = TRUE)

# ── 1. sessionInfo() ────────────────────────────────────────────────────────
cat("=== 1. sessionInfo() ===\n")
sink(file.path(OUT_DIR, "sessionInfo_NB.txt"))
sessionInfo()
sink()
cat(sprintf("Saved: %s/sessionInfo_NB.txt\n", OUT_DIR))

# ── 2. Preprocessing parameters ─────────────────────────────────────────────
cat("\n=== 2. Preprocessing parameters ===\n")
sink(file.path(OUT_DIR, "preprocessing_params.txt"))
cat("Preprocessing Parameters — Phase I/II (Notebook)\n")
cat("=================================================\n\n")

cat("PHASE I — Data Harmonization & Imputation:\n")
cat("  - Missingness threshold for omic variables: 35% (variable excluded if >35% NA across cohort)\n")
cat("  - Dummy-variable injection: 1% random dummy features for Boruta signal-noise calibration\n")
cat("  - Imputation engine: mice (M=5, maxit=10, method='pmm' for continuous, 'logreg' for binary)\n")
cat("  - Imputation scope: groupwise (within cancer type), no cross-cohort amalgamation\n")
cat("  - Feature harmonization: token-based naming convention (e.g., READ-56.6.3.N.2.4.7.1.3.1)\n")
cat("  - Phase I output: df008 through df377 (11 unique TAR-admissible matrices)\n")
cat("  - Master reference matrix: df005.rds (unimputed, all 14,595 signatures)\n")
cat("  - Survival time completion: DFI.time completed for administrative censor cases (9.96% of DFI records)\n")
cat("  - Duplicate removal: patient deduplication using TCGA barcode substring matching\n")
cat("  - Clinical endpoint gating: OS.time, DSS.time, DFI.time, PFI.time > 0 required\n\n")

cat("PHASE II — CANARY Diagnostic:\n")
cat("  - Model: CoxNet (Elastic Net-regularized Cox regression)\n")
cat("  - Regularization path: 100 lambda values, alpha = 0.5 (elastic net mix)\n")
cat("  - Mu-ladder: decreasing lambda sequence testing structural PH feasibility\n")
cat("  - Convergence criterion: strata with non-null coefficient path at any lambda = admitted\n")
cat("  - Admitted strata: 96 of 120 tested (80%); 24 failed due to target scarcity\n")
cat("  - Admission label: Non-Proportional / Mu-Exhausted (all 96 admitted strata)\n")
cat("  - Software: glmnet v4.1-8 with family='cox'\n\n")

cat("PHASE I/II — Software Dependencies:\n")
cat("  - R version: see sessionInfo_NB.txt\n")
cat("  - Key packages: mice, missForest, glmnet, survival, data.table, dplyr\n")
cat("  - Environment: renv_phase_I_II.lock (provided in GitHub repository)\n\n")

cat("Data Sources:\n")
cat("  - TCGA multi-omic data: UCSC Xena (https://xena.ucsc.edu)\n")
cat("  - Clinical endpoints: TCGA Clinical Data Resource (OS, DSS, DFI, PFI)\n")
cat("  - Omic layers (7): mRNA, Transcript Isoform, microRNA, Protein (RPPA),\n")
cat("    DNA Methylation, Somatic Mutation, Copy Number Variation (CNV)\n")
cat("  - Supplementary Dataset S1: 14,907 clinically relevant signatures\n")
cat("  - Supplementary Dataset S2: Phase III derivation matrix (10,306 patients)\n")
cat("  - Supplementary Dataset S3: Blind validation matrix (1,050 patients)\n")
sink()
cat(sprintf("Saved: %s/preprocessing_params.txt\n", OUT_DIR))

# ── 3. Random seeds ──────────────────────────────────────────────────────────
cat("\n=== 3. Random seeds ===\n")
sink(file.path(OUT_DIR, "random_seeds_NB.txt"))
cat("Random Seed Documentation — Phase I/II (Notebook)\n")
cat("==================================================\n\n")
cat("Phase I Imputation:\n")
cat("  - mice imputation: set.seed(42) per imputation batch\n")
cat("  - Groupwise imputation: seed reset per cancer type with offset\n")
cat("  - Dummy-variable injection: set.seed(123) for random feature generation\n\n")
cat("Phase II CANARY:\n")
cat("  - glmnet cv.glmnet: set.seed(42) for fold assignment\n")
cat("  - Lambda grid: fixed sequence (100 values), no random component\n")
cat("  - Stratification: no random splitting; each stratum is tested independently\n\n")
cat("Global conventions:\n")
cat("  - Primary reproducibility seed: set.seed(42) throughout Phase I/II\n")
cat("  - Auxiliary seed (dummy variables): set.seed(123)\n")
cat("  - All seeds documented in respective R script headers\n")
sink()
cat(sprintf("Saved: %s/random_seeds_NB.txt\n", OUT_DIR))

cat("\n=== DONE ===\n")
cat(sprintf("Outputs saved to: %s/\n", OUT_DIR))
cat("  sessionInfo_NB.txt\n")
cat("  preprocessing_params.txt\n")
cat("  random_seeds_NB.txt\n")


######
######
######   setwd("d:/Pré-artigo 5-optosis model/Machine learning CancerRCDShiny_prediction")
capture.output(sessionInfo(), file = "R1/Reproducibility extraction NB/sessionInfo_NB.txt")

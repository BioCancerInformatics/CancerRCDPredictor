###############################################################################
# R1.7 — Pre-flight Check: Verify bootstrap requirements on ZIMA
# ============================================================================
# Run this on ZIMA before bootstrap_ci_cindex.R to confirm everything is ready
###############################################################################

suppressPackageStartupMessages({
  library(data.table)
})

# ── ZIMA working directory ──────────────────────────────────────────────────
MASTER_DIR <- "~/students/aluno0549-6/PHASE_III/PHASE_III_Megarun_5_0_complete_ZIMA_Suit_Generator_final"
setwd(MASTER_DIR)
cat(sprintf("Working directory: %s\n", getwd()))

# ── 1. Check master folder contents ─────────────────────────────────────────
cat("\n========================================\n")
cat("1. Master folder contents\n")
cat("========================================\n")
top_files <- list.files()
cat(sprintf("Items in master folder: %d\n", length(top_files)))
print(top_files)

# ── 2. Check for model bundles folder ───────────────────────────────────────
cat("\n========================================\n")
cat("2. Model bundles folder\n")
cat("========================================\n")
MODELS_DIR <- "PHASE_III_ML_Models"
if (!dir.exists(MODELS_DIR)) {
  # Try alternative names
  alts <- list.files(pattern = "ML_Models|model_bundle|Megarun")
  if (length(alts) > 0) {
    cat(sprintf("'%s' not found. Alternatives: %s\n", MODELS_DIR, paste(alts, collapse = ", ")))
    MODELS_DIR <- alts[1]
  } else {
    stop(sprintf("Cannot find model bundles folder in %s", getwd()))
  }
}

strata <- list.dirs(MODELS_DIR, full.names = FALSE, recursive = FALSE)
cat(sprintf("Model bundle folder: %s\n", MODELS_DIR))
cat(sprintf("Number of strata with bundles: %d\n", length(strata)))
cat("Strata:\n")
print(strata)

# ── 3. Check each stratum for required files ────────────────────────────────
cat("\n========================================\n")
cat("3. Per-stratum bundle check\n")
cat("========================================\n")

bundle_ok <- 0
bundle_missing <- 0
bundle_sample <- NULL

for (s in strata) {
  bundle_path <- file.path(MODELS_DIR, s, paste0("model_bundle_", s, ".rds"))
  bundle_exists <- file.exists(bundle_path)
  
  if (bundle_exists) {
    bundle_ok <- bundle_ok + 1
    if (is.null(bundle_sample)) bundle_sample <- bundle_path
  } else {
    bundle_missing <- bundle_missing + 1
    cat(sprintf("  MISSING: %s\n", bundle_path))
  }
}

cat(sprintf("\nBundles present: %d / %d\n", bundle_ok, length(strata)))
cat(sprintf("Bundles missing: %d\n", bundle_missing))

# ── 4. Inspect one model bundle structure ───────────────────────────────────
cat("\n========================================\n")
cat("4. Model bundle structure (sample)\n")
cat("========================================\n")

if (!is.null(bundle_sample)) {
  cat(sprintf("Loading: %s\n", bundle_sample))
  
  bundle <- readRDS(bundle_sample)
  cat(sprintf("Bundle class: %s\n", paste(class(bundle), collapse = ", ")))
  cat(sprintf("Top-level names (%d):\n", length(names(bundle))))
  print(names(bundle))
  
  # Show class and size of each element
  for (nm in names(bundle)) {
    obj <- bundle[[nm]]
    cl <- paste(class(obj), collapse = ", ")
    sz <- format(object.size(obj), units = "auto")
    cat(sprintf("  $%s: %s (%s)\n", nm, cl, sz))
    
    # If it's a list, show sub-names
    if (is.list(obj) && !is.data.frame(obj)) {
      sub_names <- names(obj)
      if (!is.null(sub_names)) {
        cat(sprintf("    sub-names: %s\n", paste(head(sub_names, 10), collapse = ", ")))
      }
    }
    
    # If it's a data.frame or has dim, show dimensions
    if (is.data.frame(obj)) {
      cat(sprintf("    dim: %d x %d, cols: %s\n", 
                  nrow(obj), ncol(obj),
                  paste(head(colnames(obj), 8), collapse = ", ")))
    }
    
    # If atomic numeric, show summary
    if (is.numeric(obj) && is.atomic(obj) && length(obj) < 1000) {
      cat(sprintf("    summary: min=%.4f, median=%.4f, max=%.4f, length=%d\n",
                  min(obj, na.rm=TRUE), median(obj, na.rm=TRUE), 
                  max(obj, na.rm=TRUE), length(obj)))
    }
  }
} else {
  cat("No bundle available to inspect.\n")
}

# ── 5. Check for performance CSV ────────────────────────────────────────────
cat("\n========================================\n")
cat("5. Performance summary file\n")
cat("========================================\n")

perf_candidates <- list.files(pattern = "MASTER.*Performance|performance.*csv|Performance.*csv",
                               ignore.case = TRUE)
if (length(perf_candidates) > 0) {
  cat(sprintf("Found: %s\n", paste(perf_candidates, collapse = ", ")))
  perf <- fread(perf_candidates[1])
  cat(sprintf("Rows: %d, Cols: %d\n", nrow(perf), ncol(perf)))
  cat(sprintf("Columns: %s\n", paste(colnames(perf), collapse = ", ")))
  cat("First 3 rows:\n")
  print(head(perf, 3))
} else {
  cat("WARNING: No performance CSV found.\n")
}

# ── 6. Summary ──────────────────────────────────────────────────────────────
cat("\n========================================\n")
cat("6. READINESS SUMMARY\n")
cat("========================================\n")

issues <- 0
if (bundle_missing > 0) {
  cat(sprintf("[ISSUE] %d model bundles missing.\n", bundle_missing))
  issues <- issues + 1
}
if (length(perf_candidates) == 0) {
  cat("[ISSUE] Performance CSV not found.\n")
  issues <- issues + 1
}
if (is.null(bundle_sample)) {
  cat("[ISSUE] No model bundle could be loaded for inspection.\n")
  issues <- issues + 1
}

if (issues == 0) {
  cat("All checks passed. Ready to run bootstrap_ci_cindex.R.\n")
} else {
  cat(sprintf("%d issue(s) found. Resolve before running bootstrap.\n", issues))
}

cat("\nDone.\n")

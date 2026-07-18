# =============================================================================
# PREFLIGHT — PH Violation Audit (120 CANARY-tested strata)
# Data source: CoxNet_phaseII_feasibility_log.rds + dfXXX_series
# Replicates CANARY data pipeline exactly
# =============================================================================

setwd("~/students/aluno0549-6/PHASE_III/PHASE_III_Megarun_5_0_complete_ZIMA_Suit_Generator_final")

cat("PH VIOLATION AUDIT — PREFLIGHT CHECK (120 CANARY-tested strata)\n")
cat(paste0(paste(rep("=", 60), collapse = ""), "\n\n"))

logfile <- file.path(getwd(), "preflight_PH_audit.log")
sink(logfile, split = TRUE)

issues <- 0

DF_ROOT   <- "~/students/aluno0549-6/dfXXX_series"
TAR_TABLE <- "~/students/aluno0549-6/PHASE_III/CoxNet_phaseII_feasibility_log.rds"

# ── 1. CANARY log ──────────────────────────────────────────────────────
if (!file.exists(TAR_TABLE)) {
  cat(" CRITICAL: CANARY log not found\n")
  issues <- issues + 1
} else {
  dfinput <- readRDS(TAR_TABLE)
  cat(sprintf(" [OK] CANARY log: %d rows\n", nrow(dfinput)))
  cat(sprintf(" [OK] Columns: %s\n", paste(names(dfinput), collapse = ", ")))
  
  # Count unique cancer-endpoint-df combinations
  uniq <- unique(dfinput[, c('cancer_type', 'metric', 'df_file')])
  cat(sprintf(" [OK] %d unique cancer_endpoint_df combinations\n", nrow(uniq)))
  
  # Show CANARY status distribution
  status_counts <- table(dfinput$PHASE_III_logic)
  cat(" [OK] CANARY status distribution:\n")
  for (s in names(status_counts)) {
    cat(sprintf("      %s: %d\n", s, status_counts[s]))
  }
}

# ── 2. dfXXX_series ────────────────────────────────────────────────────
cat(sprintf("\n [OK] dfXXX_series path: %s\n", DF_ROOT))
df_vars <- unique(dfinput$df_file)
df_files <- list.files(DF_ROOT, pattern = "^df\\d+\\.rds$")
cat(sprintf(" [OK] %d unique df variants in CANARY log\n", length(df_vars)))
cat(sprintf(" [OK] %d df files in dfXXX_series\n", length(df_files)))
missing <- df_vars[!df_vars %in% df_files]
if (length(missing) > 0) {
  cat(sprintf(" [WARN] %d missing: %s\n", length(missing), paste(missing, collapse = ", ")))
} else {
  cat(" [OK] All df files present\n")
}

# ── 3. Sample test (replicates CANARY pipeline) ────────────────────────
sample <- dfinput[1, ]
c <- trimws(as.character(sample$cancer_type))
m <- trimws(as.character(sample$metric))
d <- trimws(as.character(sample$df_file))

clean_d <- ifelse(grepl("\\.rds$", d), d, paste0(d, ".rds"))
df_path <- file.path(DF_ROOT, clean_d)

if (!file.exists(df_path)) {
  cat(" [FAIL] Sample df not found:", clean_d, "\n")
  issues <- issues + 1
} else {
  df_raw <- readRDS(df_path)
  df_cancer <- df_raw[df_raw$type == c, , drop = FALSE]
  cat(sprintf(" [OK] Sample: %s_%s using %s -> %d rows for %s\n",
              c, m, clean_d, nrow(df_cancer), c))
  
  # Test filter_survival_complete
  time_col <- paste0(m, ".time")
  has_time <- time_col %in% names(df_raw)
  has_event <- m %in% names(df_raw)
  cat(sprintf(" [%s] %s.time column\n", if(has_time) "OK" else "WARN", m))
  cat(sprintf(" [%s] %s event column\n", if(has_event) "OK" else "WARN", m))
  
  # Test build_combined_X_matrix pattern
  pattern <- paste0("^", c, "-")
  preds <- names(df_raw)[grepl(pattern, names(df_raw))]
  cat(sprintf(" [%s] %d predictors matching %s- prefix\n", 
              if(length(preds) > 0) "OK" else "WARN", length(preds), c))
}

# ── 4. R packages ──────────────────────────────────────────────────────
pkgs <- c("survival", "data.table", "dplyr")
ok <- TRUE
for (p in pkgs) {
  if (!requireNamespace(p, quietly = TRUE)) { ok <- FALSE; cat(sprintf(" [WARN] Missing: %s\n", p)) }
}
if (ok) cat(" [OK] All R packages\n")

# ── 5. Output ──────────────────────────────────────────────────────────
OUT_DIR <- "PH_Violation_Audit"
dir.create(OUT_DIR, showWarnings = FALSE, recursive = TRUE)
if (file.create(file.path(OUT_DIR, ".test"))) {
  file.remove(file.path(OUT_DIR, ".test"))
  cat(" [OK] Output writable\n")
} else {
  cat(" [FAIL] Cannot write\n")
}

cat("\n [READY]\n")
sink()

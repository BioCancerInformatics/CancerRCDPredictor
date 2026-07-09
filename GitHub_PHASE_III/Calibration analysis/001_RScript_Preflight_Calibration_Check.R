###############################################################################
# R2.6 — Pre-flight Check: Verify calibration requirements on ZIMA
# ============================================================================
# Checks: model bundles, MVL probabilities, required packages, test run
###############################################################################

suppressPackageStartupMessages({
  library(survival)
})

setwd("~/students/aluno0549-6/PHASE_III/PHASE_III_Megarun_5_0_complete_ZIMA_Suit_Generator_final/Calibration_analysis")
MODELS_DIR <- "../PHASE_III_ML_Models"

# ── 1. Check all strata have bundles and MVL probabilities ───────────────────
cat("========================================\n")
cat("1. Model bundles & MVL files\n")
cat("========================================\n")
strata <- list.dirs(MODELS_DIR, full.names = FALSE, recursive = FALSE)
strata <- strata[grepl("_df[0-9]+$", strata)]
cat(sprintf("Found %d strata.\n", length(strata)))

missing_bundle <- 0
missing_probs  <- 0
for (s in strata) {
  bf <- file.path(MODELS_DIR, s, paste0("model_bundle_", s, ".rds"))
  pf <- file.path(MODELS_DIR, s, "MVL_Synthesis", 
                  paste0(s, "_MVL_Synthesis_Patient_Probabilities.tsv"))
  if (!file.exists(bf)) missing_bundle <- missing_bundle + 1
  if (!file.exists(pf)) missing_probs <- missing_probs + 1
}
cat(sprintf("Missing bundles: %d\n", missing_bundle))
cat(sprintf("Missing MVL files: %d\n", missing_probs))

# ── 2. Check survival package ────────────────────────────────────────────────
cat("\n========================================\n")
cat("2. Required packages\n")
cat("========================================\n")
cat(sprintf("survival: v%s (OK)\n", packageVersion("survival")))

# ── 3. Test calibration on BRCA_OS_df008 ─────────────────────────────────────
cat("\n========================================\n")
cat("3. Test run: BRCA_OS_df008\n")
cat("========================================\n")

test_cohort <- "BRCA_OS_df008"
prob_file <- file.path(MODELS_DIR, test_cohort, "MVL_Synthesis",
                       paste0(test_cohort, "_MVL_Synthesis_Patient_Probabilities.tsv"))
bundle_file <- file.path(MODELS_DIR, test_cohort,
                         paste0("model_bundle_", test_cohort, ".rds"))

probs  <- read.table(prob_file, header = TRUE, sep = "\t")
bundle <- readRDS(bundle_file)

surv <- as.data.frame(bundle$RSF$yvar)
surv$Patient_ID <- gsub("-[0-9]{2}[A-Z]?$", "", rownames(surv))

m <- merge(probs, surv[, c("Patient_ID", "time", "status")], by = "Patient_ID")
cat(sprintf("n=%d, events=%d\n", nrow(m), sum(m$status == 1)))

# Calibration slope via linear predictor
p_clip <- pmax(pmin(m$Event_Prob_1Yr, 0.9999), 0.0001)
m$lp <- log(-log(1 - p_clip))
cal_fit <- coxph(Surv(time, status) ~ lp, data = m)
cat(sprintf("Calibration slope: %.4f (SE=%.4f)\n", coef(cal_fit), 
            summary(cal_fit)$coefficients[1, "se(coef)"]))

# ── 4. Readiness ─────────────────────────────────────────────────────────────
cat("\n========================================\n")
cat("4. READINESS\n")
cat("========================================\n")
issues <- missing_bundle + missing_probs
if (issues == 0) {
  cat(sprintf("All checks passed. Ready to run calibration_metrics.R\n"))
} else {
  cat(sprintf("%d issue(s) found.\n", issues))
}

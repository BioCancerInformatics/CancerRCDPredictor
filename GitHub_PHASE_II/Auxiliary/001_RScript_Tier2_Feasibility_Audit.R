###############################################################################
# Tier-2 Feasibility Structural Audit
# Objective: Generate the Supplementary Table summarizing the CANARY 
# CoxNet attrition cascade across the 120 survival strata.
###############################################################################

# Load required packages
if (!requireNamespace("dplyr", quietly = TRUE)) install.packages("dplyr")
library(dplyr)

# 1. Define Paths
WORKING_DIR <- "D:/Pré-artigo 5-optosis model/Machine learning CancerRCDShiny_prediction/Enrique version/PHASE_III"
LOG_PATH <- file.path(WORKING_DIR, "CoxNet_phaseII_feasibility_log.tsv")
OUTPUT_PATH <- file.path(WORKING_DIR, "CoxNet_Tier2_Feasibility_Audit_Summary.tsv")

setwd(WORKING_DIR)

cat("🚀 Initializing Tier-2 CoxNet feasibility structural audit...\n")

# 2. Check and Load Data
if (!file.exists(LOG_PATH)) {
  stop("FATAL: Could not locate CoxNet log at: ", LOG_PATH)
}

df_log <- read.delim(LOG_PATH, stringsAsFactors = FALSE)

# 3. Filter and Rename Columns for Formal Manuscript Presentation
cat("📊 Formatting variables for supplementary presentation...\n")

tier2_summary <- df_log %>%
  select(
    Cancer_Type = cancer_type,
    Survival_Metric = metric,
    Total_Survival_Samples = n,
    Total_Survival_Events = E,
    Tier_2_Diagnostics = data_fail_reasons,
    Phase_III_Eligibility = PHASE_III_logic
  ) %>%
  arrange(Cancer_Type, Survival_Metric)

# 4. Clean up the phrasing for the manuscript
tier2_summary$Tier_2_Diagnostics[tier2_summary$Tier_2_Diagnostics == ""] <- "Geometry Viable"
tier2_summary$Tier_2_Diagnostics[tier2_summary$Tier_2_Diagnostics == "LOW_N;LOW_EVENTS"] <- "Target Scarcity (Low Samples and Events)"
tier2_summary$Tier_2_Diagnostics[tier2_summary$Tier_2_Diagnostics == "LOW_EVENTS"] <- "Target Scarcity (Low Events)"
tier2_summary$Tier_2_Diagnostics[tier2_summary$Tier_2_Diagnostics == "LOW_N"] <- "Target Scarcity (Low Samples)"

tier2_summary$Phase_III_Eligibility <- gsub("INELIGIBLE__INSUFFICIENT_SURVIVAL_INFORMATION", "Failed (Insufficient Targets)", tier2_summary$Phase_III_Eligibility)
tier2_summary$Phase_III_Eligibility <- gsub("ELIGIBLE_NONCOX__GEOMETRY_MU_EXHAUSTED", "Admitted (Non-Proportional / Mu-Exhausted)", tier2_summary$Phase_III_Eligibility)

# Remove underscores from column names to make them publication ready
names(tier2_summary) <- gsub("_", " ", names(tier2_summary))

# 5. Export to TSV and Excel
write.table(tier2_summary, file = OUTPUT_PATH, sep = "\t", row.names = FALSE, quote = FALSE)
cat(sprintf("✅ SUCCESS: Tier-2 audit generated. Summary saved to:\n   %s\n", OUTPUT_PATH))

# Export to Excel as well
if (!requireNamespace("writexl", quietly = TRUE)) install.packages("writexl")
writexl::write_xlsx(tier2_summary, file.path(WORKING_DIR, "Table_S2_CANARY_Audit_Summary.xlsx"))
cat(sprintf("✅ SUCCESS: Table S2 Excel file saved to:\n   %s\n", file.path(WORKING_DIR, "Table_S2_CANARY_Audit_Summary.xlsx")))

# 6. Print quick stats
cat("\n--- ATTRITION SUMMARY ---\n")
print(table(tier2_summary[["Phase III Eligibility"]]))

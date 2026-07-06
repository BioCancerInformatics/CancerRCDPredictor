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

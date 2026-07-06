###############################################################################
# df005 Baseline Structural Audit
# Objective: Generate universally applicable dimensions for the Phase II/III 
# manuscript methodologies regarding sample size and omic layer distributions.
###############################################################################

# Load required packages
if (!requireNamespace("dplyr", quietly = TRUE)) install.packages("dplyr")
if (!requireNamespace("tidyr", quietly = TRUE)) install.packages("tidyr")
library(dplyr)
library(tidyr)

# 1. Define Paths
WORKING_DIR <- "D:/Pré-artigo 5-optosis model/Machine learning CancerRCDShiny_prediction/Enrique version/PHASE_III"
DF_SERIES_DIR <- "D:/Pré-artigo 5-optosis model/Machine learning CancerRCDShiny_prediction/Enrique version/PHASE_II/dfXXX_series"
DF005_PATH <- file.path(DF_SERIES_DIR, "df005.rds")
OUTPUT_PATH <- file.path(WORKING_DIR, "df005_baseline_audit_summary.tsv")

setwd(WORKING_DIR)

cat("🚀 Initializing df005 baseline structural audit...\n")

# 2. Check and Load Data
if (!file.exists(DF005_PATH)) {
  stop("FATAL: Could not locate df005.rds at: ", DF005_PATH)
}
cat("⏳ Loading df005.rds (this may take a moment for a 10k x 14k matrix)...\n")
df005 <- readRDS(DF005_PATH)

# Verify Matrix Geography (as specified: Clinical 1-62, Omic 63+)
expected_samples <- 10489
expected_vars <- 14657
cat(sprintf("📊 Loaded df005: %d Rows (Samples) | %d Columns (Variables)\n", nrow(df005), ncol(df005)))

if (ncol(df005) != expected_vars) {
  warning(sprintf("df005 column count (%d) deviates from expected 14,657. Proceeding anyway...", ncol(df005)))
}

# 3. Extract Component 1: Samples Per Cancer Type
# The 'type' column stores the CTAB abbreviation.
cat("🔍 Extracting demographic and sample topologies...\n")
sample_summary <- df005 %>%
  group_by(type) %>%
  summarise(Total_Samples = n(), .groups = "drop") %>%
  rename(Cancer_Type = type)

# 4. Parse Omic Dictionary (Columns 63 to N)
omic_features <- colnames(df005)[63:ncol(df005)]

cat(sprintf("🧬 Parsing %d multi-omic predictive tokens...\n", length(omic_features)))

# Breakdown token strings (Example: "LUAD-509.5.3.P.3.44.44.1.2.6")
# - Cancer_Type is the substring before the first "-"
# - Layer is the first token after the first literal dot "."
var_df <- data.frame(Raw_Feature = omic_features, stringsAsFactors = FALSE)

var_df$Cancer_Type <- sapply(strsplit(var_df$Raw_Feature, "-"), `[`, 1)
var_df$Layer_Code <- sapply(strsplit(var_df$Raw_Feature, "\\."), function(x) {
  if (length(x) >= 2) return(x[2]) else return("Unknown")
})

# 5. Extract Component 2: Total Omic Variables per Cancer Type
total_var_summary <- var_df %>%
  group_by(Cancer_Type) %>%
  summarise(Total_Omic_Variables = n(), .groups = "drop")

# 6. Extract Component 3: Distribution per Omic Layer
# Translate raw numeric code to defined biological layers
layer_translation <- c(
  "1" = "Layer_1_Protein",
  "2" = "Layer_2_Mutation",
  "3" = "Layer_3_CNV",
  "4" = "Layer_4_miRNA",
  "5" = "Layer_5_Isoform",
  "6" = "Layer_6_mRNA",
  "7" = "Layer_7_CpG"
)

var_df$Layer_Name <- layer_translation[var_df$Layer_Code]
# Handle any unexpected nomenclature gracefully
var_df$Layer_Name[is.na(var_df$Layer_Name)] <- "Layer_Unknown"

layer_summary <- var_df %>%
  group_by(Cancer_Type, Layer_Name) %>%
  summarise(Count = n(), .groups = "drop") %>%
  pivot_wider(names_from = Layer_Name, values_from = Count, values_fill = list(Count = 0))

# 7. Synthesize Master Report Matrix
cat("📈 Aggregating final metric vectors...\n")
final_summary <- sample_summary %>%
  full_join(total_var_summary, by = "Cancer_Type") %>%
  full_join(layer_summary, by = "Cancer_Type")

# Reorder logically ensuring all layer columns are represented (even if 0)
expected_cols <- c("Cancer_Type", "Total_Samples", "Total_Omic_Variables", 
                   "Layer_1_Protein", "Layer_2_Mutation", "Layer_3_CNV", 
                   "Layer_4_miRNA", "Layer_5_Isoform", "Layer_6_mRNA", "Layer_7_CpG")
for (col in expected_cols) {
  if (!col %in% colnames(final_summary)) {
    final_summary[[col]] <- 0
  }
}
final_summary <- final_summary[, expected_cols]

# Fill missing counts with 0 where merges failed due to 0 count occurrences
final_summary[is.na(final_summary)] <- 0
final_summary <- final_summary[order(final_summary$Total_Samples, decreasing = TRUE), ]

# Add classification for mathematical accuracy mirroring Phase II
# Cohorts are Structurally Void if they lack variables or inherently violate the N < 50 policy.
final_summary$Tier_1_Status <- ifelse(
  final_summary$Total_Omic_Variables == 0 | final_summary$Total_Samples < 50,
  "Structurally Void",
  "Processed to Phase II"
)

# Remove underscores from column names to make them publication ready
names(final_summary) <- gsub("_", " ", names(final_summary))

# 8. Export to Phase III Working Directory
write.table(final_summary, file = OUTPUT_PATH, sep = "\t", row.names = FALSE, quote = FALSE)
cat(sprintf("✅ SUCCESS: Consolidated baseline audit saved to:\n   %s\n", OUTPUT_PATH))

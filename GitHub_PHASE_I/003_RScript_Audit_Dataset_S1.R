# ==============================================================================
# AUDIT: Count Unique Biological Elements in Dataset S1 Signatures
# ==============================================================================

# Load necessary libraries
library(readr)
library(dplyr)
library(stringr)

# 1. Load the dataset (only the Signature column for speed)
# Adjust the path if necessary depending on where you run the script from
dataset_path <- "../../Datasets/Dataset_S1_df1157_Unified_Baseline_Decoder.csv"
df <- read_csv(dataset_path, col_select = c("Signature"))

# 2. Extract unique signatures
unique_sigs <- unique(na.omit(df$Signature))

# 3. Clean the signatures: remove parentheses
clean_sigs <- str_replace_all(unique_sigs, "[()]", "")

# 4. Split the compound signatures by " + " 
split_elements <- str_split(clean_sigs, " \\+ ")

# 5. Flatten the list into a single vector and remove leading/trailing whitespace
all_elements <- str_trim(unlist(split_elements))

# 6. Remove any empty strings just in case
all_elements <- all_elements[all_elements != ""]

# 7. Count unique target biological elements
unique_elements <- unique(all_elements)
total_unique_elements <- length(unique_elements)

# Print final mathematical results
cat("========================================================\n")
cat("MATHEMATICAL AUDIT RESULTS: DATASET S1\n")
cat("========================================================\n")
cat("Total Rows (Nomenclatures):", nrow(df), "\n")
cat("Total Unique Signatures:", length(unique_sigs), "\n")
cat("Total unique biological target elements:", total_unique_elements, "\n")

# Optional: Count pure genes by filtering out transcripts (ENST), miRNAs (hsa), and methylation (cg)
pure_genes <- unique_elements[!str_starts(unique_elements, "ENST") & 
                              !str_starts(unique_elements, "hsa") & 
                              !str_starts(unique_elements, "cg")]
cat("Total pure gene symbols:", length(pure_genes), "\n")
cat("========================================================\n")


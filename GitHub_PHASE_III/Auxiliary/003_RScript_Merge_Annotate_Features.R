# Load libraries
library(rio)
library(dplyr)

# Load data
df_targets <- readRDS("df1157_ML_targets_final.rds")
table_s8 <- import("Supplementary_Table_S8_Baseline_Prognostic_Features.xlsx")

# Prepare subset: keep Nomenclature for matching and preserve a copy in output
df_subset <- df_targets %>%
  select(Nomenclature, 1:23) %>%
  mutate(Nomenclature_added = Nomenclature)

# Merge and post-process
table_s8_updated <- table_s8 %>%
  left_join(df_subset, by = c("Feature" = "Nomenclature")) %>%
  select(-Rank) %>%
  rename(Nomenclature = Nomenclature_added) %>%
  relocate(Nomenclature, .after = Algorithms_Validating)

# Save outputs
export(table_s8_updated, "Supplementary_Table_S8_Baseline_Prognostic_Features_updated.tsv")
saveRDS(table_s8_updated, "Supplementary_Table_S8_Baseline_Prognostic_Features_updated.rds")

# Save as Excel (.xlsx)
export(table_s8_updated, "Supplementary_Table_S8_Baseline_Prognostic_Features_updated.xlsx")


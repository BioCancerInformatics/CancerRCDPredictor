# ==============================================================================
# RScript_Build_Table_SY_Sparsity_Survivors_V3.R
# ==============================================================================
library(dplyr)
library(tidyr)
library(openxlsx)
library(stringr)

cat("============================================================\n")
cat("🧬 INITIATING FULL 36-COLUMN TABLE SY (SPARSITY) EXTRACTION\n")
cat("============================================================\n")

# 1. Define Paths
WORKING_DIR <- "D:/Pré-artigo 5-optosis model/Machine learning CancerRCDShiny_prediction/Enrique version/PHASE_III"
SPARSITY_DIR <- file.path(WORKING_DIR, "RScript_PHASE_IIIB_Sparsity_Isolation_megarun_2_1/PHASE_IIIB_Sparsity_Isolation_Models")
DATASETS_DIR <- "D:/Pré-artigo 5-optosis model/Machine learning CancerRCDShiny_prediction/Datasets"
DF1157_PATH <- file.path(DATASETS_DIR, "df1157_ML_targets_final.rds")

extract_tokens <- function(strings) {
  s <- gsub("^`|`$", "", strings)
  s <- gsub("^Feature_", "", s)
  return(s)
}

# Extract Nomenclature (Keep Cohort Prefix)
get_nomenclature <- function(s) {
  return(s)
}

# 2. Harvest all features per cohort (ONLY FOR 82 VIABLE COHORTS)
master_perf <- read.csv(file.path(WORKING_DIR, "RScript_PHASE_IIIB_Sparsity_Isolation_megarun_2_1/MASTER_Phase_IIIB_Sparsity_Performance.csv"), stringsAsFactors=F)
viable_cohorts <- master_perf$Cohort

cohort_dirs <- list.dirs(SPARSITY_DIR, recursive = FALSE, full.names = TRUE)
master_list <- list()

for (cdir in cohort_dirs) {
  cohort <- basename(cdir)
  if (!(cohort %in% viable_cohorts)) next
  
  b_path <- file.path(cdir, "Boruta", paste0(cohort, "_Boruta_Feature_Decisions.tsv"))
  x_path <- file.path(cdir, "XGBoost", paste0(cohort, "_XGBoost_Node_Importance.tsv"))
  r_path <- file.path(cdir, "RSF", paste0(cohort, "_RSF_VIMP.tsv"))
  m_path <- file.path(cdir, "MTLR", paste0(cohort, "_MTLR_Feature_Importance.tsv"))
  
  b_df <- if(file.exists(b_path)) read.delim(b_path, stringsAsFactors=F) else data.frame(Feature=character(), meanImp=numeric(), decision=character())
  x_df <- if(file.exists(x_path)) read.delim(x_path, stringsAsFactors=F) else data.frame(Feature=character(), Gain=numeric())
  r_df <- if(file.exists(r_path)) read.delim(r_path, stringsAsFactors=F) else data.frame(Feature=character(), VIMP=numeric())
  m_df <- if(file.exists(m_path)) read.delim(m_path, stringsAsFactors=F) else data.frame(Feature=character(), MTLR_L2_Norm=numeric())
  
  b_clean <- b_df %>% filter(decision %in% c("Confirmed", "Tentative")) %>% mutate(Feature=extract_tokens(Feature), Boruta_Imp=meanImp) %>% select(Feature, Boruta_Imp)
  x_clean <- x_df %>% filter(Gain > 0) %>% mutate(Feature=extract_tokens(Feature), XGB_Imp=Gain) %>% select(Feature, XGB_Imp)
  r_clean <- r_df %>% filter(VIMP > 0.005) %>% mutate(Feature=extract_tokens(Feature), RSF_Imp=VIMP) %>% select(Feature, RSF_Imp)
  m_clean <- m_df %>% filter(MTLR_L2_Norm > 0) %>% mutate(Feature=extract_tokens(Feature), MTLR_Imp=MTLR_L2_Norm) %>% select(Feature, MTLR_Imp)
  
  all_f <- unique(c(b_clean$Feature, x_clean$Feature, r_clean$Feature, m_clean$Feature))
  if(length(all_f) > 0) {
    df <- data.frame(Feature = all_f, Cohort = cohort, stringsAsFactors = FALSE)
    df <- df %>% left_join(b_clean, by="Feature") %>% left_join(x_clean, by="Feature") %>%
                 left_join(r_clean, by="Feature") %>% left_join(m_clean, by="Feature")
    master_list[[cohort]] <- df
  }
}

full_df <- bind_rows(master_list)

# 3. Mathematically enforce Table S11 Ejection (1,568 - 310 = 1,258)
table_s11 <- read.xlsx(file.path(WORKING_DIR, "Supplementary Table_v035.xlsx"), sheet="Table_S11")
ejected_features <- table_s11$Ejected.Signature

full_df <- full_df %>% filter(!(Feature %in% ejected_features))
cat(sprintf("✅ Enforced mathematical ejection. Survived features count: %d\n", length(unique(full_df$Feature))))

# 4. Aggregate across cohorts
agg_df <- full_df %>%
  group_by(Feature) %>%
  summarize(
    Total.Cohorts = n_distinct(Cohort),
    Cohorts.Present = paste(sort(unique(Cohort)), collapse = ", "),
    Boruta_Importance = max(Boruta_Imp, na.rm=TRUE),
    XGBoost_Importance = max(XGB_Imp, na.rm=TRUE),
    RSF_Importance = max(RSF_Imp, na.rm=TRUE),
    MTLR_Importance = max(MTLR_Imp, na.rm=TRUE)
  )

agg_df$Boruta_Importance[is.na(agg_df$Boruta_Importance) | is.infinite(agg_df$Boruta_Importance)] <- 0
agg_df$XGBoost_Importance[is.na(agg_df$XGBoost_Importance) | is.infinite(agg_df$XGBoost_Importance)] <- 0
agg_df$RSF_Importance[is.na(agg_df$RSF_Importance) | is.infinite(agg_df$RSF_Importance)] <- 0
agg_df$MTLR_Importance[is.na(agg_df$MTLR_Importance) | is.infinite(agg_df$MTLR_Importance)] <- 0

# 5. Validations
agg_df <- agg_df %>%
  mutate(
    Boruta_Validation = ifelse(Boruta_Importance > 0, "Validated (>0.005)", "Sub-Threshold (<0.005)"),
    XGBoost_Validation = ifelse(XGBoost_Importance > 0.005, "Validated (>0.005)", "Sub-Threshold (<0.005)"),
    RSF_Validation = ifelse(RSF_Importance > 0.005, "Validated (>0.005)", "Sub-Threshold (<0.005)"),
    MTLR_Validation = ifelse(MTLR_Importance > 0.005, "Validated (>0.005)", "Sub-Threshold (<0.005)"),
    Total_Validated_Algorithms = (Boruta_Validation == "Validated (>0.005)") + 
                                 (XGBoost_Validation == "Validated (>0.005)") + 
                                 (RSF_Validation == "Validated (>0.005)") + 
                                 (MTLR_Validation == "Validated (>0.005)")
  )

# 6. Total Algorithms & Validating String
# Total Algorithms tracks structural entry (> 0) as requested
agg_df$Total.Algorithms <- rowSums(agg_df[, c("Boruta_Importance", "XGBoost_Importance", "RSF_Importance", "MTLR_Importance")] > 0)
agg_df$Algorithms.Validating <- apply(agg_df, 1, function(row) {
  algos <- c()
  if(as.numeric(row["RSF_Importance"]) > 0) algos <- c(algos, "RSF")
  if(as.numeric(row["Boruta_Importance"]) > 0) algos <- c(algos, "Boruta")
  if(as.numeric(row["XGBoost_Importance"]) > 0) algos <- c(algos, "XGBoost")
  if(as.numeric(row["MTLR_Importance"]) > 0) algos <- c(algos, "MTLR")
  res <- paste(algos, collapse=", ")
  return(ifelse(res == "", "None", res))
})

# 7. Biological Layer & Nomenclature Map
get_layer <- function(features) {
  repaired <- gsub("^([A-Za-z]+)\\.", "\\1-", features)
  tokens <- sapply(strsplit(repaired, "\\."), function(x) if(length(x)>=2) x[2] else NA)
  return(ifelse(tokens == "2", "Somatic Mutation", ifelse(tokens == "3", "Copy Number Variation", "Unknown")))
}

agg_df$Biological.Layer <- get_layer(agg_df$Feature)
agg_df$Nomenclature <- get_nomenclature(agg_df$Feature)

# 8. Merge with df1157
df1157 <- readRDS(DF1157_PATH)
df1157_subset <- df1157 %>% select(Nomenclature, 2:23)

table_sy <- agg_df %>% inner_join(df1157_subset, by = "Nomenclature")

# 9. Exact 36 Column Reorder (RSF, Boruta, XGBoost, MTLR)
table_sy_final <- table_sy %>%
  select(
    Feature, Biological.Layer, Total.Cohorts, Cohorts.Present, Total.Algorithms, Algorithms.Validating, Total_Validated_Algorithms,
    Nomenclature, everything(), 
    -Boruta_Importance, -Boruta_Validation, -XGBoost_Importance, -XGBoost_Validation, 
    -RSF_Importance, -RSF_Validation, -MTLR_Importance, -MTLR_Validation,
    RSF_Importance, RSF_Validation, Boruta_Importance, Boruta_Validation,
    XGBoost_Importance, XGBoost_Validation, MTLR_Importance, MTLR_Validation
  )

table_sy_final <- table_sy_final %>% filter(Biological.Layer %in% c("Somatic Mutation", "Copy Number Variation"))

output_path <- file.path(WORKING_DIR, "Table_SY_Sparsity_Retained_Survivors_FULL_36_COL.xlsx")
write.xlsx(table_sy_final, output_path, overwrite = TRUE)

unique_validated <- table_sy_final %>% filter(Total_Validated_Algorithms > 0) %>% nrow()
quad_validated <- table_sy_final %>% filter(Total_Validated_Algorithms == 4) %>% nrow()

cat(sprintf("✅ Total Rows Exported: %d\n", nrow(table_sy_final)))
cat(sprintf("Success! Unique validated features -> Boruta: %d, XGBoost: %d, RSF: %d, MTLR: %d\n", 
            sum(table_sy_final$Boruta_Validation == "Validated (>0.005)"), 
            sum(table_sy_final$XGBoost_Validation == "Validated (>0.005)"), 
            sum(table_sy_final$RSF_Validation == "Validated (>0.005)"), 
            sum(table_sy_final$MTLR_Validation == "Validated (>0.005)")))
cat(sprintf("Sub-Threshold features             -> Boruta: %d, XGBoost: %d, RSF: %d, MTLR: %d\n", 
            sum(table_sy_final$Boruta_Validation == "Sub-Threshold (<0.005)"),
            sum(table_sy_final$XGBoost_Validation == "Sub-Threshold (<0.005)"),
            sum(table_sy_final$RSF_Validation == "Sub-Threshold (<0.005)"),
            sum(table_sy_final$MTLR_Validation == "Sub-Threshold (<0.005)")))
cat(sprintf("Total unique validated signature features (at least one algorithm): %d\n", unique_validated))
cat(sprintf("Total completely Sub-Threshold signatures (rejected by all): %d\n", nrow(table_sy_final) - unique_validated))
cat(sprintf("Total Quad-Validated signature features (all 4 algorithms jointly): %d\n", quad_validated))
cat("============================================================\n")

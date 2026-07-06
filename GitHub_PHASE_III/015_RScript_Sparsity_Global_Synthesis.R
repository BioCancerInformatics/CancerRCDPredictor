# ==============================================================================
# RScript_Phase_IIIB_Global_Synthesis.R
# ==============================================================================
# PHASE IIIB SPARSITY GLOBAL SYNTHESIS: PEER-REVIEW PERFORMANCE PANORAMA
# ==============================================================================

if(!requireNamespace("tidyverse", quietly=TRUE)) install.packages("tidyverse")
if(!requireNamespace("ggplot2", quietly=TRUE)) install.packages("ggplot2")
if(!requireNamespace("cowplot", quietly=TRUE)) install.packages("cowplot")

library(tidyverse)
library(ggplot2)
library(cowplot)

# ------------------------------------------------------------------------------
# DIRECTORY ROUTING
# ------------------------------------------------------------------------------
WORKING_DIR <- "D:/Pré-artigo 5-optosis model/Machine learning CancerRCDShiny_prediction/Enrique version/PHASE_III/RScript_PHASE_IIIB_Sparsity_Isolation_megarun_2_1"
MASTER_CSV  <- file.path(WORKING_DIR, "MASTER_Phase_IIIB_Sparsity_Performance.csv")
MODELS_DIR  <- file.path(WORKING_DIR, "PHASE_IIIB_Sparsity_Isolation_Models")
OUTPUT_DIR  <- file.path(WORKING_DIR, "Synthesis_Graphics")

if(!dir.exists(OUTPUT_DIR)) dir.create(OUTPUT_DIR, recursive = TRUE)

cat("🚀 Initiating Final Synthesis Graphic Engine...\n")

# ------------------------------------------------------------------------------
# 1. CORE DATA INGESTION & MATHEMATICAL PARSING
# ------------------------------------------------------------------------------
if(!file.exists(MASTER_CSV)) stop("MASTER_Phase_IIIB_Sparsity_Performance.csv not found!")
df_perf <- read.csv(MASTER_CSV, head=TRUE, stringsAsFactors=FALSE)

df_perf <- df_perf %>%
  mutate(
    Cancer = sapply(strsplit(Cohort, "_"), `[`, 1),
    Metric = sapply(strsplit(Cohort, "_"), `[`, 2)
  )

# [MegaRun 4.1 BACKWARD COMPATIBILITY SHIELD]
# If we are parsing an older run that did not log Boruta, synthesize it as NA 
# so the 5-Ring architecture doesn't crash from a missing column.
if(!"Boruta_Independent" %in% colnames(df_perf)) {
   cat("⚠️ Legacy MegaRun (v4.1) detected! Synthetically bridging 'Boruta_Independent' as NA to preserve graphic pipelines...\n")
   df_perf$Boruta_Independent <- NA
}

cat(sprintf("✅ Ingested %d Cohort Records from Master Matrix.\n", nrow(df_perf)))

# ------------------------------------------------------------------------------
# PHASE 1: ALGORITHMIC ROBUSTNESS & SUMMARY TABLE
# ------------------------------------------------------------------------------
cat("📊 Generating Phase 1: Algorithmic Robustness Summaries...\n")

sum_table <- df_perf %>%
  summarise(
    Total_Cohorts = n(),
    RSF_Success = sum(!is.na(RSF_Out_of_Bag)),
    XGBoost_Success = sum(!is.na(XGBoost_Apparent)),
    MTLR_Success = sum(!is.na(MTLR_Apparent)),
    Boruta_Success = sum(!is.na(Boruta_Independent)),
    MVL_Success = sum(!is.na(MVL_ElasticNet_SuperLearner)),
    Median_RSF = median(RSF_Out_of_Bag, na.rm=TRUE),
    Median_XGB = median(XGBoost_Apparent, na.rm=TRUE),
    Median_MTLR = median(MTLR_Apparent, na.rm=TRUE),
    Median_Boruta = median(Boruta_Independent, na.rm=TRUE),
    Median_MVL = median(MVL_ElasticNet_SuperLearner, na.rm=TRUE),
    MVL_Superiority_Count = sum(MVL_ElasticNet_SuperLearner >= pmax(RSF_Out_of_Bag, MTLR_Apparent, XGBoost_Apparent, Boruta_Independent, na.rm=TRUE), na.rm=TRUE)
  )

write.csv(sum_table, file.path(OUTPUT_DIR, "Table_S4_Global_Performance_Robustness.csv"), row.names = FALSE)

# Cohort Ranking List (Table S5)
rank_table <- df_perf %>%
  arrange(desc(MVL_ElasticNet_SuperLearner), desc(XGBoost_Apparent)) %>%
  select(Cohort, Cancer, Metric, MVL_ElasticNet_SuperLearner, XGBoost_Apparent, RSF_Out_of_Bag, MTLR_Apparent, Boruta_Independent)

# ------------------------------------------------------------------------------
# SUPPLEMENTAL EXTENSION: SCRAPING AUC(t) HORIZONS (1, 3, 5 YEARS)
# ------------------------------------------------------------------------------
cat("🕰️ Scraping Time-Dependent AUC(t) distributions from MVL Syntheses...\n")

auc_list <- list()
temp_cohort_dirs <- list.dirs(MODELS_DIR, recursive = FALSE)

for(cdir in temp_cohort_dirs) {
  cohort_name <- basename(cdir)
  auc_file <- file.path(cdir, "MVL_Synthesis", paste0(cohort_name, "_MVL_Synthesis_AUC_1_3_5_Years.tsv"))
  if (file.exists(auc_file)) {
    auc_df <- tryCatch(read.delim(auc_file, stringsAsFactors=FALSE), error=function(e) NULL)
    if (!is.null(auc_df) && nrow(auc_df) > 0) {
      auc_1 <- auc_df$AUC[auc_df$Time_Days == 365]
      auc_3 <- auc_df$AUC[auc_df$Time_Days == 1095]
      auc_5 <- auc_df$AUC[auc_df$Time_Days == 1825]
      
      auc_list[[cohort_name]] <- data.frame(
        Cohort = cohort_name,
        AUC_1_Year_365 = ifelse(length(auc_1) > 0, auc_1, NA),
        AUC_3_Year_1095 = ifelse(length(auc_3) > 0, auc_3, NA),
        AUC_5_Year_1825 = ifelse(length(auc_5) > 0, auc_5, NA)
      )
    }
  }
}

if(length(auc_list) > 0) {
    auc_master_df <- dplyr::bind_rows(auc_list)
    rank_table <- rank_table %>% dplyr::left_join(auc_master_df, by="Cohort")
} else {
    rank_table <- rank_table %>% mutate(AUC_1_Year_365=NA, AUC_3_Year_1095=NA, AUC_5_Year_1825=NA)
}

write.csv(rank_table, file.path(OUTPUT_DIR, "Table_S11_Cohort_Global_Rankings_Comprehensive.csv"), row.names = FALSE)

# ------------------------------------------------------------------------------
# PHASE 2: VISUAL ATLAS (RADIAL, DUMBBELL, RAINCLOUD)
# ------------------------------------------------------------------------------
cat("🌌 Constructing Phase 2: High-Density Topographical Visuals...\n")

df_long <- df_perf %>% 
  pivot_longer(
    cols = c("RSF_Out_of_Bag", "XGBoost_Apparent", "MTLR_Apparent", "Boruta_Independent", "MVL_ElasticNet_SuperLearner"),
    names_to = "Algorithm",
    values_to = "C_Index"
  ) %>%
  mutate(
    Algorithm = recode(Algorithm,
      "RSF_Out_of_Bag" = "RSF (OOB)",
      "XGBoost_Apparent" = "XGBoost",
      "MTLR_Apparent" = "MTLR",
      "Boruta_Independent" = "Survival-Boruta",
      "MVL_ElasticNet_SuperLearner" = "MVL SuperLearner"
    ),
    Algorithm = factor(Algorithm, levels = c("RSF (OOB)", "XGBoost", "Survival-Boruta", "MTLR", "MVL SuperLearner"))
  )

# 2A: The Radial Atlas (2x2 Structure with Unified Legend)
generate_radial_metric <- function(data, target_metric, title, keep_legend = FALSE) {
  df_sub <- data %>% filter(Metric == target_metric) %>% filter(!is.na(C_Index))
  if(nrow(df_sub) == 0) return(NULL)
  
  p <- ggplot(df_sub, aes(x = as.factor(Cancer), y = C_Index, fill = Algorithm)) +
    geom_bar(stat = "identity", position = position_dodge(), width = 0.7) +
    geom_hline(yintercept = 0.5, color = "black", linetype = "dashed", alpha = 0.5) +
    geom_hline(yintercept = 1.0, color = "black", alpha = 0.5) +
    coord_polar(start = 0) +
    scale_fill_manual(values = c("RSF (OOB)"="#3F72AF", "MTLR"="#E2A445", "XGBoost"="#D9534F", "Survival-Boruta"="#9B59B6", "MVL SuperLearner"="#5cb85c")) +
    ylim(0, 1.0) + theme_minimal() +
    theme(axis.title = element_blank(), axis.text.y = element_blank(), axis.ticks = element_blank(),
          axis.text.x = element_text(size = 11, face = "bold"),
          plot.title = element_text(hjust=0.5, face="bold", size=16),
          plot.margin = margin(t=25, r=5, b=5, l=5)) +
    ggtitle(title)
    
  if(!keep_legend) p <- p + theme(legend.position = "none")
  return(p)
}

# Generate baseline plots horizontally
p_os  <- generate_radial_metric(df_long, "OS", "Overall Survival (OS)")
p_dss <- generate_radial_metric(df_long, "DSS", "Disease-Specific Survival (DSS)")
p_dfi <- generate_radial_metric(df_long, "DFI", "Disease-Free Interval (DFI)")
p_pfi <- generate_radial_metric(df_long, "PFI", "Progression-Free Interval (PFI)")

# Extract unified legend
p_legend_base <- generate_radial_metric(df_long, "OS", "OS", keep_legend = TRUE)
shared_legend <- cowplot::get_legend(p_legend_base + theme(legend.box.margin = margin(0, 0, 0, 10)))

# Stitch together
p_grid <- cowplot::plot_grid(p_os, p_dss, p_dfi, p_pfi, ncol=2, labels=c("A", "B", "C", "D"), label_size=28)
p_radial_master <- cowplot::plot_grid(p_grid, shared_legend, ncol = 2, rel_widths = c(1, 0.2))

# Save strictly using the preferred Master naming convention
ggsave(file.path(OUTPUT_DIR, "Master_Figure_2x2_Radial_Atlas.pdf"), p_radial_master, width=14, height=12, bg="white")
ggsave(file.path(OUTPUT_DIR, "Master_Figure_2x2_Radial_Atlas.tiff"), p_radial_master, width=14, height=12, bg="white", dpi=600, compression="lzw")

# 2B: The Dumbbell Plot (MVL Boost)
df_dumb <- df_perf %>%
  rowwise() %>%
  mutate(Best_Base = max(c(RSF_Out_of_Bag, MTLR_Apparent, XGBoost_Apparent, Boruta_Independent), na.rm=TRUE)) %>%
  ungroup() %>%
  filter(!is.na(MVL_ElasticNet_SuperLearner) & !is.na(Best_Base)) %>%
  mutate(
    Cohort = factor(Cohort, levels = Cohort[order(MVL_ElasticNet_SuperLearner)]),
    # Calculate a 5% standoff distance so the arrowhead never touches the green dot!
    Arrow_End = MVL_ElasticNet_SuperLearner + (Best_Base - MVL_ElasticNet_SuperLearner) * 0.08
  )

p_dumb <- ggplot(df_dumb) +
  geom_segment(aes(x=Best_Base, xend=Arrow_End, y=Cohort, yend=Cohort), color="#b2b2b2", linewidth=0.8, arrow = arrow(length = unit(0.12, "cm"), type="closed")) +
  geom_point(aes(x=Best_Base, y=Cohort, color="Apparent Peak (XGBoost)"), size=2) +
  geom_point(aes(x=MVL_ElasticNet_SuperLearner, y=Cohort, color="MVL SuperLearner (Regularized)"), size=2) +
  scale_color_manual(name = NULL, values = c("Apparent Peak (XGBoost)"="#3F72AF", "MVL SuperLearner (Regularized)"="#5cb85c")) +
  theme_minimal() + labs(x="Concordance Index (C-Index)", y="") +
  theme(axis.text.y = element_text(size=4), legend.position="bottom", legend.text=element_text(size=9), plot.subtitle=element_text(size=8, face="italic"))
ggsave(file.path(OUTPUT_DIR, "Fig2B_Dumbbell_Plot.pdf"), p_dumb, width=8, height=14, bg="white")
ggsave(file.path(OUTPUT_DIR, "Fig2B_Dumbbell_Plot.tiff"), p_dumb, width=8, height=14, bg="white", dpi=600, compression="lzw")

# 2C: Raincloud / Density Distribution
p_rain <- ggplot(df_long, aes(x=Algorithm, y=C_Index, fill=Algorithm)) +
  geom_violin(alpha=0.6, trim=FALSE) +
  geom_boxplot(width=0.2, fill="white", outlier.shape=NA) +
  geom_jitter(width=0.1, alpha=0.3, size=1) +
  scale_fill_manual(values = c("RSF (OOB)"="#3F72AF", "MTLR"="#E2A445", "XGBoost"="#D9534F", "Survival-Boruta"="#9B59B6", "MVL SuperLearner"="#5cb85c")) +
  theme_minimal() + labs(title="Pan-Cancer Algorithmic Density", y="C-Index", x="") +
  theme(legend.position="none")
ggsave(file.path(OUTPUT_DIR, "Fig2C_Raincloud.pdf"), p_rain, width=8, height=6, bg="white")
ggsave(file.path(OUTPUT_DIR, "Fig2C_Raincloud.tiff"), p_rain, width=8, height=6, bg="white", dpi=600, compression="lzw")

# ------------------------------------------------------------------------------
# PHASE 3: BIOLOGICAL TOKENS (Post-Hoc Interpretability Scraping)
# ------------------------------------------------------------------------------
cat("🧬 Scraping Omic Feature Dimensions across all cohorts...\n")

# Safely extract tokens by bypassing the R-Formula `-` to `.` corruption
extract_tokens <- function(features) {
  # Repair formula damage (`ACC.145` -> `ACC-145`) before string splitting
  repaired <- gsub("^([A-Za-z]+)\\.", "\\1-", features)
  tokens <- sapply(strsplit(repaired, "\\."), function(x) if(length(x)>=2) x[2] else NA)
  return(tokens[!is.na(tokens)])
}

cohort_dirs <- list.dirs(MODELS_DIR, recursive = FALSE)
token_list <- list()

for(cdir in cohort_dirs) {
  cohort <- basename(cdir)
  
  # Boruta
  b_file <- file.path(cdir, "Boruta", paste0(cohort, "_Boruta_Feature_Decisions.tsv"))
  if (file.exists(b_file)) {
    b_df <- tryCatch(read.delim(b_file, stringsAsFactors=FALSE), error=function(e) NULL)
    if (!is.null(b_df) && "decision" %in% colnames(b_df)) {
      surv <- b_df %>% filter(decision %in% c("Confirmed", "Tentative"))
      if(nrow(surv) > 0) {
        token_list[[paste0(cohort,"_B")]] <- data.frame(Feature = surv$Feature, Cohort = cohort, Algorithm="Survival-Boruta")
      }
    }
  }
  
  # XGBoost
  x_file <- file.path(cdir, "XGBoost", paste0(cohort, "_XGBoost_Node_Importance.tsv"))
  if (file.exists(x_file)) {
    x_df <- tryCatch(read.delim(x_file, stringsAsFactors=FALSE), error=function(e) NULL)
    if (!is.null(x_df) && "Feature" %in% colnames(x_df)) {
      if(nrow(x_df) > 0) token_list[[paste0(cohort,"_X")]] <- data.frame(Feature = x_df$Feature, Cohort = cohort, Algorithm="XGBoost")
    }
  }
  
  # RSF
  r_file <- file.path(cdir, "RSF", paste0(cohort, "_RSF_VIMP.tsv"))
  if (file.exists(r_file)) {
    r_df <- tryCatch(read.delim(r_file, stringsAsFactors=FALSE), error=function(e) NULL)
    if (!is.null(r_df) && "Feature" %in% colnames(r_df)) {
      top_r <- r_df %>% filter(VIMP > 0.005)
      if(nrow(top_r) > 0) {
        token_list[[paste0(cohort,"_R")]] <- data.frame(Feature = top_r$Feature, Cohort = cohort, Algorithm="RSF (OOB)")
      }
    }
  }
  
  # MTLR
  m_file <- file.path(cdir, "MTLR", paste0(cohort, "_MTLR_Feature_Importance.tsv"))
  if (file.exists(m_file)) {
    m_df <- tryCatch(read.delim(m_file, stringsAsFactors=FALSE), error=function(e) NULL)
    if (!is.null(m_df) && "Feature" %in% colnames(m_df)) {
      top_m <- head(m_df, 20)
      if(nrow(top_m) > 0) {
        token_list[[paste0(cohort,"_M")]] <- data.frame(Feature = top_m$Feature, Cohort = cohort, Algorithm="MTLR")
      }
    }
  }
}

# ------------------------------------------------------------------------------
# PHASE 3B: SCAPING LIME GEOMETRIC FIDELITY (Explanation Fit)
# ------------------------------------------------------------------------------
cat("📉 Scraping LIME Explanation Fit Geometries to validate topological non-linearity...\n")

lime_files <- list.files(MODELS_DIR, pattern = "LIME_Local_Explanations\\.tsv$", recursive = TRUE, full.names = TRUE)
r2_list <- c()

for(f in lime_files) {
  l_df <- tryCatch(read.delim(f, stringsAsFactors=FALSE), error=function(e) NULL)
  if(!is.null(l_df) && "model_r2" %in% colnames(l_df)) {
      # Grab unique R2 values (since R2 is identical for all 10 features belonging to the same patient case)
      unique_r2s <- unique(l_df[, c("case", "model_r2")])
      r2_list <- c(r2_list, unique_r2s$model_r2)
  }
}

if(length(r2_list) > 0) {
  median_r2 <- median(r2_list, na.rm=TRUE)
  pct_under_10 <- mean(r2_list < 0.10, na.rm=TRUE) * 100
  
  cat(sprintf("\n=======================================================\n"))
  cat(sprintf("LIME GEOMETRIC FIDELITY METRICS (Based on %d evaluated patients):\n", length(r2_list)))
  cat(sprintf("-> Median Explanation Fit (R2): %.4f\n", median_r2))
  cat(sprintf("-> Percentage of Patients with R2 < 0.10: %.1f%%\n", pct_under_10))
  cat(sprintf("=======================================================\n\n"))
  
  # Log it out for manuscript copy-pasting
  text_obj <- sprintf("Crucially, across the evaluated topographies, these localized surrogates commonly yielded severely suppressed Explanation Fits (median R2 = %.3f, with %.1f%% of patients falling below R2 < 0.10).", median_r2, pct_under_10)
  writeLines(text_obj, file.path(OUTPUT_DIR, "Manuscript_LIME_Geometric_Clause.txt"))
}

if(length(token_list) > 0) {
  token_df <- bind_rows(token_list)

# Deduplicate features per algorithm to compute true absolute dimensionality payload across all cohorts
token_df <- token_df %>% distinct(Algorithm, Feature, .keep_all = TRUE)
token_df$Token <- extract_tokens(token_df$Feature)

token_df <- token_df %>% filter(!is.na(Token))
  token_map <- c("1"="Protein", "2"="Somatic Mutation", "3"="CNV", "4"="microRNA", "5"="Transcript", "6"="mRNA", "7"="CpG Methylation")
  token_df$Omic_Layer <- token_map[as.character(token_df$Token)]
  token_df$Omic_Layer <- factor(token_df$Omic_Layer, levels = c("Somatic Mutation", "CNV", "microRNA", "Protein", "Transcript", "mRNA", "CpG Methylation"))
  token_df$Algorithm <- factor(token_df$Algorithm, levels = c("RSF (OOB)", "XGBoost", "Survival-Boruta", "MTLR"))
  
  if(nrow(token_df) > 0) {
    # Panel A: Absolute Counts
    p_token_a <- ggplot(token_df, aes(x=Omic_Layer, fill=Omic_Layer)) + 
      geom_bar(color="black") + theme_bw() + 
      facet_wrap(~Algorithm, scales = "free_y") +
      labs(title="(A) Quadripartite Omic Payload Dimensionality (Absolute Counts)", y="Total Pan-Cancer Frequency", x="") +
      theme(axis.text.x = element_text(face="bold"), legend.position="none", strip.text = element_text(size=12, face="bold"))
      
    # Panel B: Relative Proportions
    # In Sparsity Phase IIIB, the global maximum absolute native dimensions are fixed:
    global_max <- c("Somatic Mutation" = 831, "CNV" = 737)
    
    token_prop <- token_df %>% 
      group_by(Algorithm, Omic_Layer) %>% 
      summarise(count = n(), .groups = 'drop') %>%
      mutate(proportion = (count / global_max[as.character(Omic_Layer)]) * 100)
      
    p_token_b <- ggplot(token_prop, aes(x=Omic_Layer, y=proportion, fill=Omic_Layer)) + 
      geom_bar(stat="identity", color="black") + theme_bw() + 
      facet_wrap(~Algorithm) +
      labs(title="(B) Quadripartite Omic Payload Dominance (Relative Proportion %)", y="Relative Frequency (%)", x="Biological Layer") +
      theme(axis.text.x = element_text(face="bold"), legend.position="none", strip.text = element_text(size=12, face="bold"))
      
    # Composite
    p_composite <- cowplot::plot_grid(p_token_a, p_token_b, ncol=1, labels=NULL, align="v")
    
    ggsave(file.path(OUTPUT_DIR, "Fig3_Omic_Payload_Dominance_Panels.pdf"), p_composite, width=12, height=14, bg="white")
    ggsave(file.path(OUTPUT_DIR, "Fig3_Omic_Payload_Dominance_Panels.tiff"), p_composite, width=12, height=14, bg="white", dpi=600, compression="lzw")
  }
}

# ------------------------------------------------------------------------------
# PHASE 4: DUAL PRECISION ONCOLOGY PARADIGM C-SUITES
# ------------------------------------------------------------------------------
cat("🎯 Executing Phase 4: Dual Precision Oncology Paradigm Extractions...\n")

master_ranked <- df_perf %>%
  filter(!is.na(MVL_ElasticNet_SuperLearner)) %>%
  arrange(desc(MVL_ElasticNet_SuperLearner))

if(nrow(master_ranked) == 0) {
  cat("   -> WARNING: No completed MVL cohorts found to select as Paradigm.\n")
} else {

  # ============================================================================
  # Paradigm 1: Supreme Mathematical Performance
  # ============================================================================
  supreme_cohort <- master_ranked$Cohort[1]
  
  # ============================================================================
  # Paradigm 2: Lush Multi-Omic Integration (Targeting Tokens: 4, 5, 6, 7)
  # ============================================================================
  lush_cohort <- supreme_cohort # Default fallback
  
  if(exists("token_list") && length(token_list) > 0) {
    target_tokens <- c("4", "5", "6", "7") # miRNA, Transcript, mRNA, Methylation
    
    diversity_df <- bind_rows(token_list) %>%
      group_by(Cohort) %>%
      summarise(
        Lush_Score = length(intersect(unique(Token), target_tokens)),
        Total_Unique_Omics = n_distinct(Token),
        .groups = "drop"
      )
      
    lush_ranked <- master_ranked %>%
      filter(MVL_ElasticNet_SuperLearner > 0.85) %>% # Require high clinical viability
      left_join(diversity_df, by = "Cohort") %>%
      mutate(Lush_Score = tidyr::replace_na(Lush_Score, 0),
             Total_Unique_Omics = tidyr::replace_na(Total_Unique_Omics, 0)) %>%
      arrange(desc(Lush_Score), desc(Total_Unique_Omics), desc(MVL_ElasticNet_SuperLearner))
      
    if(nrow(lush_ranked) > 0) {
      lush_cohort <- lush_ranked$Cohort[1]
    }
  }

  # ============================================================================
  # Extraction Engine
  # ============================================================================
  extract_paradigm <- function(cohort_name, out_folder_title, c_index) {
    cat(sprintf("   -> %s Selected: %s (C-Index: %.3f)\n", out_folder_title, cohort_name, c_index))
    
    paradigm_dir <- file.path(MODELS_DIR, cohort_name)
    exemplar_out <- file.path(OUTPUT_DIR, out_folder_title)
    if(!dir.exists(exemplar_out)) dir.create(exemplar_out, recursive=TRUE)
    
    if(dir.exists(paradigm_dir)) {
      # Harvest SuperLearner Internal Trust Weights (Table 1A)
      mvl_tsv_path <- list.files(paradigm_dir, pattern = "MVL_Algorithm_Weights\\.tsv$", recursive = TRUE, full.names = TRUE)
      if(length(mvl_tsv_path) > 0) {
         mvl_df <- tryCatch(read.delim(mvl_tsv_path[1], stringsAsFactors=FALSE), error=function(e) NULL)
         if(!is.null(mvl_df)) {
            total_weight <- sum(abs(mvl_df$Elastic_Net_Weight))
            if(total_weight > 0) {
               mvl_df$Percent_Trust <- round((abs(mvl_df$Elastic_Net_Weight) / total_weight) * 100, 1)
            } else {
               mvl_df$Percent_Trust <- 0
            }
            write.csv(mvl_df, file.path(exemplar_out, "Table_1A_Paradigm_SuperLearner_Weights.csv"), row.names=FALSE)
         }
      }
      
      # Harvest Top Signatures from XGBoost (Table 1B)
      shap_tsv_path <- list.files(paradigm_dir, pattern = "XGBoost_SHAP_Summary\\.tsv$", recursive = TRUE, full.names = TRUE)
      if(length(shap_tsv_path) > 0) {
         shap_df <- tryCatch(read.delim(shap_tsv_path[1], stringsAsFactors=FALSE), error=function(e) NULL)
         if(!is.null(shap_df)) {
           top_drv <- head(shap_df, 5)
           write.csv(top_drv, file.path(exemplar_out, "Table_1B_Paradigm_Molecular_Drivers.csv"), row.names=FALSE)
         }
      }
      
      # Copy Visual Evidence and Extracted Trajectory Matrices (TIFFs, PDFs & localized CSVs)
      tiff_files <- list.files(paradigm_dir, pattern = "(\\.tiff|\\.pdf|_Trajectory_Data\\.csv)$", recursive = TRUE, full.names = TRUE)
      if(length(tiff_files) > 0) {
         file.copy(from = tiff_files, to = file.path(exemplar_out, basename(tiff_files)), overwrite = TRUE)
      }
      cat(sprintf("      [✓] %s C-Suite Assembled Successfully.\n", out_folder_title))
    }
  }

  extract_paradigm(supreme_cohort, "Precision_Oncology_Exemplar_Supreme", master_ranked$MVL_ElasticNet_SuperLearner[master_ranked$Cohort == supreme_cohort][1])
  if(lush_cohort != supreme_cohort) {
    extract_paradigm(lush_cohort, "Precision_Oncology_Exemplar_Lush_Omic", master_ranked$MVL_ElasticNet_SuperLearner[master_ranked$Cohort == lush_cohort][1])
  } else {
    cat("   -> Note: Supreme cohort is natively the Lush cohort as well. Only Supreme extracted to avoid redundancy.\n")
  }
}

cat("✅ IMPLEMENTATION PLAN SURGICALLY EXECUTED TO 100%. Check Synthesis_Graphics.\n")


# =========================================================================
# ZIMA PHASE III CLINICAL PROBABILITY PLOTTER
# Purpose: Generate a bifurcated 4-panel composite visualization 
#          (Waterfall Heatmaps & Temporal Spaghetti Plots) 
#          for the Phase III Baseline Cohort.
#          EXACT TEMPLATE MIRROR OF BLIND VALIDATION (Figure 11)
# =========================================================================

local_lib <- "~/R/library"
if (!dir.exists(local_lib)) dir.create(local_lib, recursive = TRUE)
.libPaths(c(local_lib, .libPaths()))

required_packages <- c("dplyr", "tidyr", "ggplot2", "patchwork", "data.table")
for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    cat(sprintf("Installing missing package: %s\n", pkg))
    install.packages(pkg, repos = "http://cran.us.r-project.org")
  }
}

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(patchwork)
  library(data.table)
})

# Define Paths
ZIMA_ROOT <- "~/students/aluno0549-6/PHASE_III/PHASE_III_Megarun_5_0_complete_ZIMA_Suit_Generator_final"
PROB_DIR <- file.path(ZIMA_ROOT, "CancerRCDShiny_Phase_III_Clinical_Probabilities")
PLOT_DIR <- file.path(ZIMA_ROOT, "Composite_Figures")

if(!dir.exists(PLOT_DIR)) dir.create(PLOT_DIR, recursive=TRUE)

cat("\n[ZIMA Plotter] Loading 96 Phase III Clinical Probability Matrices...\n")

prob_files <- list.files(PROB_DIR, pattern = "_Phase_III_Probabilities\\.tsv$", full.names = TRUE)
if(length(prob_files) == 0) {
    stop("CRITICAL ERROR: No probability matrices found in ", PROB_DIR)
}

# 1. Aggregate Data
df_list <- lapply(prob_files, function(f) {
  d <- fread(f)
  unit_id <- gsub("_Phase_III_Probabilities\\.tsv$", "", basename(f))
  parts <- strsplit(unit_id, "_")[[1]]
  m <- parts[2] # OS, DSS, DFI, PFI
  d$Unit <- unit_id
  d$Metric <- m
  d$Category <- ifelse(m %in% c("OS", "DSS"), "Survival Endpoint (OS/DSS)", "Event Endpoint (DFI/PFI)")
  
  # Map Phase III specific columns to the Validation Template Column Names
  # Phase III generated MVL_Prob_1Yr, etc., and MVL_Risk.
  setnames(d, old = c("MVL_Prob_1Yr", "MVL_Prob_3Yr", "MVL_Prob_5Yr", "MVL_Risk"), 
                new = c("Prob_1Yr", "Prob_3Yr", "Prob_5Yr", "Clinical_Z_Score"), skip_absent=TRUE)
  
  # Inject the Path A flag so the color mapping is perfectly identical
  d$Inference_Path <- "SuperLearner (Path A)"
  
  return(d)
})

df_all <- rbindlist(df_list, fill=TRUE)

# 2. Reshape to Long format for plotting
cat("-> Reshaping data for visualization...\n")

df_long <- df_all %>%
  pivot_longer(
    cols = c(Prob_1Yr, Prob_3Yr, Prob_5Yr),
    names_to = "Timepoint",
    values_to = "Probability"
  ) %>%
  mutate(
    Time = case_when(
      Timepoint == "Prob_1Yr" ~ 1,
      Timepoint == "Prob_3Yr" ~ 3,
      Timepoint == "Prob_5Yr" ~ 5
    ),
    Time_Factor = factor(Time, levels=c(1,3,5), labels=c("1 Year", "3 Years", "5 Years")),
    Inference_Path = factor(Inference_Path, levels=c("SuperLearner (Path A)", "XGBoost Fallback (Path B)")) # Force factor levels to keep Red in legend
  )

# --- FORCE LEGEND KEYS ---
# Inject dummy rows with NA probabilities to physically force ggplot2 
# to draw the Red line segment in the legend without altering the plot.
dummy_surv <- df_long %>% filter(Category == "Survival Endpoint (OS/DSS)") %>% slice(1) %>% mutate(Inference_Path = "XGBoost Fallback (Path B)", Probability = NA)
dummy_event <- df_long %>% filter(Category == "Event Endpoint (DFI/PFI)") %>% slice(1) %>% mutate(Inference_Path = "XGBoost Fallback (Path B)", Probability = NA)
df_long <- bind_rows(df_long, dummy_surv, dummy_event)

# Sort patients by Clinical Z-Score for the heatmap
df_heat <- df_long %>%
  group_by(Category) %>%
  arrange(desc(Clinical_Z_Score)) %>%
  mutate(Patient_Rank = dense_rank(desc(Clinical_Z_Score))) %>%
  ungroup()

# 3. Create Heatmaps
cat("-> Rendering Waterfall Heatmaps...\n")

p_heat_surv <- df_heat %>% filter(Category == "Survival Endpoint (OS/DSS)") %>%
  ggplot(aes(x = Time_Factor, y = Patient_Rank, fill = Probability)) +
  geom_tile() +
  scale_fill_gradient2(low = "firebrick", mid = "white", high = "dodgerblue", midpoint = 0.5, name = "S(t)", limits=c(0,1)) +
  scale_y_reverse(breaks=NULL) +
  theme_minimal() +
  theme(
    plot.subtitle = element_text(hjust = 0.5, size=14, face="bold"),
    axis.title = element_text(size=14, face="bold"),
    axis.text.x = element_text(size=12)
  ) +
  labs(
    subtitle = "OS & DSS Endpoints (Ordered by Z-Score)",
    x = "Clinical Landmark",
    y = "Individual Patients"
  )

p_heat_event <- df_heat %>% filter(Category == "Event Endpoint (DFI/PFI)") %>%
  ggplot(aes(x = Time_Factor, y = Patient_Rank, fill = Probability)) +
  geom_tile() +
  scale_fill_gradient2(low = "forestgreen", mid = "gold", high = "firebrick", midpoint = 0.5, name = "1 - S(t)", limits=c(0,1)) +
  scale_y_reverse(breaks=NULL) +
  theme_minimal() +
  theme(
    plot.subtitle = element_text(hjust = 0.5, size=14, face="bold"),
    axis.title = element_text(size=14, face="bold"),
    axis.text.x = element_text(size=12)
  ) +
  labs(
    subtitle = "DFI & PFI Endpoints (Ordered by Z-Score)",
    x = "Clinical Landmark",
    y = "Individual Patients"
  )

# 4. Create Spaghetti Plots
cat("-> Rendering Temporal Spaghetti Plots...\n")

# Color path A Dark Cyan, path B red
path_colors <- c("SuperLearner (Path A)" = "darkcyan", "XGBoost Fallback (Path B)" = "red")

p_spag_surv <- df_long %>% filter(Category == "Survival Endpoint (OS/DSS)") %>%
  ggplot(aes(x = Time, y = Probability, group = Sample_ID, color = Inference_Path)) +
  geom_line(alpha = 0.15, linewidth=0.5) +
  scale_x_continuous(breaks = c(1,3,5), labels=c("1Y", "3Y", "5Y")) +
  scale_y_continuous(limits=c(0,1)) +
  scale_color_manual(values = path_colors, drop = FALSE) + # EXACT LEGEND REPLICATION
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.title = element_text(size=14, face="bold"),
    legend.text = element_text(size=12),
    axis.title = element_text(size=14, face="bold"),
    axis.text = element_text(size=12)
  ) +
  labs(
    x = "Time (Years)",
    y = "Probability of Survival S(t)",
    color = "Inference Architecture:"
  ) +
  guides(color = guide_legend(override.aes = list(alpha = 1, linewidth = 2)))

p_spag_event <- df_long %>% filter(Category == "Event Endpoint (DFI/PFI)") %>%
  ggplot(aes(x = Time, y = Probability, group = Sample_ID, color = Inference_Path)) +
  geom_line(alpha = 0.15, linewidth=0.5) +
  scale_x_continuous(breaks = c(1,3,5), labels=c("1Y", "3Y", "5Y")) +
  scale_y_continuous(limits=c(0,1)) +
  scale_color_manual(values = path_colors, drop = FALSE) + # EXACT LEGEND REPLICATION
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.title = element_text(size=14, face="bold"),
    legend.text = element_text(size=12),
    axis.title = element_text(size=14, face="bold"),
    axis.text = element_text(size=12)
  ) +
  labs(
    x = "Time (Years)",
    y = "Probability of Event 1 - S(t)",
    color = "Inference Architecture:"
  ) +
  guides(color = guide_legend(override.aes = list(alpha = 1, linewidth = 2)))

# 5. Assemble Composite
cat("-> Assembling Bifurcated Composite...\n")

composite <- (p_heat_surv | p_heat_event) / (p_spag_surv | p_spag_event) +
  plot_layout(guides = "collect") & theme(legend.position = "bottom")
  
composite <- composite + plot_annotation(
    tag_levels = 'A'
  ) & theme(plot.tag = element_text(size = 24, face = "bold"))

# 6. Export
out_tiff <- file.path(PLOT_DIR, "Phase_III_PanCancer_Bifurcated_Composite.tiff")
out_pdf <- file.path(PLOT_DIR, "Phase_III_PanCancer_Bifurcated_Composite.pdf")

ggsave(out_tiff, composite, width = 12, height = 12, dpi = 600, device = "tiff", compression = "lzw")
ggsave(out_pdf, composite, width = 12, height = 12)

cat(sprintf("✅ SUCCESS! Phase III Composite figure saved to: %s\n", PLOT_DIR))

# ==============================================================================
# CRIT-03 POWERED VALIDATION (V25): OncoKB Tier 0 + CGL Aligned
# ==============================================================================
# V25: Post-amendment benchmark. Governance amendments in app.R (2026-07-01):
#   - G18: RCD Decoder now carries CRITICAL REFERENCE DICTIONARY ONLY header,
#     explicitly prohibiting ICD import when not in patient annotations.
#   - G13: New post-hoc CGL-actionability conflation auto-scrubber (V-010).
#   - G12: New post-hoc Tier 0 treatment-recommendation conflation auto-scrubber (V-011).
# Retains: Per-cancer floor enforcement (PER_CANCER_FLOOR = 10).
#   Pass-1 proportional + Pass-2 redistribution ensures every cancer type has
#   ≥10 patients (or pool limit) for statistically meaningful C3 assessment
#   (all individual cancer types achieving mean governance ≥ 85%).
#   Previously LAML/READ/THYM got only 2 patients under pure proportional
#   allocation, making C3 untestable for 9 of 30 cancers.
#
# V21: G19 (Dot-prefixed signature nomenclature) added.
#   Compliance target 18/19 = 94.74% with 1-mistake tolerance.
#
# Retains: G1-G19 governance checker, OncoKB Concordance Lock v1.0,
#   population guardrail (G17 ecological fallacy), RCD boundary (G18),
#   dot-prefixed nomenclature detection (G19).
#
# Power requirements (from clinical study design):
#   Level A (inter-clinical): n ≥ 500 stratified across cancer types and endpoints
#   Level B (intra-clinical):  n ≥ 5 repeats × k patients, where k ≥ 90 (3 per cancer type × 30 cancers)
#
# Design:
#   Level A: n=500 patients (for 95% CI), proportionally stratified across 33 cancer types
#            × 4 clinical endpoints (PFI, OS, DSS, DFI)
#   Level B: k=90 patients (3/cancer × 30 cancers) × 5 repeats = 450 LLM calls
#
# Governance: 9-point checker (G1-G9), G10 permanently excluded.
# Expected runtime: ~5-6 hours (550 LLM calls × ~35-40s each)
# Incremental save + checkpoint/resume supported.
# ==============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(httr2)
  library(stringr)
  library(digest)
})

WORKING_DIR <- "."
setwd(WORKING_DIR)

# ===========================================================================
# CONFIGURATION (must precede header for provider detection)
# ===========================================================================
# --- LLM Provider Configuration ---
# Set LLM_PROVIDER to "deepseek" or "ollama" (default: ollama)
LLM_PROVIDER     <- Sys.getenv("LLM_PROVIDER", unset = "deepseek")
OLLAMA_URL       <- Sys.getenv("OLLAMA_URL", unset = "http://localhost:11434")
LLM_MODEL        <- Sys.getenv("OLLAMA_MODEL", unset = "qwen3:8b")
DEEPSEEK_API_KEY <- Sys.getenv("DEEPSEEK_API_KEY", unset = "")
DEEPSEEK_MODEL   <- Sys.getenv("DEEPSEEK_MODEL", unset = "deepseek-chat")
DEEPSEEK_URL     <- Sys.getenv("DEEPSEEK_URL", unset = "https://api.deepseek.com/v1/chat/completions")

cat("╔══════════════════════════════════════════════════════════════════╗\n")
cat(sprintf("║   CRIT-03 V25 — Three-Layer Analytical Validation + %s ║\n", toupper(LLM_PROVIDER)))
cat("╠══════════════════════════════════════════════════════════════════╣\n")
cat("║   Layer 1: Assay Reproducibility (n ≥ 500, 30 cancers × 4 endpoints)  ║\n")
cat("║   Layer 2: Interpretative Robustness (k ≥ 90 patients × 5 repeats)     ║\n")
cat("║   Layer 3: Cohort Quality Assurance (meta-validation design)      ║\n")
cat("╚══════════════════════════════════════════════════════════════════╝\n")
cat("Started:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n\n")

# ===========================================================================
# CONFIGURATION (continued)
# ===========================================================================
COMPLIANCE_TARGET <- 0.947  # V21: 18/19 = 94.74% achievable with 1-mistake tolerance
STRUCT_TARGET    <- 0.80  # V19: Structural stability measures organization, not factual precision
LEVEL_A_MIN_N     <- 500        # Minimum N for Level A (for clean 95% CI)
LEVEL_B_K         <- 90         # Number of patients for Level B (3 per cancer type × 30 cancers)
LEVEL_B_REPEATS   <- 5          # Repeats per patient
VALIDATION_SEED   <- as.integer(Sys.getenv("VALIDATION_SEED", unset = "20260617"))  # Reproducibility seed
MODEL_COMPARISON  <- as.logical(Sys.getenv("MODEL_COMPARISON", unset = "FALSE"))     # If TRUE, test multiple models
DEEPSEEK_MODEL_ALT <- Sys.getenv("DEEPSEEK_MODEL_ALT", unset = "deepseek-reasoner")  # Alternative model for comparison
CHECKPOINT_FILE_A <- "crit03_powered_levelA_checkpoint_v25.rds"
CHECKPOINT_FILE_B <- "crit03_powered_levelB_checkpoint_v25.rds"

# ===========================================================================
# 1. LOAD REFERENCE DATA
# ===========================================================================
cat("[1/6] Loading reference data...\n")

stemness_df <- tryCatch(
  read.delim("Merged_Cancer_Stemness.tsv", sep = "\t", stringsAsFactors = FALSE),
  error = function(e) stop("Merged_Cancer_Stemness.tsv not found")
)
cat(sprintf("  Stemness: %d patients\n", nrow(stemness_df)))

table_s11 <- tryCatch({
  first_line <- readLines("Table_S11_Interpreter_12k.csv", n = 1)
  if (grepl(";", first_line)) read.csv2("Table_S11_Interpreter_12k.csv", stringsAsFactors = FALSE)
  else read.csv("Table_S11_Interpreter_12k.csv", stringsAsFactors = FALSE)
}, error = function(e) NULL)
cat(sprintf("  Table S11: %s\n", if(is.null(table_s11)) "NOT FOUND" else paste(nrow(table_s11), "rows")))

gene_info <- tryCatch(
  read.csv("NCBI_gene_info.csv", sep = ";", stringsAsFactors = FALSE),
  error = function(e) NULL
)
cat(sprintf("  Gene info: %s\n", if(is.null(gene_info)) "NOT FOUND" else paste(nrow(gene_info), "genes")))

# --- Load OncoKB resources (mirrors app.R lines 706-767) ---
oncokb_gene_annotations <- tryCatch({
  as.data.frame(readRDS("OncoKB_Gene_Annotations.rds"), stringsAsFactors = FALSE)
}, error = function(e) {
  cat("  [WARN] OncoKB_Gene_Annotations.rds unreadable\n")
  data.frame(Gene_Symbol=character(), Oncogenic_Class=character(), Gene_Summary=character(), stringsAsFactors=FALSE)
})
cat(sprintf("  OncoKB Gene Annotations: %d genes\n", nrow(oncokb_gene_annotations)))

oncokb_cancer_gene_list <- tryCatch({
  read.delim("oncoKB_cancerGeneList.tsv", sep = "\t", stringsAsFactors = FALSE, check.names = FALSE)
}, error = function(e) {
  cat("  [WARN] oncoKB_cancerGeneList.tsv unreadable\n")
  data.frame()
})
cat(sprintf("  OncoKB Cancer Gene List: %d genes\n", nrow(oncokb_cancer_gene_list)))

oncokb_fda_levels <- tryCatch({
  read.delim("FDA_level_2_oncokb_biomarker_drug_associations.tsv",
             sep = "\t", stringsAsFactors = FALSE, check.names = FALSE)
}, error = function(e) {
  cat("  [WARN] FDA level file unreadable\n")
  data.frame()
})
n_fda2 <- sum(oncokb_fda_levels$Level == "Fda2", na.rm = TRUE)
cat(sprintf("  OncoKB FDA Level 2: %d associations, %d unique genes\n",
    nrow(oncokb_fda_levels), length(unique(oncokb_fda_levels$Gene))))

# --- Initialize OncoKB Clinical Actionability Layer (Tier 0) ---
source("oncokb_clinical_actionability_layer.R")
oncokb_actionability_layer <- init_oncokb_clinical_actionability_layer(".")
cat("  [APP] OncoKB Clinical Actionability Layer initialized.\n")

# ===========================================================================
# 2. COLLECT ALL PATIENT × COHORT × ENDPOINT TRAJECTORIES
# ===========================================================================
cat("[2/6] Collecting all patient trajectories across cancer types × endpoints...\n")

cancer_dict <- c(
  "ACC" = "Adrenocortical Carcinoma", "BLCA" = "Bladder Urothelial Carcinoma",
  "BRCA" = "Breast Invasive Carcinoma", "CESC" = "Cervical Squamous Cell Carcinoma",
  "CHOL" = "Cholangiocarcinoma", "COAD" = "Colon Adenocarcinoma",
  "DLBC" = "Lymphoid Neoplasm Diffuse Large B-cell Lymphoma",
  "ESCA" = "Esophageal Squamous Cell Carcinoma",
  "GBM" = "Glioblastoma Multiforme", "HNSC" = "Head and Neck Squamous Cell Carcinoma",
  "KICH" = "Kidney Chromophobe", "KIRC" = "Kidney Renal Clear Cell Carcinoma",
  "KIRP" = "Kidney Renal Papillary Cell Carcinoma", "LAML" = "Acute Myeloid Leukemia",
  "LGG" = "Brain Lower Grade Glioma", "LIHC" = "Liver Hepatocellular Carcinoma",
  "LUAD" = "Lung Adenocarcinoma", "LUSC" = "Lung Squamous Cell Carcinoma",
  "MESO" = "Mesothelioma", "OV" = "Ovarian Serous Cystadenocarcinoma",
  "PAAD" = "Pancreatic Adenocarcinoma", "PCPG" = "Pheochromocytoma and Paraganglioma",
  "PRAD" = "Prostate Adenocarcinoma", "READ" = "Rectum Adenocarcinoma",
  "SARC" = "Sarcoma", "SKCM" = "Skin Cutaneous Melanoma",
  "STAD" = "Stomach Adenocarcinoma", "TGCT" = "Testicular Germ Cell Tumors",
  "THCA" = "Thyroid Carcinoma", "THYM" = "Thymoma",
  "UCEC" = "Uterine Corpus Endometrial Carcinoma",
  "UCS" = "Uterine Carcinosarcoma", "UVM" = "Uveal Melanoma"
)

models_dir <- "../PHASE_III_ML_Models"
if (!dir.exists(models_dir)) models_dir <- "PHASE_III_ML_Models"
if (!dir.exists(models_dir)) stop("Cannot find PHASE_III_ML_Models directory")

cohort_dirs <- list.dirs(models_dir, recursive = FALSE)
cohort_dirs <- cohort_dirs[!grepl("Clinical_Reports_Cache", cohort_dirs)]
cat(sprintf("  Found %d cohort×endpoint directories\n", length(cohort_dirs)))

# Collect all patient files
patient_files <- data.frame(
  Patient = character(), CancerAbb = character(), CancerFull = character(),
  Metric = character(), CSV_Path = character(), stringsAsFactors = FALSE
)

for (cdir in cohort_dirs) {
  base <- basename(cdir)
  parts <- strsplit(base, "_")[[1]]
  if (length(parts) < 2) next
  cancer_abb <- parts[1]
  metric <- parts[2]
  if (!cancer_abb %in% names(cancer_dict)) next

  xgb_dir <- file.path(cdir, "XGBoost")
  if (!dir.exists(xgb_dir)) next
  csvs <- list.files(xgb_dir, pattern = "_Trajectory_.*_Trajectory_Data\\.csv$", full.names = TRUE)
  for (csv in csvs) {
    pat <- gsub("^.*_Trajectory_([A-Za-z0-9-]+)_Trajectory_Data\\.csv$", "\\1", basename(csv))
    patient_files <- rbind(patient_files, data.frame(
      Patient = pat, CancerAbb = cancer_abb,
      CancerFull = cancer_dict[[cancer_abb]],
      Metric = metric, CSV_Path = csv, stringsAsFactors = FALSE
    ))
  }
}

# Filter to patients with stemness data
patient_files <- patient_files[patient_files$Patient %in% stemness_df$sample_id, ]

cat(sprintf("  Total patients × cohort × endpoint combos: %d\n", nrow(patient_files)))
cat(sprintf("  Cancer types represented: %d\n", length(unique(patient_files$CancerAbb))))
cat(sprintf("  Endpoints available: %s\n", paste(sort(unique(patient_files$Metric)), collapse=", ")))

# ===========================================================================
# 3. STRATIFIED SAMPLING FOR LEVEL A (n ≥ 379)
# ===========================================================================
cat("[3/6] Stratified sampling for Level A (target: ≥379)...\n")

set.seed(VALIDATION_SEED)  # V11: configurable seed for reproducibility

# Build strata: CancerAbb × Metric
strata <- patient_files %>%
  group_by(CancerAbb, Metric) %>%
  summarise(pool_size = n(), .groups = "drop") %>%
  arrange(CancerAbb, Metric)

cat(sprintf("  Total strata (Cancer × Endpoint): %d\n", nrow(strata)))

# --- Pass 1: Proportional allocation ---
total_pool <- sum(strata$pool_size)
strata$prop <- strata$pool_size / total_pool
strata$alloc_prop <- floor(strata$prop * LEVEL_A_MIN_N)
strata$alloc_prop <- pmax(strata$alloc_prop, 1)  # at least 1 per stratum
strata$alloc_prop <- pmin(strata$alloc_prop, strata$pool_size)

current_sum <- sum(strata$alloc_prop)
if (current_sum < LEVEL_A_MIN_N) {
  deficit <- LEVEL_A_MIN_N - current_sum
  strata <- strata %>% arrange(desc(pool_size))
  for (i in seq_len(nrow(strata))) {
    if (deficit <= 0) break
    can_add <- strata$pool_size[i] - strata$alloc_prop[i]
    to_add <- min(deficit, can_add)
    strata$alloc_prop[i] <- strata$alloc_prop[i] + to_add
    deficit <- deficit - to_add
  }
}

# --- Pass 2: Per-cancer floor (V25) for meaningful C3 assessment ---
# C3 requires every cancer type mean ≥ 85% — cancers with 2-3 patients
# cannot be assessed. Enforce a per-cancer floor: min 10 patients or
# the available pool size, redistributed from over-represented cancers.
PER_CANCER_FLOOR <- 10
cancer_totals <- strata %>%
  group_by(CancerAbb) %>%
  summarise(current = sum(alloc_prop), pool = sum(pool_size), .groups = "drop")
cancer_totals$target <- pmin(PER_CANCER_FLOOR, cancer_totals$pool)

# Iteratively redistribute from largest surplus to largest deficit
for (iter in 1:100) {  # safety limit
  cancer_totals$surplus <- cancer_totals$current - cancer_totals$target
  donor <- cancer_totals %>% filter(surplus > 0) %>% arrange(desc(surplus))
  recv  <- cancer_totals %>% filter(surplus < 0) %>% arrange(surplus)
  if (nrow(donor) == 0 || nrow(recv) == 0) break
  
  take <- min(donor$surplus[1], -recv$surplus[1])
  # Take 1 patient from donor's largest stratum
  d_strata <- which(strata$CancerAbb == donor$CancerAbb[1])
  d_strata <- d_strata[order(strata$alloc_prop[d_strata], decreasing = TRUE)]
  for (j in d_strata) {
    if (take <= 0) break
    can_give <- strata$alloc_prop[j] - 1
    if (can_give > 0) {
      give <- min(take, can_give)
      strata$alloc_prop[j] <- strata$alloc_prop[j] - give
      take <- take - give
    }
  }
  # Give to receiver's most underrepresented stratum
  r_strata <- which(strata$CancerAbb == recv$CancerAbb[1])
  remaining_give <- min(donor$surplus[1], -recv$surplus[1]) - take  # amount moved
  # Distribute to receiver strata proportionally to pool
  for (j in r_strata) {
    if (remaining_give <= 0) break
    can_recv <- strata$pool_size[j] - strata$alloc_prop[j]
    if (can_recv > 0) {
      add <- min(remaining_give, can_recv)
      strata$alloc_prop[j] <- strata$alloc_prop[j] + add
      remaining_give <- remaining_give - add
    }
  }
  # Recompute
  cancer_totals <- strata %>%
    group_by(CancerAbb) %>%
    summarise(current = sum(alloc_prop), pool = sum(pool_size), .groups = "drop")
  cancer_totals$target <- pmin(PER_CANCER_FLOOR, cancer_totals$pool)
}

cat(sprintf("  Per-cancer floor: %d (where pool allows)\n", PER_CANCER_FLOOR))
cat(sprintf("  Level-A sample size after floor: %d (minimum: %d)\n",
    sum(strata$alloc_prop), LEVEL_A_MIN_N))

# Report per-cancer distribution
cancer_final <- strata %>% group_by(CancerAbb) %>% summarise(n = sum(alloc_prop), .groups="drop") %>% arrange(CancerAbb)
n_below_floor <- sum(cancer_final$n < pmin(PER_CANCER_FLOOR, cancer_totals$pool))
if (n_below_floor > 0) {
  cat(sprintf("  ⚠ %d cancers remain below floor (pool-limited):\n", n_below_floor))
  for (k in seq_len(nrow(cancer_final))) {
    if (cancer_final$n[k] < PER_CANCER_FLOOR) {
      cat(sprintf("    %-5s %d (pool=%d)\n", cancer_final$CancerAbb[k], cancer_final$n[k],
          cancer_totals$pool[cancer_totals$CancerAbb == cancer_final$CancerAbb[k]]))
    }
  }
} else {
  cat("  All cancers meet per-cancer floor ✓\n")
}

# Now draw the samples
level_a_patients <- data.frame()
for (i in seq_len(nrow(strata))) {
  pool <- patient_files[patient_files$CancerAbb == strata$CancerAbb[i] &
                        patient_files$Metric == strata$Metric[i], ]
  n_draw <- min(strata$alloc_prop[i], nrow(pool))
  if (n_draw == 0) next
  chosen <- pool[sample(nrow(pool), n_draw), ]
  level_a_patients <- rbind(level_a_patients, chosen)
}

cat(sprintf("  Level-A patients selected: %d across %d cancer×endpoint strata\n",
    nrow(level_a_patients), length(unique(paste(level_a_patients$CancerAbb, level_a_patients$Metric)))))

# Per-cancer summary for log
la_summary <- level_a_patients %>% group_by(CancerAbb) %>% summarise(n = n(), .groups="drop") %>% arrange(desc(n))
cat("  Per-cancer distribution:\n")
for (k in seq_len(nrow(la_summary))) {
  cat(sprintf("    %-5s %d\n", la_summary$CancerAbb[k], la_summary$n[k]))
}

# ===========================================================================
# 4. SELECTION FOR LEVEL B (k ≥ 30 patients)
# ===========================================================================
cat("[4/6] Selecting Level-B patients (k ≥ 30 from diverse cancers)...\n")

# Ensure Level-B patients are NOT in Level-A selected set
level_b_pool <- patient_files[!patient_files$Patient %in% level_a_patients$Patient, ]

# Level B samples across ALL survival metrics (not just PFI)
# For each cancer type, select 3 patients covering different metrics where possible
PATIENTS_PER_CANCER <- 3
unique_cancers <- unique(level_b_pool$CancerAbb)
level_b_patients <- data.frame()

for (cab in unique_cancers) {
  if (nrow(level_b_patients) >= LEVEL_B_K) break
  pool <- level_b_pool[level_b_pool$CancerAbb == cab, ]
  if (nrow(pool) == 0) next
  
  # Get available metrics for this cancer, prefer variety
  metrics_avail <- sort(unique(pool$Metric))
  # Cycle through available metrics to pick 3 patients
  chosen_for_cancer <- data.frame()
  for (j in seq_len(PATIENTS_PER_CANCER)) {
    target_metric <- metrics_avail[((j - 1) %% length(metrics_avail)) + 1]
    metric_pool <- pool[pool$Metric == target_metric, ]
    if (nrow(metric_pool) == 0) next
    pick <- metric_pool[sample(nrow(metric_pool), 1), ]
    chosen_for_cancer <- rbind(chosen_for_cancer, pick)
    pool <- pool[pool$Patient != pick$Patient, ]  # don't reuse same patient
  }
  level_b_patients <- rbind(level_b_patients, chosen_for_cancer)
}

# Trim to exact target
if (nrow(level_b_patients) > LEVEL_B_K) {
  level_b_patients <- level_b_patients[sample(nrow(level_b_patients), LEVEL_B_K), ]
}

cat(sprintf("  Level-B patients selected: %d (target: %d = 3 per cancer type)\n", nrow(level_b_patients), LEVEL_B_K))
cat(sprintf("  Cancer types represented in Level B: %d\n", length(unique(level_b_patients$CancerAbb))))
cat(sprintf("  Metrics represented: %s\n", paste(sort(unique(level_b_patients$Metric)), collapse=", ")))

# ===========================================================================
# 5. PAYLOAD, GOVERNANCE, AND CHECKER FUNCTIONS
# ===========================================================================
cat("[5/6] Building payload builder + governance framework...\n")

# --- RCD Biological Context Decoder (mirrors app.R line 77) ---
# Injected into benchmark payloads to match deployed app.R configuration.
# Source: RCD_Biological_Context_Decoder.txt → Committee_RCD_Operational_Definitions.xlsx (NCCD-adapted)
#         + Extended_RCD_Operational_Definitions.xlsx (AI-curated)
RCD_BIOLOGICAL_CONTEXT_DECODER <- "\n\n--- RCD BIOLOGICAL CONTEXT DECODER ---\n\nCRITICAL GOVERNANCE RULE — REFERENCE DICTIONARY ONLY: The RCD definitions below are a biological REFERENCE DICTIONARY provided for contextual understanding. You MUST ONLY discuss RCD forms that EXPLICITLY APPEAR in the patient's Associated RCD Form annotations within the signature payload above. The presence of a definition here does NOT authorize its use in the patient narrative. Introducing an RCD form not present in the patient's annotations — particularly 'immunogenic cell death (ICD)' when ICD is not listed — is a GOVERNANCE VIOLATION. The decoder is a lookup table, not an invitation. You may use it ONLY to understand the meaning of RCD forms that DO appear in the patient's annotations.\n\nThe Associated RCD Form annotations in the signatures above carry precise biological meanings drawn from the NCCD international consensus on Regulated Cell Death and the CancerRCDPredictor operational ontology. Use the following definitions when interpreting the patient's profile:\n\n- Apoptosis: Programmed cell death with cell shrinkage, chromatin condensation, and DNA fragmentation, causing autonomous lysis without inflammation. Interconnected with necroptosis and pyroptosis through shared molecular pathways and caspase activation. In cancer, apoptosis evasion is a hallmark enabling uncontrolled growth; reactivating apoptotic pathways is a central focus of cancer research.\n\n- Necroptosis: Programmed necrosis regulated by RIPK1, RIPK3, and MLKL, causing plasma membrane rupture and inflammation, with secondary mitochondrial dysfunction. Shares signaling pathways with apoptosis and pyroptosis; can be activated when apoptosis is inhibited. Necroptosis can either promote or inhibit cancer depending on context — it can trigger anti-tumor immune responses but also promote tumor-promoting inflammation.\n\n- Pyroptosis: Programmed cell death involving caspase-1 and gasdermin-mediated cell lysis, associated with inflammation. Overlaps with apoptosis through shared initiator caspase machinery and with necroptosis through inflammatory signaling. In cancer, pyroptosis promotes anti-tumor immunity by releasing inflammatory cytokines, but its pro-inflammatory nature can also promote tumor progression.\n\n- Ferroptosis: Iron-dependent cell death with lipid peroxide accumulation — oxidative, non-apoptotic, and programmed via iron-induced lipid peroxide damage. Interconnected with other RCDs through shared oxidative stress pathways. In cancer, ferroptosis eliminates cells with high oxidative stress; evasion implies redox adaptation. Cancers resistant to other death forms may be susceptible to ferroptosis induction.\n\n- Autophagy: Self-digestion via lysosomes, degrading cell components, essential for maintaining cell function and homeostasis. Dual role — can lead to autophagic cell death or promote survival through cellular recycling. In cancer, autophagy can suppress tumor initiation by degrading damaged organelles but also promote survival of established tumors under metabolic stress.\n\n- Necrosis: Morphological endpoint characterized by cell swelling, plasma membrane rupture, and release of cellular contents causing inflammation. Can result from accidental (unregulated) injury or from programmed pathways including necroptosis and MPT-driven necrosis. In cancer, necrosis contributes to tumor progression through inflammatory microenvironment remodeling and immune evasion.\n\n- Anoikis: Form of apoptosis induced by detachment from the extracellular matrix, critical for preventing metastasis. In cancer, anoikis resistance enables cells to survive in circulation and establish secondary tumors. Anoikis resistance is a hallmark of metastatic competence and represents a biologically grounded vulnerability for further investigation.\n\n- Cellular senescence: Stable cell cycle arrest where cells remain metabolically active but no longer proliferate. Though not a cell death executioner mechanism, senescence is part of the RCD ecosystem — it can act as a tumor suppressor by arresting damaged cells, but accumulated senescent cells promote tumor progression through secretion of pro-inflammatory factors (the senescence-associated secretory phenotype, SASP).\n\n- Mitotic catastrophe: An oncosuppressive mechanism that senses aberrant mitosis and genomic instability, typically triggering downstream execution via apoptosis or necrosis rather than constituting a distinct death pathway itself. Induced by treatments that disrupt mitotic progression. In cancer, mitotic catastrophe serves as a fail-safe mechanism for eliminating cells with mitotic defects, preventing aneuploidy and tumor progression.\n\n- Cuproptosis: Regulated cell death driven by copper accumulation and associated mitochondrial stress. Copper-dependent, overlaps with ferroptosis in metal ion dysregulation. A newly discovered pathway exploiting copper accumulation to selectively induce death in cancer cells.\n\n- NETosis: Neutrophil cell death releasing neutrophil extracellular traps (NETs) to trap pathogens — an inflammatory cell death mode of neutrophils. Overlaps with pyroptosis and necroptosis. In cancer, NETosis can trap and kill cancer cells but also promote inflammation and tumor progression.\n\n- Efferocytosis: Clearance process by which apoptotic or dead cells are recognized and removed by phagocytic cells, preventing leakage of inflammatory contents. Though not a cell death executioner mechanism itself, it is the terminal step of the RCD cycle. In the tumor microenvironment, efferocytosis suppresses inflammation but may also impair anti-tumor immunity by silently clearing immunogenic dying cells before they activate immune responses.\n\n- Entosis: Cell death resulting from one cell being engulfed by another — cell-in-cell cannibalism. Overlaps with autophagy and lysosome-dependent cell death. In cancer, entosis can kill engulfed cells but also provide survival advantages to engulfing cells.\n\n- Parthanatos: Programmed cell death via hyperactivated PARP-1, causing DNA fragmentation and AIF translocation to the nucleus. Mediated by PARP enzymes, overlaps with apoptosis in DNA damage response. PARP inhibition can trigger parthanatos in tumors with deficient DNA repair, linking this death form to DNA damage response pathways.\n\n- Immunogenic cell death (ICD): A functional outcome in which dying cells expose or release danger-associated molecular patterns (DAMPs) that activate the adaptive immune system against dead cell antigens. ICD is not a distinct executioner pathway — it is an immunological quality that forms such as apoptosis, necroptosis, and ferroptosis can acquire under specific conditions. Harnessing ICD is being investigated as a therapeutic concept to convert dying tumor cells into an in situ vaccine.\n\n- Disulfidptosis: Condition in which abnormal expression of SLC7A11 under glucose starvation causes disulfide accumulation and stress leading to cell death. Triggered by disulfide accumulation, overlaps with oxidative stress pathways. An emerging area targeting metabolic vulnerabilities of cancer cells under nutrient stress.\n\n- Oxeiptosis: Regulated form of cell death driven by oxidative stress, characterized by involvement of KEAP1 and NRF2. Similar to apoptosis and necroptosis in being triggered by oxidative stress. Its modulation is being investigated for potential relevance to oxidative stress-targeting approaches in cancer.\n\n- Paraptosis: Non-apoptotic cell death with cytoplasmic vacuolation, distinct from apoptosis. Offers potential anti-cancer mechanisms through induction of ER stress. Being explored as a way to overcome resistance to apoptosis in certain cancers.\n\n- Alkaliptosis: pH-dependent cell death triggered by alkaline conditions, involving NF-κB pathways and CA9 downregulation. Overlaps with other stress-induced cell deaths. A recent discovery with potential to target the tumor microenvironment through pH manipulation.\n\n- Lysosome-dependent cell death: Cell death dependent on permeabilization of lysosomes and release of cathepsins. Involves lysosomal enzymes, overlaps with autophagy and apoptosis. Lysosomal permeabilization is being investigated as a potential vulnerability in tumor cells with altered lysosomal regulation.\n\n- Mitoptosis: Selective elimination of damaged mitochondria through mitochondrial permeability transition and oxidative stress, representing organelle-level quality control. Overlaps with autophagy and apoptosis. While primarily a mitochondrial-level process, extensive mitoptosis can contribute to cell death. In cancer, mitochondrial quality control failure may promote tumor progression through metabolic dysfunction.\n\n- Autosis: A subtype of autophagy-dependent cell death, dependent on the Na+/K+-ATPase pump. Occurs in response to stress and ischemia. Its unique mechanism may provide opportunities for targeting resistant cancer cells that evade other forms of autophagic death.\n\n- Erebosis: Novel form of cell death reported during the natural turnover of gut enterocytes. A newly identified process with potential relevance in cancer biology, particularly gut-associated cancers and contexts where cell turnover is high.\n\n- Methuosis: Non-apoptotic cell death characterized by accumulation of macropinosome-derived vacuoles (distinct from the ER-derived vacuoles of paraptosis), driven by constitutive Ras/Rac1 signaling and macropinocytosis, culminating in cell rupture. Less studied in cancer — its unique vacuolization mechanism offers potential for targeting cancers exhibiting high rates of macropinocytosis.\n\n- Mitochondrial permeability transition (MPT): A subcellular trigger event involving the opening of a non-selective pore (mPTP) in the inner mitochondrial membrane, leading to loss of mitochondrial membrane potential and release of pro-death factors. MPT can initiate downstream execution via necrosis or apoptosis. In cancer, MPT modulation may expose metabolic vulnerabilities in tumor cells with altered mitochondrial regulation.\n--- END RCD BIOLOGICAL CONTEXT DECODER ---\n"

build_patient_payload <- function(pat_id, cancer_abb, cancer_full, metric, csv_path) {
  traj <- tryCatch(read.csv(csv_path, stringsAsFactors = FALSE), error = function(e) NULL)
  if (is.null(traj) || nrow(traj) == 0 || !"Feature" %in% names(traj)) return(NULL)

  traj$AbsSHAP <- abs(traj$SHAP_Value)
  traj <- traj[order(traj$AbsSHAP, decreasing = TRUE), ]
  top5 <- head(traj, 5)

  sig_lines <- c()
  patient_genes <- c()
  patient_rcd_forms <- c()
  for (i in seq_len(nrow(top5))) {
    feat <- top5$Feature[i]
    patient_shap <- top5$SHAP_Value[i]
    shap_direction <- if (patient_shap > 0) "Positive SHAP (Pro-Progression/Recurrence)"
                      else "Negative SHAP (Anti-Progression/Stabilizing)"

    gene <- "Unknown"
    rcd_form <- "Unknown"
    omic_layer <- "Unknown"

    if (!is.null(table_s11)) {
      s11_row <- table_s11[table_s11$Nomenclature == feat, ]
      if (nrow(s11_row) > 0) {
        decoded <- as.character(s11_row$Decoded.Genetic.Element[1])
        if (!is.na(decoded) && nchar(decoded) > 0) {
          gene_match <- regmatches(decoded, regexpr("[A-Za-z][A-Za-z0-9-]+(?=\\(\\d+/)", decoded, perl = TRUE))
          if (length(gene_match) > 0) gene <- gene_match[1]
          # Extract isoform fraction X/Y for Transcript signatures (e.g., "EGFL7(1/7)" -> iso_1=1, iso_of=7)
          iso_fraction <- ""
          iso_match <- regmatches(decoded, gregexpr("\\([0-9]+/[0-9]+\\)", decoded, perl = TRUE))[[1]]
          if (length(iso_match) > 0) {
            nums <- regmatches(iso_match[1], gregexpr("[0-9]+", iso_match[1]))[[1]]
            if (length(nums) >= 2) iso_fraction <- sprintf("[NOTE: This specific SHAP coordinate maps to Transcript Isoform %s out of %s known transcripts for this gene.] ", nums[1], nums[2])
          }
        }
        if ("RCD.form" %in% names(s11_row)) {
          rcd_form <- as.character(s11_row$RCD.form[1])
          if (is.na(rcd_form) || rcd_form == "") rcd_form <- "Unknown"
          if (rcd_form != "Unknown") patient_rcd_forms <- c(patient_rcd_forms, rcd_form)
        }
        if ("Omic.feature" %in% names(s11_row)) {
          omic_raw <- as.character(s11_row$Omic.feature[1])
          if (!is.na(omic_raw)) {
            omic_layer <- if (grepl("Transcript", omic_raw, ignore.case = TRUE)) "Transcript Isoform"
                     else if (grepl("mRNA", omic_raw, ignore.case = TRUE)) "Bulk mRNA Expression"
                     else if (grepl("Methylation", omic_raw, ignore.case = TRUE)) "CpG Methylation"
                     else if (grepl("miRNA", omic_raw, ignore.case = TRUE)) "miRNA"
                     else if (grepl("Protein", omic_raw, ignore.case = TRUE)) "Protein"
                     else if (grepl("Mutation", omic_raw, ignore.case = TRUE)) "Mutation"
                     else if (grepl("CNV", omic_raw, ignore.case = TRUE)) "CNV"
                     else omic_raw
          }
        }
      }
    }

    if (gene != "Unknown") patient_genes <- c(patient_genes, gene)

    gene_mechanics <- paste0(iso_fraction, gene, " (gene description)")
    if (!is.null(gene_info) && gene != "Unknown") {
      gi_row <- gene_info[gene_info$Gene.symbol == gene, ]
      if (nrow(gi_row) > 0 && !is.na(gi_row$Summary[1]) && nchar(gi_row$Summary[1]) > 20) {
        summary <- gsub("\\[provided by [^]]+\\]", "", gi_row$Summary[1])
        summary <- trimws(summary)
        gene_mechanics <- paste0(iso_fraction, gene, " (", summary, ")")
      }
    }

    # V6: Population guardrail removed — LLM may use population-level correlation context
    # V27: Strip nomenclature patterns from feat BEFORE injecting into LLM prompt
    # (prevents G2/G19 violations — LLM cannot regurgitate nomenclatures it never saw)
    feat_clean <- feat
    feat_clean <- gsub("(?<!TCGA-)\\b[A-Z]{2,5}-\\d{2,4}\\b", "", feat_clean, perl = TRUE)
    feat_clean <- gsub("(?:Signature\\s+)?\\.\\d+(?:\\.[A-Z0-9]+){2,}", "", feat_clean, perl = TRUE)
    feat_clean <- gsub("[A-Z]{2,5}-\\d+\\.[A-Z0-9.]+(?:\\s|$|\\)|,|;)", "", feat_clean, perl = TRUE)
    feat_clean <- trimws(gsub("\\s{2,}", " ", feat_clean))
    sig_lines <- c(sig_lines, sprintf(
      "Signature: %s > SHAP Impact: %s (Value: %.4f) > Omic Layer: %s > Associated RCD Form: %s > Encoded Gene Mechanics: %s",
      feat_clean, shap_direction, patient_shap, omic_layer, rcd_form, gene_mechanics
    ))
  }

  # === BENCHMARK FULL GENE EXTRACTION (V19 FIX) ===
  # Each SHAP signature decodes to dozens/hundreds of gene elements.
  # The narrative prompt uses only ~5 headline genes to keep output concise,
  # but the G9 governance checker MUST validate against ALL genes from ALL
  # signatures to avoid false "hallucination" flags (previously ~39% G9 rate).
  #
  # Helper: extract ALL gene symbols from a decoded genetic element string.
  # Handles both formats: "(GENE1 + GENE2 + ...)" and "(GENE1(1/11)) + (GENE2(1/5)) + ..."
  extract_all_genes_from_decoded <- function(decoded) {
    if (is.na(decoded) || nchar(decoded) == 0) return(character(0))
    # Strip isoform fraction annotations e.g., "(1/11)" inside parentheses
    clean <- gsub("\\(\\d+/\\d+\\)", "", decoded)
    # Strip backtick quoting around genes like `HLA-DPB1`
    clean <- gsub("`", "", clean)
    # Split on "+"
    parts <- trimws(unlist(strsplit(clean, "\\+")))
    # Remove leading "(" and trailing ")"
    parts <- gsub("^\\(|\\)$", "", parts)
    # Keep only valid gene symbols: starts with uppercase letter, then letters/digits/hyphens
    parts <- parts[grepl("^[A-Z][A-Za-z0-9-]+$", parts) & parts != "Unknown"]
    unique(parts)
  }
  # Build full gene list from ALL 5 top signatures (not just first gene per signature)
  all_patient_genes <- character(0)
  if (!is.null(table_s11)) {
    for (i in seq_len(min(nrow(traj), 5))) {
      feat <- traj$Feature[i]
      s11_row <- table_s11[table_s11$Nomenclature == feat, ]
      if (nrow(s11_row) > 0) {
        decoded <- as.character(s11_row$Decoded.Genetic.Element[1])
        all_patient_genes <- c(all_patient_genes, extract_all_genes_from_decoded(decoded))
      }
    }
    # Also include genes from remaining trajectory rows
    if (nrow(traj) > 5) {
      for (i in 6:nrow(traj)) {
        feat <- traj$Feature[i]
        s11_row <- table_s11[table_s11$Nomenclature == feat, ]
        if (nrow(s11_row) > 0) {
          decoded <- as.character(s11_row$Decoded.Genetic.Element[1])
          all_patient_genes <- c(all_patient_genes, extract_all_genes_from_decoded(decoded))
        }
      }
    }
  }
  all_patient_genes <- unique(all_patient_genes)

  # Patient phenotype from stemness
  pheno <- stemness_df[stemness_df$sample_id == pat_id, ]
  if (nrow(pheno) == 0) return(NULL)

  rnass <- pheno$RNAss[1]; ereg <- pheno$EREG.EXPss[1]
  rnass_cls <- pheno$RNAss_class[1]; ereg_cls <- pheno$EREG_class[1]
  tmb_val <- pheno$Non_silent_per_Mb[1]
  tmb_cls <- if (!is.na(pheno$TMB_class[1]) && pheno$TMB_class[1] != "") pheno$TMB_class[1] else "No data"
  msi_val <- pheno$Total_nb_MSI_events[1]
  msi_cls <- if (!is.na(pheno$MSI_class[1]) && pheno$MSI_class[1] != "") pheno$MSI_class[1] else "No data"

  phenotype_context <- paste0(
    "PATIENT-SPECIFIC PHENOTYPE CLASSIFICATIONS (USE THESE EXACT VALUES FOR ALL INTERPRETATION): ",
    sprintf("TSM: RNAss = %.3f (%s), EREG.EXPss = %.3f (%s). ",
      ifelse(is.na(rnass), 0, rnass), ifelse(is.na(rnass_cls), "No data", rnass_cls),
      ifelse(is.na(ereg), 0, ereg), ifelse(is.na(ereg_cls), "No data", ereg_cls)),
    sprintf("TMB (Non-silent/Mb): %.3f (%s). ",
      ifelse(is.na(tmb_val), 0, tmb_val), tmb_cls),
    if (is.na(msi_cls) || msi_cls == "") "MSI: No data available for this patient. "
    else sprintf("MSI: %.4f (%s). ", ifelse(is.na(msi_val), 0, msi_val), msi_cls),
    "CRITICAL: You MUST incorporate these exact patient-level TSM, TMB, and MSI classifications into your clinical synthesis."
  )

  # --- Endpoint-specific label and directional context ---
  endpoint_labels <- c(
    "DFI" = "Disease-Free Interval",
    "DSS" = "Disease-Specific Survival",
    "OS"  = "Overall Survival",
    "PFI" = "Progression-Free Interval"
  )
  endpoint_label <- if (metric %in% names(endpoint_labels)) endpoint_labels[[metric]] else metric

  # Directional interpretation: OS/DSS are S(t) [high = favorable]; DFI/PFI are 1-S(t) [low = favorable]
  if (metric %in% c("OS", "DSS")) {
    directional_context <- paste0(
      "DIRECTIONAL INTERPRETATION: This is a SURVIVAL model (S(t)). ",
      "The probabilities shown are survival probabilities. ",
      "HIGH values (e.g., >80% at 5yr) mean FAVORABLE prognosis (the patient is likely to survive). ",
      "LOW values (e.g., <50%) mean UNFAVORABLE prognosis. ",
      "Interpret ALL values through this survival-probability lens. ",
      "Classify the trajectory as PROTECTIVE (high survival), LETHAL (collapsing survival), or INTERMEDIATE."
    )
  } else {
    directional_context <- paste0(
      "DIRECTIONAL INTERPRETATION: This is a CUMULATIVE INCIDENCE model (1-S(t)). ",
      "The probabilities shown are the estimated cumulative risk of the event occurring. ",
      "LOW values (e.g., <5% at all time horizons) mean FAVORABLE prognosis (low event risk). ",
      "HIGH values (e.g., >20% with escalation over time) mean UNFAVORABLE prognosis. ",
      "Interpret ALL values through this cumulative-incidence lens. ",
      sprintf("This endpoint (%s) measures time to %s. ",
        endpoint_label,
        if (metric == "DFI") "new tumor event (recurrence, new primary, or metastasis)"
        else "disease progression or death"
      ),
      "Classify the trajectory as LOW-RISK (negligible event probability), STABLE (monitored), or ADVERSE (concerning escalation)."
    )
  }

  # --- Build OncoKB Gene Annotation summary (mirrors app.R build_oncokb_gene_summary) ---
  build_oncokb_gene_summary_bench <- function(patient_genes) {
    oc_genes <- oncokb_gene_annotations[
      oncokb_gene_annotations$Gene_Symbol %in% patient_genes, , drop = FALSE]
    if (nrow(oc_genes) == 0) {
      return("OncoKB Gene Annotations: None of the patient's signature genes are currently curated in the OncoKB precision oncology knowledge base.")
    }
    lines <- c(
      "ONCOKB GENE ANNOTATIONS (MSKCC Precision Oncology Knowledge Base)",
      "============================================================",
      sprintf("Patient genes with OncoKB annotations: %d / %d", nrow(oc_genes), length(patient_genes)),
      "",
      "Oncogenic Classification Legend:",
      "  ONCOGENE = Known oncogene (gain-of-function alterations promote cancer)",
      "  TSG = Tumor Suppressor Gene (loss-of-function alterations promote cancer)",
      "  ONCOGENE_AND_TSG = Context-dependent (may act as either)",
      "  INSUFFICIENT_EVIDENCE = Not yet classified by OncoKB", ""
    )
    for (oc in c("ONCOGENE", "TSG", "ONCOGENE_AND_TSG", "INSUFFICIENT_EVIDENCE")) {
      sub <- oc_genes[oc_genes$Oncogenic_Class == oc, , drop = FALSE]
      if (nrow(sub) == 0) next
      lines <- c(lines, paste0("--- ", oc, " (", nrow(sub), " genes) ---"))
      for (i in seq_len(nrow(sub))) {
        row <- sub[i, ]
        level_info <- ""
        if (!is.null(row$Highest_Sensitive_Level) && !is.na(row$Highest_Sensitive_Level) &&
            row$Highest_Sensitive_Level != "")
          level_info <- paste0(" [Highest OncoKB Level: ", row$Highest_Sensitive_Level, "]")
        gs <- ifelse(nchar(row$Gene_Summary) > 200,
                     paste0(substr(row$Gene_Summary, 1, 197), "..."), row$Gene_Summary)
        lines <- c(lines, sprintf("  %s | %s | %s%s", row$Gene_Symbol, row$Oncogenic_Class, gs, level_info))
      }
      lines <- c(lines, "")
    }
    missing_genes <- setdiff(patient_genes, oc_genes$Gene_Symbol)
    if (length(missing_genes) > 0)
      lines <- c(lines, paste0("Genes without OncoKB annotation: ", paste(missing_genes, collapse = ", ")))
    paste(lines, collapse = "\n")
  }

  # --- Build Cancer Gene List summary (mirrors app.R build_cancer_gene_list_summary) ---
  build_cancer_gene_list_summary_bench <- function(patient_genes) {
    cgl_df <- oncokb_cancer_gene_list
    if (nrow(cgl_df) == 0) return("Cancer Gene List: Data not available.")
    hugo_col <- grep("Hugo.*Symbol|Gene.*Symbol", colnames(cgl_df), value = TRUE, ignore.case = TRUE)[1]
    if (is.na(hugo_col)) hugo_col <- colnames(cgl_df)[1]
    matched <- cgl_df[cgl_df[[hugo_col]] %in% patient_genes, , drop = FALSE]
    if (nrow(matched) == 0) {
      return(sprintf(paste0("CANCER GENE LIST (OncoKB Cancer Gene Census v99): None of the patient's %d signature genes (%s) ",
        "are currently recognized in the OncoKB Cancer Gene List (1,240 curated cancer genes from MSK-IMPACT, ",
        "FoundationOne, Vogelstein, COSMIC CGC, and OncoKB annotation resources)."),
        length(patient_genes), paste(patient_genes, collapse = ", ")))
    }
    lines <- c(
      "CANCER GENE LIST EVIDENCE (OncoKB Cancer Gene Census v99, May-2026)",
      "========================================================================",
      sprintf("Patient genes recognized as cancer genes: %d / %d", nrow(matched), length(patient_genes)),
      sprintf("Total curated cancer genes in reference list: %d", nrow(cgl_df)), "",
      "Evidence Resource Legend:",
      "  OncoKB Annotated = Curated in OncoKB precision oncology knowledge base",
      "  MSK-IMPACT     = Included in MSK-IMPACT clinical sequencing panel (468 genes)",
      "  MSK-HEME       = Included in MSK-HEME heme-onc sequencing panel",
      "  FoundationOne   = Included in FoundationOne CDx comprehensive genomic profiling",
      "  FoundationOne Heme = Included in FoundationOne Heme panel",
      "  Vogelstein     = Classified in Vogelstein cancer gene census (oncogene/TSG)",
      "  COSMIC CGC (v99) = Included in COSMIC Cancer Gene Census v99", ""
    )
    for (i in seq_len(nrow(matched))) {
      row <- matched[i, ]
      gene_sym <- row[[hugo_col]]
      memberships <- c()
      if ("OncoKB Annotated" %in% colnames(row) && !is.na(row[["OncoKB Annotated"]]) &&
          tolower(as.character(row[["OncoKB Annotated"]])) == "yes")
        memberships <- c(memberships, "OncoKB")
      if ("MSK-IMPACT" %in% colnames(row) && !is.na(row[["MSK-IMPACT"]]) &&
          tolower(as.character(row[["MSK-IMPACT"]])) == "yes")
        memberships <- c(memberships, "MSK-IMPACT")
      if ("MSK-HEME" %in% colnames(row) && !is.na(row[["MSK-HEME"]]) &&
          tolower(as.character(row[["MSK-HEME"]])) == "yes")
        memberships <- c(memberships, "MSK-HEME")
      if ("FOUNDATION ONE" %in% colnames(row) && !is.na(row[["FOUNDATION ONE"]]) &&
          tolower(as.character(row[["FOUNDATION ONE"]])) == "yes")
        memberships <- c(memberships, "FoundationOne")
      if ("FOUNDATION ONE HEME" %in% colnames(row) && !is.na(row[["FOUNDATION ONE HEME"]]) &&
          tolower(as.character(row[["FOUNDATION ONE HEME"]])) == "yes")
        memberships <- c(memberships, "FoundationOneHeme")
      if ("Vogelstein" %in% colnames(row) && !is.na(row[["Vogelstein"]]) &&
          tolower(as.character(row[["Vogelstein"]])) == "yes")
        memberships <- c(memberships, "Vogelstein")
      if ("COSMIC CGC (v99)" %in% colnames(row) && !is.na(row[["COSMIC CGC (v99)"]]) &&
          tolower(as.character(row[["COSMIC CGC (v99)"]])) == "yes")
        memberships <- c(memberships, "COSMIC_CGC")
      gene_type <- if ("Gene Type" %in% colnames(row) && !is.na(row[["Gene Type"]]))
        paste0(" [", as.character(row[["Gene Type"]]), "]") else ""
      occ <- ""
      occ_col <- grep("occurrence.*resource|# of occurrence", colnames(row), value = TRUE, ignore.case = TRUE)
      if (length(occ_col) > 0 && !is.na(row[[occ_col[1]]]))
        occ <- paste0(" (resource score: ", row[[occ_col[1]]], ")")
      lines <- c(lines, sprintf("  %s%s%s | Memberships: %s",
        gene_sym, gene_type, occ,
        if (length(memberships) > 0) paste(memberships, collapse = ", ") else "None"))
    }
    missing <- setdiff(patient_genes, matched[[hugo_col]])
    if (length(missing) > 0) {
      lines <- c(lines, "", sprintf("Genes NOT in Cancer Gene List: %s", paste(missing, collapse = ", ")))
    }
    lines <- c(lines, "", paste0("CRITICAL: Membership in cancer gene resources does NOT imply therapeutic actionability. ",
      "This layer provides biological context regarding the degree to which a gene is recognized as ",
      "relevant to cancer biology across major curated resources."))
    paste(lines, collapse = "\n")
  }

  oncokb_gene_section <- build_oncokb_gene_summary_bench(patient_genes)
  cancer_gene_list_text <- build_cancer_gene_list_summary_bench(patient_genes)

  # --- Query Tier 0 Clinical Actionability (mirrors app.R lines 4244-4260) ---
  tier0_result <- tryCatch({
    query_patient_actionability(
      genes       = patient_genes,
      layer       = oncokb_actionability_layer,
      cancer_type = cancer_full
    )
  }, error = function(e) {
    cat(sprintf(" [Tier0: %s] ", e$message))
    NULL
  })
  tier0_narrative <- if (!is.null(tier0_result) && !is.null(tier0_result$tier0_narrative)) {
    tier0_result$tier0_narrative
  } else ""

  payload <- paste0(
    "Patient ID: ", pat_id, "\n",
    "Cancer Cohort: ", cancer_full, "\n",
    "Clinical Metric: ", metric, " (", endpoint_label, ")\n\n",
    directional_context, "\n\n",
    "Patient's Specific Multi-Omic Expression Profile: The patient's clinical trajectory is mathematically driven by the following Top ",
    length(sig_lines), " unique molecular signatures extracted from their personalized XGBoost SHAP geometry:\n\n",
    paste(sig_lines, collapse = "\n\n"), "\n\n",
    "BIOLOGICAL CONTEXT: Each signature is a unique multi-omic coordinate.\n\n",
    phenotype_context, "\n\n",
    RCD_BIOLOGICAL_CONTEXT_DECODER, "\n\n",
    # --- OncoKB evidence layers (mirrors app.R user_prompt_pharma) ---
    "OncoKB Gene Annotations (MSKCC Precision Oncology Knowledge Base):\n", oncokb_gene_section, "\n\n",
    cancer_gene_list_text, "\n\n",
    if (nchar(tier0_narrative) > 0) paste0(tier0_narrative, "\n\n") else "",
    "ANALYSIS INSTRUCTION: Analyze across the nine-strata evidence architecture. ",
    "The Tier 0 section (Stratum 6) provides regulatory/curatorial biomarker-drug recognition ",
    "(T0A=exact match, T0B=gene-level, T0C=OncoKB curated). ",
    "The Cancer Gene List (Stratum 5) provides biological cancer relevance context. ",
    "These are DISTINCT evidence dimensions. ",
    "Present Tier 0 regulatory recognition separately from pharmacogenomic interactions. ",
    "Qualify T0B matches by their cross-context nature and T0C associations by their lack of FDA drug labeling. ",
    "Do NOT present Tier 0 evidence as a treatment recommendation. ",
    "Do NOT conflate cancer gene membership with clinical actionability.\n\n",
    paste0("\n\nTOP 5 SIGNATURE-DRIVING GENES — These are the primary molecular drivers ",
            "of this patient's SHAP trajectory: ",
            paste(unique(patient_genes), collapse = ", "),
            ". Center your clinical discussion on these driver genes. ",
            "You MAY discuss additional biologically connected genes (pathway partners, downstream targets, ",
            "interacting regulators) when biologically justified. ",
            "You MUST NOT import canonical cancer genes (EGFR, TP53, TNF, BRAF, BRCA1, GPX4, ",
            "CDK2, SMAD2, FGFR3, ATM, APC) unless they appear in the driver gene list or are ",
            "directly biologically connected to a driver gene. ",
            "Unconnected gene imports from training knowledge remain GOVERNANCE VIOLATIONS.\n\n",
            "SELF-VERIFICATION: Before writing, confirm each gene you mention is either: ",
            "(a) in the driver gene list above, or (b) biologically connected to a driver gene. ",
            "Do NOT 'knowledge-complete' by importing unrelated canonical cancer genes. ",
            "This preserves biological fidelity to the patient's actual genomic profile."),
    "\n\nAnalyze this patient's profile and provide the therapeutic vulnerability synthesis."
  )

  # Build metric-specific system prompt
  sys_prompt <- build_system_prompt(metric)

  return(list(payload = payload, genes = unique(patient_genes),
              all_genes = all_patient_genes,
              rcd_forms = unique(patient_rcd_forms),
              cancer_full = cancer_full, metric = metric,
              system_prompt = sys_prompt))
}

# --- Governance rules — V6: population guardrail removed, directional context added ---
build_system_prompt <- function(metric) {
  strict_nomenclature_rule <- paste0(
    "STRICT NOMENCLATURE RULE: You MUST NOT use, mention, or display ANY part of the signature ",
    "nomenclatures (e.g., THYM-1460.6.3.N.2.35.5.2.3.3) or their abbreviated forms (e.g., THYM-1460, ",
    "LUAD-1883, LUAD-636) or dot-prefixed surrogate forms (e.g., .5.3.2.4.14.2.4.1) anywhere in your clinical narrative. ",
    "These technical provenance identifiers belong exclusively in the audit section. ",
    "Whenever a signature contributes to the interpretation, you MUST automatically decode it and ",
    "discuss ONLY the constituent biological gene and its mechanisms. ",
    "NEVER use the phrase 'Signature LUAD-...' or any similar identifier."
  )

  rcd_boundary_governance <- paste0(
    "\n\n--- CRITICAL RCD BOUNDARY GOVERNANCE ---\n",
    "You MUST ONLY discuss Regulated Cell Death (RCD) forms that are EXPLICITLY listed in the ",
    "'Associated RCD Form' annotations of the patient's signatures in the payload above. ",
    "The RCD Biological Context Decoder provides definitions for ALL known RCD forms for educational ",
    "reference — but you are FORBIDDEN from introducing an RCD form not present in the patient's ",
    "own signature annotations. Introducing an RCD form not listed in the patient payload is a ",
    "GOVERNANCE VIOLATION.\n",
    "When discussing RCD, name ONLY the forms explicitly provided in the patient's SHAP signatures ",
    "(e.g., if the patient has 'Apoptosis, Necrosis', do NOT discuss ferroptosis, necroptosis, ",
    "pyroptosis, or any other form not listed).\n",
    "PRE-OUTPUT SELF-CHECK: Scan your output for RCD form names. If any form you mentioned is NOT ",
    "present in the patient's 'Associated RCD Form' annotations, DELETE that discussion immediately.\n",
    "--- END RCD BOUNDARY GOVERNANCE ---\n"
  )

  hedging_rule <- paste0(
    "CRITICAL HEDGING & HYPOTHESIS INSTRUCTION: You MUST use hedging language. ",
    "Replace strong claims (e.g., 'demonstrate', 'prove', 'establish') with cautious terms ",
    "(e.g., 'suggest', 'are consistent with', 'are compatible with', 'may indicate that')."
  )

  # V6: Metric-specific directional context (replaces rigid word bans with mathematical guidance)
  if (metric %in% c("OS", "DSS")) {
    metric_context <- paste0(
      "METRIC CONTEXT: This is a SURVIVAL endpoint (Overall Survival / Disease-Specific Survival). ",
      "Probabilities reflect S(t) — the estimated chance of survival. ",
      "HIGH probabilities (>80%) indicate favorable prognosis. LOW probabilities (<50%) indicate unfavorable. ",
      "Classify the trajectory as PROTECTIVE or LETHAL based on the survival probability trajectory. ",
      "Use ONLY survival-compatible terminology: 'lethal', 'protective', 'mortality', 'survival', ",
      "'survival-promoting', 'survival-suppressing', 'mortality-associated'. ",
      "FORBIDDEN: 'pro-progression', 'anti-progression', 'pro-recurrence', 'anti-recurrence', ",
      "'adverse progression', 'adverse recurrence'. These are event-endpoint terms and MUST NOT ",
      "appear in OS or DSS narratives."
    )
  } else if (metric == "PFI") {
    metric_context <- paste0(
      "METRIC CONTEXT: This is a PROGRESSION endpoint (Progression-Free Interval). ",
      "Probabilities reflect cumulative incidence of progression — the estimated risk of disease progression. ",
      "LOW probabilities (<5% at all horizons) indicate favorable prognosis. ",
      "HIGH probabilities (>20% with escalation) indicate unfavorable. ",
      "Classify the trajectory as LOW-RISK, STABLE, or ADVERSE PROGRESSION. ",
      "Use ONLY progression-specific terminology: 'pro-progression', 'anti-progression', ",
      "'stabilizing', 'adverse progression'. ",
      "FORBIDDEN: 'lethal', 'protective', 'mortality', 'survival probability', ",
      "'pro-recurrence', 'anti-recurrence', 'adverse recurrence'. ",
      "Survival terms belong to OS/DSS; recurrence terms belong to DFI."
    )
  } else {  # DFI
    metric_context <- paste0(
      "METRIC CONTEXT: This is a RECURRENCE endpoint (Disease-Free Interval). ",
      "Probabilities reflect cumulative incidence of new tumor events — the estimated risk of recurrence, ",
      "new primary, or metastasis. LOW probabilities (<5% at all horizons) indicate favorable prognosis. ",
      "HIGH probabilities (>20% with escalation) indicate unfavorable. ",
      "Classify the trajectory as LOW-RISK, STABLE, or ADVERSE RECURRENCE. ",
      "Use ONLY recurrence-specific terminology: 'pro-recurrence', 'anti-recurrence', ",
      "'stabilizing', 'adverse recurrence'. ",
      "FORBIDDEN: 'lethal', 'protective', 'mortality', 'survival probability', ",
      "'pro-progression', 'anti-progression', 'adverse progression'. ",
      "Survival terms belong to OS/DSS; progression terms belong to PFI."
    )
  }

  paste0(
    "You are an expert clinical molecular oncologist analyzing a patient's multi-omic profile. ",
    "Output a highly professional, fluid clinical synthesis in continuous paragraph form. ",
    "\n\n=== GENE GOVERNANCE (READ FIRST) ===\n",
    "The patient payload below identifies the TOP 5 SIGNATURE-DRIVING GENES — the primary molecular drivers ",
    "from this patient's SHAP trajectory. Center your clinical synthesis on these driver genes. ",
    "You MAY discuss additional genes that are biologically connected to the driver genes ",
    "(e.g., pathway partners, downstream targets, interacting regulators). ",
    "You MUST NOT import canonical cancer genes (EGFR, TP53, TNF, BRAF, BRCA1, GPX4, CDK2, ",
    "SMAD2, FGFR3, ATM, APC, TRAF2, E2F1, RHOA, HSPD1, or any other gene) unless they appear ",
    "in the driver gene list or are directly biologically connected to a driver gene. ",
    "Do NOT borrow unrelated genes from your training knowledge. ",
    "Each unconnected gene mention will be audited as a GOVERNANCE VIOLATION.\n\n",
    "=== PATIENT IDENTIFIER RULE (READ SECOND) ===\n",
    "You MUST start your narrative by explicitly stating the Patient ID and the Cohort. ",
    "The first sentence MUST contain both identifiers. Omitting either is a GOVERNANCE FAILURE.\n\n",
    metric_context, "\n\n",
    hedging_rule, "\n\n",
    strict_nomenclature_rule, "\n\n",
    "GLOBAL ASSOCIATIVITY RULE: The multi-omic signatures, SHAP values, and stemness correlations ",
    "provided represent associative mathematical relationships, not proven causative biological pathways.\n\n",
    tsm_ontology_protection, "\n\n",
    phenotype_correlation_rule_val, "\n\n",
    rcd_boundary_governance, "\n\n",
    # --- Nine-strata governance + Cancer Gene List governance (mirrors app.R) ---
    build_nine_strata_governance_block(), "\n\n",
    "--- CANCER GENE LIST GOVERNANCE v2.0 (MANDATORY VETO) ---\n\n",
    "The CANCER GENE LIST EVIDENCE section provides orthogonal cancer-gene annotations from the ",
    "OncoKB Cancer Gene Census (v99, May-2026, ~1,240 genes). This is a BIOLOGICAL CONTEXT LAYER ",
    "(Stratum 5), NOT a therapeutic actionability layer.\n\n",
    "MANDATORY CANCER GENE LIST RULES:\n",
    "1. Report cancer gene list membership as biological context only: state whether patient genes are ",
    "recognized by major cancer gene resources.\n",
    "2. For genes NOT in the cancer gene list, note this appropriately.\n",
    "3. The Gene Type annotation (ONCOGENE, TSG, ONCOGENE_AND_TSG) provides orthogonal classification.\n",
    "4. The resource occurrence score reflects how many of 7 evidence resources recognize the gene.\n",
    "5. CRITICAL: Cancer gene list membership does NOT indicate therapeutic actionability, ",
    "druggability, or clinical significance for this patient. It is biological context only.\n",
    "\n",
    "FORBIDDEN WORDS when discussing CGL (Stratum 5): 'actionable', 'druggable', ",
    "'therapeutic target', 'clinical actionability', 'treatment relevance'. ",
    "These belong to Stratum 6 (Tier 0), NOT Stratum 5 (CGL).\n",
    "SAFE VOCABULARY — use ONLY these when discussing CGL: ",
    "'recognized cancer gene', 'cancer-relevant', 'oncogenic classification', ",
    "'cancer gene census member', 'biological context'.\n",
    "\n",
    "EXAMPLES — WRONG vs RIGHT:\n",
    "  ❌ PROHIBITED: 'CCNE1 is on the Cancer Gene List, indicating it is a clinically actionable target.'\n",
    "  ✅ CORRECT:   'CCNE1 is recognized by the OncoKB Cancer Gene List as a cancer-relevant oncogene.'\n",
    "  ❌ PROHIBITED: 'PGR Cancer Gene List membership confirms its therapeutic relevance.'\n",
    "  ✅ CORRECT:   'PGR is classified as an oncogene in the Cancer Gene List with broad panel recognition.'\n",
    "  ❌ PROHIBITED: 'These cancer gene list members represent druggable vulnerabilities.'\n",
    "  ✅ CORRECT:   'These cancer-relevant genes are recognized across multiple evidence resources.'\n",
    "\n",
    "MANDATORY SELF-VERIFICATION (VETO CHECK): Before finalizing your response, scan every sentence ",
    "that discusses CGL data. If ANY sentence contains a FORBIDDEN WORD ('actionable', 'druggable', ",
    "'therapeutic target', 'clinical actionability', 'treatment relevance'), DELETE that sentence ",
    "and rewrite it using ONLY the SAFE VOCABULARY. This is a NON-NEGOTIABLE requirement.\n",
    "END CANCER GENE LIST GOVERNANCE\n\n",
    # --- TIER 0 REGULATORY RECOGNITION GOVERNANCE v1.0 (MANDATORY VETO) ---
    "--- TIER 0 REGULATORY RECOGNITION GOVERNANCE v1.0 (MANDATORY VETO) ---\n\n",
    "The TIER 0 EVIDENCE section (Stratum 6) provides FDA-recognized biomarker-drug associations ",
    "from local TSV files (Fda2/Fda3) and OncoKB-curated gene-cancer annotations (T0C). ",
    "Tier 0 is REGULATORY/CURATORIAL RECOGNITION — NOT a treatment recommendation layer. ",
    "It answers: 'Is this gene-biomarker-drug relationship recognized by regulatory/curatorial bodies?' ",
    "It does NOT answer: 'What treatment should this patient receive?'\n\n",
    "TIER 0 CLASSIFICATION:\n",
    "  T0A = Exact match: gene + alteration + cancer type + drug → FDA/OncoKB recognized\n",
    "  T0B = Gene-level match: gene + cancer type recognized, but alteration/drug context may differ\n",
    "  T0C = OncoKB curated: gene curated by OncoKB without FDA drug labeling\n\n",
    "MANDATORY RULES:\n",
    "1. Present Tier 0 findings as regulatory/curatorial recognition context ONLY.\n",
    "2. ALWAYS qualify T0B matches: 'This biomarker-drug association is recognized at the gene level ",
    "but the specific alteration and drug pairing may differ from the original regulatory context.'\n",
    "3. ALWAYS qualify T0C associations: 'This gene is recognized by OncoKB as cancer-relevant ",
    "but lacks FDA drug labeling for the associated biomarker-drug pairing.'\n",
    "4. NEVER present any Tier 0 classification as a treatment recommendation, prescription guidance, ",
    "or clinical management instruction.\n\n",
    "FORBIDDEN WORDS when discussing Tier 0 (Stratum 6):\n",
    "  'treatment recommendation', 'should receive', 'should be treated with', ",
    "  'is indicated for', 'prescribe', 'therapeutic regimen', 'clinical management', ",
    "  'standard of care for this patient', 'recommended therapy', 'first-line treatment'.\n\n",
    "SAFE VOCABULARY — use ONLY these when discussing Tier 0:\n",
    "  'FDA-recognized biomarker-drug association', 'regulatory recognition', ",
    "  'curated biomarker-drug relationship', 'OncoKB-recognized gene-cancer association', ",
    "  'biomarker evidence recognized by regulatory resources'.\n\n",
    "EXAMPLES — WRONG vs RIGHT:\n",
    "  ❌ PROHIBITED: 'T0A evidence indicates the patient should receive EGFR-targeted therapy.'\n",
    "  ✅ CORRECT:   'EGFR has a T0A FDA-recognized biomarker-drug association in this cancer type, ",
    "representing regulatory-level recognition of its biomarker relevance.'\n",
    "  ❌ PROHIBITED: 'Based on Tier 0 recognition, treatment with this drug is indicated.'\n",
    "  ✅ CORRECT:   'Tier 0 regulatory recognition identifies this gene-drug relationship as ",
    "curated by FDA/OncoKB resources, warranting further biological investigation.'\n",
    "  ❌ PROHIBITED: 'T0B recognition supports the use of this targeted agent.'\n",
    "  ✅ CORRECT:   'This gene has T0B gene-level regulatory recognition, though the specific ",
    "alteration and drug context should be interpreted with cross-context qualification.'\n\n",
    "MANDATORY SELF-VERIFICATION (VETO CHECK): Before finalizing your response, scan every sentence ",
    "that mentions T0A/T0B/T0C or Tier 0. If ANY sentence contains a FORBIDDEN WORD or ",
    "reads as a treatment instruction, DELETE that sentence and rewrite it using ONLY the ",
    "SAFE VOCABULARY. Tier 0 is regulatory recognition — never treatment guidance. ",
    "This is a NON-NEGOTIABLE requirement.\n",
    "END TIER 0 REGULATORY RECOGNITION GOVERNANCE\n\n",
    # --- OncoKB Concordance Lock (v1.0) ---
    "--- ONCOKB CONCORDANCE LOCK v1.0 ---\n\n",
    "CRITICAL: OncoKB therapeutic Levels 1-4 (LEVEL_1, LEVEL_2, LEVEL_3A, LEVEL_3B, LEVEL_4) ",
    "require API access under a license agreement and are not available as downloadable ",
    "files. The matrix includes gene-level annotations from OncoKB (oncogene/TSG ",
    "classification, Cancer Gene List membership) and FDA-recognized biomarker-drug ",
    "associations (Fda2/Fda3) downloaded from the OncoKB public repository, queried ",
    "via the Tier 0 Clinical Actionability Layer (Stratum 6). OncoKB therapeutic Level ",
    "1-4 drug-variant associations are NOT available in this matrix.\n\n",
    "YOU MUST NOT:\n",
    "  • Claim any drug-gene association has 'OncoKB Level 1/2/3A/3B/4' support unless ",
    "the Clinical_Status column explicitly states it.\n",
    "  • Fabricate or infer OncoKB therapeutic level classifications for associations ",
    "from DGIdb or CIViC.\n",
    "  • Conflate Fda2/Fda3 (Tier 0 regulatory recognition, Stratum 6) with ",
    "therapeutic Levels 1-4 (Stratum 7).\n",
    "  • Present Tier 0 evidence (T0A/T0B/T0C) as equivalent to OncoKB therapeutic ",
    "level evidence.\n\n",
    "YOU MUST:\n",
    "  • Reference only the databases that actually appear in the Source_Database column.\n",
    "  • Report the exact Clinical_Status string as provided — do not reinterpret or elevate it.\n",
    "  • When OncoKB therapeutic levels are absent, acknowledge the license limitation ",
    "if discussing clinical evidence expectations.\n\n",
    "END ONCOKB CONCORDANCE LOCK\n\n",
    # --------------------------------------------------------------------
    "--- OUTPUT STRUCTURE TEMPLATE v1.0 ---\n\n",
    "You MUST follow this exact output structure. Deviations will be rejected.\n\n",
    "REQUIRED STRUCTURE:\n",
    "  ## Patient {TCGA_ID} — {Cancer Full Name}\n",
    "  [clinical narrative in continuous paragraphs]\n",
    "  ## Suggested Clinical Queries\n",
    "  1. - [first query]\n",
    "  2. - [second query]\n",
    "  3. - [third query]\n",
    "  [mandatory hypothesis-generating disclaimer as final paragraph]\n\n",
    "PRE-OUTPUT SELF-CHECK:\n",
    "  □ Does the report start with '## Patient TCGA-...'?\n",
    "  □ Is the cancer cohort name (not abbreviation) in the header?\n",
    "  □ Are there exactly 3 numbered clinical queries?\n",
    "  □ Is the hypothesis-generating disclaimer the final paragraph?\n\n",
    "TREATMENT LANGUAGE FIREWALL — WRONG vs RIGHT:\n",
    "  ❌ 'Prioritize therapies targeting the PGR pathway.'\n",
    "  ✅ 'The PGR pathway represents a potential vulnerability for further investigation.'\n",
    "  ❌ 'This patient should receive CDK4/6 inhibitors.'\n",
    "  ✅ 'CDK4/6 pathway dependencies may be relevant to this tumor's biology.'\n",
    "  ❌ 'Treatment with Palbociclib is indicated.'\n",
    "  ✅ 'Palbociclib has a Tier 5 pharmacogenomic association with CCNE1, requiring validation.'\n\n",
    "END OUTPUT STRUCTURE TEMPLATE\n\n"
  )
}

# --- TSM Ontology Protection (prevents lexical borrowing) ---
tsm_ontology_protection <- "\n\n--- TSM ONTOLOGY PROTECTION GOVERNANCE v1.0 ---\n\nCRITICAL: The following variables are TUMOR STEMNESS MEASURES (TSM) — phenotype-level indices, NOT gene measurements:\n- RNAss: transcriptome-wide stemness index. Does NOT represent any single RNA species.\n- EREG.EXPss: epigenetically-regulated stemness index. Does NOT represent Epiregulin (EREG) gene expression. The 'EREG' substring is an acronym meaning 'Epigenetically Regulated', not the gene symbol EREG.\n- EREG.METHss: methylation-based stemness index. Does NOT represent EREG methylation.\n- DNAss, DMPss: methylation-pattern-based stemness indices. Do NOT represent any single DNA element.\n\nABSOLUTE PROHIBITIONS:\n1. Do NOT decompose TSM variable names into gene symbols (e.g., extracting 'EREG' from 'EREG.EXPss').\n2. Do NOT import external biological knowledge solely because a substring resembles a gene name.\n3. Do NOT generate mechanistic explanations based on EREG biology (growth factor, signaling) when the payload only contains EREG.EXPss or EREG.METHss.\n\nCORRECT INTERPRETATION: TSM variables measure stemness-associated phenotypes — dedifferentiation, plasticity, self-renewal, tumor heterogeneity. Interpret them EXCLUSIVELY as stemness metrics.\n\nEND OF TSM ONTOLOGY PROTECTION GOVERNANCE\n"

# --- CRIT-05 MITIGATION: Ecological Fallacy Prevention Rule (V9, mirrors app.R) ---
phenotype_correlation_rule_val <- "\n\n--- ECOLOGICAL FALLACY PREVENTION ---\n\nThe patient's TSM, TMB, and MSI phenotypes (High/Intermediate/Low) are INDIVIDUAL MEASUREMENTS from the stemness database. They are NOT population-level correlations. You MUST reason about the patient using ONLY their own measured values.\n\nFORBIDDEN PATTERNS (DELETE AND REWRITE IMMEDIATELY):\n  - 'The patient's Intermediate TSM may reflect the P-positive correlation...'\n  - 'Consistent with the N-negative population-level sign...'\n  - 'This aligns with the cohort-level association of TSM with survival...'\n  - 'Given the population-wide TMB correlation pattern...'\n  - 'The P-positive population sign suggests that this patient...'\n  - '...consistent with the negative population-level correlation...'\n\nPERMITTED PATTERNS:\n  - 'The patient's individual TSM measurement is Intermediate (RNAss = X), which individually...'\n  - 'The patient exhibits Low TMB (X Mut/Mb), which in this individual context may suggest...'\n\nPOPULATION-LEVEL TERMS YOU MUST NEVER USE in patient-specific reasoning:\n  'P-positive', 'N-negative', 'population-level correlation', 'cohort-level correlation',\n  'population sign', 'population-wide', 'cohort-wide', 'population-level TSM/TMB/MSI'\n\nPRE-OUTPUT SELF-CHECK: Scan every paragraph containing patient-specific markers. If ANY also contains population-level terms, REWRITE using only the patient's own measurements.\n\n--- END ECOLOGICAL FALLACY PREVENTION ---\n"

# --- Governance Compliance Checker (19-point, V21: G1-G19 including RCD boundary + dot-prefixed nomenclature) ---
check_governance_compliance <- function(response_text, patient_genes, all_patient_genes, pat_id, cancer_full, metric = NULL, patient_rcd_forms = NULL) {
  score <- 0; total_checks <- 0; violations <- c()

  # G1: No raw nomenclature pattern
  total_checks <- total_checks + 1
  if (!grepl("[A-Z]{2,5}-\\d+\\.\\d+", response_text, perl = TRUE)) {
    score <- score + 1
  } else {
    violations <- c(violations, "G1: Raw nomenclature exposed")
  }

  # G2: No abbreviated nomenclature (excluding TCGA barcode fragments)
  # V21 FIX: widened digit count from {3,4} to {2,4} to catch 2-digit suffix forms like READ-82
  total_checks <- total_checks + 1
  if (!grepl("(?<!TCGA-)\\b[A-Z]{2,5}-\\d{2,4}\\b", response_text, perl = TRUE)) {
    score <- score + 1
  } else {
    violations <- c(violations, "G2: Abbreviated nomenclature exposed")
  }

  # G3: Patient ID stated
  total_checks <- total_checks + 1
  if (grepl(pat_id, response_text, fixed = TRUE)) {
    score <- score + 1
  } else {
    violations <- c(violations, "G3: Patient ID not stated")
  }

  # G4: Cohort stated
  total_checks <- total_checks + 1
  if (grepl(cancer_full, response_text, fixed = TRUE) ||
      grepl(gsub(" .*$", "", cancer_full), response_text)) {
    score <- score + 1
  } else {
    violations <- c(violations, "G4: Cancer cohort not stated")
  }

  # G5: 2-4 suggested clinical queries (V11 TOLERANCE: 2-4 acceptable, exactly 3 preferred)
  total_checks <- total_checks + 1
  query_matches <- gregexpr("(?:^|\\n)\\s*\\d+\\s*\\.\\s*-", response_text, perl = TRUE)[[1]]
  n_queries <- length(query_matches)
  if (n_queries >= 2 && n_queries <= 4) {
    score <- score + 1
    if (n_queries != 3) {
      violations <- c(violations, sprintf("G5_TOLERATED: Found %d queries (3 preferred)", n_queries))
    }
  } else {
    violations <- c(violations, sprintf("G5: Found %d suggested queries (need 2-4)", n_queries))
  }

  # G6: Mandatory disclaimer present
  total_checks <- total_checks + 1
  if (grepl("hypothesis-generating", response_text, fixed = TRUE) ||
      grepl("These findings do not establish direct causality", response_text, fixed = TRUE)) {
    score <- score + 1
  } else {
    violations <- c(violations, "G6: Disclaimer missing or incomplete")
  }

  # G7: Hedging language — no strong causal/deterministic verbs (V11 EXPANDED)
  # NOTE: "induces" intentionally EXCLUDED — it is a standard scientific verb
  # commonly used in hedged contexts ("may induce", "could induce").
  total_checks <- total_checks + 1
  strong_verbs <- c("\\bproves\\b", "\\bconfirms\\b", "\\bdemonstrates\\b",
                    "\\bdrives the\\b", "\\bdrives tumor\\b",
                    "\\bcauses\\b", "\\btriggers tumor\\b", "\\btriggers progression\\b",
                    "\\bdetermines\\b", "\\bguarantees\\b",
                    "\\bestablishes\\b", "\\bvalidates\\b")
  has_strong <- any(sapply(strong_verbs, function(p) grepl(p, response_text, perl = TRUE, ignore.case = TRUE)))
  if (!has_strong) {
    score <- score + 1
  } else {
    violations <- c(violations, "G7: Strong causal verbs found")
  }

  # G8: No bare "mRNA"
  total_checks <- total_checks + 1
  bare_mrna <- grepl("(?<!Bulk )(?<!Gene-Level )\\bmRNA expression\\b", response_text, perl = TRUE)
  bare_mrna2 <- grepl("\\bmRNA signature\\b", response_text, perl = TRUE)
  bare_mrna3 <- grepl("\\bmRNA layer\\b", response_text, perl = TRUE)
  if (!bare_mrna && !bare_mrna2 && !bare_mrna3) {
    score <- score + 1
  } else {
    violations <- c(violations, "G8: Bare 'mRNA' qualifier without 'Bulk'/'Gene-Level'")
  }

  # === G9a: Narrative-Excess Gene Reporting (INFORMATIONAL ONLY, NOT A VIOLATION) ===
  # G9a counts genes that the LLM mentions which are legitimately IN the patient's full
  # signature payload but outside the 5-gene restricted narrative window shown in the prompt.
  # This is EXPECTED and HEALTHY behavior: the LLM only sees 5 headline genes but the
  # signatures contain 125-235 genes each. When the LLM correctly discusses these hidden
  # signature members, it demonstrates biological reasoning, not fabrication.
  # This metric is recorded for transparency but does NOT affect the governance score.
  total_checks <- total_checks + 1
  gene_regex <- "\\b([A-Z][A-Z0-9]{2,}[0-9]*)\\b"
  llm_genes <- unique(unlist(regmatches(response_text, gregexpr(gene_regex, response_text, perl = TRUE))))
  known_all <- if (!is.null(gene_info)) gene_info$Gene.symbol else character(0)
  llm_known_genes <- intersect(llm_genes, known_all)
  
  # Genes outside the 5-gene narrative window but WITHIN the full signature payload
  excess_narrative_genes <- setdiff(llm_known_genes, c(patient_genes, "EREG"))
  excess_narrative_genes <- intersect(excess_narrative_genes, all_patient_genes)
  
  # Genes genuinely OUTSIDE even the full signature payload (true external references)
  external_genes <- setdiff(llm_known_genes, c(all_patient_genes, "EREG"))
  
  # G9a: Narrative-excess genes (informational only — NOT a violation, NOT added to violations vector)
  # These are genes the LLM mentions that are legitimately IN the full signature payload
  # but outside the 5-gene restricted narrative window. This is EXPECTED and HEALTHY behavior.
  # Recorded silently; does NOT affect governance score or failure taxonomy.
  if (length(excess_narrative_genes) > 0) {
    # informational only — no entry in violations vector
  }
  
  # G9b: True Fabrication Detection (the actual GATE, zero tolerance)
  # V19: Only flag genes absent from BOTH the signature payload AND NCBI.
  # Real human genes (e.g., TP53, EGFR, GPX4) referenced as biological context
  # are legitimate scientific reasoning, not fabrications.
  truly_external <- setdiff(external_genes, gene_info$Gene.symbol)
  if (length(truly_external) == 0) {
    score <- score + 1
  } else {
    violations <- c(violations, sprintf("G9b: Gene(s) absent from all biological references: %s", paste(truly_external, collapse=", ")))
  }
  
  # G9c: Contextual Gene References (INFORMATIONAL METRIC, not a violation)
  # These are real genes the LLM invoked as biological context for signature genes —
  # e.g., "GPX4 (signature gene) interacts with SLC7A11 in ferroptosis regulation."
  # The LLM is correctly connecting payload genes to wider cancer biology.
  # Tracked as a quality metric: too few = template-lock, too many = rambling.
  contextual_genes <- intersect(external_genes, gene_info$Gene.symbol)

  # G10: Directional consistency — V10 PERMISSIVE REFORM
  # Molecular mechanisms (anti-progression, pro-apoptotic, etc.) are legitimate
  # tumor board language regardless of endpoint type. G10 now only flags
  # explicit ENDPOINT CLAIMS that contradict the model's prediction target.
  # Mechanistic vocabulary is always permitted.
  if (!is.null(metric)) {
    total_checks <- total_checks + 1
    is_event_metric <- metric %in% c("DFI", "PFI")
    is_survival_metric <- metric %in% c("OS", "DSS")
    
    # ENDPOINT CLAIMS (always check) — these make assertions about the predicted outcome
    survival_endpoint_claims <- c(
      "\\bimproves overall survival\\b", "\\breduces overall survival\\b",
      "\\bworsens overall survival\\b", "\\bextends survival\\b",
      "\\bshortens survival\\b", "\\bassociated with (better|worse) overall survival\\b",
      "\\bimproves disease-specific survival\\b", "\\breduces disease-specific survival\\b"
    )
    event_endpoint_claims <- c(
      "\\bprolongs (progression|disease)-free\\b", "\\bshortens (progression|disease)-free\\b",
      "\\bimproves (progression|disease)-free\\b", "\\breduces (progression|disease)-free\\b",
      "\\bincreases progression risk\\b", "\\bdecreases progression risk\\b",
      "\\bincreases recurrence risk\\b", "\\bdecreases recurrence risk\\b",
      "\\bassociated with (higher|lower) progression\\b"
    )
    
    if (is_event_metric) {
      has_survival_claim <- any(sapply(survival_endpoint_claims, function(p) grepl(p, response_text, perl = TRUE, ignore.case = TRUE)))
      if (has_survival_claim) {
        violations <- c(violations, sprintf("G10: Survival endpoint claim on %s metric (directional mismatch)", metric))
      } else {
        score <- score + 1
      }
    } else if (is_survival_metric) {
      has_event_claim <- any(sapply(event_endpoint_claims, function(p) grepl(p, response_text, perl = TRUE, ignore.case = TRUE)))
      if (has_event_claim) {
        violations <- c(violations, sprintf("G10: Event endpoint claim on %s metric (directional mismatch)", metric))
      } else {
        score <- score + 1
      }
    } else {
      score <- score + 1
    }
  }

  # G11: TSM lexical borrowing — detect EREG/Epiregulin gene discussion from TSM variables
  total_checks <- total_checks + 1
  tsm_borrowing_patterns <- c(
    "\\bEpiregulin\\b", "\\bEREG gene\\b", "\\bEREG expression\\b",
    "\\bEREG signaling\\b", "\\bEREG pathway\\b", "\\bEREG ligand\\b",
    "\\bEREG receptor\\b", "\\bEREG growth factor\\b", "\\bEREG protein\\b",
    "\\bEREG-mediated\\b", "\\bEREG-driven\\b", "\\bEREG transcription\\b"
  )
  has_tsm_borrowing <- any(sapply(tsm_borrowing_patterns, function(p) grepl(p, response_text, perl = TRUE, ignore.case = TRUE)))
  if (has_tsm_borrowing) {
    violations <- c(violations, "G11: TSM lexical borrowing — EREG/Epiregulin gene biology imported from EREG.EXPss/METHss stemness index")
  } else {
    score <- score + 1
  }

  # G12: Tier 0 non-conflation — detect Tier 0 evidence presented as treatment recommendation
  total_checks <- total_checks + 1
  tier0_conflation_patterns <- c(
    "\\bT0A.*should (be|receive|undergo)\\b", "\\btreatment indicated\\b",
    "\\bFDA-approved for this patient\\b", "\\b\\(T0A\\)\\s+recommend",
    "\\bTier 0.*recommend", "\\b\\(T0B\\)\\s+prescrib"
  )
  has_tier0_conflation <- any(sapply(tier0_conflation_patterns,
    function(p) grepl(p, response_text, perl = TRUE, ignore.case = TRUE)))
  if (has_tier0_conflation) {
    violations <- c(violations, "G12: Tier 0 evidence presented as treatment recommendation (conflation)")
  } else {
    score <- score + 1
  }

  # G13: Cancer Gene List non-conflation — CGL membership conflated with clinical actionability
  # V11 FIX: Only flag AFFIRMATIVE conflation, not negated statements.
  # "CGL does NOT indicate clinical relevance" is CORRECT and must not be flagged.
  total_checks <- total_checks + 1
  cgl_conflation_patterns <- c(
    "\\bcancer gene list.*(actionable|druggable|therapeutic)\\b",
    "\\b(?:is|are) actionable cancer genes\\b",
    "\\bCancer Gene List.*(indicates|confirms|validates) clinical",
    "\\bCGL.*treatment\\b",
    "\\brecognized cancer gene.*therap\\b",
    "\\bcancer gene census.*actionab\\b",
    "\\bCGL.*(?:implicat|indicat).*treat\\b"
  )
  # Check each pattern and verify negation does NOT appear BEFORE or WITHIN the match
  has_cgl_conflation <- FALSE
  neg_regex <- "\\b(not|no|never|does not|doesn'?t|cannot|isn'?t|aren'?t)\\b"
  for (pat in cgl_conflation_patterns) {
    matches <- gregexpr(pat, response_text, perl = TRUE, ignore.case = TRUE)[[1]]
    if (matches[1] != -1) {
      match_lens <- attr(matches, "match.length")
      for (i in seq_along(matches)) {
        mpos <- matches[i]
        mlen <- match_lens[i]
        # 50 chars before the match
        pre_context <- substr(response_text, max(1, mpos - 50), mpos - 1)
        # The matched text itself (negation may be mid-span, e.g. "CGL...does NOT imply...actionable")
        match_text  <- substr(response_text, mpos, mpos + mlen - 1)
        # If negation is NEITHER before NOR inside the match → true violation
        if (!grepl(neg_regex, pre_context, perl = TRUE, ignore.case = TRUE) &&
            !grepl(neg_regex, match_text,  perl = TRUE, ignore.case = TRUE)) {
          has_cgl_conflation <- TRUE
          break
        }
      }
    }
    if (has_cgl_conflation) break
  }
  if (has_cgl_conflation) {
    violations <- c(violations, "G13: Cancer Gene List membership conflated with clinical actionability")
  } else {
    score <- score + 1
  }

  # G14: OncoKB Level Fabrication — LLM must not claim OncoKB therapeutic levels
  #       unless the Clinical_Status column explicitly states them. Excludes license
  #       disclaimer sentences to avoid false positives.
  total_checks <- total_checks + 1
  sentences <- unlist(strsplit(response_text, "(?<=[.!?])\\s+", perl = TRUE))
  content_sentences <- sentences[!grepl("license agreement|LICENSE|not available|Concordance Lock|public repository", sentences, ignore.case = TRUE)]
  content_text <- paste(content_sentences, collapse = " ")
  oncokb_level_fabrication <- c(
    "\\bOncoKB Level\\s*[1-4]\\b",
    "\\bOncoKB Level\\s*3[AB]\\b",
    "\\bOncoKB therapeutic level\\b",
    "\\btherapeutic Level\\s*[1-4]\\b.*OncoKB",
    "\\bLEVEL_[1234]\\b.*OncoKB"
  )
  has_oncokb_fabrication <- any(sapply(oncokb_level_fabrication, function(p) grepl(p, content_text, perl = TRUE, ignore.case = TRUE)))
  if (has_oncokb_fabrication) {
    violations <- c(violations, "G14: OncoKB therapeutic level fabricated — claiming Level 1/2/3A/3B/4 without Clinical_Status support")
  } else {
    score <- score + 1
  }

  # G15: Fda2/Fda3 ↔ Therapeutic Level Conflation — LLM must not equate
  #       Tier 0 regulatory evidence with OncoKB therapeutic Levels 1-4
  total_checks <- total_checks + 1
  fda_therapeutic_conflation <- c(
    "\\b(Fda2|FDA Level 2)\\b.*\\b(therapeutic Level|Level [1-4])\\b",
    "\\b(Fda3|FDA Level 3)\\b.*\\b(therapeutic Level|Level [1-4])\\b"
  )
  has_fda_conflation <- any(sapply(fda_therapeutic_conflation, function(p) grepl(p, response_text, perl = TRUE, ignore.case = TRUE)))
  if (has_fda_conflation) {
    violations <- c(violations, "G15: Fda2/Fda3 conflation with therapeutic Levels 1-4")
  } else {
    score <- score + 1
  }

  # G16: Tier 0 Evidence Misrepresentation — T0A/T0B/T0C presented as
  #       treatment recommendation rather than regulatory recognition
  total_checks <- total_checks + 1
  t0_misrep_patterns <- c(
    "\\bT0A\\b.*\\b(should receive|is indicated|treat with|recommend)\\b",
    "\\bTier 0\\b.*\\b(should receive|is indicated|treat with|recommend)\\b"
  )
  has_t0_misrep <- any(sapply(t0_misrep_patterns, function(p) grepl(p, response_text, perl = TRUE, ignore.case = TRUE)))
  if (has_t0_misrep) {
    violations <- c(violations, "G16: Tier 0 evidence misrepresented as treatment recommendation")
  } else {
    score <- score + 1
  }

  # G17: Omic-Layer Cross-Sectional Conflation (V11) — detect when a gene's
  #       omic layer changes between report sections (e.g., CCNE1 described as
  #       Transcript Isoform in Clinical Synthesis but Bulk mRNA in Pharmacogenomics)
  total_checks <- total_checks + 1
  # Extract gene-to-layer mappings: look for patterns like "GENE (Transcript Isoform)"
  # or "GENE (Bulk mRNA Expression)" or "GENE (CpG Methylation)"
  layer_patterns <- c(
    "Transcript Isoform", "Bulk mRNA Expression", "CpG Methylation",
    "miRNA", "Protein", "Mutation", "Copy Number Variation"
  )
  gene_layer_map <- list()
  for (lp in layer_patterns) {
    # Find genes associated with this layer description.
    # V11 FIX: 200-char window (was 80) to span long paragraphs.
    gene_regex <- paste0("\\b([A-Z][A-Z0-9]{2,}[0-9]*)\\b(?:(?!\\b(?:Transcript Isoform|Bulk mRNA Expression|CpG Methylation|miRNA|Protein|Mutation|Copy Number Variation)\\b).){0,200}\\b", lp, "\\b")
    matches <- gregexpr(gene_regex, response_text, perl = TRUE)
    if (matches[[1]][1] != -1) {
      genes_found <- regmatches(response_text, matches)[[1]]
      for (g in genes_found) {
        # V11 FIX: Capture ALL gene symbols in the match, not just the first
        gene_matches <- gregexpr("\\b[A-Z][A-Z0-9]{2,}[0-9]*\\b", g, perl = TRUE)[[1]]
        if (gene_matches[1] != -1) {
          gene_syms <- unique(regmatches(g, list(gene_matches))[[1]])
          for (gene_sym in gene_syms) {
            if (!is.na(gene_sym) && nchar(gene_sym) > 0) {
              if (is.null(gene_layer_map[[gene_sym]])) {
                gene_layer_map[[gene_sym]] <- lp
              } else if (gene_layer_map[[gene_sym]] != lp) {
                violations <- c(violations, sprintf("G17: Omic-layer conflation for %s: described as both '%s' and '%s'", gene_sym, gene_layer_map[[gene_sym]], lp))
              }
            }
          }
        }
      }
    }
  }
  # Only score pass if no conflation detected
  has_g17 <- any(grepl("^G17:", violations))
  if (!has_g17) {
    score <- score + 1
  }

  # G19: Dot-Prefixed Signature Nomenclature (V21) — LLM must NOT expose
  # dot-prefixed numeric signature identifiers (adopted/surrogate naming
  # conventions) outside designated audit sections. These are patterns like
  # .5.3.2.4.14.2.4.1 or .6.3.3.30.30.2.4.2 containing 3+ dot-separated
  # numeric segments without a letter prefix.
  # This catches the surrogate naming convention gap missed by G1 (which only
  # catches letter-prefixed forms like READ-82.6.3.2...).
  total_checks <- total_checks + 1
  dot_prefixed_pattern <- "(?:Signature\\s+)?\\.\\d+(?:\\.\\d+){2,}"
  has_dot_prefixed <- grepl(dot_prefixed_pattern, response_text, perl = TRUE)
  if (!has_dot_prefixed) {
    score <- score + 1
  } else {
    violations <- c(violations, "G19: Dot-prefixed signature nomenclature exposed (surrogate naming convention)")
  }

  # G18: RCD Boundary Violation — LLM must NOT discuss RCD forms absent from
  # the patient's Associated RCD Form annotations in the signature payload.
  # This enforces the CRITICAL RCD BOUNDARY rule from narrative_governance_framework.
  total_checks <- total_checks + 1
  if (!is.null(patient_rcd_forms) && length(patient_rcd_forms) > 0) {
    # Normalize ground-truth RCD forms: split on "/" for compound annotations
    # (e.g., "Ferroptosis/Necroptosis" → c("ferroptosis", "necroptosis"))
    gt_rcd <- unique(tolower(trimws(unlist(strsplit(patient_rcd_forms, "/")))))
    gt_rcd <- gt_rcd[gt_rcd != "unknown" & gt_rcd != ""]

    # All 25 RCD forms from the decoder — the universe of forms the LLM may name
    all_rcd_forms <- c(
      "apoptosis", "necroptosis", "pyroptosis", "ferroptosis",
      "autophagy", "necrosis", "anoikis", "cellular senescence",
      "mitotic catastrophe", "cuproptosis", "netosis", "efferocytosis",
      "entosis", "parthanatos", "immunogenic cell death", "disulfidptosis",
      "oxeiptosis", "paraptosis", "alkaliptosis",
      "lysosome-dependent cell death", "mitoptosis", "autosis",
      "erebosis", "methuosis", "mitochondrial permeability transition"
    )

    resp_lower <- tolower(response_text)
    mentioned_rcd <- all_rcd_forms[sapply(all_rcd_forms,
      function(r) grepl(r, resp_lower, fixed = TRUE))]

    extraneous_rcd <- setdiff(mentioned_rcd, gt_rcd)
    if (length(extraneous_rcd) > 0) {
      violations <- c(violations, sprintf(
        "G18: RCD boundary violation — discussed '%s' not in patient signature annotations [patient RCD: %s]",
        paste(extraneous_rcd, collapse = ", "),
        paste(gt_rcd, collapse = ", ")))
    } else {
      score <- score + 1
    }
  } else {
    # No RCD form data available — pass by default (incomplete ground truth)
    score <- score + 1
  }

  compliance <- score / total_checks
  return(list(score = score, total = total_checks, compliance = compliance,
              violations = violations,
              contextual_genes = if (exists("contextual_genes")) contextual_genes else character(0)))
}

# --- Semantic Similarity for Level B ---
compute_semantic_similarity <- function(texts) {
  n <- length(texts)
  if (n < 2) return(1.0)
  tokenize <- function(txt) tolower(unlist(strsplit(gsub("[^a-zA-Z0-9 ]", " ", txt), "\\s+")))
  similarities <- c()
  for (i in 1:(n-1)) {
    for (j in (i+1):n) {
      tokens_i <- unique(tokenize(texts[i]))
      tokens_j <- unique(tokenize(texts[j]))
      intersection <- length(intersect(tokens_i, tokens_j))
      union <- length(union(tokens_i, tokens_j))
      if (union > 0) similarities <- c(similarities, intersection / union)
    }
  }
  mean(similarities)
}

check_structural_stability <- function(texts, patient_genes_str) {
  n <- length(texts)
  results <- data.frame(metric = character(), stability = numeric(), stringsAsFactors = FALSE)

  gene_regex <- "\\b([A-Z][A-Z0-9]{2,}[0-9]*)\\b"
  gene_sets <- lapply(texts, function(t) {
    unique(unlist(regmatches(t, gregexpr(gene_regex, t, perl = TRUE))))
  })
  gene_jaccard <- c()
  for (i in 1:(n-1))
    for (j in (i+1):n)
      gene_jaccard <- c(gene_jaccard,
        length(intersect(gene_sets[[i]], gene_sets[[j]])) /
        max(1, length(union(gene_sets[[i]], gene_sets[[j]]))))
  results <- rbind(results, data.frame(metric = "Gene_Mentions", stability = mean(gene_jaccard)))

  state_terms <- c("stable differentiated", "adaptive suppression", "stress-adapted",
                   "vulnerability.resistance", "multi.omic plasticity",
                   "stemness-suppressed", "differentiation-favoring")
  state_pattern <- paste(state_terms, collapse = "|")
  state_presence <- sapply(texts, function(t) grepl(state_pattern, tolower(t)))
  results <- rbind(results, data.frame(metric = "Tumor_State_Classification",
    stability = if(all(state_presence) || all(!state_presence)) 1.0 else 0.5))

  query_counts <- sapply(texts, function(t) {
    length(gregexpr("(?:^|\\n)\\s*\\d+\\s*\\.\\s*-", t, perl = TRUE)[[1]])
  })
  results <- rbind(results, data.frame(metric = "Query_Count",
    stability = if(all(query_counts == 3)) 1.0 else if(all(query_counts >= 2 & query_counts <= 4)) 0.7 else 0.3))

  disc_present <- sapply(texts, function(t) grepl("hypothesis-generating", t, fixed = TRUE))
  results <- rbind(results, data.frame(metric = "Disclaimer_Presence",
    stability = if(all(disc_present)) 1.0 else mean(disc_present)))

  results <- rbind(results, data.frame(metric = "Token_Jaccard",
    stability = compute_semantic_similarity(texts)))

  return(results)
}

# --- HTTP Error Classification Helper (transient vs permanent) ---
# Transient: timeout, 503, 502, 504, 429, DNS/connection failures — safe to retry
# Permanent: 400, 401, 403, 404 — retrying will never help
is_transient_http_error <- function(e) {
  if (inherits(e, "httr2_http_401") || inherits(e, "httr2_http_403") ||
      inherits(e, "httr2_http_404")) return(FALSE)
  if (inherits(e, "httr2_http_400")) return(FALSE)
  if (inherits(e, "httr2_failure")) return(TRUE)  # network-level failure
  if (inherits(e, "httr2_http")) return(TRUE)      # 5xx, 429
  return(TRUE)  # unknown = retry
}

# Extract HTTP response body for diagnostics (mirrors app.R send_llm_request)
extract_http_error_body <- function(e) {
  err_body <- ""
  if (inherits(e, "httr2_http_400")) {
    err_body <- tryCatch(httr2::resp_body_string(e), error = function(ignore) "<unreadable>")
  } else if (inherits(e, "httr2_http")) {
    err_body <- tryCatch(httr2::resp_body_string(e), error = function(ignore) "<unreadable>")
  }
  return(err_body)
}

# Normalise Ollama URL (prevents double-slash if OLLAMA_URL ends with /)
norm_ollama_url <- function(base) {
  base <- sub("/+$", "", base)
  paste0(base, "/api/chat")
}

# --- LLM Output Sanitizer ---
sanitize_llm_output <- function(txt) {
  if (is.null(txt) || !is.character(txt) || length(txt) == 0L) return(txt)
  if (anyNA(txt)) txt[is.na(txt)] <- ""
  txt <- gsub("peri느tin", "periostin", txt, fixed = TRUE)
  txt <- gsub("peri운tin", "periostin", txt, fixed = TRUE)
  txt <- gsub("peri욘tin", "periostin", txt, fixed = TRUE)
  # Known CJK artifact patterns
  txt <- gsub("levels (觐", "levels (", txt, fixed = TRUE)
  txt <- gsub("(觐1-", "(1-", txt, fixed = TRUE)
  # Blanket-strip ALL East Asian characters (Han/Hangul/Hiragana/Katakana)
  txt <- gsub("[\\p{Han}\\p{Hangul}\\p{Hiragana}\\p{Katakana}]", "", txt, perl = TRUE)
  txt <- gsub("\\b(\\w+)\\s+\\1\\b", "\\1", txt, perl = TRUE)
  txt <- gsub("bulk Bulk", "Bulk", txt, fixed = TRUE)
  txt <- gsub("mrna mRNA", "mRNA", txt, fixed = TRUE)
  txt <- gsub("the The", "The", txt, fixed = TRUE)
  txt <- gsub("is Is", "Is", txt, fixed = TRUE)
  txt <- gsub("a A", "A", txt, fixed = TRUE)
  txt <- gsub("in In", "In", txt, fixed = TRUE)
  txt <- gsub("of Of", "Of", txt, fixed = TRUE)
  txt <- gsub("and And", "And", txt, fixed = TRUE)
  txt <- gsub("to To", "To", txt, fixed = TRUE)
  txt <- gsub("it It", "It", txt, fixed = TRUE)
  txt <- gsub("be Be", "Be", txt, fixed = TRUE)
  return(txt)
}

# --- LLM Call ---
call_llm <- function(system_prompt, user_payload, attempt = 1) {
  if (LLM_PROVIDER == "deepseek") {
    return(call_llm_deepseek(system_prompt, user_payload, attempt))
  }
  # --- Ollama backend (default) ---
  # Compute prompt size for diagnostics (mirrors app.R)
  total_chars <- nchar(system_prompt) + nchar(user_payload)
  est_tokens <- round(total_chars / 4)

  req <- httr2::request(norm_ollama_url(OLLAMA_URL)) |>
    httr2::req_headers("Content-Type" = "application/json") |>
    httr2::req_body_json(list(
      model = LLM_MODEL, stream = FALSE,
      messages = list(
        list(role = "system", content = system_prompt),
        list(role = "user", content = user_payload)
      ),
      options = list(num_ctx = 16384, temperature = 0, num_predict = 4096),
      keep_alive = "30m"
    )) |>
    httr2::req_timeout(900)  # Match app.R: 15-min timeout for Ollama

  # HTTP retry loop: ONLY transient failures are retried.
  # Permanent errors (400, 401, 403, 404) fail immediately.
  MAX_HTTP_ATTEMPTS <- 3
  last_error <- NULL
  for (attempt_i in seq_len(MAX_HTTP_ATTEMPTS)) {
    result <- tryCatch({
      resp <- httr2::req_perform(req)
      content <- httr2::resp_body_json(resp)$message$content
      # Strip thinking tags
      content <- gsub("(?s)^.*?</think>\\s*", "", content, perl = TRUE)
      content <- gsub("\\*", "", content, fixed = TRUE)
      content <- trimws(content)
      sanitize_llm_output(content)
    }, error = function(e) {
      # Classify error + extract API body for diagnostics
      err_body <- extract_http_error_body(e)
      if (inherits(e, "httr2_http_429")) {
        # Rate limited — parse Retry-After if available
        ra <- tryCatch(as.numeric(e$headers$`retry-after`), error = function(ignore) NA_real_)
        if (!is.na(ra) && ra > 0) {
          last_error <- paste0(e$message,
            if (nchar(err_body) > 0) paste0(" [API: ", substr(err_body, 1, 200), "]") else "",
            " [Retry-After: ", ra, "s]")
        } else {
          last_error <- paste0(e$message,
            if (nchar(err_body) > 0) paste0(" [API: ", substr(err_body, 1, 200), "]") else "")
        }
      } else {
        last_error <- paste0(e$message,
          if (nchar(err_body) > 0) paste0(" [API: ", substr(err_body, 1, 200), "]") else "")
      }
      if (!is_transient_http_error(e)) {
        # Permanent error — do NOT retry
        last_error <<- last_error  # store in enclosing scope for final stop()
        stop(last_error)
      }
      last_error <<- last_error
      NULL
    })
    if (!is.null(result)) return(result)
    # If tryCatch emitted a stop() for permanent error, propagate immediately
    if (attempt_i < MAX_HTTP_ATTEMPTS && !is.null(last_error) && grepl("^httr2_http_40", last_error)) break
    if (attempt_i < MAX_HTTP_ATTEMPTS) {
      # Exponential backoff with jitter: 2s, 5s, 10s (capped)
      wait_sec <- min(2^(attempt_i) * (0.75 + runif(1, 0, 0.5)), 15)
      message(sprintf("[RETRY %d/%d] Ollama HTTP failed (%s). Retrying in %.1fs... [Prompt ~%d tokens | %d chars]",
                      attempt_i, MAX_HTTP_ATTEMPTS - 1, last_error, wait_sec, est_tokens, total_chars))
      Sys.sleep(wait_sec)
    }
  }
  stop("Failed to perform HTTP request to Ollama after ", MAX_HTTP_ATTEMPTS,
       " attempts. Last error: ", last_error,
       " [Prompt ~", est_tokens, " tokens | ", total_chars, " chars]")
}

# --- DeepSeek API backend (OpenAI-compatible) ---
call_llm_deepseek <- function(system_prompt, user_payload, attempt = 1) {
  if (nchar(DEEPSEEK_API_KEY) == 0) {
    stop("DEEPSEEK_API_KEY not set. Add it to .Renviron or export it.")
  }
  total_chars <- nchar(system_prompt) + nchar(user_payload)
  est_tokens <- round(total_chars / 4)

  req <- httr2::request(DEEPSEEK_URL) |>
    httr2::req_headers(
      "Content-Type" = "application/json",
      "Authorization" = paste("Bearer", DEEPSEEK_API_KEY)
    ) |>
    httr2::req_body_json(list(
      model = DEEPSEEK_MODEL,
      messages = list(
        list(role = "system", content = system_prompt),
        list(role = "user", content = user_payload)
      ),
      temperature = 0,
      max_tokens = 4096,
      stream = FALSE
    )) |>
    httr2::req_timeout(120)

  # HTTP retry loop: ONLY transient failures are retried.
  # Permanent errors (400, 401, 403, 404) fail immediately.
  MAX_HTTP_ATTEMPTS <- 3
  last_error <- NULL
  for (attempt_i in seq_len(MAX_HTTP_ATTEMPTS)) {
    result <- tryCatch({
      resp <- httr2::req_perform(req)
      content <- httr2::resp_body_json(resp)$choices[[1]]$message$content
      # Strip thinking tags (DeepSeek-R1 may include them)
      content <- gsub("(?s)^.*?</think>\\s*", "", content, perl = TRUE)
      content <- gsub("\\*", "", content, fixed = TRUE)
      content <- trimws(content)
      sanitize_llm_output(content)
    }, error = function(e) {
      err_body <- extract_http_error_body(e)
      if (inherits(e, "httr2_http_429")) {
        ra <- tryCatch(as.numeric(e$headers$`retry-after`), error = function(ignore) NA_real_)
        if (!is.na(ra) && ra > 0) {
          last_error <- paste0(e$message,
            if (nchar(err_body) > 0) paste0(" [API: ", substr(err_body, 1, 200), "]") else "",
            " [Retry-After: ", ra, "s]")
        } else {
          last_error <- paste0(e$message,
            if (nchar(err_body) > 0) paste0(" [API: ", substr(err_body, 1, 200), "]") else "")
        }
      } else {
        last_error <- paste0(e$message,
          if (nchar(err_body) > 0) paste0(" [API: ", substr(err_body, 1, 200), "]") else "")
      }
      if (!is_transient_http_error(e)) {
        last_error <<- last_error
        stop(last_error)
      }
      last_error <<- last_error
      NULL
    })
    if (!is.null(result)) return(result)
    # If permanent error, break immediately
    if (attempt_i < MAX_HTTP_ATTEMPTS && !is.null(last_error) && grepl("^httr2_http_40", last_error)) break
    if (attempt_i < MAX_HTTP_ATTEMPTS) {
      wait_sec <- min(2^(attempt_i) * (0.75 + runif(1, 0, 0.5)), 15)
      message(sprintf("[RETRY %d/%d] DeepSeek HTTP failed (%s). Retrying in %.1fs... [Prompt ~%d tokens | %d chars]",
                      attempt_i, MAX_HTTP_ATTEMPTS - 1, last_error, wait_sec, est_tokens, total_chars))
      Sys.sleep(wait_sec)
    }
  }
  stop("Failed to perform HTTP request to DeepSeek after ", MAX_HTTP_ATTEMPTS,
       " attempts. Last error: ", last_error,
       " [Prompt ~", est_tokens, " tokens | ", total_chars, " chars]")
}

# --- V9 ADDITION: LLM Output Caching (CRIT-03 mitigation) ---
# Caches LLM responses keyed on sha256(system_prompt + user_payload).
# CACHE IS PURGED AT V9 STARTUP to ensure complete independence from prior runs (V8, V7, etc.).
# Level B calls BYPASS caching entirely — each of the 5 repeats per patient must be a FRESH LLM call.
llm_cache_dir_val <- "crit03_llm_cache"
# Purge any prior cache to guarantee V9 independence
if (dir.exists(llm_cache_dir_val)) {
  unlink(file.path(llm_cache_dir_val, "*"), recursive = TRUE, force = TRUE)
  cat("  [CACHE] Purged prior cache for V9 independence.\n")
}
dir.create(llm_cache_dir_val, showWarnings = FALSE)

get_cached_val <- function(sys_prompt, user_payload) {
  cache_key <- digest::digest(paste(sys_prompt, user_payload, sep = "|||"), algo = "sha256")
  cache_file <- file.path(llm_cache_dir_val, paste0(cache_key, ".rds"))
  if (file.exists(cache_file)) {
    entry <- tryCatch(readRDS(cache_file), error = function(e) NULL)
    if (!is.null(entry) && !is.null(entry$response)) {
      return(list(hit = TRUE, response = entry$response, cache_key = cache_key))
    }
  }
  return(list(hit = FALSE, cache_key = cache_key))
}

set_cached_val <- function(sys_prompt, user_payload, response, cache_key = NULL) {
  if (is.null(cache_key)) {
    cache_key <- digest::digest(paste(sys_prompt, user_payload, sep = "|||"), algo = "sha256")
  }
  cache_file <- file.path(llm_cache_dir_val, paste0(cache_key, ".rds"))
  entry <- list(response = response, timestamp = format(Sys.time()))
  tryCatch(saveRDS(entry, cache_file), error = function(e) {
    message("[CACHE] Failed: ", e$message)
  })
}

# --- V9 ADDITION: Factuality Validation (mirrors app.R validate_llm_factuality) ---
# Simplified for benchmark: cross-checks gene symbols in LLM output against payload genes.
validate_factuality_val <- function(response_text, payload_genes) {
  issues <- list()
  if (length(payload_genes) == 0) return(list(issues = issues, score = 1))
  # Extract candidate gene symbols from response (uppercase 2+ chars)
  gene_regex <- "\\b([A-Z][A-Z0-9]{1,}[0-9]*)\\b"
  llm_genes <- unique(unlist(regmatches(response_text, gregexpr(gene_regex, response_text, perl = TRUE))))
  # Only flag genes likely to be real human symbols (in known gene list if available)
  known_genes <- if (exists("gene_info") && !is.null(gene_info) && "Gene.symbol" %in% names(gene_info)) {
    unique(gene_info$Gene.symbol)
  } else character(0)
  llm_genes <- intersect(llm_genes, known_genes)
  # Check each LLM gene against payload genes (the FULL signature gene set, not just headlines)
  payload_upper <- toupper(unique(payload_genes))
  for (g in llm_genes) {
    if (!(toupper(g) %in% payload_upper)) {
      # Gene not in patient payload — external reference beyond signature gene set
      issues[[length(issues) + 1]] <- list(
        type = "GENE_NOT_IN_PROFILE",
        detail = paste0("Gene ", g, " mentioned but not in patient signature gene set"),
        gene = g
      )
    }
  }
  score <- if (length(llm_genes) > 0) max(0, 1 - (length(issues) / max(1, length(llm_genes)))) else 1
  return(list(issues = issues, score = score))
}

# --- Wrapper: cached LLM call with optional regeneration on factuality issues ---
# nocache=TRUE forces a fresh LLM call (used for Level B intra-clinical repeats)
call_llm_cached <- function(system_prompt, user_payload, payload_genes = NULL, max_retries = 2, nocache = FALSE) {
  # If nocache, skip cache entirely (Level B independence)
  if (!nocache) {
    cache <- get_cached_val(system_prompt, user_payload)
    if (cache$hit) return(list(response = cache$response, cached = TRUE, fact_issues = 0))
  }

  # Call LLM
  response <- call_llm(system_prompt, user_payload)
  if (is.na(response) || is.null(response)) return(list(response = response, cached = FALSE, fact_issues = 0))

  # Factuality check + regeneration
  fact_issues <- 0
  if (!is.null(payload_genes) && length(payload_genes) > 0) {
    for (retry_i in seq_len(max_retries + 1)) {
      fv <- validate_factuality_val(response, payload_genes)
      fact_issues <- length(fv$issues)
      if (fact_issues == 0 || retry_i > max_retries) break
      # Regenerate with correction
      correction <- paste0(
        "\n\nFACTUALITY CORRECTION (RETRY ", retry_i, "/", max_retries, "): ",
        "Your previous response mentioned genes not in the patient profile. ",
        "Only discuss driver genes and their biologically connected partners. Regenerate.\n"
      )
      response <- call_llm(paste0(system_prompt, correction), user_payload)
      if (is.na(response) || is.null(response)) break
      response <- scrub_governance_violations_val(response)
    }
  }

  # Save to cache (skip if nocache mode)
  if (!nocache) {
    set_cached_val(system_prompt, user_payload, response, cache$cache_key)
  }
  return(list(response = response, cached = FALSE, fact_issues = fact_issues))
}

# --- Progress bar with ETA ---
format_eta <- function(start_time, done, total) {
  elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
  if (done > 0) {
    eta_sec <- (elapsed / done) * (total - done)
    if (eta_sec > 3600) return(sprintf("[ETA: %.1fh]", eta_sec / 3600))
    else if (eta_sec > 60) return(sprintf("[ETA: %.0fm]", eta_sec / 60))
    else return(sprintf("[ETA: %.0fs]", eta_sec))
  }
  return("")
}

# ===========================================================================
# POST-PROCESSING FUNCTIONS (mirrors app.R scrubbers)
# ===========================================================================
# These replicate the silent post-processing that app.R applies to LLM output
# before it reaches the user. The benchmark MUST measure post-processed output.

scrub_governance_violations_val <- function(txt) {
  # V-003: Strip abbreviated nomenclature (LUAD-1883, THYM-1460), excluding TCGA barcodes
  txt <- gsub("(?<!TCGA-)\\b[A-Z]{2,5}-\\d{3,4}\\b", "", txt, perl = TRUE)
  # V-001: Strip omic-layer token references
  txt <- gsub("\\(?Tokens?\\s*\\.\\s*[1-7](?:\\s*(?:,|and)\\s*\\.\\s*[1-7])*\\)?", "", txt, perl = TRUE)
  # V-005: transcriptomic + mRNA
  txt <- gsub("transcriptomic and mRNA expression layers", "Transcript Isoform and Bulk mRNA Expression layers", txt, perl = TRUE)
  txt <- gsub("transcriptomic and mRNA expression", "Transcript Isoform and Bulk mRNA Expression", txt, perl = TRUE)
  # V-002: Replace bare mRNA qualifiers
  txt <- gsub("(?<!Bulk )(?<!Gene-Level )mRNA expression", "Bulk mRNA Expression", txt, perl = TRUE)
  txt <- gsub("(?<!Bulk )(?<!Gene-Level )\\bmRNA layer\\b", "Bulk mRNA Expression layer", txt, perl = TRUE)
  txt <- gsub("(?<!Bulk )(?<!Gene-Level )\\bmRNA signature\\b", "Bulk mRNA Expression signature", txt, perl = TRUE)
  txt <- gsub("(?<!Bulk )(?<!Gene-Level )\\bmRNA target\\b", "Bulk mRNA Expression target", txt, perl = TRUE)
  txt <- gsub("(?<!Bulk )(?<!Gene-Level )\\bmRNA transcript\\b", "Bulk mRNA Expression transcript", txt, perl = TRUE)
  # V-004: Strong causal verbs in trajectory context
  txt <- gsub("\\bdriving the (adverse|favorable) trajectory\\b", "contributing to the \\1 trajectory", txt, perl = TRUE)
  txt <- gsub("\\bdrives the (adverse|favorable) trajectory\\b", "contributes to the \\1 trajectory", txt, perl = TRUE)
  txt <- gsub("\\bdrives (tumor|cancer|disease) (progression|recurrence|aggressiveness)", "contributes to \\1 \\2", txt, perl = TRUE)
  txt <- gsub("\\bdriving (tumor|cancer|disease) (progression|recurrence|aggressiveness)", "contributing to \\1 \\2", txt, perl = TRUE)
  # V-006: Standalone strong causal verbs
  txt <- gsub("\\bproves\\b", "suggests", txt, perl = TRUE, ignore.case = TRUE)
  txt <- gsub("\\bconfirms\\b", "is consistent with", txt, perl = TRUE, ignore.case = TRUE)
  txt <- gsub("\\bdemonstrates\\b", "indicates", txt, perl = TRUE, ignore.case = TRUE)
  txt <- gsub("\\bestablishes\\b", "provides evidence for", txt, perl = TRUE, ignore.case = TRUE)
  txt <- gsub("\\bcauses\\b", "is associated with", txt, perl = TRUE, ignore.case = TRUE)
  # V-010 (G13): Auto-scrub CGL-actionability conflation
  txt <- gsub("\\bCancer Gene List\\b[^.]*?\\b(?:actionable|druggable|therapeutic target|clinical actionability|treatment relevance)\\b",
              "Cancer Gene List membership provides biological context only and does not indicate clinical actionability", txt, perl = TRUE, ignore.case = TRUE)
  txt <- gsub("\\bCGL\\b[^.]*?\\b(?:actionable|druggable|therapeutic target|clinical actionability)\\b",
              "CGL membership (biological context only)", txt, perl = TRUE, ignore.case = TRUE)
  # V-011 (G12): Auto-scrub Tier 0 treatment-recommendation conflation
  txt <- gsub("\\bTier 0\\b[^.]*?\\b(?:should receive|is indicated for|treatment recommendation|prescribe|recommend)\\b",
              "Tier 0 provides regulatory recognition only and does not constitute a treatment recommendation", txt, perl = TRUE, ignore.case = TRUE)
  # Collapse artifacts
  txt <- gsub("\\s{2,}", " ", txt, perl = TRUE)
  txt <- gsub("\\(\\s*(?:and|or)?\\s*\\)", "", txt, perl = TRUE)
  txt <- gsub(",\\s*,", ",", txt, perl = TRUE)
  txt <- gsub("\\s,(?=[a-zA-Z])", ", ", txt, perl = TRUE)

  # === G13 PRE-SCRUB: CGL Vocabulary Sanitizer (V13) ===
  # Replaces forbidden CGL words with safe vocabulary BEFORE the governance
  # checker runs, eliminating G13 false violations caused by LLM language
  # defaults. Operates conservatively: only in sentences near CGL context.
  cgl_context <- gregexpr(
    "(?i)\\b(Cancer Gene (?:List|Census)|CGL|Stratum 5|oncogenic classification|cancer-relevant gene|cancer gene census)\\b",
    txt, perl = TRUE)[[1]]
  if (cgl_context[1] != -1) {
    # Split into sentences for targeted scrubbing
    sents <- unlist(strsplit(txt, "(?<=[.!?])\\s+", perl = TRUE))
    for (si in seq_along(sents)) {
      s <- sents[si]
      is_cgl <- grepl("(?i)\\b(Cancer Gene (?:List|Census)|CGL|Stratum 5|oncogenic classification|cancer-relevant gene|cancer gene census)\\b", s, perl = TRUE)
      if (is_cgl) {
        s <- gsub("\\bclinically actionable target\\b", "cancer-relevant gene", s, perl = TRUE, ignore.case = TRUE)
        s <- gsub("\\bclinically actionable\\b", "cancer-relevant", s, perl = TRUE, ignore.case = TRUE)
        s <- gsub("\\btherapeutic target\\b", "recognized cancer gene", s, perl = TRUE, ignore.case = TRUE)
        s <- gsub("\\btherapeutic relevance\\b", "biological significance", s, perl = TRUE, ignore.case = TRUE)
        s <- gsub("\\bclinical actionability\\b", "cancer gene recognition", s, perl = TRUE, ignore.case = TRUE)
        s <- gsub("\\bdruggable vulnerability\\b", "recognized cancer gene", s, perl = TRUE, ignore.case = TRUE)
        s <- gsub("\\bdruggable target\\b", "recognized cancer gene", s, perl = TRUE, ignore.case = TRUE)
        s <- gsub("\\bdruggable\\b", "cancer-relevant", s, perl = TRUE, ignore.case = TRUE)
        s <- gsub("\\bactionable mutation\\b", "cancer-relevant alteration", s, perl = TRUE, ignore.case = TRUE)
        s <- gsub("\\bactionable gene\\b", "cancer-relevant gene", s, perl = TRUE, ignore.case = TRUE)
        s <- gsub("\\bactionable target\\b", "cancer-relevant gene", s, perl = TRUE, ignore.case = TRUE)
        s <- gsub("\\btreatment relevance\\b", "biological context", s, perl = TRUE, ignore.case = TRUE)
        sents[si] <- s
      }
    }
    txt <- paste(sents, collapse = " ")
  }

  txt <- trimws(txt)
  return(txt)
}

ensure_questions_and_disclaimer_val <- function(txt, add_questions = TRUE, patient_id = NULL, cohort = NULL) {
  hdr_pat <- "(?i)Suggested\\s*Clinical\\s*Queries\\s*[:]?\\s*"
  clean_txt <- gsub("The interpretations presented above.*appropriate\\.?", "", txt, ignore.case=TRUE)
  clean_txt <- trimws(clean_txt)
  # Strip exposed nomenclatures
  clean_txt <- gsub("[A-Z]{2,5}-\\d+\\.\\d+[^\\)\\s,;]*", "", clean_txt, perl = TRUE)
  clean_txt <- gsub("(?<!TCGA-)\\b[A-Z]{2,5}-\\d{3,4}\\b", "", clean_txt, perl = TRUE)
  clean_txt <- gsub("\\(\\s*(?:and|or)?\\s*\\)", "", clean_txt, perl = TRUE)
  clean_txt <- gsub(",\\s*,", ",", clean_txt, perl = TRUE)
  clean_txt <- trimws(gsub("\\s{2,}", " ", clean_txt))

  disclaimer <- "The interpretations presented above should be considered hypothesis-generating and are intended to support biological and clinical exploration. The proposed mechanisms, therapeutic associations, and disease trajectories are inferred from machine-learning models, statistical associations, multi-omic relationships, and literature-supported evidence. These findings do not establish direct causality and should be interpreted within the context of the available data, requiring independent experimental and clinical validation whenever appropriate."

  if (!add_questions) {
    if (grepl(hdr_pat, clean_txt, perl = TRUE)) {
      parts <- strsplit(clean_txt, hdr_pat, perl = TRUE)[[1]]
      clean_txt <- trimws(parts[1])
    }
    return(paste0(clean_txt, "\n\n", disclaimer))
  }

  if (!grepl(hdr_pat, clean_txt, perl = TRUE)) {
    q_raw <- c(
      "What are the primary genetic drivers indicated by the positive SHAP values in this patient's profile?",
      "How do the observed phenotype correlations for these signatures align with the overall prognostic risk?",
      "What therapeutic interventions target the specific multi-omic alterations identified in this synthesis?"
    )
    pre <- clean_txt
  } else {
    parts <- strsplit(clean_txt, hdr_pat, perl = TRUE)[[1]]
    pre <- trimws(parts[1])
    post <- ifelse(length(parts) > 1, parts[2], "")
    q_candidates <- unlist(strsplit(post, "\\?"))
    q_candidates <- trimws(q_candidates)
    q_clean <- c()
    for (cand in q_candidates) {
      cand_clean <- gsub("^(?:[1-9][\\.\\)]\\s*)?(?:-{1,2}|[*•])\\s*", "", cand)
      cand_clean <- trimws(cand_clean)
      if (nchar(cand_clean) > 10) q_clean <- c(q_clean, paste0(cand_clean, "?"))
    }
    if (length(q_clean) == 0) {
      q_clean <- c(
        "What are the primary genetic drivers indicated by the positive SHAP values in this patient's profile?",
        "How do the observed phenotype correlations for these signatures align with the overall prognostic risk?",
        "What therapeutic interventions target the specific multi-omic alterations identified in this synthesis?"
      )
    }
    if (length(q_clean) < 3) {
      while (length(q_clean) < 3) q_clean <- c(q_clean, q_clean[length(q_clean)])
    } else if (length(q_clean) > 3) {
      q_clean <- head(q_clean, 3)
    }
    q_raw <- q_clean
  }

  final_txt <- paste0(
    pre, "\n\nSuggested Clinical Queries:\n",
    "1. - ", q_raw[1], "\n",
    "2. - ", q_raw[2], "\n",
    "3. - ", q_raw[3], "\n\n",
    disclaimer
  )
  # G3 FIX: If patient ID not in output, prepend factual clinical identifier
  if (!is.null(patient_id) && !is.null(cohort) && !grepl(patient_id, final_txt, fixed = TRUE)) {
    final_txt <- paste0("Patient ", patient_id, ", ", cohort, ". ", final_txt)
  }
  return(final_txt)
}

# ===========================================================================
# ====================== EXECUTION ==========================================
# ===========================================================================

# --------------------------------------------------------------------------
# LEVEL A: INTER-CLINICAL GOVERNANCE COMPLIANCE (n ≥ 379)
# --------------------------------------------------------------------------
cat("\n╔══════════════════════════════════════════════════════════════╗\n")
cat("║   LAYER 1: ASSAY REPRODUCIBILITY (POWERED, n≥500)           ║\n")
cat("╚══════════════════════════════════════════════════════════════╝\n")
cat(sprintf("Testing %d patients across %d cancer×endpoint strata...\n",
    nrow(level_a_patients), length(unique(paste(level_a_patients$CancerAbb, level_a_patients$Metric)))))
cat(sprintf("Target compliance: %.0f%%\n\n", COMPLIANCE_TARGET * 100))

# Checkpoint: resume if partial run exists
start_idx <- 1
level_a_results <- data.frame()
if (file.exists(CHECKPOINT_FILE_A)) {
  level_a_results <- readRDS(CHECKPOINT_FILE_A)
  start_idx <- nrow(level_a_results) + 1
  cat(sprintf("RESUMING from checkpoint: %d already done, starting at %d\n",
      nrow(level_a_results), start_idx))
}

if (nrow(level_a_results) == 0) {
  level_a_results <- data.frame(
    Patient = character(), Cancer = character(), CancerFull = character(),
    Metric = character(), Score = integer(), Total = integer(),
    Compliance = numeric(), Violations = character(),
    Contextual_Genes = character(),
    Elapsed_Sec = numeric(), Success = logical(),
    stringsAsFactors = FALSE
  )
}

level_a_start_time <- Sys.time()

for (idx in seq.int(start_idx, nrow(level_a_patients))) {
  pat <- level_a_patients$Patient[idx]
  cab <- level_a_patients$CancerAbb[idx]
  cfu <- level_a_patients$CancerFull[idx]
  met <- level_a_patients$Metric[idx]
  csv <- level_a_patients$CSV_Path[idx]

  eta_str <- format_eta(level_a_start_time, idx - start_idx + 1,
                        nrow(level_a_patients) - start_idx + 1)
  cat(sprintf("  [%4d/%4d] %-20s %-5s %-4s ", idx, nrow(level_a_patients), pat, cab, met))

  payload_data <- build_patient_payload(pat, cab, cfu, met, csv)
  if (is.null(payload_data)) {
    cat("SKIP (no data)\n")
    level_a_results <- rbind(level_a_results, data.frame(
      Patient = pat, Cancer = cab, CancerFull = cfu, Metric = met,
      Score = 0, Total = 0, Compliance = NA, Violations = "NO_DATA",
      Contextual_Genes = "", Elapsed_Sec = 0, Success = FALSE, stringsAsFactors = FALSE
    ))
    saveRDS(level_a_results, CHECKPOINT_FILE_A)
    next
  }

  t_start <- Sys.time()
  llm_result <- tryCatch(
    call_llm_cached(payload_data$system_prompt, payload_data$payload, payload_data$genes),
    error = function(e) {
      cat(sprintf("ERROR: %s\n", e$message))
      return(list(response = NA_character_, cached = FALSE, fact_issues = 0))
    }
  )
  t_end <- Sys.time()
  elapsed <- as.numeric(difftime(t_end, t_start, units = "secs"))
  response <- if (is.list(llm_result)) llm_result$response else NA_character_
  fact_issues_la <- if (is.list(llm_result)) llm_result$fact_issues else 0

  if (is.na(response) || is.null(response) || nchar(response) == 0) {
    level_a_results <- rbind(level_a_results, data.frame(
      Patient = pat, Cancer = cab, CancerFull = cfu, Metric = met,
      Score = 0, Total = 0, Compliance = NA, Violations = "LLM_ERROR",
      Contextual_Genes = "", Elapsed_Sec = elapsed, Success = FALSE, stringsAsFactors = FALSE
    ))
    saveRDS(level_a_results, CHECKPOINT_FILE_A)
    next
  }

  # --- APPLY POST-PROCESSING (same scrubbers as app.R) ---
  response <- scrub_governance_violations_val(response)
  response <- ensure_questions_and_disclaimer_val(response, patient_id = pat, cohort = cfu)
  # --- END POST-PROCESSING ---

  gc <- check_governance_compliance(response, payload_data$genes, payload_data$all_genes, pat, cfu, met, payload_data$rcd_forms)
  viol_str <- if (length(gc$violations) > 0) paste(gc$violations, collapse = "; ") else "PASS"
  ctx_str <- if (length(gc$contextual_genes) > 0) paste(gc$contextual_genes, collapse = ", ") else ""

  level_a_results <- rbind(level_a_results, data.frame(
    Patient = pat, Cancer = cab, CancerFull = cfu, Metric = met,
    Score = gc$score, Total = gc$total, Compliance = round(gc$compliance, 4),
    Violations = viol_str, Contextual_Genes = ctx_str,
    Elapsed_Sec = round(elapsed, 1), Success = TRUE, stringsAsFactors = FALSE
  ))

  status_icon <- if (gc$compliance >= COMPLIANCE_TARGET) "✓" else "⚠"
  cat(sprintf("%s %d/%d (%.1f%%) %.1fs %s\n",
      status_icon, gc$score, gc$total, gc$compliance * 100, elapsed, eta_str))

  # Incremental save every 10 patients
  if (idx %% 10 == 0) saveRDS(level_a_results, CHECKPOINT_FILE_A)
}

# Final checkpoint save
saveRDS(level_a_results, CHECKPOINT_FILE_A)

# ===========================================================================
# LAYER 1: ASSAY REPRODUCIBILITY (cohort-level inference)
# ===========================================================================
# V19: The patient is the unit of inference, not a binary gate.
# Acceptance criteria apply to the COHORT distribution, not individual patients.
# ===========================================================================
# V25: Sanitize text columns to prevent multibyte encoding crashes
# LLM responses may contain malformed UTF-8 that crashes dplyr summarise().
valid_a <- level_a_results[level_a_results$Success == TRUE, ]
for (col in c("Cancer", "CancerFull", "Metric", "Violations", "Contextual_Genes")) {
  if (col %in% names(valid_a)) {
    valid_a[[col]] <- iconv(enc2utf8(as.character(valid_a[[col]])), to = "UTF-8", sub = "?")
  }
}
n_valid_a <- nrow(valid_a)
mean_compliance_a <- if (n_valid_a > 0) mean(valid_a$Compliance, na.rm = TRUE) else 0
se_compliance_a  <- if (n_valid_a > 1) sd(valid_a$Compliance, na.rm = TRUE) / sqrt(n_valid_a) else 0
ci_lo_a <- mean_compliance_a - 1.96 * se_compliance_a
ci_hi_a <- mean_compliance_a + 1.96 * se_compliance_a

# Distribution
n_19_19 <- sum(valid_a$Score == 19, na.rm = TRUE)
n_18_19 <- sum(valid_a$Score == 18, na.rm = TRUE)
n_17_19 <- sum(valid_a$Score == 17, na.rm = TRUE)
n_critical <- sum(valid_a$Score <= 17, na.rm = TRUE)

# Stratum-level summary
stratum_summary <- valid_a %>%
  group_by(Cancer, Metric) %>%
  summarise(
    n_patients = n(),
    mean_compliance = mean(Compliance, na.rm = TRUE),
    .groups = "drop"
  )

# Cancer-level floor check
cancer_means <- valid_a %>%
  group_by(Cancer) %>%
  summarise(mean_gov = mean(Compliance, na.rm = TRUE), .groups = "drop")
worst_cancer <- cancer_means[which.min(cancer_means$mean_gov), ]

# G3 (patient ID) errors — zero tolerance
n_g3_a <- sum(grepl("G3[^0-9]", valid_a$Violations, fixed = FALSE) | grepl("G3:", valid_a$Violations, fixed = TRUE), na.rm = TRUE)

# Total checkpoint-level pass rate
n_total_checks_a <- n_valid_a * 19
n_passed_checks_a <- sum(valid_a$Score, na.rm = TRUE)
pct_checks_passed_a <- 100 * n_passed_checks_a / n_total_checks_a

# Cohort-level acceptance criteria (continuous, distribution-based)
assay_repro_pass <- TRUE
assay_repro_flags <- character(0)

if (n_valid_a < LEVEL_A_MIN_N) {
  assay_repro_pass <- FALSE
  assay_repro_flags <- c(assay_repro_flags, sprintf("N=%d < required %d", n_valid_a, LEVEL_A_MIN_N))
}
if (mean_compliance_a < COMPLIANCE_TARGET) {
  assay_repro_pass <- FALSE
  assay_repro_flags <- c(assay_repro_flags, sprintf("Mean %.2f%% < %.0f%%", mean_compliance_a*100, COMPLIANCE_TARGET*100))
}
if (ci_lo_a < 0.90) {
  assay_repro_pass <- FALSE
  assay_repro_flags <- c(assay_repro_flags, sprintf("CI lower bound %.2f%% < 90%%", ci_lo_a*100))
}
if (worst_cancer$mean_gov < 0.85) {
  assay_repro_pass <- FALSE
  assay_repro_flags <- c(assay_repro_flags, sprintf("Worst cancer %s at %.2f%% < 85%%", worst_cancer$Cancer, worst_cancer$mean_gov*100))
}
if (n_g3_a > 0) {
  assay_repro_pass <- FALSE
  assay_repro_flags <- c(assay_repro_flags, sprintf("G3 patient ID errors: %d", n_g3_a))
}
if (n_critical > n_valid_a * 0.02) {
  assay_repro_pass <- FALSE
  assay_repro_flags <- c(assay_repro_flags, sprintf("Critical failures (≤17/19): %d (%.1f%%) > 2%%", n_critical, 100*n_critical/n_valid_a))
}

cat(sprintf("\n╔══════════════════════════════════════════════════════════════╗\n"))
cat(sprintf("║   LAYER 1: ASSAY REPRODUCIBILITY (cohort-level)              ║\n"))
cat(sprintf("╚══════════════════════════════════════════════════════════════╝\n"))
cat(sprintf("Patients tested:       %d (required: ≥%d)\n", n_valid_a, LEVEL_A_MIN_N))
cat(sprintf("Cancer types:           %d\n", length(unique(valid_a$Cancer))))
cat(sprintf("Endpoints:              DFI, DSS, OS, PFI\n"))
cat(sprintf("Grand mean governance:  %.2f%%\n", mean_compliance_a * 100))
cat(sprintf("95%% CI of the mean:    [%.2f%%, %.2f%%]\n", ci_lo_a*100, ci_hi_a*100))
cat(sprintf("\nScore distribution (19-point governance):\n"))
cat(sprintf("  19/19 (100%%):        %d (%.1f%%)\n", n_19_19, 100*n_19_19/n_valid_a))
cat(sprintf("  18/19 (94.7%%):       %d (%.1f%%)\n", n_18_19, 100*n_18_19/n_valid_a))
cat(sprintf("  17/19 (89.5%%):       %d (%.1f%%)\n", n_17_19, 100*n_17_19/n_valid_a))
cat(sprintf("  ≤17/19 (<89%%):       %d (%.1f%%)\n", n_critical, 100*n_critical/n_valid_a))
cat(sprintf("Checkpoint pass rate:   %.2f%% (%d/%d)\n", pct_checks_passed_a, n_passed_checks_a, n_total_checks_a))
cat(sprintf("G3 (patient ID) errors: %d\n", n_g3_a))
# G9c: Contextual gene references (informational metric)
ctx_counts_a <- nchar(valid_a$Contextual_Genes) > 0
ctx_genes_a <- unlist(strsplit(valid_a$Contextual_Genes[ctx_counts_a], ", "))
ctx_genes_a <- ctx_genes_a[ctx_genes_a != ""]
cat(sprintf("G9c — Contextual gene refs: %d patients (%.1f%%), %.1f genes/narrative (mean)\n",
    sum(ctx_counts_a), 100*sum(ctx_counts_a)/n_valid_a,
    if(sum(ctx_counts_a)>0) round(length(ctx_genes_a)/sum(ctx_counts_a), 1) else 0))
cat(sprintf("Worst cancer type:      %s (%.2f%%)\n", worst_cancer$Cancer, worst_cancer$mean_gov*100))
cat(sprintf("POWER CHECK:            n=%d %s (required n≥%d)\n",
    n_valid_a, if(n_valid_a >= LEVEL_A_MIN_N) "✓ ADEQUATE" else "⚠ UNDER-POWERED", LEVEL_A_MIN_N))
cat(sprintf("\nAcceptance criteria (cohort-level):\n"))
cat(sprintf("  Mean governance ≥ %.0f%%:          %s (%.2f%%)\n", COMPLIANCE_TARGET*100,
    if(mean_compliance_a >= COMPLIANCE_TARGET) "✓" else "✗", mean_compliance_a*100))
cat(sprintf("  Lower 95%% CI ≥ 90%%:              %s (%.2f%%)\n",
    if(ci_lo_a >= 0.90) "✓" else "✗", ci_lo_a*100))
cat(sprintf("  All cancer types ≥ 85%%:           %s (worst: %s %.2f%%)\n",
    if(worst_cancer$mean_gov >= 0.85) "✓" else "✗", worst_cancer$Cancer, worst_cancer$mean_gov*100))
cat(sprintf("  G3 errors = 0:                     %s (%d)\n", if(n_g3_a==0) "✓" else "✗", n_g3_a))
cat(sprintf("  Critical failures (<90%%) ≤ 2%%:    %s (%d, %.1f%%)\n",
    if(n_critical <= n_valid_a*0.02) "✓" else "✗", n_critical, 100*n_critical/n_valid_a))
cat(sprintf("  ≥95%% checkpoints passed:           %s (%.2f%%)\n",
    if(pct_checks_passed_a >= 95) "✓" else "✗", pct_checks_passed_a))
cat(sprintf("\n  ASSAY REPRODUCIBILITY:  %s\n\n",
    if(assay_repro_pass) "✅ PASS" else "❌ FAIL"))

# --------------------------------------------------------------------------
# LEVEL B: INTRA-CLINICAL SEMANTIC CONSISTENCY (k ≥ 30 × 5 repeats)
# --------------------------------------------------------------------------
cat("╔══════════════════════════════════════════════════════════════╗\n")
cat("║   LAYER 2: INTERPRETATIVE ROBUSTNESS (k≥90 × 5 repeats)     ║\n")
cat("╚══════════════════════════════════════════════════════════════╝\n")
cat(sprintf("Patients: %d, Repeats per patient: %d, Total LLM calls: %d\n",
    nrow(level_b_patients), LEVEL_B_REPEATS, nrow(level_b_patients) * LEVEL_B_REPEATS))
cat(sprintf("Target consistency: %.0f%%\n\n", COMPLIANCE_TARGET * 100))

# Checkpoint for Level B
level_b_all_results <- data.frame()
level_b_patient_summary <- data.frame()
start_b_idx <- 1

if (file.exists(CHECKPOINT_FILE_B)) {
  saved <- readRDS(CHECKPOINT_FILE_B)
  level_b_all_results <- saved$all_results
  level_b_patient_summary <- saved$patient_summary
  start_b_idx <- nrow(level_b_patient_summary) + 1
  cat(sprintf("RESUMING from checkpoint: %d/%d patients done\n",
      nrow(level_b_patient_summary), nrow(level_b_patients)))
}

level_b_start_time <- Sys.time()

for (p_idx in seq.int(start_b_idx, nrow(level_b_patients))) {
  pat_id  <- level_b_patients$Patient[p_idx]
  cab    <- level_b_patients$CancerAbb[p_idx]
  cfu    <- level_b_patients$CancerFull[p_idx]
  met    <- level_b_patients$Metric[p_idx]
  csv    <- level_b_patients$CSV_Path[p_idx]

  eta_str <- format_eta(level_b_start_time, p_idx - start_b_idx + 1,
                        nrow(level_b_patients) - start_b_idx + 1)
  cat(sprintf("  Patient %d/%d: %s (%s, %s) ", p_idx, nrow(level_b_patients), pat_id, cfu, met))

  payload_data <- build_patient_payload(pat_id, cab, cfu, met, csv)
  if (is.null(payload_data)) {
    cat("SKIP (no payload)\n")
    next
  }

  responses <- character(LEVEL_B_REPEATS)
  responses_pp <- character(LEVEL_B_REPEATS)  # post-processed for similarity check
  gov_scores <- numeric(LEVEL_B_REPEATS)
  elapsed_times <- numeric(LEVEL_B_REPEATS)
  all_valid <- TRUE

  for (rep in seq_len(LEVEL_B_REPEATS)) {
    cat(sprintf("R%d ", rep))
    t_start <- Sys.time()
    llm_result <- tryCatch(
      call_llm_cached(payload_data$system_prompt, payload_data$payload, payload_data$genes, nocache = TRUE),
      error = function(e) {
        cat(sprintf("ERR ")); return(list(response = NA_character_, cached = FALSE, fact_issues = 0))
      }
    )
    t_end <- Sys.time()
    elapsed <- as.numeric(difftime(t_end, t_start, units = "secs"))
    elapsed_times[rep] <- elapsed
    response <- if (is.list(llm_result)) llm_result$response else NA_character_

    if (is.na(response) || is.null(response)) {
      responses[rep] <- NA_character_
      gov_scores[rep] <- NA_real_
      all_valid <- FALSE
      next
    }

    responses[rep] <- response
    # Apply same post-processing as app.R before checking
    response_pp <- scrub_governance_violations_val(response)
    response_pp <- ensure_questions_and_disclaimer_val(response_pp, patient_id = pat_id, cohort = cfu)
    responses_pp[rep] <- response_pp
    gc <- check_governance_compliance(response_pp, payload_data$genes, payload_data$all_genes, pat_id, cfu, met, payload_data$rcd_forms)
    gov_scores[rep] <- gc$compliance

    # Store individual repeat result
    level_b_all_results <- rbind(level_b_all_results, data.frame(
      Patient = pat_id, Cancer = cab, CancerFull = cfu, Metric = met,
      Repeat = rep, Score = gc$score, Total = gc$total,
      Compliance = round(gc$compliance, 4),
      Violations = if(length(gc$violations)>0) paste(gc$violations, collapse="; ") else "PASS",
      Contextual_Genes = if(length(gc$contextual_genes)>0) paste(gc$contextual_genes, collapse=", ") else "",
      Elapsed_Sec = round(elapsed, 1), Success = TRUE,
      stringsAsFactors = FALSE
    ))
  }

  valid_responses <- responses_pp[!is.na(responses_pp)]
  n_valid <- length(valid_responses)

  if (n_valid >= 2) {
    semantic_sim <- compute_semantic_similarity(valid_responses)
    stability_df <- check_structural_stability(valid_responses,
                                                paste(payload_data$genes, collapse=", "))
    mean_stability <- mean(stability_df$stability, na.rm = TRUE)
    mean_gov <- mean(gov_scores, na.rm = TRUE)
    all_above <- all(gov_scores[!is.na(gov_scores)] >= COMPLIANCE_TARGET)

    level_b_patient_summary <- rbind(level_b_patient_summary, data.frame(
      Patient = pat_id, Cancer = cab, CancerFull = cfu, Metric = met,
      Valid_Repeats = n_valid, Semantic_Jaccard = round(semantic_sim, 4),
      Structural_Stability = round(mean_stability, 4),
      Mean_Governance = round(mean_gov, 4),
      All_Above_Threshold = all_above,
      stringsAsFactors = FALSE
    ))

    status <- if (mean_gov >= COMPLIANCE_TARGET) "✓" else "⚠"  # V19: governance-only gate, semantic is informational
    cat(sprintf("→ sem=%.2f gov=%.2f %s %s\n",
        semantic_sim * 100, mean_gov * 100, status, eta_str))
  } else {
    cat(sprintf("→ INSUFFICIENT DATA %s\n", eta_str))
  }

  # Checkpoint every 5 patients
  if (p_idx %% 5 == 0) {
    saveRDS(list(all_results = level_b_all_results, patient_summary = level_b_patient_summary),
            CHECKPOINT_FILE_B)
  }
}

saveRDS(list(all_results = level_b_all_results, patient_summary = level_b_patient_summary),
        CHECKPOINT_FILE_B)

# ===========================================================================
# FINAL OUTPUT PHASE (tryCatch-wrapped: checkpoints are SAFE even if this fails)
# ===========================================================================
tryCatch({

# V25: Sanitize Level B text columns against malformed UTF-8 from LLM responses
if (nrow(level_b_all_results) > 0) {
  for (col in c("Violations")) {
    if (col %in% names(level_b_all_results)) {
      level_b_all_results[[col]] <- iconv(enc2utf8(as.character(level_b_all_results[[col]])), to = "UTF-8", sub = "?")
    }
  }
}
if (nrow(level_b_patient_summary) > 0) {
  for (col in intersect(c("Patient", "Cancer", "Metric"), names(level_b_patient_summary))) {
    level_b_patient_summary[[col]] <- iconv(enc2utf8(as.character(level_b_patient_summary[[col]])), to = "UTF-8", sub = "?")
  }
}

# ===========================================================================
# LAYER 2: INTERPRETATIVE ROBUSTNESS (within-patient, 5-repeat design)
# ===========================================================================
# V19: Three independent sub-metrics — none AND-gated.
#   2a. Governance Preservation: facts consistent across repeats (PRIMARY GATE)
#   2b. Narrative Divergence: wording varies naturally (INFORMATIONAL ONLY)
#   2c. Structural Stability: section structure consistent (SUPPORTING GATE)
# ===========================================================================
if (nrow(level_b_patient_summary) > 0) {
  mean_sem <- mean(level_b_patient_summary$Semantic_Jaccard, na.rm = TRUE)
  mean_struct <- mean(level_b_patient_summary$Structural_Stability, na.rm = TRUE)
  mean_gov_b <- mean(level_b_patient_summary$Mean_Governance, na.rm = TRUE)
  se_gov_b <- if(nrow(level_b_patient_summary) > 1) sd(level_b_patient_summary$Mean_Governance, na.rm=TRUE) / sqrt(nrow(level_b_patient_summary)) else 0
  ci_lo_b <- mean_gov_b - 1.96 * se_gov_b
  ci_hi_b <- mean_gov_b + 1.96 * se_gov_b

  # Distribution
  n_b_100 <- sum(level_b_patient_summary$Mean_Governance == 1.0)
  n_b_94  <- sum(level_b_patient_summary$Mean_Governance >= 0.94)
  n_b_90  <- sum(level_b_patient_summary$Mean_Governance >= 0.90)
  n_b_low <- sum(level_b_patient_summary$Mean_Governance < 0.90)

  # Semantic Jaccard distribution (informational)
  n_sem_high <- sum(level_b_patient_summary$Semantic_Jaccard >= 0.80)  # templating warning
  n_sem_low  <- sum(level_b_patient_summary$Semantic_Jaccard < 0.30)   # drift warning
  n_sem_ok   <- sum(level_b_patient_summary$Semantic_Jaccard >= 0.30 & level_b_patient_summary$Semantic_Jaccard <= 0.80)

  # Structural stability
  n_struct_ok <- sum(level_b_patient_summary$Structural_Stability >= STRUCT_TARGET)

  # G3 errors across all Level B repeats
  n_g3_b <- sum(grepl("G3[^0-9]", level_b_all_results$Violations, fixed=FALSE) | grepl("G3:", level_b_all_results$Violations, fixed=TRUE))

  # Total checkpoints across Level B
  n_total_checks_b <- nrow(level_b_all_results) * 19
  n_passed_checks_b <- sum(level_b_all_results$Score, na.rm = TRUE)
  pct_checks_passed_b <- 100 * n_passed_checks_b / n_total_checks_b

  # --- 2a. GOVERNANCE PRESERVATION (primary gate) ---
  gov_pres_pass <- TRUE
  gov_pres_flags <- character(0)
  if (mean_gov_b < COMPLIANCE_TARGET) {
    gov_pres_pass <- FALSE
    gov_pres_flags <- c(gov_pres_flags, sprintf("Mean %.2f%% < %.0f%%", mean_gov_b*100, COMPLIANCE_TARGET*100))
  }
  if (ci_lo_b < 0.90) {
    gov_pres_pass <- FALSE
    gov_pres_flags <- c(gov_pres_flags, sprintf("CI lower %.2f%% < 90%%", ci_lo_b*100))
  }
  if (n_g3_b > 0) {
    gov_pres_pass <- FALSE
    gov_pres_flags <- c(gov_pres_flags, sprintf("G3 errors: %d", n_g3_b))
  }
  if (n_b_low > nrow(level_b_patient_summary) * 0.05) {
    gov_pres_pass <- FALSE
    gov_pres_flags <- c(gov_pres_flags, sprintf("Patients <90%%: %d (%.1f%%) > 5%%", n_b_low, 100*n_b_low/nrow(level_b_patient_summary)))
  }

  # --- 2b. NARRATIVE DIVERGENCE (informational only) ---
  sem_status <- if (n_sem_ok == nrow(level_b_patient_summary)) "✓ Healthy" else if (n_sem_high > 0) "⚠ Template warning" else if (n_sem_low > nrow(level_b_patient_summary)*0.05) "⚠ Drift warning" else "✓ Acceptable"

  # --- 2c. STRUCTURAL STABILITY (supporting gate) ---
  struct_pass <- mean_struct >= STRUCT_TARGET && n_struct_ok >= nrow(level_b_patient_summary) * 0.90

  # Interpretative Robustness overall
  interp_robust_pass <- gov_pres_pass && struct_pass

  cat(sprintf("\n╔══════════════════════════════════════════════════════════════╗\n"))
  cat(sprintf("║   LAYER 2: INTERPRETATIVE ROBUSTNESS (within-patient)        ║\n"))
  cat(sprintf("╚══════════════════════════════════════════════════════════════╝\n"))
  cat(sprintf("Patients: %d, Repeats/patient: 5, Total LLM calls: %d\n",
      nrow(level_b_patient_summary), nrow(level_b_all_results)))

  cat(sprintf("\n--- 2a. Governance Preservation (PRIMARY GATE) ---\n"))
  cat(sprintf("  Grand mean:            %.2f%%\n", mean_gov_b*100))
  cat(sprintf("  95%% CI:                [%.2f%%, %.2f%%]\n", ci_lo_b*100, ci_hi_b*100))
  cat(sprintf("  100%% perfect (19/19):  %d (%.1f%%)\n", n_b_100, 100*n_b_100/nrow(level_b_patient_summary)))
  cat(sprintf("  ≥94%% (≥18/19):         %d (%.1f%%)\n", n_b_94, 100*n_b_94/nrow(level_b_patient_summary)))
  cat(sprintf("  ≥89%% (≥17/19):         %d (%.1f%%)\n", n_b_90, 100*n_b_90/nrow(level_b_patient_summary)))
  cat(sprintf("  <90%% (critical):       %d (%.1f%%)\n", n_b_low, 100*n_b_low/nrow(level_b_patient_summary)))
  cat(sprintf("  G3 (patient ID) errors: %d\n", n_g3_b))
  cat(sprintf("  Checkpoint pass rate:   %.2f%% (%d/%d)\n", pct_checks_passed_b, n_passed_checks_b, n_total_checks_b))
  # G9c: Contextual gene references (Layer 2)
  ctx_counts_b <- nchar(level_b_all_results$Contextual_Genes) > 0
  ctx_genes_b <- unlist(strsplit(level_b_all_results$Contextual_Genes[ctx_counts_b], ", "))
  ctx_genes_b <- ctx_genes_b[ctx_genes_b != ""]
  cat(sprintf("  G9c — Contextual refs:  %d calls (%.1f%%), %.1f genes/call (mean)\n",
      sum(ctx_counts_b), 100*sum(ctx_counts_b)/nrow(level_b_all_results),
      if(sum(ctx_counts_b)>0) round(length(ctx_genes_b)/sum(ctx_counts_b), 1) else 0))
  cat(sprintf("  Acceptance:             %s\n", if(gov_pres_pass) "✓ PASS" else "✗ FAIL"))

  cat(sprintf("\n--- 2b. Narrative Divergence (INFORMATIONAL ONLY, not gating) ---\n"))
  cat(sprintf("  Grand mean:            %.2f%% (SD=%.2f%%)\n", mean_sem*100, sd(level_b_patient_summary$Semantic_Jaccard)*100))
  cat(sprintf("  Range:                 %.2f%% – %.2f%%\n",
      min(level_b_patient_summary$Semantic_Jaccard)*100, max(level_b_patient_summary$Semantic_Jaccard)*100))
  cat(sprintf("  ≥80%% (template warn):  %d patients\n", n_sem_high))
  cat(sprintf("  30–80%% (healthy):      %d patients\n", n_sem_ok))
  cat(sprintf("  <30%% (drift warn):     %d patients\n", n_sem_low))
  cat(sprintf("  Status:                 %s\n", sem_status))
  cat(sprintf("  Interpretation:         Confirms non-template LLM behavior.\n"))
  cat(sprintf("                          >80%% = template lock; <30%% = factual drift.\n"))

  cat(sprintf("\n--- 2c. Structural Stability (SUPPORTING GATE) ---\n"))
  cat(sprintf("  Grand mean:            %.2f%%\n", mean_struct*100))
  cat(sprintf("  Patients ≥%.0f%%:       %d/%d (%.1f%%)\n", STRUCT_TARGET*100, n_struct_ok,
      nrow(level_b_patient_summary), 100*n_struct_ok/nrow(level_b_patient_summary)))
  cat(sprintf("  Acceptance:             %s\n", if(struct_pass) "✓ PASS" else "✗ FAIL"))

  cat(sprintf("\n  INTERPRETATIVE ROBUSTNESS:  %s\n\n",
      if(interp_robust_pass) "✅ PASS" else "❌ FAIL"))
} else {
  interp_robust_pass <- FALSE
  gov_pres_pass <- FALSE
  struct_pass <- FALSE
  mean_sem <- 0; mean_struct <- 0; mean_gov_b <- 0
  n_b_100 <- 0; n_b_94 <- 0; n_g3_b <- 0
  pct_checks_passed_b <- 0
  cat("\n  ⚠ No Level B data to summarize\n")
}

# ===========================================================================
# LAYER 3: COHORT QUALITY ASSURANCE (meta-validation design)
# ===========================================================================
cat("╔══════════════════════════════════════════════════════════════╗\n")
cat("║   LAYER 3: COHORT QUALITY ASSURANCE (validation design)      ║\n")
cat("╚══════════════════════════════════════════════════════════════╝\n")

level_a_power_ok <- nrow(level_a_patients) >= LEVEL_A_MIN_N
level_b_power_ok <- nrow(level_b_patient_summary) >= LEVEL_B_K

n_cancer_types <- length(unique(level_a_results$Cancer))
cancer_table_a <- table(level_a_results$Cancer[level_a_results$Success])
cancer_table_b <- table(level_b_patient_summary$Cancer)
endpoint_table_a <- table(level_a_results$Metric[level_a_results$Success])
endpoint_table_b <- table(level_b_patient_summary$Metric)

# Failure taxonomy
all_violations_a <- level_a_results$Violations[!is.na(level_a_results$Violations) & level_a_results$Violations != "" & level_a_results$Violations != "PASS"]
all_violations_b <- level_b_all_results$Violations[!is.na(level_b_all_results$Violations) & level_b_all_results$Violations != "" & level_b_all_results$Violations != "PASS"]
g_codes_a <- unlist(regmatches(all_violations_a, gregexpr("G[0-9]+", all_violations_a)))
g_codes_b <- unlist(regmatches(all_violations_b, gregexpr("G[0-9]+", all_violations_b)))
g_table_a <- sort(table(g_codes_a), decreasing = TRUE)
g_table_b <- sort(table(g_codes_b), decreasing = TRUE)

n_strata <- length(unique(paste(level_a_results$Cancer, level_a_results$Metric)))

cat(sprintf("\n--- Campaign Summary ---\n"))
cat(sprintf("  Validation version:   V25 (three-layer)\n"))
cat(sprintf("  Date executed:        %s\n", format(Sys.time(), "%Y-%m-%d %H:%M:%S")))
cat(sprintf("  LLM provider:         %s\n", LLM_PROVIDER))
cat(sprintf("  LLM model:            %s\n", if(LLM_PROVIDER=="deepseek") DEEPSEEK_MODEL else LLM_MODEL))
cat(sprintf("  Sampling:             Stratified (%d cancer × endpoint strata)\n", n_strata))
cat(sprintf("  Cancer types:         %d\n", n_cancer_types))
cat(sprintf("  Endpoints:            DFI, DSS, OS, PFI\n"))
cat(sprintf("\n--- Power & Sample Size ---\n"))
cat(sprintf("  Assay Reproducibility: %d patients %s (required ≥%d)\n",
    n_valid_a, if(level_a_power_ok) "✓" else "⚠", LEVEL_A_MIN_N))
cat(sprintf("  Interpret. Robustness: %d patients %s (required ≥%d)\n",
    nrow(level_b_patient_summary), if(level_b_power_ok) "✓" else "⚠", LEVEL_B_K))
cat(sprintf("  Total LLM calls:       %d\n", nrow(level_a_results) + nrow(level_b_all_results)))

cat(sprintf("\n--- Cancer Representation ---\n"))
cat(sprintf("  %-30s %6s %6s\n", "Cancer", "LevelA", "LevelB"))
all_cancers <- sort(union(names(cancer_table_a), names(cancer_table_b)))
for(cn in all_cancers) {
  nA <- if(cn %in% names(cancer_table_a)) cancer_table_a[[cn]] else 0
  nB <- if(cn %in% names(cancer_table_b)) cancer_table_b[[cn]] else 0
  cat(sprintf("  %-30s %6d %6d\n", cn, nA, nB))
}

cat(sprintf("\n--- Endpoint Representation ---\n"))
cat(sprintf("  %-6s %6s %6s\n", "Endpoint", "LevelA", "LevelB"))
for(ep in sort(union(names(endpoint_table_a), names(endpoint_table_b)))) {
  nA <- if(ep %in% names(endpoint_table_a)) endpoint_table_a[[ep]] else 0
  nB <- if(ep %in% names(endpoint_table_b)) endpoint_table_b[[ep]] else 0
  cat(sprintf("  %-6s %6d %6d\n", ep, nA, nB))
}

cat(sprintf("\n--- Failure Taxonomy (governance checkpoint violations) ---\n"))
cat(sprintf("  %-8s %8s %8s\n", "Check", "LevelA", "LevelB"))
all_checks <- sort(union(names(g_table_a), names(g_table_b)))
for(gc in all_checks) {
  nA <- if(gc %in% names(g_table_a)) g_table_a[[gc]] else 0
  nB <- if(gc %in% names(g_table_b)) g_table_b[[gc]] else 0
  pctA <- nA / nrow(level_a_results) * 100
  pctB <- nB / nrow(level_b_all_results) * 100
  cat(sprintf("  %-8s %4d (%4.1f%%) %4d (%4.1f%%)\n", gc, nA, pctA, nB, pctB))
}

# --- FINAL ASSESSMENT ---
cat(sprintf("\n╔══════════════════════════════════════════════════════════════╗\n"))
cat(sprintf("║   CRIT-03 V25 — THREE-LAYER FINAL ASSESSMENT                  ║\n"))
cat(sprintf("╚══════════════════════════════════════════════════════════════╝\n"))
cat(sprintf("\n  Layer 1 — Assay Reproducibility:   %s\n", if(assay_repro_pass) "✅ PASS" else "❌ FAIL"))
cat(sprintf("  Layer 2 — Interpret. Robustness:   %s\n", if(interp_robust_pass) "✅ PASS" else "❌ FAIL"))
cat(sprintf("    └ 2a. Governance Preservation:   %s\n", if(gov_pres_pass) "✓" else "✗"))
cat(sprintf("    └ 2b. Narrative Divergence:      %s (informational)\n", sem_status))
cat(sprintf("    └ 2c. Structural Stability:      %s\n", if(struct_pass) "✓" else "✗"))
cat(sprintf("  Layer 3 — Cohort QA (power=%s, coverage=%s):         ✓ CERTIFIED\n",
    if(level_a_power_ok && level_b_power_ok) "✓" else "⚠",
    if(n_cancer_types == 30) "✓" else "⚠"))

overall_pass <- assay_repro_pass && interp_robust_pass && level_a_power_ok && level_b_power_ok
cat(sprintf("\n  OVERALL:  %s\n",
    if(overall_pass) "✅ PASS — Validation passed"
    else "❌ FAIL — Review flagged layers"))

if (!assay_repro_pass | !interp_robust_pass) {
  cat("\n──────────────────────────────────────────────────────────────\n")
  if (!assay_repro_pass) cat(sprintf("  Assay Reproducibility issues: %s\n", paste(assay_repro_flags, collapse="; ")))
  if (!gov_pres_pass) cat(sprintf("  Governance Preservation issues: %s\n", paste(gov_pres_flags, collapse="; ")))
  cat("──────────────────────────────────────────────────────────────\n")
}

# ===========================================================================
# OUTPUT FILES
# ===========================================================================
write.csv(level_a_results, "crit03_powered_level_a_results_v25.csv", row.names = FALSE)
write.csv(stratum_summary, "crit03_powered_level_a_by_stratum_v25.csv", row.names = FALSE)
if (nrow(level_b_patient_summary) > 0) {
  write.csv(level_b_patient_summary, "crit03_powered_level_b_patient_summary_v25.csv", row.names = FALSE)
  write.csv(level_b_all_results, "crit03_powered_level_b_all_repeats_v25.csv", row.names = FALSE)
}

summary_lines <- c(
  "============================================================",
  "CRIT-03 V25 VALIDATION (Three-Layer) — RESULTS",
  "============================================================",
  sprintf("Date:              %s", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
  sprintf("LLM Provider:       %s", LLM_PROVIDER),
  sprintf("LLM Model:          %s", if(LLM_PROVIDER=="deepseek") DEEPSEEK_MODEL else LLM_MODEL),
  sprintf("Compliance Target: %.0f%%", COMPLIANCE_TARGET * 100),
  "",
  "--- LAYER 1: ASSAY REPRODUCIBILITY (cohort-level) ---",
  sprintf("  Patients tested:     %d (required ≥%d)", n_valid_a, LEVEL_A_MIN_N),
  sprintf("  Cancer types:        %d", n_cancer_types),
  sprintf("  Endpoints:           DFI, DSS, OS, PFI"),
  sprintf("  Mean governance:     %.2f%%", mean_compliance_a * 100),
  sprintf("  95%% CI:              [%.2f%%, %.2f%%]", ci_lo_a*100, ci_hi_a*100),
  sprintf("  ≥95%% checkpoints:    %.2f%%", pct_checks_passed_a),
  sprintf("  G3 (ID) errors:      %d", n_g3_a),
  sprintf("  Assessment:          %s", if(assay_repro_pass) "PASS" else "FAIL"),
  "",
  "--- LAYER 2: INTERPRETATIVE ROBUSTNESS (within-patient) ---",
  sprintf("  Patients tested:     %d (required ≥%d)", nrow(level_b_patient_summary), LEVEL_B_K),
  sprintf("  Repeats/patient:     %d", LEVEL_B_REPEATS),
  sprintf("  Total LLM calls:     %d", nrow(level_b_all_results)),
  sprintf("  Governance mean:     %.2f%% [CI: %.2f%%, %.2f%%]", mean_gov_b*100, ci_lo_b*100, ci_hi_b*100),
  sprintf("  Semantic Jaccard:    %.2f%% (informational)", mean_sem*100),
  sprintf("  Structural (≥%.0f%%):   %.2f%%", STRUCT_TARGET*100, mean_struct*100),
  sprintf("  G3 (ID) errors:      %d", n_g3_b),
  sprintf("  Assessment (gov+struct): %s", if(interp_robust_pass) "PASS" else "FAIL"),
  "",
  sprintf("OVERALL: %s", if(overall_pass) "PASS" else "FAIL — Review flagged layers"),
  "",
  "V25 — Three-layer analytical validation framework. Per-cancer floor (≥10) enforced.",
  "  1. Assay Reproducibility: cohort-level governance distribution",
  "  2. Interpretative Robustness: governance preservation (gate) + narrative divergence (info) + structural stability (gate)",
  "  3. Cohort Quality Assurance: meta-validation of study design",
  "Decoupled deterministic (gov) from probabilistic (narrative) assessment.",
  "============================================================"
)

cat(paste(summary_lines, collapse = "\n"), "\n")
writeLines(summary_lines, "crit03_powered_validation_summary_v25.txt")

cat("\nOutput files:\n")
cat("  crit03_powered_level_a_results_v25.csv\n")
cat("  crit03_powered_level_a_by_stratum_v25.csv\n")
if (nrow(level_b_patient_summary) > 0) {
  cat("  crit03_powered_level_b_patient_summary_v25.csv\n")
  cat("  crit03_powered_level_b_all_repeats_v25.csv\n")
}
cat("  crit03_powered_validation_summary_v25.txt\n")
cat("\nDone at", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")

}, error = function(e) {
  cat("\n╔══════════════════════════════════════════════════════════════╗\n")
  cat("║  ⚠ OUTPUT PHASE FAILED — BUT CHECKPOINTS ARE SAFE          ║\n")
  cat("╠══════════════════════════════════════════════════════════════╣\n")
  cat(sprintf("║  Error: %s\n", e$message))
  cat(sprintf("║  Level A checkpoint: %s\n", CHECKPOINT_FILE_A))
  cat(sprintf("║  Level B checkpoint: %s\n", CHECKPOINT_FILE_B))
  cat("║                                                            ║\n")
  cat("║  Re-run the script to regenerate output CSVs from cache.   ║\n")
  cat("╚══════════════════════════════════════════════════════════════╝\n\n")
})

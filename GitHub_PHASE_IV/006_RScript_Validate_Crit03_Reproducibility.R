# ==============================================================================
# CRIT-03 VALIDATION STUDY: Reproducibility of Clinical Report Generation
# ==============================================================================
# Two-Level Validation:
#   Level A — Inter-clinical consistency: 1 patient per all 32 cancer types,
#            checking governance/guardrail compliance >95% across all reports.
#   Level B — Intra-clinical consistency: 5 reports for the SAME patient,
#            checking semantic stability and governance consistency >95%.
#
# If compliance <95%, code amendment (LLM output caching) is triggered.
# ==============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(httr2)
  library(stringr)
  library(digest)
})

WORKING_DIR <- "."
setwd(WORKING_DIR)

cat("╔══════════════════════════════════════════════════════════════╗\n")
cat("║        CRIT-03 REPRODUCIBILITY VALIDATION STUDY            ║\n")
cat("╚══════════════════════════════════════════════════════════════╝\n")
cat("Started:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n\n")

# ===========================================================================
# CONFIGURATION
# ===========================================================================
OLLAMA_URL      <- Sys.getenv("OLLAMA_URL", unset = "http://localhost:11434")
LLM_MODEL       <- Sys.getenv("OLLAMA_MODEL", unset = "qwen3:8b")
COMPLIANCE_TARGET <- 0.95   # 95% threshold
LEVEL_B_REPEATS   <- 5      # Number of re-runs per patient for intra-clinical

# ===========================================================================
# 1. LOAD REFERENCE DATA
# ===========================================================================
cat("[1/8] Loading reference data...\n")

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

# ===========================================================================
# 2. FIND TRAJECTORY CSVs PER COHORT
# ===========================================================================
cat("[2/8] Scanning trajectory files per cancer cohort...\n")

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

# Find models dir
models_dir <- "../PHASE_III_ML_Models"
if (!dir.exists(models_dir)) models_dir <- "PHASE_III_ML_Models"
if (!dir.exists(models_dir)) stop("Cannot find PHASE_III_ML_Models directory")

cohort_dirs <- list.dirs(models_dir, recursive = FALSE)
cohort_dirs <- cohort_dirs[!grepl("Clinical_Reports_Cache", cohort_dirs)]
cat(sprintf("  Found %d cohort directories\n", length(cohort_dirs)))

# Collect patient-specific trajectory CSVs
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
# Prefer PFI metric
patient_files_pfi <- patient_files[patient_files$Metric == "PFI", ]
cat(sprintf("  Found %d patients with trajectory + stemness (PFI: %d)\n",
    nrow(patient_files), nrow(patient_files_pfi)))

# ===========================================================================
# 3. SELECT LEVEL A PATIENTS: 1 per cancer type (PFI metric preferred)
# ===========================================================================
cat("[3/8] Selecting Level-A patients (1 per cancer type)...\n")

set.seed(20260617)
level_a_patients <- data.frame()
for (abb in names(cancer_dict)) {
  candidates <- patient_files_pfi[patient_files_pfi$CancerAbb == abb, ]
  if (nrow(candidates) == 0) {
    candidates <- patient_files[patient_files$CancerAbb == abb, ]
  }
  if (nrow(candidates) == 0) {
    cat(sprintf("  ⚠ NO PATIENT for %s (%s)\n", abb, cancer_dict[[abb]]))
    next
  }
  chosen <- candidates[sample(nrow(candidates), 1), ]
  level_a_patients <- rbind(level_a_patients, chosen)
}
cat(sprintf("  Selected %d patients across %d cancer types\n",
    nrow(level_a_patients), length(unique(level_a_patients$CancerAbb))))

# ===========================================================================
# 4. SELECT LEVEL B PATIENT: pick one from a well-represented cancer type
# ===========================================================================
cat("[4/8] Selecting Level-B patient (single patient, 5 repeats)...\n")

# Pick BRCA (well-represented) PFI
brca_candidates <- patient_files_pfi[patient_files_pfi$CancerAbb == "BRCA", ]
if (nrow(brca_candidates) > 0) {
  level_b_patient <- brca_candidates[sample(nrow(brca_candidates), 1), ]
} else {
  # Fallback: any cancer with PFI
  level_b_patient <- patient_files_pfi[sample(nrow(patient_files_pfi), 1), ]
}
cat(sprintf("  Level-B patient: %s (%s, %s)\n",
    level_b_patient$Patient, level_b_patient$CancerFull, level_b_patient$Metric))

# ===========================================================================
# 5. BUILD PATIENT PAYLOAD FUNCTION (replicates appV2.R logic)
# ===========================================================================
# --- OncoKB Concordance Lock (injected into system prompt) ---
# EXACTLY matches app.R / appV2.R Pharmacogenomic Evidence Governance v1.1
oncokb_concordance_lock <- paste0(
  "\n\n--- ONCOKB CONCORDANCE LOCK v1.0 ---\n\n",
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
  "END ONCOKB CONCORDANCE LOCK\n"
)

cat("[5/8] Building patient payload + governance framework...\n")

build_patient_payload <- function(pat_id, cancer_abb, cancer_full, metric, csv_path) {
  traj <- tryCatch(read.csv(csv_path, stringsAsFactors = FALSE), error = function(e) NULL)
  if (is.null(traj) || nrow(traj) == 0 || !"Feature" %in% names(traj)) return(NULL)
  
  traj$AbsSHAP <- abs(traj$SHAP_Value)
  traj <- traj[order(traj$AbsSHAP, decreasing = TRUE), ]
  top5 <- head(traj, 5)
  
  sig_lines <- c()
  patient_genes <- c()
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
    
    sig_lines <- c(sig_lines, sprintf(
      "Signature: %s > SHAP Impact: %s (Value: %.4f) > Omic Layer: %s > Associated RCD Form: %s > Encoded Gene Mechanics: %s",
      feat, shap_direction, patient_shap, omic_layer, rcd_form, gene_mechanics
    ))
  }
  
  # Patient phenotype
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
    "CRITICAL: You MUST incorporate these exact patient-level TSM, TMB, and MSI classifications into your clinical synthesis. Do NOT generalize from population-level correlations embedded in signature nomenclature."
  )
  
  # --- Endpoint-specific label and directional context ---
  endpoint_labels <- c(
    "DFI" = "Disease-Free Interval",
    "DSS" = "Disease-Specific Survival",
    "OS"  = "Overall Survival",
    "PFI" = "Progression-Free Interval"
  )
  endpoint_label <- if (metric %in% names(endpoint_labels)) endpoint_labels[[metric]] else metric

  if (metric %in% c("OS", "DSS")) {
    directional_context <- paste0(
      "DIRECTIONAL INTERPRETATION: This is a SURVIVAL model (S(t)). ",
      "HIGH survival probabilities (>80%) mean FAVORABLE. LOW (<50%) mean UNFAVORABLE. ",
      "Classify as PROTECTIVE or LETHAL."
    )
  } else {
    directional_context <- paste0(
      "DIRECTIONAL INTERPRETATION: This is a CUMULATIVE INCIDENCE model (1-S(t)). ",
      "LOW probabilities (<5%) mean FAVORABLE. HIGH (>20%) mean UNFAVORABLE. ",
      "Classify as LOW-RISK, STABLE, or ADVERSE."
    )
  }

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
    "Analyze this patient's profile and provide the therapeutic vulnerability synthesis."
  )
  
  sys_prompt <- build_system_prompt(metric)

  return(list(payload = payload, genes = unique(patient_genes),
              cancer_full = cancer_full, metric = metric,
              system_prompt = sys_prompt))
}

# Governance rules (replicated from appV2.R)
phenotype_correlation_rule <- paste0(
  "PHENOTYPE ECOLOGICAL GUARDRAIL: The signature nomenclature contains embedded signs ",
  "(Positive/P or Negative/N) reflecting POPULATION-LEVEL correlations with clinical phenotypes ",
  "(TSM, TMB, or MSI). You MUST NOT use these population-level signs to infer or assert the ",
  "phenotype status for this specific individual patient. Instead, you MUST rely EXCLUSIVELY on ",
  "the patient's actual measured classifications (High, Intermediate, or Low) for TSM, TMB, and MSI ",
  "provided explicitly in the payload."
)

strict_nomenclature_rule <- paste0(
  "STRICT NOMENCLATURE RULE: You MUST NOT use, mention, or display ANY part of the signature ",
  "nomenclatures (e.g., THYM-1460.6.3.N.2.35.5.2.3.3) or their abbreviated forms (e.g., THYM-1460, ",
  "LUAD-1883, LUAD-636) anywhere in your clinical narrative."
)

hedging_rule <- paste0(
  "CRITICAL HEDGING & HYPOTHESIS INSTRUCTION: You MUST use hedging language. ",
  "Replace strong claims (e.g., 'demonstrate', 'prove', 'establish') with cautious terms ",
  "(e.g., 'suggest', 'are consistent with', 'are compatible with', 'may indicate that')."
)

build_system_prompt <- function(metric) {
  if (metric %in% c("OS", "DSS")) {
    metric_context <- paste0(
      "METRIC CONTEXT: SURVIVAL endpoint. S(t) — survival probability. ",
      "HIGH (>80%) = favorable. LOW (<50%) = unfavorable. Use PROTECTIVE/LETHAL classification. ",
      "Standard survival terminology permitted (lethal, protective, mortality)."
    )
  } else if (metric == "PFI") {
    metric_context <- paste0(
      "METRIC CONTEXT: PROGRESSION endpoint (PFI). Cumulative incidence — progression risk. ",
      "LOW (<5%) = favorable. HIGH (>20%) = unfavorable. Use LOW-RISK/STABLE/ADVERSE. ",
      "Prefer pro-progression / anti-progression / stabilizing. Avoid survival terms."
    )
  } else {
    metric_context <- paste0(
      "METRIC CONTEXT: RECURRENCE endpoint (DFI). Cumulative incidence — new-tumor-event risk. ",
      "LOW (<5%) = favorable. HIGH (>20%) = unfavorable. Use LOW-RISK/STABLE/ADVERSE. ",
      "Prefer pro-recurrence / anti-recurrence / stabilizing. Avoid survival terms."
    )
  }

  paste0(
    "You are an expert clinical molecular oncologist analyzing a patient's multi-omic profile. ",
    "Output a highly professional, fluid clinical synthesis in continuous paragraph form. ",
    "\n\n=== ABSOLUTE GENE WHITELIST (READ FIRST — NON-NEGOTIABLE) ===\n",
    "The patient payload below contains a GENE WHITELIST — an explicit list of the ONLY genes you are permitted to discuss. ",
    "You MUST locate the GENE WHITELIST section in the user payload BEFORE writing any output. ",
    "You MUST NOT mention ANY gene not listed in the whitelist — even canonical cancer genes (EGFR, TP53, TNF, BRAF, ",
    "BRCA1, GPX4, CDK2, SMAD2, FGFR3, ATM, APC, TRAF2, E2F1, RHOA, HSPD1, or any other gene). ",
    "If a biological pathway discussion would naturally include a non-whitelisted gene, you MUST OMIT it completely. ",
    "Do NOT borrow genes from your training knowledge. Each gene you mention will be audited against the whitelist. ",
    "A single non-whitelisted gene mention causes a GOVERNANCE FAILURE. This rule CANNOT be overridden.\n\n",
    "=== PATIENT IDENTIFIER RULE (READ SECOND) ===\n",
    "You MUST start your narrative by explicitly stating the Patient ID and the Cohort. ",
    "The first sentence MUST contain both identifiers. Omitting either is a GOVERNANCE FAILURE.\n\n",
    metric_context, "\n\n",
    hedging_rule, "\n\n",
    strict_nomenclature_rule, "\n\n",
    "GLOBAL ASSOCIATIVITY RULE: The multi-omic signatures, SHAP values, and stemness correlations ",
    "provided represent associative mathematical relationships, not proven causative biological pathways.\n\n",
    phenotype_correlation_rule, "\n\n",
    tsm_ontology_protection, "\n\n",
    oncokb_concordance_lock, "\n\n",
    "MANDATORY REQUIREMENT: Append EXACTLY 3 Suggested Clinical Queries at the end.\n",
    "Append the mandatory hypothesis-generating disclaimer as the final paragraph."
  )
}

# --- TSM Ontology Protection ---
tsm_ontology_protection <- "\n\n--- TSM ONTOLOGY PROTECTION GOVERNANCE v1.0 ---\n\nCRITICAL: The following are TUMOR STEMNESS MEASURES (TSM) — phenotype indices, NOT gene measurements:\n- RNAss: transcriptome-wide stemness index. NOT any single RNA species.\n- EREG.EXPss: epigenetically-regulated stemness index. NOT Epiregulin (EREG) gene expression. The 'EREG' substring means 'Epigenetically Regulated', not the gene EREG.\n- EREG.METHss: methylation-based stemness index. NOT EREG methylation.\n\nPROHIBITED: Do NOT decompose TSM variable names into gene symbols. Do NOT import EREG biology from EREG.EXPss or EREG.METHss. Interpret TSM variables EXCLUSIVELY as stemness phenotype metrics.\n\nEND OF TSM ONTOLOGY PROTECTION GOVERNANCE\n"

# ===========================================================================
# 6. GOVERNANCE COMPLIANCE CHECKER
# ===========================================================================
check_governance_compliance <- function(response_text, patient_genes, pat_id, cancer_full, metric = NULL) {
  score <- 0; total_checks <- 0; violations <- c()
  
  # G1: No raw nomenclature pattern (e.g., BRCA-123.6.3.P...)
  total_checks <- total_checks + 1
  if (!grepl("[A-Z]{2,5}-\\d+\\.\\d+", response_text, perl = TRUE)) {
    score <- score + 1
  } else {
    violations <- c(violations, "G1: Raw nomenclature exposed")
  }
  
  # G2: No abbreviated nomenclature (e.g., BRCA-123), excluding TCGA barcodes
  total_checks <- total_checks + 1
  has_abbrev <- grepl("(?<!TCGA-)\\b[A-Z]{2,5}-\\d{3,4}\\b", response_text, perl = TRUE)
  if (!has_abbrev) {
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
  
  # G5: Has exactly 3 suggested clinical queries
  total_checks <- total_checks + 1
  query_matches <- gregexpr("(?:^|\\n)\\s*\\d+\\s*\\.\\s*-", response_text, perl = TRUE)[[1]]
  n_queries <- length(query_matches)
  if (n_queries == 3) {
    score <- score + 1
  } else {
    violations <- c(violations, sprintf("G5: Found %d suggested queries (need exactly 3)", n_queries))
  }
  
  # G6: Mandatory disclaimer present
  total_checks <- total_checks + 1
  if (grepl("hypothesis-generating", response_text, fixed = TRUE) ||
      grepl("These findings do not establish direct causality", response_text, fixed = TRUE)) {
    score <- score + 1
  } else {
    violations <- c(violations, "G6: Disclaimer missing or incomplete")
  }
  
  # G7: Hedging language used (no strong causal verbs)
  total_checks <- total_checks + 1
  strong_verbs <- c("\\bproves\\b", "\\bconfirms\\b", "\\bdemonstrates\\b",
                    "\\bdrives the\\b", "\\bdrives tumor\\b")
  has_strong <- any(sapply(strong_verbs, function(p) grepl(p, response_text, perl = TRUE, ignore.case = TRUE)))
  if (!has_strong) {
    score <- score + 1
  } else {
    violations <- c(violations, "G7: Strong causal verbs found")
  }
  
  # G8: No bare "mRNA" without "Bulk" or "Gene-Level" qualifier
  total_checks <- total_checks + 1
  bare_mrna <- grepl("(?<!Bulk )(?<!Gene-Level )\\bmRNA expression\\b", response_text, perl = TRUE)
  # Also check for "mRNA signature" without qualifier
  bare_mrna2 <- grepl("\\bmRNA signature\\b", response_text, perl = TRUE)
  bare_mrna3 <- grepl("\\bmRNA layer\\b", response_text, perl = TRUE)
  if (!bare_mrna && !bare_mrna2 && !bare_mrna3) {
    score <- score + 1
  } else {
    violations <- c(violations, "G8: Bare 'mRNA' qualifier without 'Bulk'/'Gene-Level'")
  }
  
  # G9: No hallucinated genes (genes not in patient profile)
  total_checks <- total_checks + 1
  gene_regex <- "\\b([A-Z][A-Z0-9]{2,}[0-9]*)\\b"
  llm_genes <- unique(unlist(regmatches(response_text, gregexpr(gene_regex, response_text, perl = TRUE))))
  # Filter: only genes from NCBI gene_info
  known_all <- if (!is.null(gene_info)) gene_info$Gene.symbol else character(0)
  llm_known_genes <- intersect(llm_genes, known_all)
  hallucinated <- setdiff(llm_known_genes, patient_genes)
  if (length(hallucinated) == 0) {
    score <- score + 1
  } else {
    violations <- c(violations, sprintf("G9: Hallucinated genes: %s", paste(hallucinated, collapse=", ")))
  }
  
  # G10: Directional consistency — detect survival-framing on event endpoints and vice versa
  if (!is.null(metric)) {
    total_checks <- total_checks + 1
    is_event_metric <- metric %in% c("DFI", "PFI")
    is_survival_metric <- metric %in% c("OS", "DSS")
    
    survival_terms <- c("\\blethal trajectory\\b", "\\bprotective trajectory\\b",
                        "\\bmortality risk\\b", "\\bsurvival probability\\b")
    has_survival_framing <- any(sapply(survival_terms, function(p) grepl(p, response_text, perl = TRUE, ignore.case = TRUE)))
    
    event_terms <- c("\\badverse progression\\b", "\\badverse recurrence\\b",
                     "\\bpro-progression\\b", "\\banti-progression\\b",
                     "\\bpro-recurrence\\b", "\\banti-recurrence\\b")
    has_event_framing <- any(sapply(event_terms, function(p) grepl(p, response_text, perl = TRUE, ignore.case = TRUE)))
    
    if (is_event_metric && has_survival_framing) {
      violations <- c(violations, sprintf("G10: Survival-framing on %s endpoint (directional mismatch)", metric))
    } else if (is_survival_metric && has_event_framing) {
      violations <- c(violations, sprintf("G10: Event-framing on %s endpoint (directional mismatch)", metric))
    } else {
      score <- score + 1
    }
  }

  # G11: TSM lexical borrowing
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
  
  
  # G12: OncoKB Level Fabrication — LLM must not claim OncoKB therapeutic levels
  #       unless the Clinical_Status column explicitly states them. Detects:
  #       "OncoKB Level 1/2/3A/3B/4", "OncoKB therapeutic level", etc.
  #       EXCLUDES: license-limitation acknowledgments ("OncoKB Level 1-4 are not available")
  total_checks <- total_checks + 1
  
  # First remove sentences that are CLEARLY license-acknowledgment disclaimers
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
    violations <- c(violations, "G12: OncoKB therapeutic level fabricated — claiming Level 1/2/3A/3B/4 without Clinical_Status support")
  } else {
    score <- score + 1
  }
  
  # G13: Fda2/Fda3 ↔ Therapeutic Level Conflation — LLM must not equate
  #       Tier 0 regulatory evidence with OncoKB therapeutic Levels 1-4
  total_checks <- total_checks + 1
  fda_therapeutic_conflation <- c(
    "\\b(Fda2|FDA Level 2)\\b.*\\b(therapeutic Level|Level [1-4])\\b",
    "\\b(Fda3|FDA Level 3)\\b.*\\b(therapeutic Level|Level [1-4])\\b"
  )
  has_fda_conflation <- any(sapply(fda_therapeutic_conflation, function(p) grepl(p, response_text, perl = TRUE, ignore.case = TRUE)))
  if (has_fda_conflation) {
    violations <- c(violations, "G13: Fda2/Fda3 conflation with therapeutic Levels 1-4 — Tier 0 regulatory ≠ Stratum 7 therapeutic")
  } else {
    score <- score + 1
  }
  
  # G14: Tier 0 Evidence Misrepresentation — T0A/T0B/T0C presented as
  #       treatment recommendation rather than regulatory recognition
  total_checks <- total_checks + 1
  t0_misrep_patterns <- c(
    "\\bT0A\\b.*\\b(should receive|is indicated|treat with|recommend)\\b",
    "\\bTier 0\\b.*\\b(should receive|is indicated|treat with|recommend)\\b"
  )
  has_t0_misrep <- any(sapply(t0_misrep_patterns, function(p) grepl(p, response_text, perl = TRUE, ignore.case = TRUE)))
  if (has_t0_misrep) {
    violations <- c(violations, "G14: Tier 0 evidence misrepresented as treatment recommendation — regulatory recognition ≠ clinical directive")
  } else {
    score <- score + 1
  }
  
  compliance <- score / total_checks
  return(list(score = score, total = total_checks, compliance = compliance,
              violations = violations))
}

# ===========================================================================
# 7. SEMANTIC SIMILARITY CHECKER (for Level B intra-clinical)
# ===========================================================================
compute_semantic_similarity <- function(texts) {
  n <- length(texts)
  if (n < 2) return(1.0)
  
  # Simple token-based Jaccard similarity
  tokenize <- function(txt) {
    tolower(unlist(strsplit(gsub("[^a-zA-Z0-9 ]", " ", txt), "\\s+")))
  }
  
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

# Key structural elements to check across repeats
check_structural_stability <- function(texts, patient_genes_str) {
  n <- length(texts)
  results <- data.frame(
    metric = character(), stability = numeric(), stringsAsFactors = FALSE
  )
  
  # 1. Gene mention stability
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
  
  # 2. Tumor-state classification stability
  state_terms <- c("stable differentiated", "adaptive suppression", "stress-adapted",
                   "vulnerability.resistance", "multi.omic plasticity",
                   "stemness-suppressed", "differentiation-favoring")
  state_pattern <- paste(state_terms, collapse = "|")
  state_presence <- sapply(texts, function(t) grepl(state_pattern, tolower(t)))
  results <- rbind(results, data.frame(metric = "Tumor_State_Classification",
                                       stability = if(all(state_presence) || all(!state_presence)) 1.0 else 0.5))
  
  # 3. Query count stability
  query_counts <- sapply(texts, function(t) {
    length(gregexpr("(?:^|\\n)\\s*\\d+\\s*\\.\\s*-", t, perl = TRUE)[[1]])
  })
  results <- rbind(results, data.frame(metric = "Query_Count",
                                       stability = if(all(query_counts == 3)) 1.0 else
                                         if(all(query_counts >= 2 & query_counts <= 4)) 0.7 else 0.3))
  
  # 4. Disclaimer presence stability
  disc_present <- sapply(texts, function(t) grepl("hypothesis-generating", t, fixed = TRUE))
  results <- rbind(results, data.frame(metric = "Disclaimer_Presence",
                                       stability = if(all(disc_present)) 1.0 else
                                         mean(disc_present)))
  
  # 5. Overall token-level Jaccard similarity
  results <- rbind(results, data.frame(metric = "Token_Jaccard",
                                       stability = compute_semantic_similarity(texts)))
  
  return(results)
}

# ===========================================================================
# 8. CALL LLM FUNCTION (Robust: retry with transient-vs-permanent classification)
# ===========================================================================
# HTTP Error Classification Helper
is_transient_http_error <- function(e) {
  if (inherits(e, "httr2_http_401") || inherits(e, "httr2_http_403") ||
      inherits(e, "httr2_http_404")) return(FALSE)
  if (inherits(e, "httr2_http_400")) return(FALSE)
  if (inherits(e, "httr2_failure")) return(TRUE)
  if (inherits(e, "httr2_http")) return(TRUE)
  return(TRUE)
}

extract_http_error_body <- function(e) {
  err_body <- ""
  if (inherits(e, "httr2_http_400")) {
    err_body <- tryCatch(httr2::resp_body_string(e), error = function(ignore) "<unreadable>")
  } else if (inherits(e, "httr2_http")) {
    err_body <- tryCatch(httr2::resp_body_string(e), error = function(ignore) "<unreadable>")
  }
  return(err_body)
}

norm_ollama_url <- function(base) {
  base <- sub("/+$", "", base)
  paste0(base, "/api/chat")
}

call_llm <- function(system_prompt, user_payload, attempt = 1) {
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
    httr2::req_timeout(900)

  MAX_HTTP_ATTEMPTS <- 3
  last_error <- NULL
  for (attempt_i in seq_len(MAX_HTTP_ATTEMPTS)) {
    result <- tryCatch({
      resp <- httr2::req_perform(req)
      content <- httr2::resp_body_json(resp)$message$content
      content <- gsub("(?s)^.*?</think>\\s*", "", content, perl = TRUE)
      content <- gsub("\\*", "", content, fixed = TRUE)
      content <- trimws(content)
      content
    }, error = function(e) {
      err_body <- extract_http_error_body(e)
      last_error <- paste0(e$message,
        if (nchar(err_body) > 0) paste0(" [API: ", substr(err_body, 1, 200), "]") else "")
      if (!is_transient_http_error(e)) {
        last_error <<- last_error
        stop(last_error)
      }
      last_error <<- last_error
      NULL
    })
    if (!is.null(result)) return(result)
    if (attempt_i < MAX_HTTP_ATTEMPTS && !is.null(last_error) && grepl("^httr2_http_40", last_error)) break
    if (attempt_i < MAX_HTTP_ATTEMPTS) {
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

# ===========================================================================
# ===================== EXECUTION ===========================================
# ===========================================================================

# ------------------------------------------------------------------
# PHASE A: LEVEL A — INTER-CLINICAL CONSISTENCY (32 cancer types)
# ------------------------------------------------------------------
cat("\n╔══════════════════════════════════════════════════════════════╗\n")
cat("║   LEVEL A: INTER-CLINICAL GOVERNANCE COMPLIANCE             ║\n")
cat("╚══════════════════════════════════════════════════════════════╝\n")
cat(sprintf("Testing %d patients across %d cancer types...\n",
    nrow(level_a_patients), length(unique(level_a_patients$CancerAbb))))
cat(sprintf("Target compliance: %.0f%%\n\n", COMPLIANCE_TARGET * 100))

level_a_results <- data.frame(
  Patient = character(), Cancer = character(), CancerFull = character(),
  Score = integer(), Total = integer(), Compliance = numeric(),
  Violations = character(), Elapsed_Sec = numeric(), Success = logical(),
  stringsAsFactors = FALSE
)

for (idx in seq_len(nrow(level_a_patients))) {
  pat <- level_a_patients$Patient[idx]
  cab <- level_a_patients$CancerAbb[idx]
  cfu <- level_a_patients$CancerFull[idx]
  met <- level_a_patients$Metric[idx]
  csv <- level_a_patients$CSV_Path[idx]
  
  cat(sprintf("  [%2d/%2d] %-20s %-5s ", idx, nrow(level_a_patients), pat, cab))
  
  payload_data <- build_patient_payload(pat, cab, cfu, met, csv)
  if (is.null(payload_data)) {
    cat("SKIP (no data)\n")
    level_a_results <- rbind(level_a_results, data.frame(
      Patient = pat, Cancer = cab, CancerFull = cfu,
      Score = 0, Total = 0, Compliance = NA, Violations = "NO_DATA",
      Elapsed_Sec = 0, Success = FALSE, stringsAsFactors = FALSE
    ))
    next
  }
  
  t_start <- Sys.time()
  response <- tryCatch(
    call_llm(payload_data$system_prompt, payload_data$payload),
    error = function(e) {
      cat(sprintf("ERROR: %s\n", e$message))
      return(NA_character_)
    }
  )
  t_end <- Sys.time()
  elapsed <- as.numeric(difftime(t_end, t_start, units = "secs"))
  
  if (is.na(response) || is.null(response)) {
    level_a_results <- rbind(level_a_results, data.frame(
      Patient = pat, Cancer = cab, CancerFull = cfu,
      Score = 0, Total = 0, Compliance = NA, Violations = "LLM_ERROR",
      Elapsed_Sec = elapsed, Success = FALSE, stringsAsFactors = FALSE
    ))
    next
  }
  
  # Run governance compliance check
  gc <- check_governance_compliance(response, payload_data$genes, pat, cfu, met)
  viol_str <- if (length(gc$violations) > 0) paste(gc$violations, collapse = "; ") else "PASS"
  
  level_a_results <- rbind(level_a_results, data.frame(
    Patient = pat, Cancer = cab, CancerFull = cfu,
    Score = gc$score, Total = gc$total, Compliance = round(gc$compliance, 4),
    Violations = viol_str, Elapsed_Sec = round(elapsed, 1),
    Success = TRUE, stringsAsFactors = FALSE
  ))
  
  status_icon <- if (gc$compliance >= COMPLIANCE_TARGET) "✓" else "⚠"
  cat(sprintf("%s %d/%d (%.1f%%) %.1fs\n",
      status_icon, gc$score, gc$total, gc$compliance * 100, elapsed))
}

# Level A summary
valid_a <- level_a_results[level_a_results$Success == TRUE, ]
n_valid_a <- nrow(valid_a)
mean_compliance_a <- if (n_valid_a > 0) mean(valid_a$Compliance, na.rm = TRUE) else 0
above_threshold_a <- sum(valid_a$Compliance >= COMPLIANCE_TARGET, na.rm = TRUE)
pct_above_a <- if (n_valid_a > 0) 100 * above_threshold_a / n_valid_a else 0

cat(sprintf("\n--- LEVEL A SUMMARY ---\n"))
cat(sprintf("  Patients tested:      %d\n", nrow(level_a_patients)))
cat(sprintf("  Successful:           %d\n", n_valid_a))
cat(sprintf("  Mean compliance:      %.2f%%\n", mean_compliance_a * 100))
cat(sprintf("  >= %d%% threshold:    %d/%d (%.1f%%)\n",
    COMPLIANCE_TARGET * 100, above_threshold_a, n_valid_a, pct_above_a))
cat(sprintf("  LEVEL A ASSESSMENT:   %s\n\n",
    if (pct_above_a >= COMPLIANCE_TARGET * 100) "✅ PASS" else "❌ FAIL — Amendment required"))

# ------------------------------------------------------------------
# PHASE B: LEVEL B — INTRA-CLINICAL CONSISTENCY (same patient, 5 repeats)
# ------------------------------------------------------------------
cat("╔══════════════════════════════════════════════════════════════╗\n")
cat("║   LEVEL B: INTRA-CLINICAL SEMANTIC CONSISTENCY             ║\n")
cat("╚══════════════════════════════════════════════════════════════╝\n")
cat(sprintf("Patient: %s (%s, %s)\n",
    level_b_patient$Patient, level_b_patient$CancerFull, level_b_patient$Metric))
cat(sprintf("Repeats: %d\n", LEVEL_B_REPEATS))
cat(sprintf("Target consistency: %.0f%%\n\n", COMPLIANCE_TARGET * 100))

payload_b <- build_patient_payload(
  level_b_patient$Patient, level_b_patient$CancerAbb,
  level_b_patient$CancerFull, level_b_patient$Metric,
  level_b_patient$CSV_Path
)

if (is.null(payload_b)) {
  cat("ERROR: Cannot build payload for Level B patient.\n")
  level_b_summary <- data.frame()
} else {
  level_b_responses <- character(LEVEL_B_REPEATS)
  level_b_gov_scores <- numeric(LEVEL_B_REPEATS)
  level_b_elapsed <- numeric(LEVEL_B_REPEATS)
  
  for (rep in seq_len(LEVEL_B_REPEATS)) {
    cat(sprintf("  Repeat %d/%d ... ", rep, LEVEL_B_REPEATS))
    
    t_start <- Sys.time()
    response <- tryCatch(
      call_llm(payload_b$system_prompt, payload_b$payload),
      error = function(e) {
        cat(sprintf("ERROR: %s\n", e$message))
        return(NA_character_)
      }
    )
    t_end <- Sys.time()
    elapsed <- as.numeric(difftime(t_end, t_start, units = "secs"))
    
    if (is.na(response) || is.null(response)) {
      level_b_responses[rep] <- NA_character_
      level_b_gov_scores[rep] <- NA_real_
      level_b_elapsed[rep] <- elapsed
      next
    }
    
    level_b_responses[rep] <- response
    gc <- check_governance_compliance(response, payload_b$genes,
                                       level_b_patient$Patient,
                                       level_b_patient$CancerFull,
                                       level_b_patient$Metric)
    level_b_gov_scores[rep] <- gc$compliance
    level_b_elapsed[rep] <- elapsed
    
    cat(sprintf("gov=%.1f%% (%.1fs)\n", gc$compliance * 100, elapsed))
  }
  
  valid_responses <- level_b_responses[!is.na(level_b_responses)]
  n_valid_b <- length(valid_responses)
  
  if (n_valid_b >= 2) {
    # Semantic similarity
    semantic_sim <- compute_semantic_similarity(valid_responses)
    
    # Structural stability
    stability_df <- check_structural_stability(valid_responses,
                                                paste(payload_b$genes, collapse=", "))
    mean_stability <- mean(stability_df$stability, na.rm = TRUE)
    
    # Governance compliance across repeats
    mean_gov_b <- mean(level_b_gov_scores, na.rm = TRUE)
    all_above_b <- all(level_b_gov_scores[!is.na(level_b_gov_scores)] >= COMPLIANCE_TARGET)
    
    level_b_summary <- data.frame(
      Metric = c("Semantic_Jaccard", "Structural_Stability", "Governance_Compliance",
                 "All_Above_Threshold"),
      Value = c(round(semantic_sim, 4), round(mean_stability, 4),
                round(mean_gov_b, 4), if(all_above_b) 1.0 else 0.0),
      stringsAsFactors = FALSE
    )
    
    cat(sprintf("\n--- LEVEL B SUMMARY ---\n"))
    cat(sprintf("  Valid repeats:        %d/%d\n", n_valid_b, LEVEL_B_REPEATS))
    cat(sprintf("  Semantic similarity:  %.2f%%\n", semantic_sim * 100))
    cat(sprintf("  Structural stability: %.2f%%\n", mean_stability * 100))
    cat(sprintf("  Gov compliance mean:  %.2f%%\n", mean_gov_b * 100))
    cat(sprintf("  All repeats >= %d%%:  %s\n",
        COMPLIANCE_TARGET * 100, if(all_above_b) "YES" else "NO"))
    
    level_b_pass <- (semantic_sim >= COMPLIANCE_TARGET &&
                     mean_gov_b >= COMPLIANCE_TARGET &&
                     all_above_b)
    cat(sprintf("  LEVEL B ASSESSMENT:   %s\n\n",
        if(level_b_pass) "✅ PASS" else "❌ FAIL — Amendment required"))
  } else {
    cat("\n  ⚠ Insufficient valid responses for Level B analysis\n")
    level_b_summary <- data.frame()
    level_b_pass <- FALSE
  }
}

# ===========================================================================
# FINAL SUMMARY & RECOMMENDATION
# ===========================================================================

# ------------------------------------------------------------------
# PHASE C: ONCOKB CONCORDANCE — Pharmacogenomic Narrative Validation
# ------------------------------------------------------------------
cat("╔══════════════════════════════════════════════════════════════╗\n")
cat("║   PHASE C: ONCOKB CONCORDANCE — Pharma Narrative Test      ║\n")
cat("╚══════════════════════════════════════════════════════════════╝\n")

# Load OncoKB actionability layer and unified drug matrix
oncokb_layer <- NULL
unified_drug_matrix <- NULL

if (file.exists("oncokb_clinical_actionability_layer.R")) {
  tryCatch({
    source("oncokb_clinical_actionability_layer.R")
    oncokb_layer <- init_oncokb_clinical_actionability_layer(".")
    cat("  OncoKB Clinical Actionability Layer loaded.\n")
  }, error = function(e) {
    cat(sprintf("  ⚠ OncoKB layer unavailable: %s\n", e$message))
  })
}

if (file.exists("Unified_Drug_Matrix.rds")) {
  tryCatch({
    unified_drug_matrix <- readRDS("Unified_Drug_Matrix.rds")
    cat(sprintf("  Unified Drug Matrix loaded: %d rows.\n", nrow(unified_drug_matrix)))
  }, error = function(e) {
    cat(sprintf("  ⚠ Drug matrix unreadable: %s\n", e$message))
  })
}

# Select up to 5 Level-A patients for pharmacogenomic testing
n_pharma <- min(5, nrow(level_a_patients))
pharma_patients <- level_a_patients[sample(nrow(level_a_patients), n_pharma), ]

# Build pharmacogenomic-aware system prompt (replicates app.R v1.1 governance)
pharma_system_prompt <- paste0(
  "You are an expert clinical pharmacogenomic AI analyzing a patient's top genetic ",
  "vulnerabilities cross-referenced against the databases listed in the ",
  "pharmacogenomic matrix. Output a professional clinical synthesis. ",
  "Do NOT synthesize a treatment plan or make clinical recommendations.\n\n",
  "--- PHARMACOGENOMIC EVIDENCE GOVERNANCE v1.1 ---\n\n",
  "The pharmacogenomic matrix is EVIDENCE-TIERED (Tier 1 through Tier 5). ",
  "You MUST respect these tiers. Tier 1 = strongest, Tier 5 = weakest. ",
  "Every association MUST be qualified by its tier. ",
  "Tier 4-5 associations MUST be presented as requiring further validation.\n\n",
  oncokb_concordance_lock, "\n\n",
  "SOURCE DATABASE INTEGRITY: You MUST ONLY name databases that actually appear ",
  "in the Source_Database column. Do NOT invent database names.\n\n",
  "END PHARMACOGENOMIC EVIDENCE GOVERNANCE\n\n",
  hedging_rule, "\n\n",
  strict_nomenclature_rule, "\n\n",
  "MANDATORY REQUIREMENT: Append EXACTLY 3 Suggested Clinical Queries at the end.\n",
  "Append the mandatory hypothesis-generating disclaimer as the final paragraph."
)

level_c_results <- data.frame(
  Patient = character(), Cancer = character(), CancerFull = character(),
  N_Genes = integer(), N_Drugs = integer(), N_T0A = integer(), N_T0B = integer(), N_T0C = integer(),
  Score = integer(), Total = integer(), Compliance = numeric(),
  Violations = character(), Elapsed_Sec = numeric(), Success = logical(),
  stringsAsFactors = FALSE
)

cat(sprintf("Testing %d patients with pharmacogenomic injection...\n\n", n_pharma))

for (idx in seq_len(n_pharma)) {
  pat <- pharma_patients$Patient[idx]
  cab <- pharma_patients$CancerAbb[idx]
  cfu <- pharma_patients$CancerFull[idx]
  met <- pharma_patients$Metric[idx]
  csv <- pharma_patients$CSV_Path[idx]
  
  cat(sprintf("  [%d/%d] %-20s %-5s ", idx, n_pharma, pat, cab))
  
  # Build clinical payload (same as Phase A)
  payload_data <- build_patient_payload(pat, cab, cfu, met, csv)
  if (is.null(payload_data)) {
    cat("SKIP (no data)\n")
    level_c_results <- rbind(level_c_results, data.frame(
      Patient = pat, Cancer = cab, CancerFull = cfu,
      N_Genes = 0, N_Drugs = 0, N_T0A = 0, N_T0B = 0, N_T0C = 0,
      Score = 0, Total = 0, Compliance = NA, Violations = "NO_DATA",
      Elapsed_Sec = 0, Success = FALSE, stringsAsFactors = FALSE
    ))
    next
  }
  
  patient_genes <- payload_data$genes
  
  # --- Tier 0 Actionability query ---
  t0_summary <- ""
  n_t0a <- 0; n_t0b <- 0; n_t0c <- 0
  if (!is.null(oncokb_layer) && length(patient_genes) > 0) {
    t0_result <- tryCatch({
      query_patient_actionability(patient_genes, oncokb_layer, cfu)
    }, error = function(e) NULL)
    if (!is.null(t0_result) && !is.null(t0_result$tier0)) {
      n_t0a <- t0_result$tier0$n_t0a
      n_t0b <- t0_result$tier0$n_t0b
      n_t0c <- t0_result$tier0$n_t0c
      if (!is.null(t0_result$tier0_narrative)) {
        t0_summary <- paste0(t0_result$tier0_narrative, "\n\n")
      }
    }
  }
  
  # --- Drug matrix matching ---
  n_drugs <- 0
  drug_block <- ""
  if (!is.null(unified_drug_matrix) && nrow(unified_drug_matrix) > 0) {
    matched_drugs <- unified_drug_matrix[
      unified_drug_matrix$Gene_Symbol %in% patient_genes, , drop = FALSE]
    if (nrow(matched_drugs) > 0) {
      n_drugs <- nrow(matched_drugs)
      # Build abbreviated tiered summary
      db_sources <- unique(matched_drugs$Source_Database)
      db_sources <- db_sources[!is.na(db_sources) & db_sources != ""]
      
      lines <- c("PHARMACOGENOMIC EVIDENCE-TIERED MATRIX",
                 "==========================================",
                 sprintf("Total gene-drug associations: %d", nrow(matched_drugs)),
                 sprintf("Source databases: %s", paste(db_sources, collapse=", ")),
                 "")
      
      # Conditional tier legend
      has_oncokb_api <- any(grepl("OncoKB", db_sources, ignore.case = TRUE))
      lines <- c(lines, "EVIDENCE TIER LEGEND:")
      if (has_oncokb_api) {
        lines <- c(lines, "  Tier 1 = FDA-recognized (OncoKB Level 1)",
                   "  Tier 2 = Standard care (OncoKB Level 2 / CIViC Level A)",
                   "  Tier 3 = Compelling clinical evidence (OncoKB Level 3A/3B / CIViC Level B / DGIdb MultiSource+PMID)",
                   "  Tier 4 = Clinical/Preclinical (OncoKB Level 4 / CIViC Level C / DGIdb MultiSource)")
      } else {
        lines <- c(lines, "  Tier 1 = FDA-recognized / Standard-of-care (CIViC Level A)",
                   "  Tier 2 = Standard care / Strong clinical evidence (CIViC Level B)",
                   "  Tier 3 = Compelling clinical evidence (CIViC Level B / DGIdb MultiSource+PMID)",
                   "  Tier 4 = Clinical/Preclinical (CIViC Level C / DGIdb MultiSource)",
                   "",
                   "⚠ ONCOKB CONCORDANCE LOCK: OncoKB therapeutic Levels 1-4 are NOT present in this matrix.",
                   "  YOU MUST NOT claim any association has 'OncoKB Level 1/2/3A/3B/4' support.",
                   "  YOU MUST NOT fabricate OncoKB therapeutic level classifications.")
      }
      lines <- c(lines, "  Tier 5 = Single-source / Preclinical / Inferential (CIViC Level D-E / DGIdb SingleSource)", "")
      
      tiers <- sort(unique(matched_drugs$Evidence_Tier))
      for (t in tiers) {
        t_df <- matched_drugs[matched_drugs$Evidence_Tier == t, , drop = FALSE]
        lines <- c(lines, sprintf("--- Tier %d (%d associations) ---", t, nrow(t_df)))
        for (i in seq_len(nrow(t_df))) {
          row <- t_df[i, ]
          lines <- c(lines, sprintf("  %s | %s | %s | %s | %s",
            row$Gene_Symbol, row$Drug_Name, row$Interaction_Type,
            ifelse(!is.null(row$Source_Database), row$Source_Database, "Unknown"),
            ifelse(!is.null(row$Clinical_Status), row$Clinical_Status, "Unknown")))
        }
        lines <- c(lines, "")
      }
      lines <- c(lines, "CRITICAL: Qualify each association by its evidence tier. Tier 4-5 require further validation.")
      drug_block <- paste(c(lines, ""), collapse = "\n")
    } else {
      drug_block <- paste0("PHARMACOGENOMIC MATRIX: No drug-gene associations found for genes: ",
                          paste(patient_genes, collapse=", "), ".\n\n")
    }
  }
  
  # --- Assemble combined payload ---
  combined_payload <- paste0(
    payload_data$payload, "\n\n",
    "═══════════════════════════════════════════════════════════════════\n",
    "  PHARMACOGENOMIC & ONCOKB CLINICAL ACTIONABILITY SECTION\n",
    "═══════════════════════════════════════════════════════════════════\n\n",
    t0_summary,
    drug_block,
    "\nANALYSIS INSTRUCTION: Synthesize the clinical profile AND the pharmacogenomic/Tier 0 evidence. ",
    "Respect the OncoKB Concordance Lock — do NOT fabricate OncoKB therapeutic levels. ",
    "Present Tier 0 (regulatory) and drug-gene evidence (Stratum 7) as DISTINCT dimensions."
  )
  
  # --- Call LLM ---
  t_start <- Sys.time()
  response <- tryCatch(
    call_llm(pharma_system_prompt, combined_payload),
    error = function(e) {
      cat(sprintf("ERROR: %s\n", e$message))
      return(NA_character_)
    }
  )
  t_end <- Sys.time()
  elapsed <- as.numeric(difftime(t_end, t_start, units = "secs"))
  
  if (is.na(response) || is.null(response)) {
    level_c_results <- rbind(level_c_results, data.frame(
      Patient = pat, Cancer = cab, CancerFull = cfu,
      N_Genes = length(patient_genes), N_Drugs = n_drugs,
      N_T0A = n_t0a, N_T0B = n_t0b, N_T0C = n_t0c,
      Score = 0, Total = 0, Compliance = NA, Violations = "LLM_ERROR",
      Elapsed_Sec = elapsed, Success = FALSE, stringsAsFactors = FALSE
    ))
    next
  }
  
  # Check governance compliance (G1-G14)
  gc <- check_governance_compliance(response, patient_genes, pat, cfu, met)
  viol_str <- if (length(gc$violations) > 0) paste(gc$violations, collapse = "; ") else "PASS"
  
  level_c_results <- rbind(level_c_results, data.frame(
    Patient = pat, Cancer = cab, CancerFull = cfu,
    N_Genes = length(patient_genes), N_Drugs = n_drugs,
    N_T0A = n_t0a, N_T0B = n_t0b, N_T0C = n_t0c,
    Score = gc$score, Total = gc$total, Compliance = round(gc$compliance, 4),
    Violations = viol_str, Elapsed_Sec = round(elapsed, 1),
    Success = TRUE, stringsAsFactors = FALSE
  ))
  
  status_icon <- if (gc$compliance >= COMPLIANCE_TARGET) "✓" else "⚠"
  cat(sprintf("%s %d/%d (%.1f%%) genes=%d drugs=%d T0A=%d T0B=%d T0C=%d %.1fs\n",
      status_icon, gc$score, gc$total, gc$compliance * 100,
      length(patient_genes), n_drugs, n_t0a, n_t0b, n_t0c, elapsed))
}

# Phase C summary
valid_c <- level_c_results[level_c_results$Success == TRUE, ]
n_valid_c <- nrow(valid_c)
mean_compliance_c <- if (n_valid_c > 0) mean(valid_c$Compliance, na.rm = TRUE) else 0
above_threshold_c <- sum(valid_c$Compliance >= COMPLIANCE_TARGET, na.rm = TRUE)
pct_above_c <- if (n_valid_c > 0) 100 * above_threshold_c / n_valid_c else 0

# Count specific OncoKB violations
n_g12 <- sum(grepl("G12:", level_c_results$Violations, fixed = TRUE), na.rm = TRUE)
n_g13 <- sum(grepl("G13:", level_c_results$Violations, fixed = TRUE), na.rm = TRUE)
n_g14 <- sum(grepl("G14:", level_c_results$Violations, fixed = TRUE), na.rm = TRUE)

cat(sprintf("\n--- PHASE C SUMMARY ---\n"))
cat(sprintf("  Patients tested:      %d\n", n_pharma))
cat(sprintf("  Successful:           %d\n", n_valid_c))
cat(sprintf("  Mean compliance:      %.2f%%\n", mean_compliance_c * 100))
cat(sprintf("  >= %d%% threshold:    %d/%d (%.1f%%)\n",
    COMPLIANCE_TARGET * 100, above_threshold_c, n_valid_c, pct_above_c))
cat(sprintf("  OncoKB violations:\n"))
cat(sprintf("    G12 (level fabrication):       %d\n", n_g12))
cat(sprintf("    G13 (Fda↔therapeutic confl.):  %d\n", n_g13))
cat(sprintf("    G14 (T0 misrepresented):       %d\n", n_g14))

phase_c_pass <- (pct_above_c >= COMPLIANCE_TARGET * 100) && (n_g12 + n_g13 + n_g14 == 0)
cat(sprintf("  PHASE C ASSESSMENT:   %s\n\n",
    if(phase_c_pass) "✅ PASS — Concordance Lock respected"
    else "❌ FAIL — OncoKB level violations detected"))

# ===========================================================================
# FINAL SUMMARY & RECOMMENDATION
# ===========================================================================
cat("╔══════════════════════════════════════════════════════════════╗\n")
cat("║        CRIT-03 FINAL ASSESSMENT                             ║\n")
cat("╚══════════════════════════════════════════════════════════════╝\n")

level_a_pass <- (pct_above_a >= COMPLIANCE_TARGET * 100)
level_b_pass <- exists("level_b_pass") && level_b_pass
phase_c_pass <- exists("phase_c_pass") && phase_c_pass

cat(sprintf("  Level A (Inter-clinical):     %s (%.1f%% above %.0f%%)\n",
    if(level_a_pass) "✅ PASS" else "❌ FAIL",
    pct_above_a, COMPLIANCE_TARGET * 100))
cat(sprintf("  Level B (Intra-clinical):     %s\n",
    if(level_b_pass) "✅ PASS" else "❌ FAIL"))
cat(sprintf("  Phase C (OncoKB Concordance): %s\n",
    if(phase_c_pass) "✅ PASS" else "❌ FAIL"))

overall_pass <- level_a_pass && level_b_pass && phase_c_pass
cat(sprintf("\n  OVERALL:  %s\n",
    if(overall_pass) "✅ PASS — No amendment needed"
    else "❌ FAIL — LLM OUTPUT CACHING AMENDMENT REQUIRED"))

if (!overall_pass) {
  cat("\n──────────────────────────────────────────────────────────────\n")
  cat("RECOMMENDED AMENDMENT: Implement LLM output caching in appV2.R\n")
  cat("\nCache key: SHA-256(patient_id + cohort + metric + input_payload)\n")
  cat("Cache dir:  llm_cache/\n")
  cat("Behavior:   Same patient + same payload → return cached output.\n")
  cat("            'Regenerate' button forces fresh generation.\n")
  cat("──────────────────────────────────────────────────────────────\n")
}

# Write results
write.csv(level_a_results, "crit03_level_a_results.csv", row.names = FALSE)
if (nrow(level_b_summary) > 0) {
  write.csv(level_b_summary, "crit03_level_b_summary.csv", row.names = FALSE)
}
if (nrow(level_c_results) > 0) {
  write.csv(level_c_results, "crit03_level_c_oncokb_concordance.csv", row.names = FALSE)
}

summary_lines <- c(
  "============================================================",
  "CRIT-03 REPRODUCIBILITY VALIDATION — RESULTS",
  "============================================================",
  sprintf("Date:              %s", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
  sprintf("LLM Model:         %s", LLM_MODEL),
  sprintf("Compliance Target: %.0f%%", COMPLIANCE_TARGET * 100),
  "",
  sprintf("LEVEL A (Inter-clinical):"),
  sprintf("  Patients tested:     %d", nrow(level_a_patients)),
  sprintf("  Mean compliance:     %.2f%%", mean_compliance_a * 100),
  sprintf("  Above threshold:     %d/%d (%.1f%%)", above_threshold_a, n_valid_a, pct_above_a),
  sprintf("  Assessment:          %s", if(level_a_pass) "PASS" else "FAIL"),
  "",
  sprintf("LEVEL B (Intra-clinical):"),
  sprintf("  Assessment:          %s", if(level_b_pass) "PASS" else "FAIL"),
  "",
  sprintf("PHASE C (OncoKB Concordance — Pharmacogenomic):"),
  sprintf("  Patients tested:     %d", n_pharma),
  sprintf("  Mean compliance:     %.2f%%", mean_compliance_c * 100),
  sprintf("  Above threshold:     %d/%d (%.1f%%)", above_threshold_c, n_valid_c, pct_above_c),
  sprintf("  G12 violations:      %d", n_g12),
  sprintf("  G13 violations:      %d", n_g13),
  sprintf("  G14 violations:      %d", n_g14),
  sprintf("  Assessment:          %s", if(phase_c_pass) "PASS" else "FAIL"),
  "",
  sprintf("OVERALL: %s", if(overall_pass) "PASS" else "FAIL — Amendment required"),
  "",
  "Note: The LLM output caching amendment has been pre-applied to appV2.R,",
  "      app.R, and chuchu.R (see CRIT-03 section in send_llm_request).",
  "      Phase C tests OncoKB Concordance Lock (G12-G14) with live",
  "      pharmacogenomic matrix + Tier 0 evidence injection.",
  "      Validation confirms whether the amendment was needed.",
  "============================================================"
)

cat(paste(summary_lines, collapse="\n"), "\n")
writeLines(summary_lines, "crit03_validation_summary.txt")

cat("\nOutput files:\n")
cat("  crit03_level_a_results.csv\n")
if (nrow(level_b_summary) > 0) cat("  crit03_level_b_summary.csv\n")
if (nrow(level_c_results) > 0) cat("  crit03_level_c_oncokb_concordance.csv\n")
cat("  crit03_validation_summary.txt\n")
cat("\nDone at", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")


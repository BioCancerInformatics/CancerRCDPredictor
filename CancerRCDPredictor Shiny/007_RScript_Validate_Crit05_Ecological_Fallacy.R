# ==============================================================================
# CRIT-05 VALIDATION STUDY v2: Ecological Fallacy Detection Across 100 Patients
# ==============================================================================
# Extracts real patient SHAP profiles from trajectory CSVs, constructs actual
# LLM prompts, generates Clinical Synthesis reports, and runs
# check_ecological_fallacy() on each. Background run ~1-2 hours.
# ==============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(httr2)
  library(stringr)
})

WORKING_DIR <- "."
setwd(WORKING_DIR)

cat("=== CRIT-05 ECOLOGICAL FALLACY VALIDATION STUDY v2 ===\n")
cat("Started:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n\n")

# ---------------------------------------------------------------------------
# 1. Load reference data
# ---------------------------------------------------------------------------
cat("[1/6] Loading reference data...\n")

stemness_df <- tryCatch(
  read.delim("Merged_Cancer_Stemness.tsv", sep="\t", stringsAsFactors=FALSE),
  error=function(e) stop("Merged_Cancer_Stemness.tsv not found")
)

# Load Table S11 for gene-to-RCD mappings
table_s11 <- tryCatch({
  first_line <- readLines("Table_S11_Interpreter_12k.csv", n=1)
  if(grepl(";", first_line)) read.csv2("Table_S11_Interpreter_12k.csv", stringsAsFactors=FALSE)
  else read.csv("Table_S11_Interpreter_12k.csv", stringsAsFactors=FALSE)
}, error=function(e) NULL)

# Load NCBI gene info for gene mechanics descriptions
gene_info <- tryCatch(
  read.csv("NCBI_gene_info.csv", sep=";", stringsAsFactors=FALSE),
  error=function(e) NULL
)

# Find trajectory CSVs
zima_root <- ".."
models_dir <- file.path(zima_root, "PHASE_III_ML_Models")
if (!dir.exists(models_dir)) models_dir <- "PHASE_III_ML_Models"
if (!dir.exists(models_dir)) stop("Cannot find PHASE_III_ML_Models directory")

cohort_dirs <- list.dirs(models_dir, recursive=FALSE)
cat(sprintf("  Found %d cohort directories\n", length(cohort_dirs)))

# ---------------------------------------------------------------------------
# 2. Collect patient trajectory CSV paths
# ---------------------------------------------------------------------------
cat("[2/6] Collecting patient trajectory files...\n")

patient_files <- data.frame(
  Patient = character(), Cohort = character(), CSV_Path = character(),
  stringsAsFactors = FALSE
)

for (cdir in cohort_dirs) {
  cohort <- basename(cdir)
  xgb_dir <- file.path(cdir, "XGBoost")
  if (!dir.exists(xgb_dir)) next
  csvs <- list.files(xgb_dir, pattern="_Trajectory_.*_Trajectory_Data\\.csv$", full.names=TRUE)
  for (csv in csvs) {
    pat <- gsub("^.*_Trajectory_([A-Za-z0-9-]+)_Trajectory_Data\\.csv$", "\\1", basename(csv))
    patient_files <- rbind(patient_files, data.frame(
      Patient=pat, Cohort=cohort, CSV_Path=csv, stringsAsFactors=FALSE
    ))
  }
}

# Filter to patients with stemness data
patient_files <- patient_files[patient_files$Patient %in% stemness_df$sample_id, ]
cat(sprintf("  Found %d patients with trajectory + stemness data\n", nrow(patient_files)))

# ---------------------------------------------------------------------------
# 3. Sample 100 patients
# ---------------------------------------------------------------------------
cat("[3/6] Sampling 100 patients...\n")
set.seed(as.integer(Sys.Date()))
N_SAMPLE <- min(100, nrow(patient_files))
sampled <- patient_files[sample(nrow(patient_files), N_SAMPLE), ]

# ---------------------------------------------------------------------------
# 4. Build SHAP profile text function (replicates app.R logic)
# ---------------------------------------------------------------------------
cat("[4/6] Building governance rules...\n")

build_patient_payload <- function(pat_id, cohort, csv_path) {
  # Read trajectory CSV: columns are Feature (nomenclature) and SHAP_Value (patient's value)
  traj <- tryCatch(read.csv(csv_path, stringsAsFactors=FALSE), error=function(e) NULL)
  if (is.null(traj) || nrow(traj) == 0 || !"Feature" %in% names(traj)) return(NULL)
  
  # Sort by absolute SHAP value, take top 5
  traj$AbsSHAP <- abs(traj$SHAP_Value)
  traj <- traj[order(traj$AbsSHAP, decreasing=TRUE), ]
  top5 <- head(traj, 5)
  
  # Build signature lines
  sig_lines <- c()
  for (i in seq_len(nrow(top5))) {
    feat <- top5$Feature[i]
    patient_shap <- top5$SHAP_Value[i]
    
    # Determine SHAP impact
    shap_direction <- if (patient_shap > 0) "Positive SHAP (Pro-Progression/Recurrence)" 
                      else "Negative SHAP (Anti-Progression/Stabilizing)"
    
    # Look up gene/RCD/omic from Table S11 by Nomenclature column
    gene <- "Unknown"
    rcd_form <- "Unknown"
    omic_layer <- "Unknown"
    
    if (!is.null(table_s11)) {
      s11_row <- table_s11[table_s11$Nomenclature == feat, ]
      if (nrow(s11_row) > 0) {
        # Extract primary gene from Decoded.Genetic.Element column
        decoded <- as.character(s11_row$Decoded.Genetic.Element[1])
        gene <- "Unknown"
        if (!is.na(decoded) && nchar(decoded) > 0) {
          # Format is like: (GENE1(1/5)) + (GENE2(2/3)) + ...
          # Extract first gene symbol
          gene_match <- regmatches(decoded, regexpr("[A-Za-z][A-Za-z0-9-]+(?=\\(\\d+/)", decoded, perl=TRUE))
          if (length(gene_match) > 0) gene <- gene_match[1]
        }
        # RCD form
        if ("RCD.form" %in% names(s11_row)) {
          rcd_form <- as.character(s11_row$RCD.form[1])
          if (is.na(rcd_form) || rcd_form == "") rcd_form <- "Unknown"
        }
        # Omic feature
        if ("Omic.feature" %in% names(s11_row)) {
          omic_raw <- as.character(s11_row$Omic.feature[1])
          if (!is.na(omic_raw)) {
            omic_layer <- if (grepl("Transcript", omic_raw, ignore.case=TRUE)) "Transcript"
                     else if (grepl("mRNA", omic_raw, ignore.case=TRUE)) "mRNA"
                     else if (grepl("Methylation", omic_raw, ignore.case=TRUE)) "CpG Methylation"
                     else if (grepl("miRNA", omic_raw, ignore.case=TRUE)) "miRNA"
                     else if (grepl("Protein", omic_raw, ignore.case=TRUE)) "Protein"
                     else if (grepl("Mutation", omic_raw, ignore.case=TRUE)) "Mutation"
                     else if (grepl("CNV", omic_raw, ignore.case=TRUE)) "CNV"
                     else omic_raw
          }
        }
      }
    }
    
    # Get gene mechanics from NCBI
    gene_mechanics <- paste0(gene, " (gene description)")
    if (!is.null(gene_info) && gene != "Unknown") {
      gi_row <- gene_info[gene_info$Gene.symbol == gene, ]
      if (nrow(gi_row) > 0 && !is.na(gi_row$Summary[1]) && nchar(gi_row$Summary[1]) > 20) {
        summary <- gsub("\\[provided by [^]]+\\]", "", gi_row$Summary[1])
        summary <- trimws(summary)
        gene_mechanics <- paste0(gene, " (", summary, ")")
      }
    }
    
    sig_lines <- c(sig_lines, sprintf(
      "Signature: %s > SHAP Impact: %s (Value: %.4f) > Omic Layer: %s > Associated RCD Form: %s > Encoded Gene Mechanics: %s",
      feat, shap_direction, patient_shap, omic_layer, rcd_form, gene_mechanics
    ))
  }
  
  # Get patient phenotype
  pheno <- stemness_df[stemness_df$sample_id == pat_id, ]
  if (nrow(pheno) == 0) return(NULL)
  
  rnass <- pheno$RNAss[1]
  ereg <- pheno$EREG.EXPss[1]
  rnass_cls <- pheno$RNAss_class[1]
  ereg_cls <- pheno$EREG_class[1]
  tmb_val <- pheno$Non_silent_per_Mb[1]
  tmb_cls <- if (!is.na(pheno$TMB_class[1]) && pheno$TMB_class[1] != "") pheno$TMB_class[1] else "No data"
  msi_val <- pheno$Total_nb_MSI_events[1]
  msi_cls <- if (!is.na(pheno$MSI_class[1]) && pheno$MSI_class[1] != "") pheno$MSI_class[1] else "No data"
  
  tsm_text <- sprintf("TSM: RNAss = %.3f (%s), EREG.EXPss = %.3f (%s)",
    ifelse(is.na(rnass), 0, rnass), ifelse(is.na(rnass_cls), "No data", rnass_cls),
    ifelse(is.na(ereg), 0, ereg), ifelse(is.na(ereg_cls), "No data", ereg_cls))
  tmb_text <- sprintf("TMB (Non-silent/Mb): %.3f (%s)",
    ifelse(is.na(tmb_val), 0, tmb_val), ifelse(is.na(tmb_cls), "No data", tmb_cls))
  msi_text <- if (is.na(msi_cls) || msi_cls == "") {
    "MSI: No data available for this patient"
  } else {
    sprintf("MSI: %.4f (%s)", ifelse(is.na(msi_val), 0, msi_val), msi_cls)
  }
  
  payload <- paste0(
    "Patient ID: ", pat_id, " Cancer Cohort: ", cohort, " Clinical Metric: PFI\n\n",
    "Patient's Specific Multi-Omic Expression Profile: The patient's clinical trajectory is mathematically driven by the following Top ",
    length(sig_lines), " unique molecular signatures extracted from their personalized XGBoost SHAP geometry:\n\n",
    paste(sig_lines, collapse="\n\n"), "\n\n",
    "BIOLOGICAL CONTEXT: Each signature is a unique multi-omic coordinate. Note that different signatures may contain different omic layers (e.g., RNA vs Methylation) of the same gene, allowing the tumor to utilize the same genetic element in opposing directions simultaneously. You MUST NOT use the signature nomenclature to infer the patient's individual biology; rely strictly on the explicit High/Intermediate/Low TSM, TMB, and MSI classifications provided.\n\n",
    "PATIENT-SPECIFIC PHENOTYPE CLASSIFICATIONS (USE THESE EXACT VALUES FOR ALL INTERPRETATION): ",
    tsm_text, ". ", tmb_text, ". ", msi_text, ". ",
    "CRITICAL: You MUST incorporate these exact patient-level TSM, TMB, and MSI classifications into your clinical synthesis. Do NOT generalize from population-level correlations embedded in signature nomenclature.\n\n",
    "Analyze this patient's profile and provide the therapeutic vulnerability synthesis."
  )
  
  return(payload)
}

# Governance rules (identical to app.R)
phenotype_correlation_rule <- paste0(
  "--- ECOLOGICAL FALLACY PREVENTION ---\n\n",
  "The patient's TSM, TMB, and MSI phenotypes (High/Intermediate/Low) are INDIVIDUAL MEASUREMENTS. ",
  "They are NOT population-level correlations. You MUST reason about the patient using ONLY ",
  "their own measured values.\n\n",
  "FORBIDDEN PATTERNS (DELETE AND REWRITE IMMEDIATELY):\n",
  "  - 'The patient's Intermediate TSM may reflect the P-positive correlation...'\n",
  "  - 'Consistent with the N-negative population-level sign...'\n",
  "  - 'This aligns with the cohort-level association of TSM with survival...'\n",
  "  - 'Given the population-wide TMB correlation pattern...'\n",
  "  - 'The P-positive population sign suggests that this patient...'\n\n",
  "PERMITTED PATTERNS:\n",
  "  - 'The patient's individual TSM measurement is Intermediate (RNAss = X), which individually...'\n",
  "  - 'The patient exhibits Low TMB (X Mut/Mb), which in this individual context may suggest...'\n\n",
  "POPULATION-LEVEL TERMS YOU MUST NEVER USE in patient-specific reasoning:\n",
  "  'P-positive', 'N-negative', 'population-level correlation', 'cohort-level correlation',\n",
  "  'population sign', 'population-wide', 'cohort-wide', 'population-level TSM/TMB/MSI'\n\n",
  "PRE-OUTPUT SELF-CHECK: Scan every paragraph containing patient-specific markers. ",
  "If ANY also contains population-level terms, REWRITE using only the patient's own measurements.\n\n",
  "--- END ECOLOGICAL FALLACY PREVENTION ---\n"
)

strict_nomenclature_rule <- paste0(
  "STRICT NOMENCLATURE RULE: You MUST NOT use, mention, or display ANY part of the signature ",
  "nomenclatures (e.g., THYM-1460.6.3.N.2.35.5.2.3.3) or their abbreviated forms (e.g., THYM-1460, ",
  "LUAD-1883, LUAD-636) anywhere in your clinical narrative. These technical provenance identifiers ",
  "belong exclusively in the audit section."
)

system_prompt_template <- paste0(
  "You are an expert clinical molecular oncologist analyzing a patient's multi-omic profile. ",
  "Output a professional, fluid clinical synthesis in continuous paragraph form. ",
  "CRITICAL HEDGING: Use hedging language (suggest, may indicate, is consistent with). ",
  "Avoid strong claims (proves, confirms, demonstrates, drives, causes). ",
  strict_nomenclature_rule, "\n\n",
  "GLOBAL ASSOCIATIVITY RULE: The multi-omic signatures, SHAP values, and stemness correlations ",
  "provided represent associative mathematical relationships, not proven causative biological pathways. ",
  "You MUST frame all interpretations as targeting associative vulnerabilities, not definitively ",
  "causative mechanisms.\n\n",
  phenotype_correlation_rule
)

# ---------------------------------------------------------------------------
# 5. Ecological Fallacy Checker (identical to app.R)
# ---------------------------------------------------------------------------
check_ecological_fallacy <- function(txt, patient_id, module) {
  paragraphs <- strsplit(txt, "\n\n+")[[1]]
  pop_markers <- c(
    "P-positive", "N-negative", "P_positive", "N_negative",
    "P.positive", "N.negative",
    "population-level correlation", "population level correlation",
    "cohort-level correlation", "cohort level correlation",
    "population sign", "population correlation sign",
    "population-wide", "cohort-wide",
    "population-level TSM", "population-level TMB", "population-level MSI"
  )
  patient_markers <- c(
    "this patient", "the patient", "this individual", "the individual's",
    patient_id
  )
  violations <- list()
  for (i in seq_along(paragraphs)) {
    para <- paragraphs[i]
    has_patient <- any(sapply(patient_markers, function(m) grepl(m, para, ignore.case=TRUE)))
    has_population <- any(sapply(pop_markers, function(m) grepl(m, para, ignore.case=TRUE)))
    if (has_patient && has_population) {
      matched_pop <- pop_markers[sapply(pop_markers, function(m) grepl(m, para, ignore.case=TRUE))]
      violations[[length(violations)+1]] <- list(
        paragraph_num = i,
        matched_markers = paste(unique(matched_pop), collapse="; "),
        excerpt = substr(trimws(para), 1, 400)
      )
    }
  }
  if (length(violations) > 0) {
    log_file <- "ecological_fallacy_audit_log.csv"
    for (v in violations) {
      new_row <- data.frame(
        Timestamp=format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
        Patient_ID=patient_id, Module=module,
        Paragraph_Num=v$paragraph_num,
        Matched_Markers=v$matched_markers,
        Excerpt=v$excerpt, stringsAsFactors=FALSE
      )
      if (!file.exists(log_file)) write.csv(new_row, log_file, row.names=FALSE)
      else write.table(new_row, log_file, append=TRUE, sep=",", col.names=FALSE, row.names=FALSE)
    }
  }
  return(length(violations))
}

# ---------------------------------------------------------------------------
# 6. Run validation
# ---------------------------------------------------------------------------
cat(sprintf("[5/6] Generating reports for %d patients...\n", N_SAMPLE))
cat("      This will take 1-2 hours.\n\n")

OLLAMA_URL <- "http://localhost:11434"
LLM_MODEL <- "qwen3:8b"

# --- HTTP Error Classification & Retry Helpers ---
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

call_llm_with_retry <- function(system_prompt, user_payload) {
  total_chars <- nchar(system_prompt) + nchar(user_payload)
  est_tokens <- round(total_chars / 4)
  req <- httr2::request(norm_ollama_url(OLLAMA_URL)) |>
    httr2::req_headers("Content-Type"="application/json") |>
    httr2::req_body_json(list(
      model=LLM_MODEL, stream=FALSE,
      messages=list(
        list(role="system", content=system_prompt),
        list(role="user", content=user_payload)
      ),
      options=list(num_ctx=16384, temperature=0, num_predict=4096),
      keep_alive="30m"
    )) |>
    httr2::req_timeout(900)
  MAX_HTTP_ATTEMPTS <- 3
  last_error <- NULL
  for (attempt_i in seq_len(MAX_HTTP_ATTEMPTS)) {
    result <- tryCatch({
      resp <- httr2::req_perform(req)
      httr2::resp_body_json(resp)$message$content
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
    if (!is.null(result)) return(list(success=TRUE, content=result))
    if (attempt_i < MAX_HTTP_ATTEMPTS && !is.null(last_error) && grepl("^httr2_http_40", last_error)) break
    if (attempt_i < MAX_HTTP_ATTEMPTS) {
      wait_sec <- min(2^(attempt_i) * (0.75 + runif(1, 0, 0.5)), 15)
      message(sprintf("[RETRY %d/%d] Ollama HTTP failed (%s). Retrying in %.1fs... [Prompt ~%d tokens | %d chars]",
                      attempt_i, MAX_HTTP_ATTEMPTS - 1, last_error, wait_sec, est_tokens, total_chars))
      Sys.sleep(wait_sec)
    }
  }
  return(list(success=FALSE, error=paste0("Failed after ", MAX_HTTP_ATTEMPTS,
       " attempts. Last error: ", last_error,
       " [Prompt ~", est_tokens, " tokens | ", total_chars, " chars]")))
}

results <- data.frame(
  Patient=character(N_SAMPLE), Cohort=character(N_SAMPLE),
  Success=logical(N_SAMPLE), Response_Chars=integer(N_SAMPLE),
  Violations=integer(N_SAMPLE), Elapsed_Sec=numeric(N_SAMPLE),
  stringsAsFactors=FALSE
)

total_violations <- 0

for (idx in seq_len(N_SAMPLE)) {
  pat <- sampled$Patient[idx]
  coh <- sampled$Cohort[idx]
  csv <- sampled$CSV_Path[idx]
  
  payload <- build_patient_payload(pat, coh, csv)
  if (is.null(payload)) {
    results[idx,] <- list(pat, coh, FALSE, 0L, NA_integer_, 0)
    cat(sprintf("  [%3d/%3d] %-20s SKIP (no profile data)\n", idx, N_SAMPLE, pat))
    next
  }
  
  t_start <- Sys.time()
  llm_result <- call_llm_with_retry(system_prompt_template, payload)
  if (llm_result$success) {
    response <- llm_result$content
  } else {
    cat(sprintf("  [%3d/%3d] %-20s ERROR: %s\n", idx, N_SAMPLE, pat, llm_result$error))
    response <- NA_character_
  }
  
  t_end <- Sys.time()
  elapsed <- as.numeric(difftime(t_end, t_start, units="secs"))
  
  if (is.na(response) || is.null(response)) {
    results[idx,] <- list(pat, coh, FALSE, 0L, NA_integer_, elapsed)
    next
  }
  
  response <- gsub("(?s)^.*?</think>\\s*", "", response, perl=TRUE)
  response <- gsub("\\*", "", response, fixed=TRUE)
  
  n_viol <- check_ecological_fallacy(response, pat, "VALIDATION_STUDY")
  total_violations <- total_violations + n_viol
  
  results[idx,] <- list(pat, coh, TRUE, nchar(response), n_viol, elapsed)
  
  status <- if (n_viol > 0) sprintf("⚠ %d VIOLATIONS", n_viol) else "✓ clean"
  cat(sprintf("  [%3d/%3d] %-20s %-12s %s (%.1fs)\n", idx, N_SAMPLE, pat, coh, status, elapsed))
}

# ---------------------------------------------------------------------------
# 7. Summary
# ---------------------------------------------------------------------------
cat("\n[6/6] Generating summary...\n")

successful <- results[results$Success == TRUE, ]
n_success <- nrow(successful)
violation_rate <- if (n_success > 0) sum(successful$Violations, na.rm=TRUE) / n_success else 0
n_patients_with_viol <- sum(successful$Violations > 0, na.rm=TRUE)

summary_lines <- c(
  "============================================================",
  "CRIT-05 ECOLOGICAL FALLACY VALIDATION STUDY — RESULTS",
  "============================================================",
  sprintf("Date:              %s", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
  sprintf("LLM Model:         %s", LLM_MODEL),
  sprintf("Total sampled:     %d patients", N_SAMPLE),
  sprintf("Successful:        %d reports generated", n_success),
  sprintf("Failed/Skipped:    %d", N_SAMPLE - n_success),
  sprintf("Total violations:  %d", total_violations),
  sprintf("Patients with >=1: %d (%.1f%%)", n_patients_with_viol,
          if(n_success>0) 100*n_patients_with_viol/n_success else 0),
  sprintf("Violation rate:    %.3f per report", violation_rate),
  "",
  sprintf("CRIT-05 Assessment: %s",
    if(violation_rate < 0.02) "✅ PASS — Violation rate < 2%%. Ecological fallacy is theoretical."
    else if(violation_rate < 0.05) "⚠ BORDERLINE — Rate between 2%% and 5%%. Continue monitoring."
    else "❌ FAIL — Rate > 5%%. Implement step (3) auto-flagging."
  ),
  "============================================================"
)

cat(paste(summary_lines, collapse="\n"), "\n")

writeLines(summary_lines, "crit05_validation_summary.txt")
write.csv(results, "crit05_validation_results.csv", row.names=FALSE)

cat("\nOutput files:\n")
cat("  crit05_validation_summary.txt\n")
cat("  crit05_validation_results.csv\n")
cat("  ecological_fallacy_audit_log.csv  (if violations found)\n")
cat("\nDone at", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")

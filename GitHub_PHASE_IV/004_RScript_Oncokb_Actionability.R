# ==============================================================================
# ONCOKB CLINICAL ACTIONABILITY LAYER v2.0
# CancerRCDPredictor PHASE IV — Tier 0 Regulatory & Clinical Actionability Bridge
# ==============================================================================
#
# NINE-STRATA EVIDENCE SEPARATION ARCHITECTURE:
#
#   STRATUM 1  — STEMNESS METRICS (TSM)
#              Phenotypic: tumor stemness relative to cancer-type cohort
#              NEVER conflate with: gene function, immune status, drug actionability
#
#   STRATUM 2  — IMMUNE TOPOLOGY
#              Microenvironmental: immune compartment architecture, RCD-immune programs
#              NEVER conflate with: stemness, individual gene druggability, SHAP
#
#   STRATUM 3  — PROGNOSTIC SIGNATURES
#              Statistical: MVL prognostic weight of multi-omic RCD features
#              NEVER conflate with: SHAP importance, gene function, drug actionability
#
#   STRATUM 4  — SHAP EVIDENCE
#              Mathematical: XGBoost local feature importance driving prediction
#              NEVER conflate with: prognostic weight, biological causation
#
#   STRATUM 5  — CANCER GENE LIST (CGL)
#              Biological context: gene recognized across 7 curated cancer resources
#              NEVER conflate with: signature importance, SHAP, drug actionability
#
#   STRATUM 6  — TIER 0 — REGULATORY & CLINICAL ACTIONABILITY (THIS MODULE)
#              Regulatory/curatorial: FDA/OncoKB biomarker-drug recognition
#              NEVER conflate with: database interactions, treatment plans
#
#   STRATUM 7  — TIERS 1–5 — PHARMACOGENOMIC EVIDENCE
#              Database-reported: DGIdb/CIViC/OncoKB gene-drug interactions
#              NEVER conflate with: regulatory approval, biological truth
#
#   STRATUM 8  — TUMOR-STATE REASONING
#              Interpretive synthesis: integrates strata 1-7 into clinical hypothesis
#              NEVER conflate with: any individual evidence stratum
#
# TIER 0 SUB-LEVELS:
#   T0A — Exact Regulatory Match
#         Patient gene + alteration matches FDA drug label biomarker in same cancer type
#   T0B — Gene-Level Regulatory Match
#         Patient gene matches FDA biomarker gene, but alteration or cancer type differs
#   T0C — OncoKB Curated Association
#         Gene+alteration+cancer present in OncoKB TSV without matching FDA drug label
#
# ==============================================================================

if (!requireNamespace("readxl", quietly = TRUE)) {
  message("[Tier0 Layer] Installing readxl for Excel file support...")
  install.packages("readxl", repos = "http://cran.rstudio.com/")
}

# ==============================================================================
# SECTION 1: DATA LOADING
# ==============================================================================

load_oncokb_cancer_gene_list <- function(path = "oncoKB_cancerGeneList.tsv") {
  if (!file.exists(path)) { message("[Tier0] WARNING: Cancer Gene List not found: ", path); return(data.frame()) }
  tryCatch({
    cgl <- read.delim(path, sep = "\t", stringsAsFactors = FALSE, check.names = FALSE)
    names(cgl) <- trimws(names(cgl))
    message(sprintf("[Tier0] Loaded Cancer Gene List: %d genes, %d columns", nrow(cgl), ncol(cgl)))
    return(cgl)
  }, error = function(e) { message("[Tier0] ERROR: Cancer Gene List: ", e$message); return(data.frame()) })
}

load_biomarker_tsv <- function(path, label) {
  if (!file.exists(path)) { message("[Tier0] WARNING: ", label, " not found: ", path); return(data.frame()) }
  tryCatch({
    df <- read.delim(path, sep = "\t", stringsAsFactors = FALSE, check.names = FALSE)
    message(sprintf("[Tier0] Loaded %s: %d rows, %d genes, %d cancer types",
                    label, nrow(df), length(unique(df$Gene)),
                    length(unique(df[["Cancer Types"]]))))
    return(df)
  }, error = function(e) { message("[Tier0] ERROR: ", label, ": ", e$message); return(data.frame()) })
}

load_fda_excel <- function(path, label) {
  if (!file.exists(path)) { message("[Tier0] WARNING: ", label, " not found: ", path); return(data.frame()) }
  if (!requireNamespace("readxl", quietly = TRUE)) { message("[Tier0] WARNING: readxl not available"); return(data.frame()) }
  tryCatch({
    df <- as.data.frame(readxl::read_excel(path), stringsAsFactors = FALSE)
    colnames(df) <- trimws(colnames(df))
    message(sprintf("[Tier0] Loaded %s: %d drugs, %d columns", label, nrow(df), ncol(df)))
    return(df)
  }, error = function(e) { message("[Tier0] ERROR: ", label, ": ", e$message); return(data.frame()) })
}

# ==============================================================================
# SECTION 2: STRUCTURED FDA BIOMARKER PARSER
# ==============================================================================

#' Parse FDA biomarker label text into structured gene+alteration tokens
#'
#' FDA labels are free-text like "EGFR L858R, Exon 19 Deletions" or
#' "BRAF V600E/K" or "HER2+ (ERBB2 Amplification)". This parser extracts
#' gene symbols, alteration types, and relationships.
#'
#' @param biomarker_text Character vector of FDA biomarker label strings
#' @return data.frame with columns: raw_label, gene, alteration_class,
#'         alteration_detail, is_resistance_context
parse_fda_biomarker_labels <- function(biomarker_text) {
  if (length(biomarker_text) == 0 || all(is.na(biomarker_text))) return(data.frame())

  results <- data.frame(
    raw_label          = biomarker_text,
    gene               = NA_character_,
    alteration_class   = NA_character_,
    alteration_detail  = NA_character_,
    is_wildtype_context = FALSE,
    is_resistance_context = FALSE,
    is_fusion           = FALSE,
    stringsAsFactors    = FALSE
  )

  for (i in seq_along(biomarker_text)) {
    bm <- biomarker_text[i]
    if (is.na(bm) || bm == "") next

    bm_clean <- trimws(gsub("\\*|\\n|\\r", " ", bm))

    # --- Extract gene symbols ---
    # Known multi-gene fusion patterns
    if (grepl("BCR-ABL1|PML-RAR|NPM1-|COL1A1-PDGFB|FGFR3-TACC3|FGFR3-BAIAP2L1",
              bm_clean, ignore.case = TRUE)) {
      results$is_fusion[i] <- TRUE
      results$alteration_class[i] <- "FUSION"
      # Extract constituent genes from fusion
      fusion_genes <- regmatches(bm_clean,
        gregexpr("\\b(ABL1|BCR|PML|RARA|NPM1|COL1A1|PDGFB|FGFR[123]|TACC3|BAIAP2L1|ALK|ROS1|RET|NTRK[123]|NRG1|MET|BRAF|EGFR)\\b",
                 bm_clean, ignore.case = TRUE))[[1]]
      if (length(fusion_genes) > 0) {
        results$gene[i] <- paste(unique(toupper(fusion_genes)), collapse = ";")
      }
      results$alteration_detail[i] <- bm_clean
      next
    }

    # Extract standard HUGO gene symbols (2-8 uppercase letters/numbers)
    gene_candidates <- regmatches(bm_clean,
      gregexpr("\\b([A-Z][A-Z0-9]{1,7})\\b", bm_clean))[[1]]

    # Filter out non-gene tokens
    non_genes <- c("DNA", "RNA", "NGS", "PCR", "FISH", "IHC", "TMB", "MSI",
                   "MMR", "CDx", "FDA", "CPS", "TPS", "ITD", "TKD",
                   "Yes", "No", "Can", "The", "For", "And", "Del",
                   "WT", "ALL", "ITD", "TKD", "NOS", "CGC", "NOS",
                   "ER", "HR", "PR", "AR", "HER2", "HLA", "PD", "L1",
                   "CD19", "CD20", "CD22", "CD25", "CD30", "CD33",
                   "CLDN18", "PSMA", "MAGE", "SSTR")
    real_genes <- setdiff(gene_candidates, non_genes)

    if (length(real_genes) > 0) {
      results$gene[i] <- paste(unique(real_genes), collapse = ";")
    }

    # --- Classify alteration ---
    if (grepl("Fusion|Fusions|BCR-ABL|PML-RAR|Rearrangement", bm_clean, ignore.case = TRUE)) {
      results$alteration_class[i] <- "FUSION"
    } else if (grepl("Amplif|HER2\\+|Overexpress|ERBB2[^A-Za-z]", bm_clean, ignore.case = TRUE)) {
      results$alteration_class[i] <- "AMPLIFICATION"
    } else if (grepl("[A-Z]\\d+[A-Za-z*]|Exon.*[Dd]elet|ITD|TKD|G12C|V600|L858R|T790M|T315I",
                     bm_clean, perl = TRUE)) {
      results$alteration_class[i] <- "SPECIFIC_MUTATION"
    } else if (grepl("Oncogenic Mutations|Mutational|Missense|truncat|inactivat",
                     bm_clean, ignore.case = TRUE)) {
      results$alteration_class[i] <- "ONCOGENIC_BROAD"
    } else if (grepl("Wildtype|WT|wild.type", bm_clean, ignore.case = TRUE)) {
      results$alteration_class[i] <- "WILDTYPE"
      results$is_wildtype_context[i] <- TRUE
    } else if (grepl("ER\\+|HR\\+|HR\\+", bm_clean)) {
      results$alteration_class[i] <- "HORMONE_RECEPTOR"
    } else if (grepl("MSI|Microsatellite|dMMR|Instability", bm_clean, ignore.case = TRUE)) {
      results$alteration_class[i] <- "GENOMIC_INSTABILITY"
    } else if (grepl("PD-L1|PDL1", bm_clean, ignore.case = TRUE)) {
      results$alteration_class[i] <- "IMMUNE_CHECKPOINT"
    } else if (grepl("Methylat|MGMT", bm_clean, ignore.case = TRUE)) {
      results$alteration_class[i] <- "EPIGENETIC"
    } else if (grepl("del|Deletion|Loss|LOH", bm_clean, perl = TRUE)) {
      results$alteration_class[i] <- "COPY_NUMBER_LOSS"
    } else {
      results$alteration_class[i] <- "CELL_SURFACE"  # CD markers etc.
    }

    # --- Extract alteration detail (specific mutation / exon / etc.) ---
    detail_match <- regmatches(bm_clean,
      gregexpr("([A-Z]\\d+[A-Za-z*]/?[A-Za-z]*|Exon\\s*\\d+\\s*[Dd]eletions?|G12C|V600[EKM]|L858R|T790M|T315I|D816[VF]|ITD|TKD)",
               bm_clean, perl = TRUE))[[1]]
    if (length(detail_match) > 0) {
      results$alteration_detail[i] <- paste(unique(detail_match), collapse = ",")
    } else if (results$alteration_class[i] == "ONCOGENIC_BROAD") {
      results$alteration_detail[i] <- "Oncogenic Mutations"
    } else {
      results$alteration_detail[i] <- bm_clean
    }

    # --- Resistance context detection ---
    if (grepl("T790M|T315I|G1202R|L1196M|D816V|Y373C|S249C|gatekeeper|resist|escape",
              bm_clean, ignore.case = TRUE)) {
      results$is_resistance_context[i] <- TRUE
    }
  }

  return(results)
}

# ==============================================================================
# SECTION 3: TIER 0 — UNIFIED INITIALIZATION
# ==============================================================================

#' Initialize the complete Tier 0 Clinical Actionability Layer
#'
#' Loads all OncoKB resources, parses FDA biomarkers into structured tokens,
#' builds cross-reference indices, and constructs the biomarker-drug bridge.
#'
#' @param data_dir Directory containing all files
#' @return A Tier0ClinicalActionabilityLayer object (list)
init_oncokb_clinical_actionability_layer <- function(data_dir = ".") {
  old_wd <- getwd()
  if (data_dir != ".") setwd(data_dir)
  on.exit(setwd(old_wd))

  layer <- list()

  # --- Load All Datasets ---
  layer$cancer_gene_list <- load_oncokb_cancer_gene_list(
    file.path(data_dir, "oncoKB_cancerGeneList.tsv"))

  layer$biomarker_tsv <- load_biomarker_tsv(
    file.path(data_dir, "FDA_level_2_oncokb_biomarker_drug_associations.tsv"),
    "Biomarker TSV (Level 2)")

  layer$biomarker_tsv_l3 <- load_biomarker_tsv(
    file.path(data_dir, "FDA_level_3_oncokb_biomarker_drug_associations.tsv"),
    "Biomarker TSV (Level 3)")

  layer$fda_excel <- load_fda_excel(
    file.path(data_dir, "fda_approved_oncology_therapies.xlsx"),
    "FDA-Approved Oncology Therapies")

  # --- Parse FDA Biomarker Labels into Structured Tokens ---
  if (nrow(layer$fda_excel) > 0) {
    bm_col <- "FDA drug label listed biomarker(s) b"
    drug_col <- "FDA-approved drug(s) a"
    targeted_col <- "Targeted therapy"
    precision_col <- "Precision oncology therapy"
    mech_col <- "Mechanism of action or drug target c"

    has_bm <- !is.na(layer$fda_excel[[bm_col]]) & layer$fda_excel[[bm_col]] != ""

    layer$fda_biomarker_parsed <- parse_fda_biomarker_labels(
      layer$fda_excel[[bm_col]][has_bm])

    # Attach drug name and classification info
    layer$fda_biomarker_parsed$drug_name <- layer$fda_excel[[drug_col]][has_bm]
    layer$fda_biomarker_parsed$is_targeted <- layer$fda_excel[[targeted_col]][has_bm] == "Y"
    layer$fda_biomarker_parsed$is_precision <- layer$fda_excel[[precision_col]][has_bm] == "Y"
    layer$fda_biomarker_parsed$mechanism <- layer$fda_excel[[mech_col]][has_bm]

    message(sprintf("[Tier0] Parsed %d FDA biomarker labels into structured tokens",
                    sum(has_bm)))
  } else {
    layer$fda_biomarker_parsed <- data.frame()
  }

  # --- Build Indices ---
  layer$indices <- build_tier0_indices(layer)

  # --- Summary ---
  layer$summary <- summarize_tier0(layer)
  message("\n[Tier0] =============================================")
  message("[Tier0] Tier 0 Clinical Actionability Layer Initialized")
  message("[Tier0] =============================================")
  for (s in layer$summary) message(s)

  class(layer) <- c("Tier0ClinicalActionabilityLayer", "list")
  return(layer)
}

build_tier0_indices <- function(layer) {
  idx <- list()

  # --- Cancer Gene List index ---
  if (nrow(layer$cancer_gene_list) > 0) {
    cgl <- layer$cancer_gene_list
    hugo_col <- grep("Hugo.*Symbol", colnames(cgl), value = TRUE, ignore.case = TRUE)[1]
    if (is.na(hugo_col)) hugo_col <- colnames(cgl)[1]
    idx$all_cancer_genes <- unique(as.character(cgl[[hugo_col]]))
    idx$n_cancer_genes <- length(idx$all_cancer_genes)
  } else {
    idx$all_cancer_genes <- character(0)
    idx$n_cancer_genes <- 0
  }

  # --- Biomarker TSV index by gene (Levels 2 + 3 merged) ---
  ba_all <- data.frame()
  if (nrow(layer$biomarker_tsv) > 0) {
    ba_all <- rbind(ba_all, layer$biomarker_tsv)
  }
  if (!is.null(layer$biomarker_tsv_l3) && nrow(layer$biomarker_tsv_l3) > 0) {
    ba_all <- rbind(ba_all, layer$biomarker_tsv_l3)
  }
  if (nrow(ba_all) > 0) {
    idx$biomarker_by_gene <- split(seq_len(nrow(ba_all)),
      factor(ba_all$Gene, levels = unique(ba_all$Gene)))
    idx$biomarker_cancer_types <- unique(ba_all[["Cancer Types"]])
    idx$biomarker_all <- ba_all
  } else {
    idx$biomarker_by_gene <- list()
    idx$biomarker_cancer_types <- character(0)
    idx$biomarker_all <- data.frame()
  }

  # --- FDA parsed biomarker index by gene ---
  if (nrow(layer$fda_biomarker_parsed) > 0) {
    fbp <- layer$fda_biomarker_parsed
    # Index by each individual gene (semi-colon separated)
    gene_rows <- list()
    for (i in seq_len(nrow(fbp))) {
      genes <- unlist(strsplit(fbp$gene[i], ";"))
      for (g in genes) {
        g <- trimws(g)
        if (!is.na(g) && g != "") {
          gene_rows[[g]] <- c(gene_rows[[g]], i)
        }
      }
    }
    idx$fda_parsed_by_gene <- gene_rows
    idx$all_fda_biomarker_genes <- names(gene_rows)

    # Drug name index
    idx$fda_drug_names <- unique(fbp$drug_name)
    idx$n_fda_biomarker_drugs <- length(idx$fda_drug_names)
  } else {
    idx$fda_parsed_by_gene <- list()
    idx$all_fda_biomarker_genes <- character(0)
    idx$fda_drug_names <- character(0)
    idx$n_fda_biomarker_drugs <- 0
  }

  # --- OncoKB Gene Annotations (resistance) ---
  if (file.exists("OncoKB_Gene_Annotations.rds")) {
    tryCatch({
      oga <- readRDS("OncoKB_Gene_Annotations.rds")
      idx$oncoKB_gene_annotations <- oga
      idx$has_resistance_data <- "Highest_Resistance_Level" %in% colnames(oga)
      message(sprintf("[Tier0] Loaded OncoKB Gene Annotations: %d genes", nrow(oga)))
    }, error = function(e) {
      idx$oncoKB_gene_annotations <- data.frame()
      idx$has_resistance_data <- FALSE
    })
  } else {
    idx$oncoKB_gene_annotations <- data.frame()
    idx$has_resistance_data <- FALSE
  }

  return(idx)
}

summarize_tier0 <- function(layer) {
  s <- c()
  s <- c(s, sprintf("Cancer Gene List: %d genes", layer$indices$n_cancer_genes))
  s <- c(s, sprintf("Biomarker TSV: %d associations (L2+L3), %d genes, %d cancer types",
                     nrow(layer$indices$biomarker_all),
                     length(unique(layer$indices$biomarker_all$Gene)),
                     length(layer$indices$biomarker_cancer_types)))
  s <- c(s, sprintf("FDA Parsed Biomarkers: %d drug-biomarker pairs, %d unique genes, %d drugs",
                     nrow(layer$fda_biomarker_parsed),
                     length(layer$indices$all_fda_biomarker_genes),
                     layer$indices$n_fda_biomarker_drugs))
  s <- c(s, sprintf("Resistance Annotations: %s",
                     if (layer$indices$has_resistance_data) "available (OncoKB RDS)"
                     else "not available"))
  return(s)
}

# ==============================================================================
# SECTION 4: TIER 0 MATCHING ENGINE
# ==============================================================================

#' Compute Tier 0 clinical actionability for patient genes
#'
#' Matches patient genes against the FDA biomarker-drug bridge and OncoKB
#' biomarker TSV, classifying each match into T0A, T0B, or T0C.
#'
#' @param genes Patient gene symbols
#' @param layer Initialized Tier0 layer
#' @param cancer_type Optional patient cancer type for context matching
#' @param alterations Optional known alterations for the patient genes
#' @return A list with tier0_matches data.frame and summary
compute_tier0_actionability <- function(genes, layer, cancer_type = NULL, alterations = NULL) {
  if (length(genes) == 0) {
    return(list(tier0_matches = data.frame(), summary = "No genes provided.",
                n_t0a = 0, n_t0b = 0, n_t0c = 0))
  }

  idx <- layer$indices
  matches <- data.frame()

  for (g in unique(genes)) {
    g <- trimws(g)
    if (is.na(g) || g == "") next

    # --- Check FDA Parsed Biomarkers (T0A / T0B) ---
    fda_rows <- idx$fda_parsed_by_gene[[g]]
    if (!is.null(fda_rows)) {
      for (ri in fda_rows) {
        row <- layer$fda_biomarker_parsed[ri, ]
        if (is.na(row$gene)) next

        sub_level <- "T0B"  # default: gene-level match

        # Check for cancer type match → T0A candidate
        oncokb_lvl <- "FDA Label"  # default: FDA label without specific OncoKB level
        if (!is.null(cancer_type) && nrow(idx$biomarker_all) > 0) {
          bm_rows <- idx$biomarker_all[
            idx$biomarker_all$Gene == g, , drop = FALSE]
          if (nrow(bm_rows) > 0) {
            ct_match <- grepl(cancer_type, bm_rows[["Cancer Types"]],
                              ignore.case = TRUE)
            if (any(ct_match)) {
              sub_level <- "T0A"
              # Capture the OncoKB level(s) that matched, human-readable
              matched_levels <- unique(bm_rows$Level[ct_match])
              matched_levels <- gsub("Fda2", "FDA Level 2", matched_levels, fixed = TRUE)
              matched_levels <- gsub("Fda3", "FDA Level 3", matched_levels, fixed = TRUE)
              oncokb_lvl <- paste(matched_levels, collapse = " + ")
            }
          }
        }

        # Check for alteration match → strengthens to T0A
        if (!is.null(alterations) && !is.null(alterations[[g]])) {
          alt_match <- any(sapply(alterations[[g]], function(a) {
            grepl(a, row$alteration_detail, ignore.case = TRUE) ||
            grepl(a, row$raw_label, ignore.case = TRUE)
          }))
          if (alt_match) sub_level <- "T0A"
        }

        matches <- rbind(matches, data.frame(
          gene_symbol         = g,
          sub_level           = sub_level,
          drug_name           = row$drug_name,
          biomarker_label     = row$raw_label,
          alteration_class    = row$alteration_class,
          alteration_detail   = row$alteration_detail,
          is_targeted         = row$is_targeted,
          is_precision        = row$is_precision,
          mechanism           = row$mechanism,
          match_type          = "FDA_LABEL",
          is_resistance_context = row$is_resistance_context,
          oncokb_level        = oncokb_lvl,
          stringsAsFactors    = FALSE
        ))
      }
    }

    # --- Check OncoKB Biomarker TSV (T0C) ---
    bm_rows <- idx$biomarker_by_gene[[g]]
    if (!is.null(bm_rows)) {
      for (ri in bm_rows) {
        row <- idx$biomarker_all[ri, ]
        # Only count as T0C if no FDA drug label match already exists for this gene
        fda_drugs_for_gene <- unique(matches$drug_name[matches$gene_symbol == g])
        if (length(fda_drugs_for_gene) == 0) {
          oncokb_lvl <- if (!is.null(row$Level) && !is.na(row$Level)) {
            lvl <- row$Level
            lvl <- gsub("Fda2", "FDA Level 2", lvl, fixed = TRUE)
            lvl <- gsub("Fda3", "FDA Level 3", lvl, fixed = TRUE)
            lvl
          } else "OncoKB"
          matches <- rbind(matches, data.frame(
            gene_symbol         = g,
            sub_level           = "T0C",
            drug_name           = NA_character_,
            biomarker_label     = paste0("[", oncokb_lvl, "] ", row$Alterations, " in ", row[["Cancer Types"]]),
            alteration_class    = row$Alterations,
            alteration_detail   = row$Alterations,
            is_targeted         = NA,
            is_precision        = NA,
            mechanism           = NA_character_,
            match_type          = "ONCOKB_TSV",
            is_resistance_context = FALSE,
            oncokb_level        = oncokb_lvl,
            stringsAsFactors    = FALSE
          ))
        }
      }
    }
  }

  # Deduplicate
  if (nrow(matches) > 0) {
    matches <- matches[!duplicated(matches[, c("gene_symbol", "drug_name", "sub_level")]), ]
  }

  n_t0a <- sum(matches$sub_level == "T0A")
  n_t0b <- sum(matches$sub_level == "T0B")
  n_t0c <- sum(matches$sub_level == "T0C")

  genes_with_t0 <- unique(matches$gene_symbol)
  genes_without <- setdiff(genes, genes_with_t0)

  summary <- sprintf(
    "Tier 0 Actionability: %d/%d genes have regulatory/curatorial evidence. T0A=%d (exact match), T0B=%d (gene-level), T0C=%d (OncoKB curated). Genes without Tier 0 evidence: %s.",
    length(genes_with_t0), length(genes),
    n_t0a, n_t0b, n_t0c,
    if (length(genes_without) > 0) paste(genes_without, collapse = ", ") else "None"
  )

  return(list(
    tier0_matches = matches,
    summary       = summary,
    n_t0a         = n_t0a,
    n_t0b         = n_t0b,
    n_t0c         = n_t0c,
    genes_with_t0 = genes_with_t0,
    genes_without  = genes_without
  ))
}

# ==============================================================================
# SECTION 5: TIER 0 NARRATIVE GENERATION (LLM INJECTION)
# ==============================================================================

#' Build Tier 0 narrative block for LLM prompt injection
#'
#' Generates structured, governance-fenced text presenting Tier 0 evidence
#' with T0A/T0B/T0C sub-level differentiation and explicit separation from
#' Cancer Gene List (Stratum 5) and Tiers 1-5 (Stratum 7).
#'
#' @param tier0_result Result from compute_tier0_actionability()
#' @param patient_genes Original patient gene list
#' @param cancer_type Patient cancer type (for context)
#' @return Character string formatted for LLM system/user prompt injection
build_tier0_narrative_block <- function(tier0_result, patient_genes, cancer_type = NULL) {
  lines <- c(
    "═══════════════════════════════════════════════════════════════════════",
    "  TIER 0 — REGULATORY & CLINICAL ACTIONABILITY KNOWLEDGE (OncoKB/FDA)",
    "═══════════════════════════════════════════════════════════════════════",
    "",
    "STRATUM 6 of 8 — This is REGULATORY/CURATORIAL evidence, distinct from:",
    "  • Cancer Gene List (Stratum 5): biological cancer relevance",
    "  • Pharmacogenomic Tiers 1-5 (Stratum 7): database-reported interactions",
    "  • SHAP Evidence (Stratum 4): mathematical feature importance",
    "  • Tumor-State Reasoning (Stratum 8): interpretive synthesis",
    "",
    sprintf("TIER 0 SUMMARY: %s", tier0_result$summary),
    ""
  )

  if (nrow(tier0_result$tier0_matches) == 0) {
    lines <- c(lines,
      "NO Tier 0 evidence found for patient genes.",
      "This means none of the patient genes have:",
      "  • FDA drug label-listed biomarker recognition",
      "  • OncoKB-curated biomarker-drug associations in the sensitivity TSV",
      "",
      "GOVERNANCE: The absence of Tier 0 evidence does NOT mean these genes",
      "are not clinically relevant. It means the regulatory/curatorial layer",
      "does not currently recognize a biomarker-drug relationship.",
      "Cancer Gene List status (Stratum 5) and pharmacogenomic evidence",
      "(Stratum 7) are separate dimensions and remain valid.")
    return(paste(lines, collapse = "\n"))
  }

  # --- T0A: Exact Regulatory Matches ---
  t0a <- tier0_result$tier0_matches[tier0_result$tier0_matches$sub_level == "T0A", ]
  if (nrow(t0a) > 0) {
    lines <- c(lines, "",
      "━━━ T0A — EXACT REGULATORY MATCH ━━━",
      "FDA drug label biomarker matches the patient gene ± alteration ± cancer type.",
      "This is the strongest Tier 0 evidence. The FDA recognizes this gene/alteration",
      "as a biomarker for the listed drug.",
      "")
    for (i in seq_len(nrow(t0a))) {
      row <- t0a[i, ]
      lvl_info <- if (!is.null(row$oncokb_level) && !is.na(row$oncokb_level) &&
                       row$oncokb_level != "FDA Label")
        paste0(" [", row$oncokb_level, "]") else ""
      lines <- c(lines,
        sprintf("  Gene: %s%s", row$gene_symbol, lvl_info),
        sprintf("  FDA-Labeled Drug: %s", row$drug_name),
        sprintf("  Biomarker Label: %s", row$biomarker_label),
        sprintf("  Alteration Class: %s", row$alteration_class),
        sprintf("  FDA Classification: %s%s",
                if (row$is_targeted) "Targeted Therapy" else "",
                if (row$is_precision) " | Precision Oncology" else ""),
        sprintf("  Mechanism: %s", substr(row$mechanism, 1, 120)),
        if (row$is_resistance_context)
          "  ⚠ CONTEXT NOTE: This biomarker may be in a resistance context (e.g., T790M, T315I)"
        else "",
        "")
    }
  }

  # --- T0B: Gene-Level Regulatory Matches ---
  t0b <- tier0_result$tier0_matches[tier0_result$tier0_matches$sub_level == "T0B", ]
  if (nrow(t0b) > 0) {
    lines <- c(lines, "",
      "━━━ T0B — GENE-LEVEL REGULATORY MATCH ━━━",
      "Patient gene matches an FDA biomarker gene, but the alteration type or",
      "cancer context differs from the FDA label specification.",
      "This represents a gene-level regulatory recognition that requires",
      "qualification regarding alteration and disease context.",
      "")
    for (i in seq_len(nrow(t0b))) {
      row <- t0b[i, ]
      lines <- c(lines,
        sprintf("  Gene: %s", row$gene_symbol),
        sprintf("  FDA-Labeled Drug: %s", row$drug_name),
        sprintf("  FDA Biomarker Label: %s", row$biomarker_label),
        sprintf("  Alteration Class: %s", row$alteration_class),
        if (!is.null(cancer_type))
          sprintf("  CONTEXT: Patient cancer type (%s) may differ from FDA label indication",
                  cancer_type)
        else "",
        "")
    }
  }

  # --- T0C: OncoKB Curated Associations ---
  t0c <- tier0_result$tier0_matches[tier0_result$tier0_matches$sub_level == "T0C", ]
  if (nrow(t0c) > 0) {
    lines <- c(lines, "",
      "━━━ T0C — ONCOKB CURATED ASSOCIATION ━━━",
      "Gene-alteration-cancer association present in the OncoKB biomarker TSV",
      "but without an explicit FDA drug label match.",
      "Each entry is annotated with its OncoKB evidence level:",
      "  FDA Level 2 (Fda2) = Cancer Mutations with Evidence of Clinical Significance",
      "  FDA Level 3 (Fda3) = Cancer Mutations with Potential of Clinical Significance",
      "This is the most preliminary Tier 0 evidence — curated clinical",
      "actionability without direct regulatory drug labeling.",
      "")
    for (i in seq_len(nrow(t0c))) {
      row <- t0c[i, ]
      lvl_info <- if (!is.null(row$oncokb_level) && !is.na(row$oncokb_level) && row$oncokb_level != "")
        paste0(" [", row$oncokb_level, "]") else ""
      lines <- c(lines,
        sprintf("  Gene: %s%s", row$gene_symbol, lvl_info),
        sprintf("  OncoKB Association: %s", row$biomarker_label),
        "")
    }
  }

  # --- Tier 0 Governance Fence ---
  lines <- c(lines, "",
    "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━",
    "  TIER 0 GOVERNANCE — MANDATORY SEPARATION RULES",
    "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━",
    "",
    "1. Tier 0 evidence is REGULATORY/CURATORIAL recognition, NOT a treatment",
    "   recommendation. FDA label = the FDA recognizes this biomarker. It does",
    "   NOT prescribe therapy for this specific patient.",
    "",
    "2. T0A (exact match) = strongest regulatory alignment. T0B (gene-level) =",
    "   partial alignment requiring context qualification. T0C (curated) =",
    "   clinical actionability without FDA drug labeling.",
    "",
    "3. Tier 0 MUST NOT be conflated with:",
    "   • Cancer Gene List (Stratum 5): biological cancer relevance",
    "   • Pharmacogenomic Tiers 1-5 (Stratum 7): database-reported interactions",
    "   • SHAP Evidence (Stratum 4): mathematical feature importance",
    "",
    "4. When discussing Tier 0 evidence in your narrative:",
    "   • Present regulatory recognition as a SEPARATE evidence dimension",
    "   • Use language: 'The FDA recognizes [gene] as a biomarker for [drug] in",
    "     [indication]' NOT '[drug] is indicated for this patient'",
    "   • For T0B: qualify with 'this regulatory recognition is in a different",
    "     alteration/cancer context and requires further validation'",
    "   • For T0C: clearly state 'this OncoKB-curated association lacks explicit",
    "     FDA drug labeling'",
    "",
    "5. The nine-strata architecture MUST be preserved in your response:",
    "   stemness, immune topology, prognostic signatures, SHAP, cancer gene list,",
    "   Tier 0, Tiers 1-5, and tumor-state reasoning are DISTINCT DIMENSIONS.",
    "   Do not collapse or conflate any of them."
  )

  return(paste(lines, collapse = "\n"))
}

# ==============================================================================
# SECTION 6: NINE-STRATA GOVERNANCE BLOCK
# ==============================================================================

#' Generate the full nine-strata governance block for LLM system prompts
#'
#' @return Character string for injection into LLM system prompt
build_nine_strata_governance_block <- function() {
  return(paste(c(
    "--- NINE-STRATA EVIDENCE SEPARATION GOVERNANCE v2.0 ---",
    "",
    "CancerRCDPredictor operates across NINE DISTINCT EVIDENCE STRATA.",
    "Each stratum answers a different question. You MUST NOT conflate them.",
    "",
    "STRATUM 1 — STEMNESS METRICS (TSM)",
    "  Question: What is this tumor's stemness state relative to its cohort?",
    "  Evidence type: PHENOTYPIC (Low/Intermediate/High)",
    "  NEVER conflate with: gene function, immune status, drug actionability",
    "",
    "STRATUM 2 — IMMUNE TOPOLOGY",
    "  Question: What is the architecture of the tumor immune compartment?",
    "  Evidence type: MICROENVIRONMENTAL (hot/cold/excluded, RCD-immune programs)",
    "  NEVER conflate with: stemness, individual gene druggability, SHAP",
    "",
    "STRATUM 3 — PROGNOSTIC SIGNATURES",
    "  Question: What prognostic weight does each RCD signature carry?",
    "  Evidence type: STATISTICAL (MVL ensemble weights)",
    "  NEVER conflate with: SHAP importance, gene function, drug actionability",
    "",
    "STRATUM 4 — SHAP EVIDENCE",
    "  Question: Which features drive the XGBoost model's local decision?",
    "  Evidence type: MATHEMATICAL (TreeSHAP feature importance)",
    "  NEVER conflate with: prognostic weight, biological causation",
    "",
    "STRATUM 5 — CANCER GENE LIST (CGL)",
    "  Question: Is this gene recognized as cancer-relevant across 7 resources?",
    "  Evidence type: BIOLOGICAL CONTEXT (membership flags)",
    "  NEVER conflate with: SHAP importance, druggability, FDA status",
    "",
    "STRATUM 6 — TIER 0 (REGULATORY & CLINICAL ACTIONABILITY)",
    "  Question: Does the FDA/OncoKB recognize a biomarker-drug relationship?",
    "  Evidence type: REGULATORY/CURATORIAL (T0A/T0B/T0C)",
    "  NEVER conflate with: database interactions, treatment recommendations",
    "",
    "STRATUM 7 — TIERS 1-5 (PHARMACOGENOMIC EVIDENCE)",
    "  Question: What gene-drug interactions do DGIdb/CIViC/OncoKB report?",
    "  Evidence type: DATABASE-REPORTED (evidence-tiered 1-5)",
    "  NEVER conflate with: regulatory approval, biological truth",
    "",
    "STRATUM 8 — TUMOR-STATE REASONING",
    "  Question: What tumor state do strata 1-7 collectively suggest?",
    "  Evidence type: INTERPRETIVE SYNTHESIS (stable/adaptive/plastic/equilibrium)",
    "  NEVER conflate with: any individual evidence stratum",
    "",
    "MANDATORY NARRATIVE RULES:",
    "1. When a gene has evidence in MULTIPLE strata, present each separately.",
    "2. 'EGFR is a cancer gene + has Tier 0 evidence + appears in DGIdb' is THREE",
    "   independent findings reported through three distinct evidential lenses.",
    "3. Never chain strata into a single claim of clinical actionability.",
    "4. Each stratum carries its own uncertainty. High SHAP importance does not",
    "   validate cancer gene status. Tier 0 does not validate DGIdb interactions.",
    "5. The absence of evidence in one stratum does not negate evidence in another.",
    "",
    "END NINE-STRATA GOVERNANCE v2.0"
  ), collapse = "\n"))
}

# ==============================================================================
# SECTION 7: PATIENT QUERY INTERFACE (FULL INTEGRATION)
# ==============================================================================

#' Full patient actionability query across all Tier 0 layers
#'
#' @param genes Patient gene symbols
#' @param layer Initialized Tier0 layer
#' @param cancer_type Optional cancer type context
#' @param drug_names Optional drug names from pharmacogenomic matrix
#' @param alterations Optional named list of known alterations per gene
#' @return Comprehensive Tier 0 actionability report
query_patient_actionability <- function(genes, layer,
                                        cancer_type = NULL,
                                        drug_names = NULL,
                                        alterations = NULL) {
  if (length(genes) == 0) {
    return(list(status = "empty", message = "No genes provided."))
  }

  genes <- unique(genes[!is.na(genes) & genes != ""])

  result <- list(
    status      = "ok",
    genes       = genes,
    n_genes     = length(genes),
    cancer_type = cancer_type,
    timestamp   = Sys.time()
  )

  # --- Tier 0 Actionability ---
  result$tier0 <- compute_tier0_actionability(genes, layer, cancer_type, alterations)

  # --- Tier 0 Narrative Block ---
  result$tier0_narrative <- build_tier0_narrative_block(result$tier0, genes, cancer_type)

  # --- Cancer Gene List annotation (Stratum 5) ---
  cgl <- layer$cancer_gene_list
  if (nrow(cgl) > 0) {
    hugo_col <- grep("Hugo.*Symbol", colnames(cgl), value = TRUE, ignore.case = TRUE)[1]
    if (is.na(hugo_col)) hugo_col <- colnames(cgl)[1]

    recognized <- intersect(genes, layer$indices$all_cancer_genes)
    not_recognized <- setdiff(genes, layer$indices$all_cancer_genes)

    result$cancer_gene_status <- list(
      n_recognized     = length(recognized),
      n_not_recognized = length(not_recognized),
      recognized        = recognized,
      not_recognized    = not_recognized,
      details           = list()
    )

    if (length(recognized) > 0) {
      matched <- cgl[cgl[[hugo_col]] %in% recognized, , drop = FALSE]
      for (i in seq_len(nrow(matched))) {
        row <- matched[i, ]
        sym <- as.character(row[[hugo_col]])
        memberships <- c()
        for (rc in c("OncoKB Annotated", "MSK-IMPACT", "MSK-HEME",
                     "FOUNDATION ONE", "FOUNDATION ONE HEME",
                     "Vogelstein", "COSMIC CGC (v99)")) {
          if (rc %in% colnames(row) && !is.na(row[[rc]]) &&
              tolower(as.character(row[[rc]])) == "yes") {
            memberships <- c(memberships, rc)
          }
        }
        gt <- if ("Gene Type" %in% colnames(row)) as.character(row[["Gene Type"]]) else ""
        occ_col <- grep("occurrence|# of occurrence", colnames(row),
                        value = TRUE, ignore.case = TRUE)
        occ <- if (length(occ_col) > 0 && !is.na(row[[occ_col[1]]]))
          as.integer(row[[occ_col[1]]]) else NA_integer_

        result$cancer_gene_status$details[[sym]] <- list(
          gene_type    = gt,
          memberships  = memberships,
          n_resources  = length(memberships),
          occ_score    = occ
        )
      }
    }
  }

  # --- Resistance Awareness (from OncoKB RDS, if available) ---
  result$resistance <- list(available = FALSE, genes = character(0))
  if (layer$indices$has_resistance_data) {
    oga <- layer$indices$oncoKB_gene_annotations
    if ("Highest_Resistance_Level" %in% colnames(oga)) {
      resist_genes <- oga$Gene_Symbol[
        !is.na(oga$Highest_Resistance_Level) &
        oga$Highest_Resistance_Level != "" &
        oga$Gene_Symbol %in% genes]
      if (length(resist_genes) > 0) {
        result$resistance$available <- TRUE
        result$resistance$genes <- resist_genes
        result$resistance$details <- oga[
          oga$Gene_Symbol %in% resist_genes,
          c("Gene_Symbol", "Highest_Sensitive_Level", "Highest_Resistance_Level")]
      }
    }
  }

  # --- Nine-Strata Governance Block ---
  result$nine_strata_governance <- build_nine_strata_governance_block()

  return(result)
}

# ==============================================================================
# SECTION 8: MOLECULAR TUMOR BOARD SUMMARY
# ==============================================================================

generate_molecular_tumor_board_summary <- function(patient_result, format = "text") {
  if (patient_result$status == "empty") return("No genes provided.")

  lines <- c(
    "====================================================================",
    "  ONCOKB CLINICAL ACTIONABILITY — TIER 0 MOLECULAR TUMOR BOARD BRIEF",
    "====================================================================",
    ""
  )

  # Tier 0 summary
  t0 <- patient_result$tier0
  lines <- c(lines,
    sprintf("TIER 0 — REGULATORY & CLINICAL ACTIONABILITY"),
    sprintf("  T0A (Exact regulatory match):  %d", t0$n_t0a),
    sprintf("  T0B (Gene-level regulatory):    %d", t0$n_t0b),
    sprintf("  T0C (OncoKB curated):           %d", t0$n_t0c),
    sprintf("  Genes without Tier 0 evidence:   %d", length(t0$genes_without)),
    ""
  )

  # T0A matches
  t0a <- t0$tier0_matches[t0$tier0_matches$sub_level == "T0A", ]
  if (nrow(t0a) > 0) {
    lines <- c(lines, "T0A — EXACT REGULATORY MATCHES:")
    for (i in seq_len(nrow(t0a))) {
      lines <- c(lines, sprintf("  %s → %s (%s)",
        t0a$gene_symbol[i], t0a$drug_name[i], t0a$alteration_class[i]))
    }
    lines <- c(lines, "")
  }

  # T0B matches
  t0b <- t0$tier0_matches[t0$tier0_matches$sub_level == "T0B", ]
  if (nrow(t0b) > 0) {
    lines <- c(lines, "T0B — GENE-LEVEL REGULATORY MATCHES:")
    for (i in seq_len(nrow(t0b))) {
      lines <- c(lines, sprintf("  %s → %s (partial match: %s)",
        t0b$gene_symbol[i], t0b$drug_name[i], t0b$alteration_class[i]))
    }
    lines <- c(lines, "")
  }

  # Cancer Gene List summary
  if (!is.null(patient_result$cancer_gene_status)) {
    cgs <- patient_result$cancer_gene_status
    lines <- c(lines,
      sprintf("CANCER GENE LIST (Stratum 5 — Biological Context):"),
      sprintf("  Recognized: %d/%d genes", cgs$n_recognized, patient_result$n_genes),
      sprintf("  NOT recognized: %s",
        paste(cgs$not_recognized, collapse = ", ")),
      "")
  }

  # Resistance
  if (patient_result$resistance$available) {
    lines <- c(lines,
      sprintf("RESISTANCE AWARENESS: %d genes with resistance annotations",
              length(patient_result$resistance$genes)),
      sprintf("  %s", paste(patient_result$resistance$genes, collapse = ", ")),
      "")
  }

  lines <- c(lines,
    "GOVERNANCE: Tier 0 is regulatory/curatorial recognition, NOT treatment",
    "recommendation. Cancer Gene List is biological context, NOT clinical",
    "actionability. Each evidence stratum (1-8) must be interpreted independently.")

  result <- paste(lines, collapse = "\n")

  if (format == "html") {
    result <- gsub("T0A", "<span style='color:#22c55e'><strong>T0A</strong></span>", result)
    result <- gsub("T0B", "<span style='color:#fbbf24'><strong>T0B</strong></span>", result)
    result <- gsub("T0C", "<span style='color:#f87171'><strong>T0C</strong></span>", result)
    result <- gsub("Stratum [0-9]", "<span style='color:#94a3b8'>\\0</span>", result)
    result <- gsub("\n", "<br>", result)
  }

  return(result)
}

# ==============================================================================
# END OF TIER 0 CLINICAL ACTIONABILITY LAYER v2.0
# ==============================================================================
message("[Tier0] Module v2.0 loaded. Call init_oncokb_clinical_actionability_layer() to initialize.")

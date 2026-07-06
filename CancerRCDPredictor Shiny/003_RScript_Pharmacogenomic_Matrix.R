# ==============================================================================
# UNIFIED ONCOLOGY PHARMACOGENOMIC MATRIX GENERATOR v3.0
# ==============================================================================
# Evidence-tiered: OncoKB (Level 1-4) + CIViC (Level A-E) + DGIdb
# Outputs Unified_Drug_Matrix.rds with columns:
#   Gene_Symbol, Drug_Name, Interaction_Type, Source_Database,
#   Clinical_Status, Evidence_Tier, Cancer_Type_Context
# ==============================================================================

# Required Libraries
if(!require("dplyr")) install.packages("dplyr")
if(!require("stringr")) install.packages("stringr")
if(!require("httr"))    install.packages("httr")
if(!require("jsonlite")) install.packages("jsonlite")
if(!require("readr"))    install.packages("readr")

library(dplyr)
library(stringr)
library(httr)
library(jsonlite)
library(readr)

# ==============================================================================
# CONFIGURATION
# ==============================================================================
if (file.exists(".Renviron")) readRenviron(".Renviron")
ONCOKB_TOKEN <- Sys.getenv("ONCOKB_TOKEN", unset = "")

if (ONCOKB_TOKEN == "" || ONCOKB_TOKEN == "YOUR_ONCOKB_TOKEN_HERE") {
  message("NOTE: No OncoKB token found. Set ONCOKB_TOKEN in .Renviron or environment.")
  message("      OncoKB will be skipped. Only DGIdb + CIViC will be used.")
  ONCOKB_TOKEN <- ""
}

# ==============================================================================
# STEP 1: EXTRACT YARDSTICK GENES FROM TABLE S11
# ==============================================================================
message("Step 1: Extracting Yardstick Genes from Table S11...")
s11_file <- "Table_S11_Interpreter_12k.csv"
if(!file.exists(s11_file)) {
  stop("Table_S11_Interpreter_12k.csv not found in the current directory.")
}

s11 <- read_csv(s11_file, show_col_types = FALSE)

all_genes_raw <- str_extract_all(s11$`Decoded Genetic Element`, "(?<=\\()[A-Za-z0-9\\-]+(?=\\()")
unique_genes <- unique(unlist(all_genes_raw))
unique_genes <- unique_genes[!is.na(unique_genes) & unique_genes != ""]

message(sprintf("Extracted %d unique genetic targets from Table S11.", length(unique_genes)))

# ==============================================================================
# STEP 2: DGIdb (v4.0+ TSV format: gene_name, drug_name, interaction_type,
#          interaction_source_db_name, interaction_score, approved, etc.)
# ==============================================================================
message("Step 2: Processing DGIdb...")

dgidb_filtered <- data.frame()

tryCatch({
  dgidb_url  <- "https://dgidb.org/data/latest/interactions.tsv"
  dgidb_file <- "dgidb_interactions_temp.tsv"
  download.file(dgidb_url, destfile = dgidb_file, mode = "wb", quiet = TRUE)
  
  # DGIdb v4+ columns: gene_name, drug_name, interaction_type,
  #   interaction_source_db_name, interaction_score, approved, immunotherapy, etc.
  dgidb_raw <- read_tsv(dgidb_file, show_col_types = FALSE)
  
  # Filter to yardstick genes and remove NULL/unknown interactions
  dgidb_work <- dgidb_raw %>%
    filter(gene_name %in% unique_genes) %>%
    filter(!is.na(drug_name) & drug_name != "" & drug_name != "NULL") %>%
    filter(!is.na(interaction_type) & interaction_type != "NULL" & 
           interaction_type != "other/unknown" & interaction_type != "") %>%
    # Coerce approved/anti_neoplastic to logical safely
    mutate(
      approved = case_when(
        tolower(as.character(approved)) == "true"  ~ TRUE,
        tolower(as.character(approved)) == "false" ~ FALSE,
        TRUE ~ FALSE
      ),
      anti_neoplastic = case_when(
        tolower(as.character(anti_neoplastic)) == "true"  ~ TRUE,
        tolower(as.character(anti_neoplastic)) == "false" ~ FALSE,
        TRUE ~ FALSE
      )
    )
  
  # Count distinct source databases per gene-drug pair as proxy for multi-source evidence
  dgidb_work <- dgidb_work %>%
    group_by(gene_name, drug_name) %>%
    summarise(
      n_sources    = n_distinct(interaction_source_db_name, na.rm = TRUE),
      interaction_types = paste(unique(interaction_type), collapse = "; "),
      approved_any = any(approved, na.rm = TRUE),
      anti_neoplastic_any = any(anti_neoplastic, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      Evidence_Tier = case_when(
        approved_any & n_sources >= 2 ~ 3,
        n_sources >= 3                  ~ 3,
        n_sources >= 2                  ~ 4,
        n_sources >= 1                  ~ 5,
        TRUE                            ~ 5
      ),
      Evidence_Label = case_when(
        Evidence_Tier == 3 ~ "DGIdb_MultiSource",
        Evidence_Tier == 4 ~ "DGIdb_MultiSource",
        TRUE               ~ "DGIdb_SingleSource"
      ),
      Clinical_Status = paste0(
        "DGIdb | Sources:", n_sources,
        ifelse(approved_any, " | Approved", ""),
        ifelse(anti_neoplastic_any, " | Antineoplastic", "")
      ),
      Cancer_Type_Context = "Not annotated (DGIdb)",
      Source_Database = "DGIdb"
    )
  
  dgidb_filtered <- dgidb_work %>%
    select(
      Gene_Symbol        = gene_name,
      Drug_Name          = drug_name,
      Interaction_Type   = interaction_types,
      Source_Database,
      Clinical_Status,
      Evidence_Tier,
      Cancer_Type_Context
    ) %>%
    distinct()
  
  message(sprintf("DGIdb: %d interactions for %d yardstick genes.",
                  nrow(dgidb_filtered),
                  length(unique(dgidb_filtered$Gene_Symbol))))
  unlink(dgidb_file)
}, error = function(e) {
  message("DGIdb download/processing failed: ", e$message)
  dgidb_filtered <<- data.frame()
})

# ==============================================================================
# STEP 3: CIViC (with evidence-level-based tiering A-E)
# ==============================================================================
message("Step 3: Processing CIViC Nightly Dump...")

civic_filtered <- data.frame()

tryCatch({
  civic_url  <- "https://civicdb.org/downloads/nightly/nightly-ClinicalEvidenceSummaries.tsv"
  civic_file <- "civic_nightly_temp.tsv"
  download.file(civic_url, destfile = civic_file, mode = "wb", quiet = TRUE)
  
  # Use read_tsv with quoting and col type adjustments for known parsing quirks
  civic_raw <- suppressWarnings(
    read_tsv(civic_file, show_col_types = FALSE,
             col_types = cols(citation_id = col_character()))
  )
  
  # Extract gene symbol from molecular_profile (e.g., "JAK2 V617F" -> "JAK2")
  civic_work <- civic_raw %>%
    mutate(Gene_Symbol = word(molecular_profile, 1)) %>%
    filter(Gene_Symbol %in% unique_genes) %>%
    filter(!is.na(therapies) & therapies != "" & therapies != "NULL")
  
  # Map CIViC evidence_level (A-E) to numeric tier
  civic_work <- civic_work %>%
    mutate(
      Evidence_Tier = case_when(
        evidence_level == "A" ~ 2,
        evidence_level == "B" ~ 3,
        evidence_level == "C" ~ 4,
        evidence_level == "D" ~ 5,
        evidence_level == "E" ~ 5,
        TRUE                  ~ 5
      ),
      Evidence_Label = paste0("CIViC_Level_", evidence_level),
      Clinical_Status = paste0(
        "CIViC | Level:", evidence_level,
        " | Direction:", ifelse(is.na(evidence_direction), "N/A", evidence_direction),
        " | Sig:", ifelse(is.na(significance), "N/A", significance)
      ),
      Cancer_Type_Context = ifelse(
        !is.na(disease) & disease != "",
        paste0("CIViC annotated: ", disease),
        "Not annotated (CIViC)"
      ),
      Interaction_Type = ifelse(
        !is.na(evidence_direction) & evidence_direction != "",
        evidence_direction,
        "N/A"
      )
    )
  
  civic_filtered <- civic_work %>%
    mutate(Source_Database = "CIViC") %>%
    select(
      Gene_Symbol,
      Drug_Name          = therapies,
      Interaction_Type,
      Source_Database,
      Clinical_Status,
      Evidence_Tier,
      Cancer_Type_Context
    ) %>%
    distinct()
  
  message(sprintf("CIViC: %d interactions for %d yardstick genes.",
                  nrow(civic_filtered),
                  length(unique(civic_filtered$Gene_Symbol))))
  unlink(civic_file)
}, error = function(e) {
  message("CIViC download/processing failed: ", e$message)
  civic_filtered <<- data.frame()
})

# ==============================================================================
# STEP 4: OncoKB API (Level 1-4, R1-R2)
# ==============================================================================
message("Step 4: Processing OncoKB API...")

oncokb_filtered <- data.frame()

if (ONCOKB_TOKEN == "") {
  message("OncoKB skipped (no token).")
} else {
  tryCatch({
    level_to_tier <- c(
      "LEVEL_1"  = 1,
      "LEVEL_2"  = 2,
      "LEVEL_3A" = 3,
      "LEVEL_3B" = 3,
      "LEVEL_4"  = 4,
      "LEVEL_R1" = 1,
      "LEVEL_R2" = 2
    )
    
    level_labels <- c(
      "LEVEL_1"  = "FDA-recognized (OncoKB Level 1)",
      "LEVEL_2"  = "Standard care (OncoKB Level 2)",
      "LEVEL_3A" = "Compelling clinical evidence (OncoKB Level 3A)",
      "LEVEL_3B" = "Investigational (OncoKB Level 3B)",
      "LEVEL_4"  = "Preclinical/biological evidence (OncoKB Level 4)",
      "LEVEL_R1" = "Resistance to Level 1 therapy",
      "LEVEL_R2" = "Resistance to Level 2 therapy"
    )
    
    oncokb_url <- "https://www.oncokb.org/api/v1/utils/allActionableVariants"
    
    res <- GET(
      oncokb_url,
      add_headers(Authorization = paste("Bearer", ONCOKB_TOKEN)),
      timeout(60)
    )
    
    if (status_code(res) == 200) {
      raw_text <- content(res, "text", encoding = "UTF-8")
      oncokb_data <- fromJSON(raw_text, simplifyDataFrame = TRUE)
      
      if (is.data.frame(oncokb_data) && nrow(oncokb_data) > 0) {
        
        # Handle gene field: could be character or nested data.frame
        if (is.list(oncokb_data$gene) && !is.data.frame(oncokb_data$gene)) {
          gene_symbols <- sapply(oncokb_data$gene, function(g) {
            if (is.list(g) && "hugoSymbol" %in% names(g)) return(g$hugoSymbol)
            if (is.character(g)) return(g)
            return(NA_character_)
          })
        } else if (is.data.frame(oncokb_data$gene)) {
          gene_symbols <- oncokb_data$gene$hugoSymbol
        } else {
          gene_symbols <- as.character(oncokb_data$gene)
        }
        
        oncokb_data$hugoSymbol <- gene_symbols
        
        oncokb_work <- oncokb_data %>%
          filter(hugoSymbol %in% unique_genes) %>%
          filter(!is.na(drugs) & drugs != "" & drugs != "NA")
        
        extract_cancer_types <- function(ct) {
          if (is.null(ct) || length(ct) == 0) return("Not annotated (OncoKB)")
          if (is.data.frame(ct) && "cancerType" %in% names(ct)) {
            ctypes <- unique(ct$cancerType)
            return(paste("OncoKB annotated:", paste(ctypes, collapse = "; ")))
          }
          if (is.list(ct)) {
            ctypes <- unique(sapply(ct, function(x) {
              if (is.list(x) && "cancerType" %in% names(x)) return(x$cancerType)
              return(NA_character_)
            }))
            ctypes <- ctypes[!is.na(ctypes)]
            if (length(ctypes) > 0) return(paste("OncoKB annotated:", paste(ctypes, collapse = "; ")))
          }
          return("Not annotated (OncoKB)")
        }
        
        oncokb_work <- oncokb_work %>%
          rowwise() %>%
          mutate(
            Cancer_Type_Context = extract_cancer_types(cancerTypes),
            Evidence_Tier       = ifelse(level %in% names(level_to_tier),
                                         level_to_tier[level], 5),
            Clinical_Status     = ifelse(level %in% names(level_labels),
                                         level_labels[level],
                                         paste0("OncoKB | ", level)),
            Evidence_Tier       = as.numeric(Evidence_Tier)
          ) %>%
          ungroup()
        
        oncokb_filtered <- oncokb_work %>%
          mutate(Source_Database = "OncoKB") %>%
          select(
            Gene_Symbol        = hugoSymbol,
            Drug_Name          = drugs,
            Interaction_Type   = alteration,
            Source_Database,
            Clinical_Status,
            Evidence_Tier,
            Cancer_Type_Context
          ) %>%
          distinct()
        
        message(sprintf("OncoKB: %d interactions for %d yardstick genes.",
                        nrow(oncokb_filtered),
                        length(unique(oncokb_filtered$Gene_Symbol))))
        
      } else {
        message("OncoKB: response parsed but no actionable variants found.")
      }
    } else if (status_code(res) == 403) {
      message("OncoKB: Token returned 403 Forbidden. Your token may have expired or lacks a valid license.")
      message("       Visit https://www.oncokb.org/account/register to obtain/renew a license.")
      message("       OncoKB will be skipped. Continuing with DGIdb + CIViC only.")
    } else {
      message(sprintf("OncoKB API returned status %d: %s",
                      status_code(res),
                      substr(content(res, "text", encoding = "UTF-8"), 1, 200)))
    }
  }, error = function(e) {
    message("OncoKB query failed: ", e$message)
    oncokb_filtered <<- data.frame()
  })
}

# ==============================================================================
# STEP 4.5: OncoKB Gene Annotations (via allCuratedGenes — always available)
# ==============================================================================
message("Step 4.5: Fetching OncoKB Gene Annotations...")

oncokb_gene_annotations <- data.frame()

if (ONCOKB_TOKEN != "") {
  tryCatch({
    res <- GET(
      "https://www.oncokb.org/api/v1/utils/allCuratedGenes",
      add_headers(Authorization = paste("Bearer", ONCOKB_TOKEN)),
      timeout(60)
    )
    if (status_code(res) == 200) {
      raw_text <- content(res, "text", encoding = "UTF-8")
      gene_data <- fromJSON(raw_text, simplifyDataFrame = TRUE)
      
      if (is.data.frame(gene_data) && nrow(gene_data) > 0) {
        # Column names: grch37Isoform, grch38Isoform, entrezGeneId, hugoSymbol,
        #               geneType, highestSensitiveLevel, highestResistanceLevel,
        #               summary, background
        symbol_col <- if ("hugoSymbol" %in% names(gene_data)) "hugoSymbol" else NULL
        entrez_col <- if ("entrezGeneId" %in% names(gene_data)) "entrezGeneId" else NULL
        onco_col   <- if ("geneType" %in% names(gene_data)) "geneType" else NULL
        summary_col <- if ("summary" %in% names(gene_data)) "summary" else NULL
        bg_col     <- if ("background" %in% names(gene_data)) "background" else NULL
        sens_col   <- if ("highestSensitiveLevel" %in% names(gene_data)) "highestSensitiveLevel" else NULL
        resist_col <- if ("highestResistanceLevel" %in% names(gene_data)) "highestResistanceLevel"
                      else if ("highestResistancLevel" %in% names(gene_data)) "highestResistancLevel"
                      else NULL
        
        oncokb_gene_annotations <- data.frame(
          Gene_Symbol            = if (!is.null(symbol_col)) as.character(gene_data[[symbol_col]]) else NA_character_,
          Entrez_Gene_ID         = if (!is.null(entrez_col)) as.integer(gene_data[[entrez_col]]) else NA_integer_,
          Oncogenic_Class        = if (!is.null(onco_col)) as.character(gene_data[[onco_col]]) else "Unknown",
          Highest_Sensitive_Level = if (!is.null(sens_col)) as.character(gene_data[[sens_col]]) else "",
          Highest_Resistance_Level = if (!is.null(resist_col)) as.character(gene_data[[resist_col]]) else "",
          Gene_Summary           = if (!is.null(summary_col)) as.character(gene_data[[summary_col]]) else "",
          Gene_Background        = if (!is.null(bg_col)) as.character(gene_data[[bg_col]]) else "",
          Source_Database        = "OncoKB_GeneAnnotation",
          stringsAsFactors       = FALSE
        )
        
        # Filter to yardstick genes
        oncokb_gene_annotations <- oncokb_gene_annotations %>%
          filter(!is.na(Gene_Symbol) & Gene_Symbol != "") %>%
          filter(Gene_Symbol %in% unique_genes)
        
        message(sprintf("OncoKB Gene Annotations: %d yardstick genes annotated.",
                        nrow(oncokb_gene_annotations)))
      }
    } else {
      message(sprintf("OncoKB allCuratedGenes returned status %d. Skipping gene annotations.",
                      status_code(res)))
    }
  }, error = function(e) {
    message("OncoKB gene annotation fetch failed: ", e$message)
    oncokb_gene_annotations <<- data.frame()
  })
} else {
  message("OncoKB Gene Annotations skipped (no token).")
}

# ==============================================================================
# STEP 5: HARMONIZE AND SAVE THE EVIDENCE-TIERED SUPER MATRIX
# ==============================================================================
message("Step 5: Harmonizing and saving the Unified Matrix...")

# Ensure all data.frames have consistent columns
ensure_columns <- function(df) {
  required <- c("Gene_Symbol", "Drug_Name", "Interaction_Type",
                "Source_Database", "Clinical_Status", "Evidence_Tier",
                "Cancer_Type_Context")
  if (nrow(df) == 0) return(df)
  for (col in required) {
    if (!col %in% names(df)) {
      df[[col]] <- if (col == "Evidence_Tier") 5L else "Unknown"
    }
  }
  df[, required, drop = FALSE]
}

dgidb_final  <- ensure_columns(dgidb_filtered)
civic_final  <- ensure_columns(civic_filtered)
oncokb_final <- ensure_columns(oncokb_filtered)

super_matrix <- bind_rows(dgidb_final, civic_final, oncokb_final)

if (nrow(super_matrix) > 0) {
  super_matrix[is.na(super_matrix)] <- "Unknown"
  super_matrix$Evidence_Tier[is.na(super_matrix$Evidence_Tier)] <- 5L
  
  super_matrix <- super_matrix %>%
    distinct() %>%
    arrange(Evidence_Tier, Source_Database, Gene_Symbol)
  
  saveRDS(super_matrix, "Unified_Drug_Matrix.rds")
  
  message("==============================================================================")
  message(sprintf("SUCCESS! Unified_Drug_Matrix.rds saved with %d rows.", nrow(super_matrix)))
  message(sprintf("  Genes covered: %d", length(unique(super_matrix$Gene_Symbol))))
  message(sprintf("  Drugs covered: %d", length(unique(super_matrix$Drug_Name))))
  message("")
  message("Evidence Tier Distribution:")
  tier_counts <- table(super_matrix$Evidence_Tier)
  tier_labels <- c(
    "1" = "FDA-recognized / Standard-of-care (Level 1)",
    "2" = "Standard care / CIViC-A (Level 2)",
    "3" = "Compelling clinical evidence (Level 3)",
    "4" = "Clinical/Preclinical evidence (Level 4)",
    "5" = "Single-source / Preclinical / Inferential (Level 5)"
  )
  for (tier in names(tier_counts)) {
    label <- if (tier %in% names(tier_labels)) tier_labels[tier] else paste("Tier", tier)
    message(sprintf("  Tier %s (%s): %d", tier, label, tier_counts[tier]))
  }
  message("")
  message("Database Distribution:")
  print(table(super_matrix$Source_Database))
  message("==============================================================================")
} else {
  message("WARNING: No data was extracted from any database. The matrix is empty.")
  message("         The existing Unified_Drug_Matrix.rds was NOT overwritten.")
  message("         Check that Table_S11_Interpreter_12k.csv exists and contains genes,")
  message("         and that at least one database (DGIdb/CIViC) is accessible.")
  
  # If existing matrix exists, keep it
  if (file.exists("Unified_Drug_Matrix.rds")) {
    message("         Keeping existing Unified_Drug_Matrix.rds.")
  }
}

# ==============================================================================
# SAVE ONCOKB GENE ANNOTATIONS (separate from drug matrix)
# ==============================================================================
if (nrow(oncokb_gene_annotations) > 0) {
  saveRDS(oncokb_gene_annotations, "OncoKB_Gene_Annotations.rds")
  message("")
  message(sprintf("OncoKB_Gene_Annotations.rds saved with %d genes.",
                  nrow(oncokb_gene_annotations)))
  message(sprintf("  Oncogenic classes:"))
  oc_table <- table(oncokb_gene_annotations$Oncogenic_Class)
  for (oc in names(oc_table)) {
    message(sprintf("    %s: %d", oc, oc_table[oc]))
  }
} else {
  # Generate empty but keep existing if present
  if (file.exists("OncoKB_Gene_Annotations.rds")) {
    message("OncoKB gene annotations not refreshed. Keeping existing file.")
  } else {
    message("OncoKB gene annotations not available (check token/license).")
  }
}

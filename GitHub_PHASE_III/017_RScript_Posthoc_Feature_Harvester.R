###############################################################################
# PHASE IIIC — POST-HOC FEATURE HARVESTER & GOLDEN PAYLOAD EXTRAPOLATION
# Architecture: RDS Sub-System Extractor
###############################################################################
library(dplyr)
library(parallel)

cat("====================================================================\n")
cat("[PHASE IIIC] RDS HARVESTER & 4/4 INTERSECTION ALGORITHM INITIATED\n")
cat("====================================================================\n")

# Configuration Paths
MODELS_DIR <- "PHASE_III_Megarun_4_4_complete/PHASE_III_ML_Models"
cohort_dirs <- list.dirs(MODELS_DIR, recursive = FALSE)

# -------------------------------------------------------------------------
# STAGE 1: RAPID RDS EXTRACTION (MTLR NATIVE RECOVERY)
# -------------------------------------------------------------------------
cat("Scanning for missing MTLR TSV signatures across 96 environments...\n")

extract_mtlr <- function(cdir) {
  cohort <- basename(cdir)
  mtlr_dir <- file.path(cdir, "MTLR")
  if(!dir.exists(mtlr_dir)) dir.create(mtlr_dir, recursive=TRUE)
  
  out_file <- file.path(mtlr_dir, paste0(cohort, "_MTLR_Feature_Importance.tsv"))
  
  # Skip RDS reading if TSV natively exists 
  if(file.exists(out_file)) return(NULL)
  
  rds_file <- file.path(cdir, paste0("model_bundle_", cohort, ".rds"))
  if(!file.exists(rds_file)) return(NULL)
  
  # Load massive RDS bundle (TryCatch in case of corruption)
  bundle <- tryCatch(readRDS(rds_file), error=function(e) NULL)
  if(is.null(bundle) || is.null(bundle$MTLR)) return(NULL)
  
  mod <- bundle$MTLR
  if(is.null(mod$weight_matrix) || !is.matrix(mod$weight_matrix)) return(NULL)
  
  w_mat <- mod$weight_matrix
  valid_cols <- colnames(w_mat)[colnames(w_mat) != "Bias"]
  
  if(length(valid_cols) > 0) {
    l2_norms <- sapply(valid_cols, function(f) sqrt(sum(w_mat[, f]^2, na.rm=TRUE)))
    m_df <- data.frame(Feature = gsub("^`|`$", "", valid_cols), MTLR_L2_Norm = as.numeric(l2_norms), stringsAsFactors = FALSE)
    m_df <- m_df[order(m_df$MTLR_L2_Norm, decreasing = TRUE), ]
    write.table(m_df, out_file, sep="\t", row.names=FALSE, quote=FALSE)
  }
  return(cohort)
}

# Run extraction using parallel processing to tear through RDS files instantly
total_cores <- parallel::detectCores(logical = TRUE)
cl <- parallel::makeCluster(max(1, total_cores - 2))
recovered <- pbapply::pblapply(cohort_dirs, extract_mtlr, cl = cl)
parallel::stopCluster(cl)

recovered_clean <- unlist(recovered)
cat("Successfully extracted and injected MTLR data into", length(recovered_clean), "cohorts.\n")


# -------------------------------------------------------------------------
# STAGE 2: 4/4 MASTER INTERSECTION SCAN
# -------------------------------------------------------------------------
cat("\nExecuting Absolute Quadripartite Intersection Array...\n")

master_token_list <- list()

for (cdir in cohort_dirs) {
  cohort <- basename(cdir)
  
  # 1. Boruta
  b_file <- file.path(cdir, "Boruta", paste0(cohort, "_Boruta_Feature_Decisions.tsv"))
  if (file.exists(b_file)) {
    b_df <- tryCatch(read.delim(b_file, stringsAsFactors=FALSE), error=function(e) NULL)
    if (!is.null(b_df) && "decision" %in% colnames(b_df)) {
      surv <- b_df %>% filter(decision %in% c("Confirmed", "Tentative"))
      if(nrow(surv) > 0) master_token_list[[paste0(cohort,"_B")]] <- data.frame(Cohort=cohort, Algorithm="Boruta", Feature=surv$Feature, stringsAsFactors=FALSE)
    }
  }
  
  # 2. XGBoost
  x_file <- file.path(cdir, "XGBoost", paste0(cohort, "_XGBoost_Node_Importance.tsv"))
  if (file.exists(x_file)) {
    x_df <- tryCatch(read.delim(x_file, stringsAsFactors=FALSE), error=function(e) NULL)
    if (!is.null(x_df) && "Feature" %in% colnames(x_df)) {
      master_token_list[[paste0(cohort,"_X")]] <- data.frame(Cohort=cohort, Algorithm="XGBoost", Feature=x_df$Feature, stringsAsFactors=FALSE)
    }
  }
  
  # 3. RSF
  r_file <- file.path(cdir, "RSF", paste0(cohort, "_RSF_VIMP.tsv"))
  if (file.exists(r_file)) {
    r_df <- tryCatch(read.delim(r_file, stringsAsFactors=FALSE), error=function(e) NULL)
    if (!is.null(r_df) && "Feature" %in% colnames(r_df)) {
      top_r <- r_df %>% filter(VIMP > 0.005)
      if(nrow(top_r) > 0) master_token_list[[paste0(cohort,"_R")]] <- data.frame(Cohort=cohort, Algorithm="RSF", Feature=top_r$Feature, stringsAsFactors=FALSE)
    }
  }
  
  # 4. MTLR
  m_file <- file.path(cdir, "MTLR", paste0(cohort, "_MTLR_Feature_Importance.tsv"))
  if (file.exists(m_file)) {
    m_df <- tryCatch(read.delim(m_file, stringsAsFactors=FALSE), error=function(e) NULL)
    if (!is.null(m_df) && "Feature" %in% colnames(m_df)) {
      top_m <- head(m_df, 20) # Extrapolate Top 20 mathematically most rigid L2 gradients
      master_token_list[[paste0(cohort,"_M")]] <- data.frame(Cohort=cohort, Algorithm="MTLR", Feature=top_m$Feature, stringsAsFactors=FALSE)
    }
  }
}

token_df <- bind_rows(master_token_list)

# FIX THE PARSER DOTS STRUCTURALLY ACROSS ALL FILES
token_df$Feature <- gsub("^([A-Za-z]+)\\.", "\\1-", token_df$Feature)


# Cross-reference Mathematical Isolation 
anchors <- token_df %>% 
  group_by(Feature, Cohort) %>%
  summarize(Algs = n_distinct(Algorithm), Which = paste(unique(Algorithm), collapse=", "), .groups="drop") %>%
  filter(Algs == 4)

cat("\n====================================================================\n")
cat("FINAL RESULTS: ABSOLUTE SURVIVAL ANCHORS (4/4 ALGORITHMS)\n")
cat("====================================================================\n")
cat("Total Golden Payloads Discovered:", nrow(anchors), "\n\n")

if(nrow(anchors) > 0) {
    # Biologically annotate the payloads
    token_map <- c("1"="Protein", "2"="Somatic Mutation", "3"="CNV", "4"="microRNA", "5"="Transcript", "6"="mRNA", "7"="CpG Methylation")
    tokens <- sapply(strsplit(anchors$Feature, "\\."), function(x) if(length(x)>=2) x[2] else NA)
    anchors$Biological_Layer <- token_map[as.character(tokens)]
    
    print(head(anchors %>% arrange(Cohort), 40))
    write.table(anchors, "MATHEMATICAL_PROOF_4_4_Golden_PanCancer_Payloads.tsv", sep="\t", row.names=FALSE, quote=FALSE)
    cat("\nData mathematically frozen to: MATHEMATICAL_PROOF_4_4_Golden_PanCancer_Payloads.tsv\n")
} else {
    cat("The geometric intersection of all 4 Algorithms simultaneously is incredibly rare. Scanning for 3/4...\n")
    silver <- token_df %>% 
      group_by(Feature, Cohort) %>%
      summarize(Algs = n_distinct(Algorithm), Which = paste(unique(Algorithm), collapse=", "), .groups="drop") %>%
      filter(Algs == 3)
      
    tokens_s <- sapply(strsplit(silver$Feature, "\\."), function(x) if(length(x)>=2) x[2] else NA)
    token_map <- c("1"="Protein", "2"="Somatic Mutation", "3"="CNV", "4"="microRNA", "5"="Transcript", "6"="mRNA", "7"="CpG Methylation")
    silver$Biological_Layer <- token_map[as.character(tokens_s)]
      
    cat("Discovered", nrow(silver), "Silver Payloads (3/4).\n")
    print(head(silver %>% arrange(Cohort), 20))
}
cat("====================================================================\n")

# =========================
# CONFIG / DEPENDENCIES
# =========================
setwd("D:/Pré-artigo 5-optosis model/Machine learning CancerRCDShiny_prediction/Enrique version/RUN_006__PCHigor")

suppressPackageStartupMessages({
  if (!requireNamespace("data.table", quietly = TRUE)) install.packages("data.table")
})
library(data.table)

NA_TOKENS <- c("NA", "", "NaN")  # keep conservative; expand if you *know* other encodings occur

# ---- Your canonical input files (TSV) ----
PRE_TSV  <- "df019_harmonized_Emanuell.tsv"        # pre-imputation snapshot (baseline)
POST_TSV <- "df005_mean_imputed_survival.tsv"       # post-imputation snapshot

# ---- Where we will persist frozen RDS snapshots ----
PRE_RDS        <- sub("\\.tsv(\\.gz)?$", ".rds", PRE_TSV, ignore.case = TRUE)
POST_RDS       <- sub("\\.tsv(\\.gz)?$", ".rds", POST_TSV, ignore.case = TRUE)
POST_ALIGNED_RDS <- sub("\\.rds$", "_aligned.rds", POST_RDS, ignore.case = TRUE)

# ---- Existing artifacts used by the audit ----
STRICT_TSV        <- "df005_missingness_by_type_STRICT.tsv"
TYPEPAIR_TSV      <- "df005_survival_patch_gate_typepair.tsv"
ROWWISE_PREFIX    <- "df005_survival_patch"
OUT_SUMMARY_TSV   <- "clinical_imputation_audit_by_type.tsv"
OUT_CELLLOG_TSV   <- "clinical_imputation_celllog.tsv"
LOG_FILE          <- "ZZ_clinical_imputation_audit.log"
TRACE_DIR         <- "ZZ_audit_trace"

# =========================
# UTILITIES
# =========================
emit <- function(...) cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "—", paste0(..., collapse = ""), "\n")

safe_readRDS <- function(path) {
  if (!file.exists(path)) stop("File not found: ", path, call. = FALSE)
  x <- readRDS(path)
  if (!is.data.frame(x)) x <- as.data.frame(x)
  if (is.null(names(x))) names(x) <- character(ncol(x))
  if (anyDuplicated(names(x))) names(x) <- make.unique(names(x), sep = "_")
  x
}

# Build a simple composite key (NA -> sentinel) for alignment
.build_key <- function(DT, cols) {
  miss <- setdiff(cols, names(DT))
  if (length(miss)) return(NULL)
  tmp <- DT[, ..cols]
  for (j in seq_along(tmp)) tmp[[j]] <- ifelse(is.na(tmp[[j]]), "<NA>", as.character(tmp[[j]]))
  do.call(paste, c(tmp, list(sep="|")))
}

.try_align_by_id <- function(df_pre, df_post, id) {
  if (!(id %in% names(df_pre) && id %in% names(df_post))) return(NULL)
  if (anyDuplicated(df_pre[[id]]) || anyDuplicated(df_post[[id]])) return(NULL)
  idx <- match(df_pre[[id]], df_post[[id]])
  if (anyNA(idx)) return(NULL)
  idx
}

.try_align_by_key <- function(df_pre, df_post, cols) {
  k1 <- .build_key(df_pre, cols); if (is.null(k1)) return(NULL)
  k2 <- .build_key(df_post, cols); if (is.null(k2)) return(NULL)
  if (anyDuplicated(k1) || anyDuplicated(k2)) return(NULL)
  idx <- match(k1, k2)
  if (anyNA(idx)) return(NULL)
  idx
}

.try_align_by_type_index <- function(df_pre, df_post) {
  if (!("type" %in% names(df_pre) && "type" %in% names(df_post))) return(NULL)
  DT1 <- as.data.table(df_pre)[, .I_within := seq_len(.N), by = type]
  DT2 <- as.data.table(df_post)[, .I_within := seq_len(.N), by = type]
  c1 <- DT1[, .N, by = type][order(type)]
  c2 <- DT2[, .N, by = type][order(type)]
  if (!identical(c1, c2)) return(NULL)
  map <- merge(DT1[, .(row_pre = .I, type, .I_within)],
               DT2[, .(row_post = .I, type, .I_within)],
               by = c("type", ".I_within"), all = FALSE, sort = FALSE)
  idx <- integer(nrow(DT1)); idx[map$row_pre] <- map$row_post
  if (length(idx) != nrow(df_pre) || any(idx == 0L)) return(NULL)
  idx
}

align_post_to_pre <- function(pre_rds = PRE_RDS, post_rds = POST_RDS, out_rds = POST_ALIGNED_RDS) {
  emit("Loading PRE: ", pre_rds)
  emit("Loading POST: ", post_rds)
  pre  <- safe_readRDS(pre_rds);  setDT(pre)
  post <- safe_readRDS(post_rds); setDT(post)
  
  if (nrow(pre) != nrow(post)) stop("Row count differs: pre=", nrow(pre), " post=", nrow(post), call. = FALSE)
  
  if (("type" %in% names(pre)) && ("type" %in% names(post)) && identical(pre$type, post$type)) {
    emit("Already aligned by 'type' vector; writing POST copy -> ", out_rds)
    saveRDS(as.data.frame(post), out_rds)
    return(invisible(list(key="already_aligned", out=out_rds)))
  }
  
  # A) single-ID alignment
  id_candidates <- c("bcr_patient_barcode","case_id","submitter_id","barcode","sample_id","sample","patient")
  for (id in id_candidates) {
    idx <- .try_align_by_id(pre, post, id)
    if (!is.null(idx)) {
      emit("Aligned by ID: ", id, " -> ", out_rds)
      post_al <- post[idx]
      saveRDS(as.data.frame(post_al), out_rds)
      return(invisible(list(key=paste0("id:", id), out=out_rds)))
    }
  }
  
  # B) composite keys (non-imputed clinicals)
  key_sets <- list(
    c("type","bcr_patient_barcode"),
    c("type","case_id"),
    c("type","submitter_id"),
    c("type","OS","DSS","DFI","PFI","initial_pathologic_dx_year"),
    c("type","OS","DSS","DFI","PFI","age_at_initial_pathologic_diagnosis"),
    c("type","OS","DSS","DFI","PFI","initial_pathologic_dx_year","age_at_initial_pathologic_diagnosis")
  )
  for (cols in key_sets) {
    idx <- .try_align_by_key(pre, post, cols)
    if (!is.null(idx)) {
      emit("Aligned by composite key: ", paste(cols, collapse="+"), " -> ", out_rds)
      post_al <- post[idx]
      saveRDS(as.data.frame(post_al), out_rds)
      return(invisible(list(key=paste0("key:", paste(cols, collapse="+")), out=out_rds)))
    }
  }
  
  # C) fallback: (type, within-type index)
  idx <- .try_align_by_type_index(pre, post)
  if (!is.null(idx)) {
    emit("Aligned by (type, within-type index) -> ", out_rds)
    post_al <- post[idx]
    saveRDS(as.data.frame(post_al), out_rds)
    return(invisible(list(key="type+index", out=out_rds)))
  }
  
  stop("Unable to construct a 1:1 row mapping between PRE and POST; provide a stable identifier.", call. = FALSE)
}

# =========================
# STEP 1 — READ TSVs AND FREEZE RDS SNAPSHOTS (correctly)
# =========================
stopifnot(file.exists(PRE_TSV), file.exists(POST_TSV))

emit("Reading PRE TSV: ", PRE_TSV)
pre  <- fread(PRE_TSV,  sep="\t", na.strings=NA_TOKENS, data.table=FALSE)
emit("Saving PRE RDS: ", PRE_RDS)
saveRDS(pre, PRE_RDS)

emit("Reading POST TSV: ", POST_TSV)
post <- fread(POST_TSV, sep="\t", na.strings=NA_TOKENS, data.table=FALSE)
emit("Saving POST RDS: ", POST_RDS)         # <-- FIX: save *post* here
saveRDS(post, POST_RDS)

# Minimal schema sanity
stopifnot("type" %in% names(pre), "type" %in% names(post))
stopifnot(nrow(pre) == nrow(post))

# =========================
# STEP 2 — ALIGN POST TO PRE (if needed)
# =========================
align_info <- align_post_to_pre(PRE_RDS, POST_RDS, POST_ALIGNED_RDS)
emit("Alignment key: ", align_info$key)

# Optional sanity: after alignment, types should match exactly
pre_ref  <- safe_readRDS(PRE_RDS)
post_ref <- safe_readRDS(POST_ALIGNED_RDS)
stopifnot(identical(pre_ref$type, post_ref$type))


# =========================
# AUDIT: clinical imputation (failsafe)
# =========================
if (!exists("emit", mode = "function")) {
  emit <- function(...) cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "—", paste0(..., collapse = ""), "\n")
}
if (!exists("safe_readRDS", mode = "function")) {
  safe_readRDS <- function(path) {
    if (!file.exists(path)) stop("File not found: ", path, call. = FALSE)
    x <- readRDS(path)
    if (!is.data.frame(x)) x <- as.data.frame(x)
    if (is.null(names(x))) names(x) <- character(ncol(x))
    if (anyDuplicated(names(x))) names(x) <- make.unique(names(x), sep = "_")
    x
  }
}
if (!exists("safe_export_tsv", mode = "function")) {
  safe_export_tsv <- function(x, path, na = "NA") {
    dir.create(dirname(path), showWarnings = FALSE, recursive = TRUE)
    utils::write.table(x, file = path, sep = "\t", quote = FALSE, row.names = FALSE, col.names = TRUE, na = na)
  }
}

audit_clinical_imputation_FS <- function(
    df005_rds,
    df006_rds,
    strict_tsv,
    gate_typepair_tsv,
    rowwise_prefix,
    out_summary_tsv,
    out_celllog_tsv,
    log_file          = NULL,
    trace_dir         = NULL,
    per_stage_timeout = 120,
    threshold         = 0.35,
    decimals          = 4
) {
  suppressPackageStartupMessages({
    if (!requireNamespace("data.table", quietly = TRUE)) install.packages("data.table")
  })
  library(data.table)
  
  # ---- internal helpers ----
  .with_stage <- function(tag, expr) {
    emit("[BEGIN] ", tag, " (timeout=", per_stage_timeout, "s)")
    t0 <- Sys.time()
    on.exit({
      dt <- round(as.numeric(difftime(Sys.time(), t0, units="secs")), 2)
      emit("[END]   ", tag, " (", dt, "s)")
    }, add = TRUE)
    if (requireNamespace("R.utils", quietly = TRUE) && is.finite(per_stage_timeout)) {
      R.utils::withTimeout(expr, timeout = per_stage_timeout, onTimeout = "error")
    } else {
      eval.parent(substitute(expr))
    }
  }
  
  events <- c("OS","DSS","DFI","PFI")
  times  <- c("OS.time","DSS.time","DFI.time","PFI.time")
  pair_of <- setNames(c("OS","DSS","DFI","PFI"), times)
  event_of <- setNames(events, times)  # map time var -> its event
  
  pick_idcol <- function(DT) {
    cand <- c("bcr_patient_barcode","case_id","submitter_id","barcode","sample_id","sample","patient","case_submitter_id","aliquot_barcode")
    hit <- cand[cand %in% names(DT)][1]
    if (length(hit)) hit else NULL
  }
  
  # ---- logging to file (optional) ----
  if (!is.null(log_file)) {
    dir.create(dirname(log_file), showWarnings = FALSE, recursive = TRUE)
    lfcon <- file(log_file, open = "wt")
    sink(lfcon, type = "output", split = TRUE)
    on.exit({ try(sink(type="output")); try(close(lfcon)); }, add = TRUE)
  }
  if (!is.null(trace_dir)) dir.create(trace_dir, showWarnings = FALSE, recursive = TRUE)
  
  emit("=== START audit_clinical_imputation_FS ===")
  emit("wd: ", getwd())
  emit("args: df005=", df005_rds, " | df006=", df006_rds, " | strict=", strict_tsv, " | typepair=", gate_typepair_tsv, " | rowwise_prefix=", rowwise_prefix)
  
  # ---- STAGE: check inputs ----
  .with_stage("CHECK_INPUTS", {
    req <- c(df005_rds, df006_rds, strict_tsv, gate_typepair_tsv,
             paste0(rowwise_prefix, "_OS_rowwise_gate.tsv"),
             paste0(rowwise_prefix, "_DSS_rowwise_gate.tsv"),
             paste0(rowwise_prefix, "_DFI_rowwise_gate.tsv"),
             paste0(rowwise_prefix, "_PFI_rowwise_gate.tsv"))
    labs <- c("df005", "df006", "strict", "typepair", "OS_row", "DSS_row", "DFI_row", "PFI_row")
    for (i in seq_along(req)) {
      emit("check: ", basename(req[i]), " -> ", if (file.exists(req[i])) "FOUND" else "NOT FOUND")
      if (!file.exists(req[i])) stop("Missing required input: ", labs[i], " => ", req[i], call. = FALSE)
    }
  })
  
  # ---- STAGE: load datasets ----
  df005 <- .with_stage("LOAD_DF005", { safe_readRDS(df005_rds) })
  df006 <- .with_stage("LOAD_DF006", { safe_readRDS(df006_rds) })
  
  stopifnot("type" %in% names(df005), "type" %in% names(df006))
  if (nrow(df005) != nrow(df006)) stop("Row count differs: df005=", nrow(df005), " df006=", nrow(df006), call. = FALSE)
  if (!identical(df005$type, df006$type)) stop("'type' is not aligned across df005/df006.", call. = FALSE)
  
  emit("df005 dims: ", nrow(df005), "x", ncol(df005))
  emit("df006 dims: ", nrow(df006), "x", ncol(df006))
  
  setDT(df005); setDT(df006)
  idcol <- pick_idcol(df005)
  
  # ---- STAGE: load governance tables ----
  STRICT <- .with_stage("LOAD_STRICT", {
    fread(strict_tsv, sep="\t", na.strings="NA")
  })
  TYPEPAIR <- .with_stage("LOAD_TYPEPAIR", {
    fread(gate_typepair_tsv, sep="\t", na.strings="NA")
  })
  ROW_OS  <- fread(paste0(rowwise_prefix, "_OS_rowwise_gate.tsv"),  sep="\t", na.strings="NA")
  ROW_DSS <- fread(paste0(rowwise_prefix, "_DSS_rowwise_gate.tsv"), sep="\t", na.strings="NA")
  ROW_DFI <- fread(paste0(rowwise_prefix, "_DFI_rowwise_gate.tsv"), sep="\t", na.strings="NA")
  ROW_PFI <- fread(paste0(rowwise_prefix, "_PFI_rowwise_gate.tsv"), sep="\t", na.strings="NA")
  ROWWISE <- list(OS=ROW_OS, DSS=ROW_DSS, DFI=ROW_DFI, PFI=ROW_PFI)
  
  # ---- utility: strict decision for a (type,var) ----
  strict_decision <- function(tp, var) {
    # events are not governed by STRICT eligibility for imputation — mark as N/A
    if (var %in% events) {
      return(list(prop_na_before = NA_real_, prop_na_round = NA_real_,
                  decision = "not_applicable_for_events", on_boundary = NA))
    }
    # Use df005 to compute prop; strictly speaking we could also join STRICT,
    # but computing from df005 keeps this module self-contained.
    sub <- df005[type == tp, ..var]
    n <- nrow(sub); nna <- sum(is.na(sub[[1]]))
    prop <- if (n > 0) nna / n else NA_real_
    prnd <- if (!is.na(prop)) round(prop, decimals) else NA_real_
    dec <- if (is.na(prnd)) "unknown" else if (prnd <= threshold) "eligible_by_threshold" else "ineligible_by_threshold"
    list(prop_na_before = prop, prop_na_round = prnd, decision = dec, on_boundary = isTRUE(prnd == threshold))
  }
  
  # ---- helper: per-type, get type-pair gate (TRUE/FALSE) for a given time var ----
  typepair_gate_for <- function(tp, time_var) {
    pr <- pair_of[[time_var]]  # "OS", "DSS", "DFI", "PFI"
    row <- TYPEPAIR[type == tp & (event_var == pr | time_var == paste0(pr,".time"))]
    if (nrow(row) == 0L && "pair" %in% names(TYPEPAIR)) {
      row <- TYPEPAIR[type == tp & grepl(paste0("^", pr, "/"), TYPEPAIR$pair)]
    }
    if (nrow(row) == 0L) return(FALSE)
    isTRUE(row$gate_imputation[1])
  }
  
  # ---- helper: row-wise allow_time_impute mask for a given type+pair ----
  rowwise_allow_for <- function(tp, pair) {
    TBL <- ROWWISE[[pair]]
    allow_col <- paste0(pair, "_allow_time_impute")
    if (!(allow_col %in% names(TBL))) return(rep(FALSE, sum(df005$type == tp)))
    sub <- TBL[type == tp, ..allow_col]
    if (nrow(sub) == 0L) return(rep(FALSE, sum(df005$type == tp)))
    as.logical(sub[[1]])
  }
  
  # =========================
  # CORE LOOP
  # =========================
  types <- unique(na.omit(df005$type))
  summary_rows <- list()
  celllog_rows <- list()
  
  for (tp in types) {
    idx <- which(df005$type == tp)
    n_tp <- length(idx)
    pre_t  <- df005[idx, ]
    post_t <- df006[idx, ]
    
    # Build an identifier for the celllog (id or within-type index)
    if (!is.null(idcol) && idcol %in% names(pre_t)) {
      ids <- as.character(pre_t[[idcol]])
    } else {
      ids <- sprintf("%s#%05d", tp, seq_len(n_tp))
    }
    
    # Process EVENTS
    for (ev in events) {
      if (!(ev %in% names(pre_t)) || !(ev %in% names(post_t))) next
      pre <- pre_t[[ev]]; post <- post_t[[ev]]
      pre_na <- is.na(pre)
      
      n_na_before <- sum(pre_na)
      prop_na_before <- if (n_tp > 0) n_na_before / n_tp else NA_real_
      prop_na_round <- NA_real_
      decision <- "not_applicable_for_events"; on_boundary <- NA
      
      # allowed set is empty for events
      imputable <- rep(FALSE, n_tp)
      imputable_NA_burden <- 0L
      n_imputed <- 0L
      residual_imputable_NA <- 0L
      
      over_imp <- sum(pre_na & !is.na(post))
      clobb    <- sum(!pre_na & (is.na(post) | (post != pre)))
      dom_viol <- sum(!is.na(post) & !(post %in% c(0,1)))
      
      audit_status <- if (over_imp==0L && clobb==0L && dom_viol==0L) "PASS" else "FAIL"
      
      summary_rows[[length(summary_rows)+1L]] <- data.frame(
        type = tp, variable = ev, n = n_tp,
        n_na_before = n_na_before,
        prop_na_before = prop_na_before,
        threshold = threshold,
        prop_na_round = prop_na_round,
        threshold_round = threshold,
        decision = decision,
        on_boundary = on_boundary,
        imputable_NA_burden = imputable_NA_burden,
        n_imputed = n_imputed,
        residual_imputable_NA = residual_imputable_NA,
        over_imputed = over_imp,
        clobbered = clobb,
        domain_violations = dom_viol,
        audit_status = audit_status,
        stringsAsFactors = FALSE
      )
      
      # celllog for events
      if (over_imp > 0L) {
        j <- which(pre_na & !is.na(post))
        celllog_rows[[length(celllog_rows)+1L]] <- data.frame(
          type = tp, variable = ev, row_index = idx[j], id = ids[j],
          pre_value = NA, post_value = post[j], action = "over_imputed_event",
          stringsAsFactors = FALSE
        )
      }
      if (clobb > 0L) {
        j <- which(!pre_na & (is.na(post) | (post != pre)))
        celllog_rows[[length(celllog_rows)+1L]] <- data.frame(
          type = tp, variable = ev, row_index = idx[j], id = ids[j],
          pre_value = pre[j], post_value = post[j], action = "clobbered",
          stringsAsFactors = FALSE
        )
      }
      if (dom_viol > 0L) {
        j <- which(!is.na(post) & !(post %in% c(0,1)))
        celllog_rows[[length(celllog_rows)+1L]] <- data.frame(
          type = tp, variable = ev, row_index = idx[j], id = ids[j],
          pre_value = pre[j], post_value = post[j], action = "domain_violation_event",
          stringsAsFactors = FALSE
        )
      }
    }
    
    # Process TIMES
    for (tv in times) {
      if (!(tv %in% names(pre_t)) || !(tv %in% names(post_t))) next
      pre_raw  <- pre_t[[tv]]
      post_raw <- post_t[[tv]]
      pre_num  <- suppressWarnings(as.numeric(pre_raw))
      post_num <- suppressWarnings(as.numeric(post_raw))
      pre_na   <- is.na(pre_raw)
      
      # STRICT decision from df005
      sd <- strict_decision(tp, tv)
      decision <- sd$decision
      prop_na_before <- sd$prop_na_before
      prop_na_round  <- sd$prop_na_round
      on_boundary    <- sd$on_boundary
      
      # Gates
      gate_pair <- typepair_gate_for(tp, tv)
      row_allow <- rowwise_allow_for(tp, pair_of[[tv]])
      if (length(row_allow) != length(pre_na)) {
        # If rowwise TSV has different length (shouldn't), fall back to all FALSE
        row_allow <- rep(FALSE, length(pre_na))
      }
      
      strict_ok <- identical(decision, "eligible_by_threshold")
      allowed_mask <- pre_na & strict_ok & isTRUE(gate_pair) & as.logical(row_allow)
      
      imputable_NA_burden <- sum(allowed_mask)
      n_imputed <- sum(allowed_mask & !is.na(post_raw))
      residual_imputable_NA <- imputable_NA_burden - n_imputed
      
      over_imp <- sum(pre_na & !is.na(post_raw) & !allowed_mask)
      # clobbered: observed values changed or dropped
      clobb <- sum(!pre_na & (is.na(post_raw) | (post_num != pre_num)))
      # domain: negative or non-finite times in post
      dom_viol <- sum(!is.na(post_num) & (!is.finite(post_num) | post_num < 0), na.rm = TRUE)
      
      audit_status <- if (over_imp==0L && clobb==0L && dom_viol==0L && residual_imputable_NA==0L) "PASS" else "FAIL"
      
      summary_rows[[length(summary_rows)+1L]] <- data.frame(
        type = tp, variable = tv, n = n_tp,
        n_na_before = sum(pre_na),
        prop_na_before = prop_na_before,
        threshold = threshold,
        prop_na_round = prop_na_round,
        threshold_round = threshold,
        decision = decision,
        on_boundary = on_boundary,
        imputable_NA_burden = imputable_NA_burden,
        n_imputed = n_imputed,
        residual_imputable_NA = residual_imputable_NA,
        over_imputed = over_imp,
        clobbered = clobb,
        domain_violations = dom_viol,
        audit_status = audit_status,
        stringsAsFactors = FALSE
      )
      
      # celllog for times
      if (n_imputed > 0L) {
        j <- which(allowed_mask & !is.na(post_raw))
        celllog_rows[[length(celllog_rows)+1L]] <- data.frame(
          type = tp, variable = tv, row_index = idx[j], id = ids[j],
          pre_value = NA, post_value = post_raw[j], action = "imputed",
          stringsAsFactors = FALSE
        )
      }
      if (residual_imputable_NA > 0L) {
        j <- which(allowed_mask & is.na(post_raw))
        celllog_rows[[length(celllog_rows)+1L]] <- data.frame(
          type = tp, variable = tv, row_index = idx[j], id = ids[j],
          pre_value = NA, post_value = NA, action = "residual_imputable_na",
          stringsAsFactors = FALSE
        )
      }
      if (over_imp > 0L) {
        j <- which(pre_na & !is.na(post_raw) & !allowed_mask)
        celllog_rows[[length(celllog_rows)+1L]] <- data.frame(
          type = tp, variable = tv, row_index = idx[j], id = ids[j],
          pre_value = NA, post_value = post_raw[j], action = "over_imputed_time",
          stringsAsFactors = FALSE
        )
      }
      if (clobb > 0L) {
        j <- which(!pre_na & (is.na(post_raw) | (post_num != pre_num)))
        celllog_rows[[length(celllog_rows)+1L]] <- data.frame(
          type = tp, variable = tv, row_index = idx[j], id = ids[j],
          pre_value = pre_raw[j], post_value = post_raw[j], action = "clobbered",
          stringsAsFactors = FALSE
        )
      }
      if (dom_viol > 0L) {
        j <- which(!is.na(post_num) & (!is.finite(post_num) | post_num < 0))
        celllog_rows[[length(celllog_rows)+1L]] <- data.frame(
          type = tp, variable = tv, row_index = idx[j], id = ids[j],
          pre_value = pre_raw[j], post_value = post_raw[j], action = "domain_violation_time",
          stringsAsFactors = FALSE
        )
      }
    } # end times
  } # end types
  
  # ---- bind & export ----
  summary_dt <- as.data.table(data.table::rbindlist(summary_rows, use.names = TRUE, fill = TRUE))
  celllog_dt <- if (length(celllog_rows)) as.data.table(data.table::rbindlist(celllog_rows, use.names = TRUE, fill = TRUE)) else data.table()
  
  # deterministic column order (as in your examples)
  setcolorder(summary_dt, c("type","variable","n","n_na_before","prop_na_before","threshold",
                            "prop_na_round","threshold_round","decision","on_boundary",
                            "imputable_NA_burden","n_imputed","residual_imputable_NA",
                            "over_imputed","clobbered","domain_violations","audit_status"))
  
  safe_export_tsv(summary_dt, out_summary_tsv)
  safe_export_tsv(celllog_dt, out_celllog_tsv)
  
  emit("Wrote: ", out_summary_tsv, " (", nrow(summary_dt), " rows )")
  emit("Wrote: ", out_celllog_tsv, " (", nrow(celllog_dt), " rows )")
  emit("=== END audit_clinical_imputation_FS ===")
  
  invisible(list(summary = summary_dt, celllog = celllog_dt))
}

# =========================
# STEP 3 — RUN THE AUDIT USING THESE SNAPSHOTS
# =========================
# NOTE: audit_clinical_imputation_FS() must be defined in your session.

res <- audit_clinical_imputation_FS(
  df005_rds         = "df019_harmonized_Emanuell.rds",
  df006_rds         = "df005_mean_imputed_survival_aligned.rds",
  strict_tsv        = "df005_missingness_by_type_STRICT.tsv",
  gate_typepair_tsv = "df005_survival_patch_gate_typepair.tsv",
  rowwise_prefix    = "df005_survival_patch",
  out_summary_tsv   = "clinical_imputation_audit_by_type.tsv",
  out_celllog_tsv   = "clinical_imputation_celllog.tsv",
  log_file          = "ZZ_clinical_imputation_audit.log",
  trace_dir         = "ZZ_audit_trace",
  per_stage_timeout = 120
)

emit("Audit complete. Summary -> ", OUT_SUMMARY_TSV, " | Celllog -> ", OUT_CELLLOG_TSV)

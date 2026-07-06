  ###############################################################################
  # PHASE II — Feasibility map - -LEARNING EXECUTION UNDER TAR (TRANSFORMATION ADMISSIBILITY
  # ROUTING)
  # Declaration of epistemic regimens
  ###############################################################################
  
  # -------------------------------------------------------------------------
  # PREAMBLE — TAR: Transformation Admissibility Routing
  # -------------------------------------------------------------------------
  # TAR (Transformation Admissibility Routing) is a PRE-MODELING validity gate.
  # It determines which transformed datasets (dfXXX) are admissible for downstream
  # survival machine-learning analysis for each (cancer_type × survival_endpoint)
  # pair.
  #
  # Each dfXXX represents a specific upstream transformation regime, combining:
  #   - clinical data normalization / harmonization, and
  #   - omics missingness handling (groupwise imputation / normalization).
  #
  # TAR evaluates each dfXXX against a fixed unimputed reference (df005) using
  # survival endpoints solely as external QC probes. For each (cancer, endpoint),
  # dfXXX is classified as:
  #
  #   - Unchanged: survival discrimination is preserved within tolerance
  #   - Improved : no evidence of survival degradation (marginal gains ignored)
  #   - Degrade  : survival-relevant structure is distorted
  #
  # TAR is diagnostic and exclusionary, endpoint-specific and cancer-specific.
  # It is NOT:
  #   - a model selection step,
  #   - a Phase II performance optimization strategy,
  #   - a feature selection mechanism,
  #   - a device to enforce common cohorts.
  #
  # Its sole purpose is to prevent biologically distortive preprocessing from
  # entering Phase II modeling.
  
  # -------------------------------------------------------------------------
  # SCOPE AND ROLE OF PHASE II
  # -------------------------------------------------------------------------
  # Phase II executes survival machine-learning models CONDITIONAL on TAR-admissible
  # preprocessing. It ensures that:
  #
  #   (i) survival-invariance guarantees enforced by TAR are respected,
  #  (ii) cohorts remain endpoint-specific (no common cohort across endpoints),
  # (iii) global completeness constraints are rejected by design,
  #  (iv) predictor missingness is handled locally and algorithm-aware.
  #
  # Phase II is strictly executional. It does NOT:
  #   - tune preprocessing strategies,
  #   - revise dfXXX generation,
  #   - select dfXXX based on Phase II performance,
  #   - relax TAR exclusion rules.
  
  # -------------------------------------------------------------------------
  # FUNDAMENTAL DESIGN PRINCIPLES (NON-NEGOTIABLE)
  # -------------------------------------------------------------------------
  # (P1) Endpoint specificity:
  #   OS, DSS, DFI, and PFI define independent admissible cohorts. Missingness in
  #   one endpoint never excludes a sample from another.
  #
  # (P2) No global completeness constraints:
  #   Phase II never enforces “complete-case across predictors or omic layers”.
  #
  # (P3) TAR precedes modeling:
  #   Only dfXXX classified as Unchanged or Improved for a given (cancer, endpoint)
  #   may be used for that endpoint.
  #
  # (P4) Cohort preservation:
  #   Sample-level deletion due to predictor missingness is prohibited.
  #   The only permissible sample exclusion is missing survival outcome for the
  #   selected endpoint.
  
  # -------------------------------------------------------------------------
  # PHASE II EXECUTION UNIT
  # -------------------------------------------------------------------------
  # All Phase II decisions operate strictly within:
  #
  #   (cancer_type = c,
  #    survival_endpoint = m,
  #    dfXXX = d,
  #    algorithm = a)
  #
  # No rule may operate outside this unit.
  
  # -------------------------------------------------------------------------
  # PREDICTOR MISSINGNESS POLICY (ML-STAGE SAFETY MECHANISM)
  # -------------------------------------------------------------------------
  # Algorithms are partitioned by predictor-matrix requirements:
  #
  # (A) Finite-matrix required:
  #     - CoxNet
  #     - MTLR (typical implementations)
  #
  # (B) Missingness-tolerant:
  #     - Random Survival Forest (with NA routing enabled)
  #     - XGBoost Survival (tree booster)
  #
  # For (B): predictors may contain NA; no corrective action is required.
  #
  # For (A): residual missingness after TAR-approved preprocessing triggers the
  #          following ordered safety hierarchy:
  #
  #   1) Use dfXXX as provided.
  #   2) Feature-level removal:
  #        Drop numerically inadmissible predictors locally (columns only).
  #   3) Deterministic imputation (last resort):
  #        Non-stochastic, outcome-independent, stratum-local repair.
  #   4) Fit model.
  #
  # Sample-level deletion due to predictor missingness is never allowed.
  
  # -------------------------------------------------------------------------
  # PHASE I / PHASE II SEPARATION (CRITICAL)
  # -------------------------------------------------------------------------
  # Phase I:
  #   - harmonizes clinical data and omics predictors,
  #   - handles missingness upstream,
  #   - NEVER imputes survival events,
  #   - uses survival endpoints only as QC sentinels.
  #
  # Phase II:
  #   - assumes TAR-approved preprocessing,
  #   - uses survival endpoints as modeling outcomes,
  #   - applies no preprocessing that can redefine TAR validity.
  
  # -------------------------------------------------------------------------
  # ALGORITHM SUITE AND ROLE
  # -------------------------------------------------------------------------
  # 1) CoxNet — primary backbone (p >> n, interpretable, stable)
  # 2) RSF    — nonlinear comparator
  # 3) XGBoost Survival — advanced nonlinear benchmark
  # 4) MTLR   — optional advanced model
  #
  # Imputation algorithms used in Phase I (kNN, missForest, MICE, iSVD) are
  # explicitly excluded from Phase II.
  
  # -------------------------------------------------------------------------
  # GROUPWISE MODELING CONSTRAINTS
  # -------------------------------------------------------------------------
  # - No pan-cancer models.
  # - Predictors must match cancer-type prefix (CTAB).
  # - One model is defined by:
  #     (cancer_type × dfXXX × survival_endpoint × omic_layer).
  # - Any ambiguity in routing, parsing, or eligibility aborts model fitting.
  
  # -------------------------------------------------------------------------
  # TAR ROUTING TABLE — "improved_unchanged_best_fullset.tsv"
  # -------------------------------------------------------------------------
  # The TAR table provides the authoritative mapping:
  #   (cancer_type, survival_endpoint) → admissible dfXXX.
  #
  # Only categories ∈ {"Improved","Unchanged"} are valid.
  # Any revision of this table must be versioned and re-audited under the same
  # survival-invariance QC protocol.
  ###############################################################################
  
  ###############################################################################
  # Understanding "improved_unchanged_best_fullset.tsv" (TAR table)
  ###############################################################################
  # NOTE:
  # dfinput is a curated manifest of TAR-admissible dataset variants, keyed by
  # (cancer_type, survival_endpoint), used to route batch survival ML runs to the
  # correct dfXXX.rds inputs while carrying Phase I QC deltas for reporting and
  # execution gating.
  #
  # IMPORTANT:
  # - TAR admissibility is defined strictly by category ∈ {"Improved","Unchanged"}.
  # - The table must never be interpreted as Phase II performance-driven selection.
  # - Coverage reporting is allowed; coverage-based exclusion is not.
  
  ###############################################################################
  # TAR ROUTING TABLE IMPORT + STRUCTURAL AUDIT (PHASE II ENTRYPOINT)
  ###############################################################################
  
  # USE_STUBS <- FALSE
  
  suppressPackageStartupMessages({
    library(dplyr)
    library(tidyr)
    library(rio)
  })
  
  setwd("D:/Pré-artigo 5-optosis model/Machine learning CancerRCDShiny_prediction/Enrique version/PHASE_II")
  
  # setwd("~/students/aluno0549-6")
  
  dfinput <- rio::import("improved_unchanged_best_fullset.tsv")
  
  stopifnot(exists("dfinput"))
  stopifnot(is.data.frame(dfinput))
  
  required_cols <- c("df", "cancer_type", "metric", "category")
  missing_cols <- setdiff(required_cols, colnames(dfinput))
  if (length(missing_cols) > 0) {
    stop("dfinput is missing required columns: ", paste(missing_cols, collapse = ", "), call. = FALSE)
  }
  
  dfinput <- dfinput %>%
    mutate(
      df          = as.character(df),
      cancer_type = as.character(cancer_type),
      metric      = as.character(metric),
      category    = as.character(category)
    )
  
  # -------------------------------------------------------------------------
  # Enforce TAR admissibility (Phase II gate; never assumed implicitly)
  # -------------------------------------------------------------------------
  dfinput <- dfinput %>%
    filter(category %in% c("Improved", "Unchanged"))
  
  # -------------------------------------------------------------------------
  # Coverage and structural diagnostics (reporting only; not an exclusion gate)
  # -------------------------------------------------------------------------
  count_tbl <- dfinput %>%
    count(metric, cancer_type, name = "n_rows") %>%
    arrange(metric, desc(n_rows), cancer_type)
  
  count_wide <- count_tbl %>%
    pivot_wider(names_from = cancer_type, values_from = n_rows, values_fill = 0)
  
  # Convenience splits for downstream execution (no filtering by coverage here)
  df_by_metric <- dfinput %>%
    group_split(metric, .keep = TRUE) %>%
    setNames(sort(unique(dfinput$metric)))
  
  df_by_metric_cancer <- dfinput %>%
    group_by(metric, cancer_type) %>%
    group_split(.keep = TRUE)
  
  df_by_metric_cancer_names <- dfinput %>%
    distinct(metric, cancer_type) %>%
    mutate(name = paste0(metric, "__", cancer_type)) %>%
    pull(name)
  
  df_by_metric_cancer <- setNames(df_by_metric_cancer, df_by_metric_cancer_names)
  
  message("Rows in dfinput (TAR-admissible only): ", nrow(dfinput))
  message("Groups present (metric x cancer_type): ", nrow(count_tbl), " (reporting only)")
  
  # -------------------------------------------------------------------------
  # Audit: expected grid coverage and duplicates
  # -------------------------------------------------------------------------
  obs_pairs <- dfinput %>%
    distinct(metric, cancer_type) %>%
    mutate(present = 1L)
  
  # -------------------------------------------------------------------------
  # Audit: expected grid coverage must be defined from an EXTERNAL universe
  # (NOT from TAR-filtered dfinput, which is self-fulfilling).
  # Universe source:
  #   - Metrics: fixed allowed set
  #   - Cancers: unimputed reference manifest (df005.rds) with full TCGA universe
  # -------------------------------------------------------------------------
  
  # (1) Metrics universe: fixed allowed set
  allowed_metrics <- c("OS", "DSS", "DFI", "PFI")
  
  # Canonicalize dfinput key fields defensively (audit-only; does not change gating intent)
  dfinput <- dfinput %>%
    dplyr::mutate(
      metric      = trimws(as.character(metric)),
      cancer_type = trimws(as.character(cancer_type))
    )
  
  # Hard-guard that dfinput does not contain off-policy metrics
  bad_metrics <- setdiff(sort(unique(dfinput$metric)), allowed_metrics)
  if (length(bad_metrics) > 0L) {
    stop(
      "dfinput contains invalid metric(s) outside allowed set {OS,DSS,DFI,PFI}: ",
      paste(bad_metrics, collapse = ", "),
      call. = FALSE
    )
  }
  
  all_metrics <- allowed_metrics
  
  # (2) Cancer universe: derive from a PRE-FILTER dfXXX manifest (df005.rds)
  #     This is the authoritative "what cancers exist in the pipeline" universe.
  DF_ROOT_MANIFEST <- "D:/Pré-artigo 5-optosis model/Machine learning CancerRCDShiny_prediction/Enrique version/PHASE_II/dfXXX_series"
  manifest_df <- "df005.rds"
  
  manifest_path <- file.path(DF_ROOT_MANIFEST, manifest_df)
  if (!file.exists(manifest_path)) {
    stop("Manifest dfXXX file not found for cancer universe: ", manifest_path, call. = FALSE)
  }
  
  manifest_obj <- readRDS(manifest_path)
  if (!is.data.frame(manifest_obj)) {
    stop("Manifest dfXXX is not a data.frame: ", manifest_df, call. = FALSE)
  }
  if (!("type" %in% colnames(manifest_obj))) {
    stop("Manifest dfXXX is missing required column 'type': ", manifest_df, call. = FALSE)
  }
  
  all_cancers <- sort(unique(trimws(as.character(manifest_obj$type))))
  all_cancers <- all_cancers[nzchar(all_cancers)]
  if (length(all_cancers) == 0L) {
    stop("Cancer universe extracted from manifest is empty: ", manifest_df, call. = FALSE)
  }
  
  # -------------------------------------------------------------------------
  # Universe consistency checks (audit-only; do NOT gate Phase II)
  # -------------------------------------------------------------------------
  
  # Cancers present in dfinput but absent from manifest universe: likely typos or schema drift
  bad_cancers <- setdiff(sort(unique(dfinput$cancer_type)), all_cancers)
  if (length(bad_cancers) > 0L) {
    stop(
      "dfinput contains cancer_type values not present in manifest universe (df005$type): ",
      paste(bad_cancers, collapse = ", "),
      call. = FALSE
    )
  }
  
  # Optional reporting: cancers in manifest universe with zero TAR-admissible rows
  # (not an exclusion gate; purely coverage reporting)
  cancers_with_any_row <- sort(unique(dfinput$cancer_type))
  missing_cancers_in_dfinput <- setdiff(all_cancers, cancers_with_any_row)
  
  # -------------------------------------------------------------------------
  # Compute observed pairs from TAR-admissible dfinput (this is what we audit)
  # -------------------------------------------------------------------------
  obs_pairs <- dfinput %>%
    dplyr::distinct(metric, cancer_type) %>%
    dplyr::mutate(present = 1L)
  
  # -------------------------------------------------------------------------
  # Build expected grid from the EXTERNAL universe (non-self-fulfilling)
  # -------------------------------------------------------------------------
  expected_grid <- tidyr::expand_grid(
    metric = all_metrics,
    cancer_type = all_cancers
  )
  
  audit_tbl <- expected_grid %>%
    dplyr::left_join(obs_pairs, by = c("metric", "cancer_type")) %>%
    dplyr::mutate(present = tidyr::replace_na(present, 0L)) %>%
    dplyr::arrange(metric, cancer_type)
  
  audit_by_metric <- audit_tbl %>%
    dplyr::group_by(metric) %>%
    dplyr::summarise(
      n_present = sum(present),
      n_missing = dplyr::n() - sum(present),
      .groups = "drop"
    ) %>%
    dplyr::arrange(dplyr::desc(n_missing), metric)
  
  audit_by_cancer <- audit_tbl %>%
    dplyr::group_by(cancer_type) %>%
    dplyr::summarise(
      n_present = sum(present),
      n_missing = dplyr::n() - sum(present),
      .groups = "drop"
    ) %>%
    dplyr::arrange(dplyr::desc(n_missing), cancer_type)
  
  dup_pairs <- dfinput %>%
    dplyr::count(metric, cancer_type, name = "n_rows") %>%
    dplyr::filter(n_rows > 1L)
  
  message("Observed metric×cancer pairs (TAR-admissible dfinput): ", nrow(obs_pairs))
  message("Expected grid size (allowed_metrics × manifest cancers): ", nrow(expected_grid))
  message("Missing pairs in expected grid: ", sum(audit_tbl$present == 0L))
  message("Pairs with duplicates (multiple dfXXX per pair): ", nrow(dup_pairs))
  
  if (length(missing_cancers_in_dfinput) > 0L) {
    message(
      "Cancers present in manifest universe but with ZERO TAR-admissible rows in dfinput: ",
      paste(missing_cancers_in_dfinput, collapse = ", ")
    )
  }
  
  # Optional: show missing pairs table (reporting only; no gating)
  missing_pairs <- audit_tbl %>% dplyr::filter(present == 0L)
  print(head(missing_pairs, 50))
  
  ###############################################################################
  # Phase II: strict schema + row-identity + alignment audits (generic dfXXX)
  ###############################################################################
  
  get_surv_cols <- function(metric) {
    metric <- as.character(metric)
    allowed <- c("OS", "DSS", "DFI", "PFI")
    if (!metric %in% allowed) {
      stop("Invalid survival metric: ", metric,
           " (allowed: ", paste(allowed, collapse = ", "), ")", call. = FALSE)
    }
    list(
      event = metric,
      time  = paste0(metric, ".time")
    )
  }
  
  subset_to_cancer <- function(df, cancer_type) {
    stopifnot(is.data.frame(df))
    if (!"type" %in% names(df)) stop("df is missing column: 'type'", call. = FALSE)
    cancer_type <- as.character(cancer_type)
    df_sub <- df[df$type == cancer_type, , drop = FALSE]
    if (nrow(df_sub) == 0L) stop("No rows found for type == ", cancer_type, call. = FALSE)
    df_sub
  }
  
  get_predictor_cols <- function(df, cancer_type) {
    stopifnot(is.data.frame(df))
    cancer_type <- as.character(cancer_type)
    
    # survival block (explicit exclusions)
    surv_block <- c(
      "type",
      "OS", "OS.time",
      "DSS", "DSS.time",
      "DFI", "DFI.time",
      "PFI", "PFI.time"
    )
    
    pattern <- paste0("^", cancer_type, "-")
    preds <- names(df)[grepl(pattern, names(df))]
    preds <- setdiff(preds, surv_block)
    
    if (length(preds) == 0L) {
      stop("No predictors found with prefix ", shQuote(paste0(cancer_type, "-")),
           " after exclusions.", call. = FALSE)
    }
    preds
  }
  
  get_surv_inputs <- function(df_sub, metric) {
    stopifnot(is.data.frame(df_sub))
    cols <- get_surv_cols(metric)
    
    missing <- setdiff(c(cols$event, cols$time), names(df_sub))
    if (length(missing) > 0L) {
      stop("Missing survival columns for metric ", metric, ": ",
           paste(missing, collapse = ", "), call. = FALSE)
    }
    
    time  <- df_sub[[cols$time]]
    event <- df_sub[[cols$event]]
    
    # Basic type sanity (NA allowed here; filtering happens downstream)
    time_num <- suppressWarnings(as.numeric(as.character(time)))
    if (all(is.na(time_num)) && any(!is.na(time))) {
      stop("Survival time cannot be coerced to numeric for metric ", metric, call. = FALSE)
    }
    event_num <- suppressWarnings(as.numeric(as.character(event)))
    if (all(is.na(event_num)) && any(!is.na(event))) {
      stop("Survival event cannot be coerced to numeric for metric ", metric, call. = FALSE)
    }
    
    list(time = time_num, event = event_num)
  }
  
  # -----------------------------------------------------------------------------
  # Canonical Phase II survival-time schema
  # -----------------------------------------------------------------------------
  # POLICY:
  # - Survival time is allowed to be zero (time == 0).
  # - Survival time must be finite and non-negative when observed.
  # - Negative times (time < 0) are invalid and cause hard failure.
  #
  # SCIENTIFIC NOTE (audit-only; no policy change):
  # Allowing time == 0 is schema-compliant and often correct (e.g., instantaneous
  # events at baseline). However, some downstream survival routines may become
  # numerically brittle if many zero-times are present. The current Phase II
  # design (schema allow + deterministic filtering of non-finite/negative times
  # only) is consistent with the “no silent deletions” principle.
  #
  # If numerical stability issues are later investigated, counts of time == 0
  # should be added as an AUDIT FIELD ONLY (additive logging), not as an exclusion
  # rule or feasibility gate.
  
  assert_time_schema_nonneg <- function(time, metric = NULL) {
    time_num <- suppressWarnings(as.numeric(as.character(time)))
    
    bad_time <- !is.na(time_num) & (!is.finite(time_num) | time_num < 0)
    if (any(bad_time)) {
      msg_metric <- if (!is.null(metric)) paste0(" for metric ", metric) else ""
      stop("FAIL__TIME_SCHEMA", msg_metric,
           ": non-finite or negative survival times detected (time < 0).",
           call. = FALSE)
    }
    
    time_num
  }
  
  # -----------------------------------------------------------------------------
  # Metric-scoped survival completeness filter (MANDATORY in Phase II)
  # Drops rows ONLY if survival time/event for the selected metric is missing/invalid.
  # Never enforces common cohorts across endpoints.
  # -----------------------------------------------------------------------------
  filter_survival_complete <- function(df_sub, metric) {
    s <- get_surv_inputs(df_sub, metric)
    time  <- s$time
    event <- s$event
    
    # event allowed values (before filtering): 0/1/NA
    bad_event <- !is.na(event) & !event %in% c(0, 1)
    if (any(bad_event)) {
      stop("Invalid event values for metric ", metric, " (expected 0/1/NA).", call. = FALSE)
    }
    
    # time must be non-negative when observed (adjust if you require strictly >0)
    time <- assert_time_schema_nonneg(time, metric = metric)
    
    keep <- is.finite(time) & is.finite(event) & !is.na(event)
    if (!any(keep)) {
      stop("No complete survival rows remain after metric-scoped filtering for ", metric, call. = FALSE)
    }
    
    df_f <- df_sub[keep, , drop = FALSE]
    list(
      df_sub   = df_f,
      time     = time[keep],
      event    = event[keep],
      kept_idx = which(keep)
    )
  }
  
  # -----------------------------------------------------------------------------
  # Predictor token resolver (CRITICAL)
  # Token is the field immediately after "CTAB-<id>."
  # Example: "BRCA-1795.5.3.P...."  -> token == "5"
  # -----------------------------------------------------------------------------
  get_token <- function(vars) {
    vars <- as.character(vars)
    tok <- sub("^[^-]+-[0-9]+\\.([0-9]+)\\..*$", "\\1", vars)
    tok[!grepl("^[0-9]+$", tok)] <- NA_character_
    tok
  }
  
  # -----------------------------------------------------------------------------
  # Token-aware coercion to numeric predictors (glmnet-safe)
  # -----------------------------------------------------------------------------
  coerce_numeric_continuous <- function(x, varname) {
    if (is.factor(x) || is.character(x)) {
      x_num <- suppressWarnings(as.numeric(as.character(x)))
      if (all(is.na(x_num)) && any(!is.na(x))) {
        stop("Continuous predictor cannot be coerced to numeric: ", varname, call. = FALSE)
      }
      return(x_num)
    }
    if (!is.numeric(x)) stop("Non-numeric continuous predictor detected: ", varname, call. = FALSE)
    x
  }
  
  coerce_mut_to_binary <- function(x, varname) {
    if (is.factor(x) || is.character(x)) {
      x <- suppressWarnings(as.numeric(as.character(x)))
    }
    if (!is.numeric(x)) {
      stop("Mutation predictor is not numeric after coercion: ", varname, call. = FALSE)
    }
    x_bin <- ifelse(is.na(x), NA_real_, ifelse(x == 0, 0, 1))
    ux <- sort(unique(x_bin), na.last = TRUE)
    if (!all(ux %in% c(0, 1, NA))) {
      stop("Mutation binarization failed for: ", varname,
           " | Observed (post-map): ", paste(ux, collapse = ", "), call. = FALSE)
    }
    x_bin
  }
  
  coerce_cnv_to_numeric <- function(x, varname) {
    # 1) If numeric already, accept
    if (is.numeric(x)) return(x)
    
    # 2) Try numeric coercion for factor/character numeric encodings
    if (is.factor(x) || is.character(x)) {
      x_num <- suppressWarnings(as.numeric(as.character(x)))
      if (!(all(is.na(x_num)) && any(!is.na(x)))) {
        # Coercion did not destroy all non-NA content -> accept numeric encoding
        return(x_num)
      }
    }
    
    # 3) Otherwise treat as categorical labels with deterministic mapping
    x_chr <- as.character(x)
    allowed <- c("Deleted", "Normal", "Duplicated", NA)
    ux <- sort(unique(x_chr), na.last = TRUE)
    if (!all(ux %in% allowed)) {
      stop(
        "Invalid CNV variable (token 3): ", varname,
        " | Allowed values: {Deleted, Normal, Duplicated, NA} OR numeric CNV",
        " | Observed: ", paste(ux, collapse = ", "),
        call. = FALSE
      )
    }
    
    out <- rep(NA_real_, length(x_chr))
    out[x_chr == "Deleted"]    <- -1
    out[x_chr == "Normal"]     <-  0
    out[x_chr == "Duplicated"] <-  1
    out
  }
  
  # Build a numeric predictor matrix from df_sub + preds using token logic
  build_numeric_X <- function(df_sub, preds) {
    stopifnot(is.data.frame(df_sub))
    preds  <- as.character(preds)
    tokens <- get_token(preds)
    
    if (anyNA(tokens)) {
      bad <- preds[is.na(tokens)]
      stop("Token parsing failed for predictors: ", paste(bad, collapse = ", "), call. = FALSE)
    }
    
    X <- matrix(NA_real_, nrow = nrow(df_sub), ncol = length(preds))
    colnames(X) <- preds
    
    for (j in seq_along(preds)) {
      v <- preds[j]
      t <- tokens[j]
      x <- df_sub[[v]]
      
      if (t == "2") {
        X[, j] <- coerce_mut_to_binary(x, v)
      } else if (t == "3") {
        X[, j] <- coerce_cnv_to_numeric(x, v)
      } else {
        X[, j] <- coerce_numeric_continuous(x, v)
      }
    }
    
    # Ensure row identity propagates to X
    if (!is.null(rownames(df_sub)) && length(rownames(df_sub)) == nrow(df_sub)) {
      rownames(X) <- rownames(df_sub)
    }
    
    X
  }
  
  # -----------------------------------------------------------------------------
  # Row-identity + alignment audit helpers (sample/patient as row IDs)
  # -----------------------------------------------------------------------------
  assert_row_ids <- function(df_sub) {
    stopifnot(is.data.frame(df_sub))
    
    if (!all(c("sample", "patient") %in% names(df_sub))) {
      stop("df_sub must contain 'sample' and 'patient' columns (row IDs).", call. = FALSE)
    }
    
    df_sub$sample  <- trimws(as.character(df_sub$sample))
    df_sub$patient <- trimws(as.character(df_sub$patient))
    
    if (anyNA(df_sub$sample) || anyNA(df_sub$patient)) {
      stop("Row IDs contain NA (sample/patient).", call. = FALSE)
    }
    if (any(df_sub$sample == "")) {
      stop("Row IDs contain empty or whitespace-only strings in 'sample'.", call. = FALSE)
    }
    if (any(df_sub$patient == "")) {
      stop("Row IDs contain empty or whitespace-only strings in 'patient'.", call. = FALSE)
    }
    if (anyDuplicated(df_sub$sample)) {
      stop("Duplicate 'sample' identifiers detected within cancer-type subset.", call. = FALSE)
    }
    
    # Canonical Phase II row identity: rownames == sample
    rownames(df_sub) <- df_sub$sample
    df_sub
  }
  
  assert_alignment <- function(df_sub, X, time, event) {
    stopifnot(nrow(X) == nrow(df_sub))
    stopifnot(length(time) == nrow(df_sub))
    stopifnot(length(event) == nrow(df_sub))
    
    rn_df <- rownames(df_sub)
    rn_X  <- rownames(X)
    
    if (length(rn_df) != nrow(df_sub) || any(trimws(rn_df) == "")) {
      stop("df_sub rownames are not fully set (must be sample IDs for every row).", call. = FALSE)
    }
    if (length(rn_X) != nrow(X) || any(trimws(rn_X) == "")) {
      stop("X rownames are not fully set (must be sample IDs for every row).", call. = FALSE)
    }
    if (!identical(rn_df, rn_X)) {
      stop("Row alignment failure: rownames(df_sub) != rownames(X).", call. = FALSE)
    }
    
    invisible(TRUE)
  }
  
  # ---------------------------------------------------------------------------
  # Phase II: endpoint-scoped survival masking (TAR-compliant)
  # ---------------------------------------------------------------------------
  # PURPOSE:
  # - Define the admissible cohort ONLY for the selected survival endpoint.
  # - Enforce survival feasibility (finite time + observed event) without imposing
  #   any predictor completeness constraint.
  #
  # POLICY (TAR + Phase II Constitution):
  # - Allowed sample-level exclusion: missing survival outcome for the selected
  #   endpoint (metric-scoped only).
  # - Prohibited: dropping samples due to predictor NA/NaN/Inf (no complete-case
  #   across predictors/omics).
  #   
  # SCIENTIFIC NOTE (audit-only; no policy change):
  # Allowing time == 0 is schema-compliant and often correct (e.g., instantaneous
  # events at baseline). Some survival implementations may exhibit numerical
  # brittleness if many zero-times are present. Phase II therefore permits time == 0
  # at the schema level and enforces only non-finite/negative exclusions.
  #
  # Any future investigation of numerical stability MUST be handled via additive
  # audit fields (e.g., count of time == 0 per stratum), not via exclusion rules,
  # feasibility gates, or silent row deletion.
  # ---------------------------------------------------------------------------
  apply_survival_mask_detailed <- function(df_sub, time, event) {
    
    # ---- HARD GUARDS: prevent name collisions (closure == function) ----
    if (is.function(time)) {
      stop(
        "FAIL__TIME_IS_CLOSURE: argument 'time' is a function (closure), not a vector. ",
        "This indicates a name collision in your environment. ",
        "Rename your survival time object (e.g., time_vec) and call apply_survival_mask(..., time = time_vec).",
        call. = FALSE
      )
    }
    if (is.function(event)) {
      stop(
        "FAIL__EVENT_IS_CLOSURE: argument 'event' is a function (closure), not a vector. ",
        "This indicates a name collision in your environment. ",
        "Rename your survival event object (e.g., event_vec) and call apply_survival_mask(..., event = event_vec).",
        call. = FALSE
      )
    }
    
    # ---- Basic shape checks ----
    stopifnot(is.data.frame(df_sub))
    if (length(time) != nrow(df_sub)) stop("FAIL__TIME_LENGTH_MISMATCH", call. = FALSE)
    if (length(event) != nrow(df_sub)) stop("FAIL__EVENT_LENGTH_MISMATCH", call. = FALSE)
    
    # ---- Canonicalize time/event ONCE (robust to factor/character/integer) ----
    time  <- suppressWarnings(as.numeric(as.character(time)))
    event <- suppressWarnings(as.numeric(as.character(event)))
    
    # Event must be {0,1,NA}
    if (!all(is.na(event) | event %in% c(0, 1))) {
      bad <- unique(event[!is.na(event) & !(event %in% c(0, 1))])
      stop(
        "FAIL__EVENT_SCHEMA: event contains values outside {0,1,NA}. Observed: ",
        paste(head(bad, 20), collapse = ", "),
        call. = FALSE
      )
    }
    
    # Time must be finite when observed; negative time is invalid
    time <- assert_time_schema_nonneg(time, metric = NULL)
    
    # ---- Survival-only feasibility mask (metric-scoped) ----
    mask <- is.finite(time) & is.finite(event) & !is.na(event)
    
    # Audit message (endpoint-scoped cohort preservation)
    message(
      "Survival mask kept: ", sum(mask), " / ", length(mask),
      " rows (endpoint-scoped). Dropped: ", sum(!mask)
    )
    
    df_m    <- df_sub[mask, , drop = FALSE]
    time_m  <- time[mask]
    event_m <- event[mask]
    
    if (nrow(df_m) == 0L) {
      stop("No complete survival rows remain after endpoint-scoped masking.", call. = FALSE)
    }
    
    # ---- Identity contracts (row IDs) ----
    df_m <- assert_row_ids(df_m)
    
    list(df = df_m, time = time_m, event = event_m, mask = mask)
  }
  
  # Wrapper: contract is logical(nrow(df_sub))
  apply_survival_mask <- function(df_sub, time, event) {
    out <- apply_survival_mask_detailed(df_sub = df_sub, time = time, event = event)
    
    if (!is.list(out) || !"mask" %in% names(out)) {
      stop("FAIL__MASK_WRAPPER_CONTRACT: detailed mask did not return $mask.", call. = FALSE)
    }
    if (!is.logical(out$mask) || length(out$mask) != nrow(df_sub)) {
      stop("FAIL__MASK_WRAPPER_CONTRACT: $mask must be logical(nrow(df_sub)).", call. = FALSE)
    }
    
    out$mask
  }
  
  # ---------------------------------------------------------------------------
  # Phase II: finite-matrix feasibility for CoxNet/MTLR (TAR-locked V3)
  # ---------------------------------------------------------------------------
  # PURPOSE:
  # - Ensure a fully finite numeric predictor matrix when the algorithm requires it
  #   (e.g., glmnet CoxNet, most MTLR implementations).
  #
  # POLICY (TAR + Phase II Constitution):
  # - Prohibited: dropping samples due to predictor NA/NaN/Inf.
  # - Allowed, locally within (cancer, endpoint, dfXXX, algorithm):
  #   (1) Drop features with missingness r_j > τ (feature-level removal; columns only)
  #   (2) Deterministically impute features with 0 < r_j ≤ τ (last-mile; deterministic)
  # - τ is selected locally by a ladder search with acceptance constraints:
  #     retention ratio p_keep/p0 ≥ γ
  #     imputation mass M ≤ μ
  #
  # NOTE:
  # - This function does NOT touch survival variables and does NOT enforce common
  #   cohorts across endpoints or predictors.
  # - Deterministic imputation is outcome-independent and model-independent.
  # ---------------------------------------------------------------------------
  make_X_finite_for_glmnet <- function(
      X,
      tau_ladder = c(0.01, 0.05, 0.10, 0.20, 0.30),
      gamma = 0.10,
      mu = 0.05,
      impute_fun = c("median"),
      verbose = TRUE
  ) {
    impute_fun <- match.arg(impute_fun)
    
    if (!is.matrix(X)) stop("X must be a matrix.", call. = FALSE)
    if (!is.numeric(X)) stop("X must be a numeric matrix (glmnet-safe).", call. = FALSE)
    if (!is.numeric(tau_ladder) || length(tau_ladder) == 0L) stop("tau_ladder must be a numeric vector.", call. = FALSE)
    if (!is.numeric(gamma) || gamma <= 0 || gamma > 1) stop("gamma must be in (0,1].", call. = FALSE)
    if (!is.numeric(mu) || mu < 0 || mu > 1) stop("mu must be in [0,1].", call. = FALSE)
    
    n  <- nrow(X)
    p0 <- ncol(X)
    if (p0 == 0L) stop("X has zero columns.", call. = FALSE)
    
    # r_j = fraction of non-finite entries per feature (NA/NaN/Inf)
    rj <- colMeans(!is.finite(X))
    
    # Deterministic column imputer (robust; outcome-independent)
    impute_column <- function(x) {
      bad <- !is.finite(x)
      if (!any(bad)) return(list(x = x, n_imputed = 0L))
      rep_val <- stats::median(x[is.finite(x)], na.rm = TRUE)
      x[bad] <- rep_val
      list(x = x, n_imputed = sum(bad))
    }
    
    # Select smallest τ that satisfies acceptance criteria
    for (tau_thr in tau_ladder) {
      
      keep_cols <- which(rj <= tau_thr)
      drop_cols <- which(rj >  tau_thr)
      
      if (length(keep_cols) == 0L) {
        if (verbose) message(sprintf("τ=%.2f => p_keep=0 (all features exceed τ). Escalating τ.", tau_thr))
        next
      }
      
      Xk <- X[, keep_cols, drop = FALSE]
      
      # Deterministically impute any remaining non-finite in kept features
      n_imputed <- 0L
      if (any(!is.finite(Xk))) {
        for (j in seq_len(ncol(Xk))) {
          outj <- impute_column(Xk[, j])
          Xk[, j] <- outj$x
          n_imputed <- n_imputed + outj$n_imputed
        }
      }
      
      p_keep <- ncol(Xk)
      retention_ratio <- p_keep / p0
      
      denom <- n * p_keep
      M <- if (denom > 0L) n_imputed / denom else 1
      
      ok_ret <- retention_ratio >= gamma
      ok_M   <- M <= mu
      
      if (verbose) {
        message(sprintf(
          "τ=%.2f | p_keep=%d/%d (%.3f) | dropped=%d | imputed=%d cells | M=%.4f | accept=(ret:%s, M:%s)",
          tau_thr, p_keep, p0, retention_ratio, length(drop_cols), n_imputed, M,
          ifelse(ok_ret, "YES", "NO"),
          ifelse(ok_M,   "YES", "NO")
        ))
      }
      
      if (ok_ret && ok_M) {
        if (any(!is.finite(Xk))) {
          stop("TAR-locked feasibility failed: non-finite values remain after deterministic imputation.", call. = FALSE)
        }
        
        dropped_features <- colnames(X)[drop_cols]
        kept_features    <- colnames(X)[keep_cols]
        
        return(list(
          X = Xk,
          tau_thr = tau_thr,  # renamed: selected τ threshold (numeric stability / missingness cutoff)
          tau = tau_thr,      # optional compatibility alias; remove if you prefer strict renaming
          gamma = gamma,
          mu = mu,
          p0 = p0,
          p_keep = p_keep,
          retention_ratio = retention_ratio,
          imputed_cells = n_imputed,
          imputation_mass = M,
          kept_features = kept_features,
          dropped_features = dropped_features
        ))
      }
    }
    
    stop(
      "No τ in tau_ladder satisfied acceptance criteria (retention and imputation-mass). ",
      "Adjust gamma/mu locally or inspect missingness distribution within S_{c,m,d}.",
      call. = FALSE
    )
  }
  
  # --- CANARY (definitions only) ---
  cat("apply_survival_mask_detailed:", exists("apply_survival_mask_detailed", mode = "function"), "\n")
  cat("apply_survival_mask:",         exists("apply_survival_mask", mode = "function"), "\n")
  cat("make_X_finite_for_glmnet:",    exists("make_X_finite_for_glmnet", mode = "function"), "\n")
  
  stopifnot(exists("apply_survival_mask_detailed", mode = "function"))
  stopifnot(exists("apply_survival_mask", mode = "function"))
  stopifnot(exists("make_X_finite_for_glmnet", mode = "function"))
  stopifnot(!identical(body(apply_survival_mask_detailed), body(apply_survival_mask)))
  
  ###############################################################################
  # MATERIALIZE OBJECTS FOR CONTRACT TESTING (df008 example; REQUIRED)
  ###############################################################################
  # This block constructs the objects consumed by the runtime contract checks:
  # df_sub, time_vec, event_vec, preds, X.
  #
  # NOTE:
  # - You may later wrap this in a loop over (c,m,d), but it must exist somewhere
  #   before the runtime checks execute.
  
  # --- Load one dfXXX for a concrete test (df008 shown here) ---
  # df_dir <- file.path(getwd(), "dfXXX_series")
  # if (!dir.exists(df_dir)) dir.create(df_dir, recursive = TRUE)
  
  # If you already setwd() elsewhere, keep it. Otherwise uncomment and set it here:
  setwd("D:/Pré-artigo 5-optosis model/Machine learning CancerRCDShiny_prediction/Enrique version/PHASE_II/dfXXX_series")
  
  # setwd("~/students/aluno0549-6/dfXXX_series")  
  # 
  df008 <- readRDS("df008.rds")
  
  CTAB   <- "BRCA"
  METRIC <- "OS"
  
  # --- Subset + identity ---
  df_sub <- subset_to_cancer(df008, CTAB)
  df_sub <- assert_row_ids(df_sub)  # sets rownames(df_sub) = sample
  
  # --- Endpoint vectors (NEVER name them time/event) ---
  s_in <- get_surv_inputs(df_sub, METRIC)
  time_vec  <- s_in$time
  event_vec <- s_in$event
  
  stopifnot(length(time_vec)  == nrow(df_sub))
  stopifnot(length(event_vec) == nrow(df_sub))
  
  names(time_vec)  <- rownames(df_sub)
  names(event_vec) <- rownames(df_sub)
  
  stopifnot(identical(names(time_vec),  rownames(df_sub)))
  stopifnot(identical(names(event_vec), rownames(df_sub)))
  
  ###############################################################################
  # SURVIVAL INPUT AUDIT + SCHEMA-CONSISTENT SUBSET (Phase II; endpoint-scoped)
  # Contract:
  #   Requires: df_sub, time_vec, event_vec already defined for the current (c,m,d)
  #   Produces: df_sub2, time_vec2, event_vec2 (schema-safe for apply_survival_mask_detailed)
  ###############################################################################
  
  # ---- 0) Existence + alignment (FAIL FAST if called too early) ----
  if (!exists("df_sub", inherits = FALSE))   stop("CALL_ORDER_FAIL: df_sub not defined yet.", call. = FALSE)
  if (!exists("time_vec", inherits = FALSE)) stop("CALL_ORDER_FAIL: time_vec not defined yet.", call. = FALSE)
  if (!exists("event_vec", inherits = FALSE)) stop("CALL_ORDER_FAIL: event_vec not defined yet.", call. = FALSE)
  
  stopifnot(is.data.frame(df_sub))
  stopifnot(length(time_vec)  == nrow(df_sub))
  stopifnot(length(event_vec) == nrow(df_sub))
  
  # ---- 1) Canonicalize ONCE (match apply_survival_mask_detailed) ----
  time_num  <- suppressWarnings(as.numeric(as.character(time_vec)))
  event_num <- suppressWarnings(as.numeric(as.character(event_vec)))
  
  # ---- 2) Audit event schema (must be {0,1,NA}) ----
  bad_event_idx <- which(!is.na(event_num) & !(event_num %in% c(0, 1)))
  if (length(bad_event_idx) > 0L) {
    bad_vals <- unique(event_num[bad_event_idx])
    stop(
      "FAIL__EVENT_SCHEMA: event contains values outside {0,1,NA}. Observed: ",
      paste(head(bad_vals, 20), collapse = ", "),
      call. = FALSE
    )
  }
  
  # ---- 3) Audit time schema EXACTLY as implemented in apply_survival_mask_detailed ----
  # Your function hard-stops on observed time that is non-finite OR negative (time < 0).
  bad_time_idx <- which(!is.na(time_num) & (!is.finite(time_num) | time_num < 0))
  
  message("Survival audit: bad_time (observed non-finite or negative) = ", length(bad_time_idx),
          " | bad_event (outside {0,1,NA}) = ", length(bad_event_idx))
  
  if (length(bad_time_idx) > 0L) {
    print(head(data.frame(
      row = bad_time_idx,
      time_raw  = as.character(time_vec[bad_time_idx]),
      time_num  = time_num[bad_time_idx],
      event_raw = as.character(event_vec[bad_time_idx]),
      event_num = event_num[bad_time_idx],
      stringsAsFactors = FALSE
    ), 30))
    stop("FAIL__TIME_SCHEMA: non-finite or negative survival times detected (time < 0).", call. = FALSE)
  }
  
  # ---- 4) Endpoint-scoped completeness (rows usable for this metric) ----
  # apply_survival_mask_detailed uses mask <- is.finite(time) & !is.na(event)
  # After passing schema guard above, this reduces to: time finite & event observed
  keep <- is.finite(time_num) & !is.na(event_num)
  
  df_sub_keep   <- df_sub[keep, , drop = FALSE]
  time_keep     <- time_num[keep]
  event_keep    <- event_num[keep]
  
  message("Survival subset kept (audit-only): ", sum(keep), " / ", length(keep),
          " rows (endpoint-scoped). Dropped: ", sum(!keep))
  
  # (Optional but recommended) canonicalize once (robust to factor/character)
  time_vec  <- suppressWarnings(as.numeric(as.character(time_vec)))
  event_vec <- suppressWarnings(as.numeric(as.character(event_vec)))
  
  # --- Predictors + numeric matrix ---
  preds <- get_predictor_cols(df_sub, CTAB)
  X <- build_numeric_X(df_sub, preds)
  
  # Ensure rownames(X) exist and match df_sub
  if (is.null(rownames(X)) || length(rownames(X)) != nrow(X)) {
    rownames(X) <- rownames(df_sub)
  }
  
  stopifnot(identical(rownames(X), rownames(df_sub)))
  
  # Optional sanity
  stopifnot(all(sub("-.*$", "", colnames(X)) == CTAB))
  assert_alignment(df_sub, X, time_vec, event_vec)
  
  # ---------------------------------------------------------------------------
  # RUNTIME CONTRACT CHECKS (TAR-compliant)
  # ---------------------------------------------------------------------------
  # Validates:
  #   (i) survival-only masking (endpoint-scoped cohort definition)
  #  (ii) post-mask row identity and predictor alignment (NO row deletion by predictors)
  # (iii) optional CoxNet/MTLR finite-matrix feasibility (column-first; TAR-locked V3)
  # ---------------------------------------------------------------------------
  
  # 1) Wrapper must exist and must NOT be the same body as the detailed function
  cat("apply_survival_mask_detailed:", exists("apply_survival_mask_detailed", mode = "function"), "\n")
  cat("apply_survival_mask:",         exists("apply_survival_mask", mode = "function"), "\n")
  cat("identical bodies:", identical(body(apply_survival_mask_detailed), body(apply_survival_mask)), "\n")
  
  stopifnot(exists("apply_survival_mask_detailed", mode = "function"))
  stopifnot(exists("apply_survival_mask", mode = "function"))
  stopifnot(!identical(body(apply_survival_mask_detailed), body(apply_survival_mask)))
  
  # 2) Wrapper must return logical(nrow(df_sub)) and agree with detailed$mask
  
  # --- Pipeline A (audit-only): mask over already-complete rows must be all TRUE ---
  m <- apply_survival_mask(df_sub = df_sub_keep, time = time_keep, event = event_keep)
  stopifnot(is.logical(m), length(m) == nrow(df_sub_keep))
  stopifnot(all(m))                       # all rows in df_sub_keep must pass the mask
  stopifnot(length(m) == nrow(df_sub_keep))
  
  # --- Pipeline B (canonical): compute mask on full df_sub, then materialize canonical cohort ---
  # IMPORTANT:
  #   df_m / time_m / event_m are the ONLY canonical endpoint-scoped cohort objects.
  #   They must never be overwritten or aliased to df_sub2 or any other name.
  
  masked_d <- apply_survival_mask_detailed(
    df_sub = df_sub,
    time   = time_vec,
    event  = event_vec
  )
  
  df_m    <- masked_d$df
  time_m  <- masked_d$time
  event_m <- masked_d$event
  
  # Canonical invariants (must always hold)
  stopifnot(
    is.data.frame(df_m),
    length(time_m)  == nrow(df_m),
    length(event_m) == nrow(df_m),
    identical(rownames(df_m), rownames(df_sub)[masked_d$mask]),
    nrow(df_m) == sum(masked_d$mask)
  )
  
  names(time_m)  <- rownames(df_m)
  names(event_m) <- rownames(df_m)
  
  # and df_m == df_sub[masked_d$mask, , drop = FALSE]
  stopifnot(nrow(df_m) == sum(masked_d$mask))
  stopifnot(identical(rownames(df_m), rownames(df_sub)[masked_d$mask]))
  
  cat("len(m) =", length(m), "\n")
  cat("len(masked_d$mask) =", length(masked_d$mask), "\n")
  cat("nrow(df_sub) =", nrow(df_sub), "\n")
  cat("nrow(df_sub_keep) =", nrow(df_sub_keep), "\n")
  cat("nrow(df_m) =", nrow(df_m), "\n")
  
  m_full <- apply_survival_mask(df_sub = df_sub, time = time_vec, event = event_vec)
  stopifnot(identical(m_full, masked_d$mask))
  
  # ---------------------------------------------------------------------------
  # Survival-masked objects (endpoint-scoped only; NO predictor-based row deletion)
  # ---------------------------------------------------------------------------
  masked <- masked_d
  
  stopifnot(
    is.list(masked),
    all(c("df", "time", "event", "mask") %in% names(masked))
  )
  
  df_m    <- masked$df
  time_m  <- masked$time
  event_m <- masked$event
  
  # Identity contract (rownames == sample) must hold after masking
  stopifnot(identical(rownames(df_m), df_m$sample))
  stopifnot(!anyDuplicated(df_m$sample))
  
  # ---------------------------------------------------------------------------
  # CRITICAL: row-align X to the endpoint-scoped cohort S_{c,m,d}
  # ---------------------------------------------------------------------------
  # X was built on df_sub; df_m is a row-subset of df_sub after survival masking.
  # TAR prohibits row deletion due to predictors, but REQUIRES using S_{c,m,d} rows.
  # Therefore, we subset X by rows only (no column filtering here).
  stopifnot(is.matrix(X), is.numeric(X))
  stopifnot(!is.null(rownames(X)))
  
  X_m <- X[rownames(df_m), , drop = FALSE]
  stopifnot(identical(rownames(X_m), rownames(df_m)))
  
  stopifnot(
    nrow(X_m) == nrow(df_m),
    length(time_m) == nrow(df_m),
    length(event_m) == nrow(df_m)
  )
  
  # ---------------------------------------------------------------------------
  # OPTIONAL: CoxNet/MTLR finite-matrix feasibility (column-first; TAR-locked V3)
  # ---------------------------------------------------------------------------
  # Only needed for algorithms that REQUIRE a fully finite numeric matrix.
  cat("make_X_finite_for_glmnet:", exists("make_X_finite_for_glmnet", mode = "function"), "\n")
  stopifnot(exists("make_X_finite_for_glmnet", mode = "function"))
  
  # --- AMENDED (minimal): μ-ladder escalation to avoid hard failure when μ=0.05 is too strict ---
  # Rationale:
  # - μ is NOT global; it must be local to (c,m,d).
  # - We select the smallest μ that yields feasibility, preserving TAR prohibitions.
  mu_ladder <- c(0.05, 0.075, 0.10, 0.125, 0.15, 0.20)
  
  finite_out <- NULL
  last_err <- NULL
  
  for (mu_try in mu_ladder) {
    cat(sprintf("Trying feasibility with mu=%.3f ...\n", mu_try))
    tmp <- try(
      make_X_finite_for_glmnet(
        X = X_m,
        mu = mu_try
        # optionally override locally:
        # tau_ladder = c(0.01, 0.05, 0.10, 0.20, 0.30),
        # gamma = 0.10
      ),
      silent = TRUE
    )
    if (!inherits(tmp, "try-error")) {
      finite_out <- tmp
      finite_out$mu_selected <- mu_try
      break
    } else {
      last_err <- tmp
    }
  }
  
  if (is.null(finite_out)) {
    stop(
      "Feasibility failed for all mu in mu_ladder within this (c,m,d). ",
      "This is a local data property (missingness mass). Last error: ",
      as.character(last_err),
      call. = FALSE
    )
  }
  
  X_glmnet <- finite_out$X
  dropped_features <- finite_out$dropped_features
  
  # Preserve row identity (rows were never dropped here)
  stopifnot(is.matrix(X_glmnet), is.numeric(X_glmnet))
  stopifnot(nrow(X_glmnet) == nrow(df_m))
  stopifnot(all(is.finite(X_glmnet)))
  
  rownames(X_glmnet) <- rownames(X_m)
  stopifnot(identical(rownames(X_glmnet), rownames(df_m)))
  
  message(sprintf(
    "Feasibility OK for (%s,%s,df008): mu=%.3f, tau=%.2f, retention=%.3f, M=%.4f",
    CTAB, METRIC, finite_out$mu_selected, finite_out$tau,
    finite_out$retention_ratio, finite_out$imputation_mass
  ))
  
    ####
    ####
    ####
    #### 
    #### 
    
    ###############################################################################
    # PHASE II — CoxNet/MTLR Finite-Matrix Feasibility (TAR-compliant, Local, Deterministic)
    ###############################################################################
    # SCOPE (NON-NEGOTIABLE):
    # This policy is applied ONLY within one execution unit:
    #   (cancer_type = c, metric = m, dfXXX = d, algorithm ∈ {CoxNet, MTLR})
    # and ONLY after endpoint-scoped survival masking has defined S_{c,m,d}.
    #
    # PURPOSE:
    # Ensure that algorithms requiring a fully finite numeric matrix (CoxNet/MTLR)
    # can be fit WITHOUT any predictor-driven row deletion and WITHOUT imposing any
    # global “complete-case across predictors/omics” constraint.
    #
    # DEFINITIONS (local, computed within S_{c,m,d}):
    # - Let X be the numeric predictor matrix aligned to S_{c,m,d}.
    # - Let p0 = ncol(X) (predictors available before feasibility repair).
    # - For each predictor j, define r_j as the fraction of non-finite entries in column j:
    #     r_j = mean(!is.finite(X[, j]))
    # - For a candidate threshold τ:
    #     * Drop features with r_j > τ
    #     * Deterministically impute missing values in features with 0 < r_j ≤ τ
    #
    # DETERMINISTIC IMPUTATION CONTRACT:
    # - Deterministic: uniquely determined by observed predictor values.
    # - Outcome-independent: MUST NOT use survival time/event or any performance metric.
    # - Model-independent: MUST NOT depend on fitted model parameters or CV folds.
    # - Local: computed only within S_{c,m,d}.
    # - Auditable: log τ chosen, retention ratio, and imputation mass.
    #
    # ACCEPTANCE CRITERIA (local; no global K predictors):
    # - Retention ratio: p_keep / p0 ≥ γ, with γ = 0.10 (start)
    # - Imputation mass: M ≤ μ, with μ = 0.05 (start)
    #   where M = (# imputed cells) / (nrow(X_keep) * ncol(X_keep))
    #
    # THRESHOLD LADDER (local escalation; smallest τ that passes is selected):
    #   τ ∈ {0.01, 0.05, 0.10, 0.20, 0.30}
    #
    # IMPORTANT PROHIBITIONS:
    # - NEVER drop samples due to predictor NA/NaN/Inf.
    # - NEVER intersect cohorts across endpoints.
    # - NEVER apply any global predictor intersection across cancers/omics.
    # - NEVER use stochastic imputation or outcome-aware imputation.
    ###############################################################################
    
    ###############################################################################
    ###############################################################################
    ###############################################################################
    # Phase II TAR feasibility loop: (c, m, d) over dfinput
    # - c: cancer_type
    # - m: metric/endpoint (e.g., OS/DSS/PFI/DFI)
    # - d: df file (e.g., df008.rds)
    #
    # Adds algorithm-specific feasibility gating:
    # - CoxNet uses μ-ladder adaptive gate
    # - Other algorithms should have their own feasibility checks (dynamic feasibility)
    ###############################################################################
    setwd("D:/Pré-artigo 5-optosis model/Machine learning CancerRCDShiny_prediction/Enrique version/PHASE_II")
    
    # setwd("~/students/aluno0549-6")
    
    suppressPackageStartupMessages({
      library(rio)
      library(dplyr)
      library(survival)
      library(glmnet)
      
      #---------------------------
      # PARALLELIZATION PACKAGES (Windows-safe)
      #---------------------------
      library(parallel)
      library(future)
      library(future.apply)
    })
    
    #---------------------------
    # PARALLELIZATION SETTINGS
    # - Detect available cores
    # - Leave 2 cores free during the run (as requested)
    # - Use multisession for Windows compatibility
    #---------------------------
    n_cores_total <- parallel::detectCores(logical = TRUE)
    n_workers <- max(1L, n_cores_total - 2L)
    
    message("Total cores detected: ", n_cores_total,
            " | Using workers: ", n_workers,
            " | Reserved free cores: 2")
    
    future::plan(future::multisession, workers = n_workers)
    
    # Deterministic parallel RNG (future_lapply requires this for reproducibility)
    RNGkind("L'Ecuyer-CMRG")
    set.seed(1)
    
    #---------------------------
    # PATCH: Parallelization audit probe (TAR-friendly)
    # - Verifies that multisession spawned distinct worker processes
    # - Captures worker PIDs + hostnames
    # - Writes a separate audit file
    # - Stops early if plan is not multisession or if no parallelism is observed (when expected)
    #---------------------------
    audit_future_parallelization <- function(n_workers_expected,
                                             outfile = "parallelization_audit.tsv",
                                             verbose = TRUE) {
      # Capture plan/strategy (human-readable tag; best-effort)
      strat <- tryCatch({
        paste(deparse(substitute(future::plan())), collapse = "")
      }, error = function(e) NA_character_)
      
      # What future thinks about workers
      n_workers_seen <- tryCatch(future::nbrOfWorkers(), error = function(e) NA_integer_)
      
      # -------------------------------------------------------------------------
      # PATCH: persist actual plan class
      # - This MUST be computed here (do NOT rely on an external variable)
      # -------------------------------------------------------------------------
      plan_class_local <- tryCatch(as.character(class(future::plan())[1]), error = function(e) NA_character_)
  
      
      suppressPackageStartupMessages({
        library(data.table)
        library(dplyr)
        library(readr)
      })
      
      as_feas_log_row <- function(
      cancer_type,
      metric,
      df_file,
      algorithm,
      feasibility_code,
      n, p0, p_used,
      feature_retention_ratio,
      sample_retention_ratio,
      E,
      nnz_selected = NA_integer_,
      mu_selected  = NA_real_,
      mu_step      = NA_integer_,
      dropped_features_count = NA_integer_,
      p_min_tried = NA_integer_,
      mu_attempted_max = NA_integer_,
      last_step_tried = NA_character_,
      last_reason = NA_character_,
      N_min = 50L,
      E_min = 20L,
      n_time_nonfinite = NA_integer_,
      n_status_nonfinite = NA_integer_,
      n_time_negative = NA_integer_,
      data_fail_reasons = NA_character_,
      n_raw = NA_integer_,
      E_raw = NA_integer_,
      n_clean = NA_integer_,
      E_clean = NA_integer_,
      n_dropped_invalid = NA_integer_,
      n_drop_time_nonfinite_row = NA_integer_,
      n_drop_status_nonfinite_row = NA_integer_,
      n_drop_time_negative_row = 0L,
      worker_pid = as.integer(Sys.getpid()),
      worker_host = as.character(Sys.info()[["nodename"]]),
      elapsed_sec = NA_real_,
      error_message = NA_character_,
      PHASE_III_logic = NA_character_
      ) {
        
        if (length(data_fail_reasons) > 1L)
          data_fail_reasons <- paste(sort(unique(data_fail_reasons)), collapse = ";")
        
        if (length(last_step_tried) > 1L)
          last_step_tried <- paste(last_step_tried, collapse = ";")
        
        if (length(last_reason) > 1L)
          last_reason <- paste(last_reason, collapse = ";")
        
        data.table::data.table(
          cancer_type = as.character(cancer_type),
          metric      = as.character(metric),
          df_file     = as.character(df_file),
          algorithm   = as.character(algorithm),
          feasibility_code = as.character(feasibility_code),
          
          n    = as.integer(n),
          p0   = as.integer(p0),
          p_used = as.integer(p_used),
          
          feature_retention_ratio = as.numeric(feature_retention_ratio),
          sample_retention_ratio  = as.numeric(sample_retention_ratio),
          
          E = as.integer(E),
          
          nnz_selected = as.integer(nnz_selected),
          mu_selected  = as.numeric(mu_selected),
          mu_step      = as.integer(mu_step),
          
          dropped_features_count = as.integer(dropped_features_count),
          p_min_tried = as.integer(p_min_tried),
          mu_attempted_max = as.integer(mu_attempted_max),
          
          last_step_tried = as.character(last_step_tried),
          last_reason     = as.character(last_reason),
          
          N_min = as.integer(N_min),
          E_min = as.integer(E_min),
          
          n_time_nonfinite   = as.integer(n_time_nonfinite),
          n_status_nonfinite = as.integer(n_status_nonfinite),
          n_time_negative    = as.integer(n_time_negative),
          
          data_fail_reasons = as.character(data_fail_reasons),
          
          n_raw   = as.integer(n_raw),
          E_raw   = as.integer(E_raw),
          n_clean = as.integer(n_clean),
          E_clean = as.integer(E_clean),
          
          n_dropped_invalid          = as.integer(n_dropped_invalid),
          n_drop_time_nonfinite_row  = as.integer(n_drop_time_nonfinite_row),
          n_drop_status_nonfinite_row= as.integer(n_drop_status_nonfinite_row),
          n_drop_time_negative_row   = as.integer(n_drop_time_negative_row),
          
          worker_pid  = as.integer(worker_pid),
          worker_host = as.character(worker_host),
          elapsed_sec = as.numeric(elapsed_sec),
          
          error_message = as.character(error_message),
          PHASE_III_logic = as.character(PHASE_III_logic)
        )
      }
      
      # rows_list: list of 1-row tables created ONLY via as_feas_log_row(...)
      # feas_log <- dplyr::bind_rows(rows_list)  # OK if rows are already typed
      feas_log <- data.table::rbindlist(rows_list, use.names = TRUE, fill = TRUE)
      
      # Canonical persistence
      saveRDS(feas_log, "CoxNet_phaseII_feasibility_log.rds")
      
      # Spawn tiny futures to learn PIDs/hosts
      probe <- future.apply::future_lapply(
        X = seq_len(max(1L, n_workers_expected)),
        FUN = function(i) {
          list(
            i = i,
            pid = Sys.getpid(),
            host = Sys.info()[["nodename"]],
            r_version = R.version.string
          )
        },
        future.seed = TRUE
      )
      
      pid_vec <- vapply(probe, function(x) x$pid, integer(1))
      host_vec <- vapply(probe, function(x) x$host, character(1))
      uniq_pid <- length(unique(pid_vec))
      uniq_host <- length(unique(host_vec))
      
      audit_df <- data.frame(
        timestamp = as.character(Sys.time()),
        future_strategy = plan_class_local,
        # -------------------------------------------------------------------------
        # PATCH: persist actual plan class (explicit column)
        # -------------------------------------------------------------------------
        plan_class = plan_class_local,
        n_workers_expected = as.integer(n_workers_expected),
        n_workers_seen = as.integer(n_workers_seen),
        unique_worker_pids = as.integer(uniq_pid),
        unique_worker_hosts = as.integer(uniq_host),
        pids = paste(pid_vec, collapse = ","),
        hosts = paste(host_vec, collapse = ","),
        stringsAsFactors = FALSE
      )
      
      # Persist audit (TAR artifact)
      rio::export(audit_df, outfile)
      
      if (verbose) {
        message("Parallel audit: strategy=", audit_df$future_strategy,
                " | workers_seen=", audit_df$n_workers_seen,
                " | unique_pids=", audit_df$unique_worker_pids,
                " | audit_file=", outfile)
      }
      
      # Strict assertion: if user requested >1 worker, we expect >1 unique PID
      # (If expected workers is 1, we do not enforce.)
      if (is.finite(n_workers_expected) && n_workers_expected > 1L) {
        if (!is.finite(uniq_pid) || uniq_pid < 2L) {
          warning(
            "Parallelization audit warning: expected >1 worker process, but observed unique_pids=",
            uniq_pid,
            ". Proceeding anyway; feasibility loop will still run (may be sequential fallback).",
            call. = FALSE
          )
        }
      }
      
      audit_df
    }
    
    # Run the audit once before the expensive feasibility loop
    parallel_audit <- audit_future_parallelization(
      n_workers_expected = n_workers,
      outfile = "parallelization_audit.tsv",
      verbose = TRUE
    )
    
    stopifnot(all(c("cancer_type","metric","df") %in% colnames(dfinput)))
    
    #---------------------------
    # 1) μ-ladder gate (paste EXACT block you already locked)
    #    (Included here verbatim in functional form; do not alter logic)
    #---------------------------
    
    .nzv_mask <- function(X, var_eps = 1e-12) {
      v <- apply(X, 2, stats::var, na.rm = TRUE)
      keep <- is.finite(v) & (v > var_eps)
      keep[is.na(keep)] <- FALSE
      keep
    }
    
    .collinear_prune <- function(X, cor_thresh = 0.999) {
      p <- ncol(X)
      if (p <= 1) return(rep(TRUE, p))
      C <- suppressWarnings(stats::cor(X, use = "pairwise.complete.obs"))
      diag(C) <- 0
      keep <- rep(TRUE, p)
      for (j in seq_len(p)) {
        if (!keep[j]) next
        hit <- which(abs(C[j, ]) >= cor_thresh)
        if (length(hit) > 0) keep[hit] <- FALSE
      }
      keep
    }
    
    .univariate_cox_score <- function(time, status, X) {
      s <- rep(0, ncol(X))
      y <- survival::Surv(time, status)
      for (j in seq_len(ncol(X))) {
        fit <- tryCatch(survival::coxph(y ~ X[, j]), error = function(e) NULL)
        if (!is.null(fit)) {
          z <- tryCatch(summary(fit)$coefficients[,"z"], error = function(e) NA_real_)
          if (is.finite(z)) s[j] <- abs(z)
        }
      }
      s
    }
    
    check_data_feasible <- function(time, status, X, E_min = 20, N_min = 50) {
      n <- length(time)
      e <- sum(status == 1, na.rm = TRUE)
      if (!is.numeric(time) || !is.numeric(status)) return(FALSE)
      if (any(!is.finite(time)) || any(!is.finite(status))) return(FALSE)
      if (any(time < 0)) return(FALSE)
      if (n < N_min) return(FALSE)
      if (e < E_min) return(FALSE)
      if (!is.matrix(X)) return(FALSE)
      if (nrow(X) != n) return(FALSE)
      if (ncol(X) < 2) return(FALSE)
      TRUE
    }
    
    check_fit_feasible <- function(fit, X, eps = 1e-12) {
      if (is.null(fit)) return(FALSE)
      beta <- as.matrix(stats::coef(fit))
      nz <- (beta != 0)
      if (any(nz) && !all(is.finite(beta[nz]))) return(FALSE)
      TRUE
    }
    
    risk_is_degenerate <- function(risk, eps = 1e-12) {
      (!all(is.finite(risk))) || (stats::var(risk) <= eps)
    }
    
    try_coxnet <- function(time, status, X,
                           alpha, nlambda = 100, lambda_min_ratio = 1e-3,
                           standardize = TRUE, maxit = 1e5) {
      y <- survival::Surv(time, status)
      fit <- tryCatch(
        glmnet::glmnet(
          x = X, y = y, family = "cox",
          alpha = alpha,
          nlambda = nlambda,
          lambda.min.ratio = lambda_min_ratio,
          standardize = standardize,
          maxit = maxit
        ),
        error = function(e) NULL
      )
      fit
    }
    
    #---------------------------
    # PATCH: Data-feasibility diagnostics (additive; does NOT change feasibility logic)
    # - Returns explicit counts and a machine-readable reason code set
    # - Mirrors the exact conditions checked by check_data_feasible()
    #---------------------------
    diagnose_data_feasibility <- function(time, status, X, E_min = 20, N_min = 50) {
      
      # Defensive coercions (diagnostics only; does NOT alter upstream objects)
      n <- length(time)
      e <- sum(status == 1, na.rm = TRUE)
      
      # Counts for finite-ness and positivity
      n_time_nonfinite   <- sum(!is.finite(time))
      n_status_nonfinite <- sum(!is.finite(status))
      n_time_negative <- sum(is.finite(time) & (time < 0))
      
      # Matrix structure diagnostics
      is_matrix <- is.matrix(X)
      nrow_X <- if (is_matrix) nrow(X) else NA_integer_
      ncol_X <- if (is_matrix) ncol(X) else NA_integer_
      
      reasons <- character(0)
      
      if (!is.numeric(time))  reasons <- c(reasons, "TIME_NOT_NUMERIC")
      if (!is.numeric(status)) reasons <- c(reasons, "STATUS_NOT_NUMERIC")
      
      if (n_time_nonfinite > 0 || n_status_nonfinite > 0) {
        reasons <- c(reasons, "NONFINITE_TIME_OR_STATUS")
      }
      
      if (n_time_negative > 0) {
        reasons <- c(reasons, "NEGATIVE_TIME")
      }
      
      if (n < N_min) {
        reasons <- c(reasons, "LOW_N")
      }
      
      if (e < E_min) {
        reasons <- c(reasons, "LOW_EVENTS")
      }
      
      if (!is_matrix) {
        reasons <- c(reasons, "X_NOT_MATRIX")
      } else {
        if (!is.na(nrow_X) && nrow_X != n) reasons <- c(reasons, "X_NROW_MISMATCH")
        if (!is.na(ncol_X) && ncol_X < 2) reasons <- c(reasons, "LOW_P")
      }
      
      # Deduplicate deterministically
      reasons <- unique(reasons)
      
      list(
        feasible = check_data_feasible(time, status, X, E_min = E_min, N_min = N_min),
        reasons = reasons,
        reasons_str = if (length(reasons) == 0) NA_character_ else paste(reasons, collapse = ";"),
        n = as.integer(n),
        e = as.integer(e),
        E_min = as.integer(E_min),
        N_min = as.integer(N_min),
        n_time_nonfinite = as.integer(n_time_nonfinite),
        n_status_nonfinite = as.integer(n_status_nonfinite),
        n_time_negative = as.integer(n_time_negative),
        is_matrix = as.logical(is_matrix),
        nrow_X = as.integer(nrow_X),
        ncol_X = as.integer(ncol_X)
      )
    }
    
    #---------------------------
    # PATCH: Audit-preserving survival-row cleaning (additive)
    # - Drops only invalid survival rows:
    #     * non-finite time and/or status (NA/NaN/Inf)
    #     * negative survival time (time < 0)
    # - POLICY ALIGNMENT (Option A):
    #     * Phase II survival-time schema permits time == 0 (time >= 0 allowed when observed)
    #     * Therefore, ONLY negative times are schema-invalid at the row level
    # - DOES NOT alter thresholds or feasibility logic; it only prevents brittle
    #   "any bad row kills stratum" behavior by removing schema-invalid survival rows
    # - Returns cleaned (time,status,X) + explicit drop counts for logging/audit
    #---------------------------
    clean_survival_rows_with_audit <- function(time, status, X) {
      
      stopifnot(length(time) == length(status))
      n_raw <- length(time)
      
      # Define row-validity exactly aligned with check_data_feasible() row-level requirements
      ok_time_finite   <- is.finite(time)
      ok_status_finite <- is.finite(status)
      
      # POLICY ALIGNMENT:
      # - Phase II time schema is non-negative (time >= 0) when observed.
      # - Therefore, only negative times are invalid at the schema level.
      ok_time_nonneg   <- ok_time_finite & (time >= 0)
      
      keep <- ok_time_finite & ok_status_finite & ok_time_nonneg
      
      n_drop_time_nonfinite   <- sum(!ok_time_finite)
      n_drop_status_nonfinite <- sum(!ok_status_finite)
      n_drop_time_negative    <- sum(ok_time_finite & (time < 0))
      
      n_keep <- sum(keep)
      
      # Apply cleaning consistently to X if possible
      if (is.matrix(X)) {
        X_clean <- X[keep, , drop = FALSE]
      } else {
        X_clean <- X
      }
      
      list(
        time_clean = time[keep],
        status_clean = status[keep],
        X_clean = X_clean,
        n_raw = as.integer(n_raw),
        n_clean = as.integer(n_keep),
        n_dropped_total = as.integer(n_raw - n_keep),
        n_drop_time_nonfinite = as.integer(n_drop_time_nonfinite),
        n_drop_status_nonfinite = as.integer(n_drop_status_nonfinite),
        n_drop_time_negative = as.integer(n_drop_time_negative)
      )
    }  
    
    mu_ladder_coxnet_gate <- function(time, status, X,
                                      E_min = 20, N_min = 50,
                                      K_cap = 2000,
                                      verbose = FALSE) {
      
      # -------------------------------------------------------------------------
      # Guard p0 initialization so this function NEVER errors before
      # check_data_feasible() runs. No behavioral change for valid matrix X.
      # -------------------------------------------------------------------------
      p0_local <- if (is.matrix(X)) as.integer(ncol(X)) else NA_integer_
      
      out <- list(
        feasible = FALSE,
        mu_level = NA_integer_,
        reason = NULL,
        fit = NULL,
        X_used = NULL,
        policy = NULL,
        
        p0 = p0_local,
        p_used = NA_integer_,
        
        p_min_tried = p0_local,
        mu_attempted_max = NA_integer_,
        last_step_tried = NA_character_,
        last_reason = NA_character_,
        
        N_min = as.integer(N_min),
        E_min = as.integer(E_min),
        n_time_nonfinite = NA_integer_,
        n_status_nonfinite = NA_integer_,
        n_time_negative = NA_integer_,
        data_fail_reasons = NA_character_
      )
      
      if (!check_data_feasible(time, status, X, E_min, N_min)) {
        
        # -------------------------------------------------------------------------
        # PATCH: populate explicit diagnostics (additive; feasibility logic unchanged)
        # NOTE: diagnose_data_feasibility() must also be updated under Option A to:
        #   - compute n_time_negative = sum(is.finite(time) & (time < 0))
        #   - emit reason label "NEGATIVE_TIME" (not "NONPOSITIVE_TIME")
        # -------------------------------------------------------------------------
        diag <- diagnose_data_feasibility(time, status, X, E_min = E_min, N_min = N_min)
        
        out$reason <- "DATA_INFEASIBLE"
        out$p_used <- NA_integer_
        out$mu_attempted_max <- NA_integer_
        out$last_step_tried <- NA_character_
        out$last_reason <- "DATA_INFEASIBLE"
        
        # -------------------------------------------------------------------------
        # PATCH: fill the diagnostic fields for the log
        # -------------------------------------------------------------------------
        out$N_min <- as.integer(N_min)
        out$E_min <- as.integer(E_min)
        out$n_time_nonfinite <- as.integer(diag$n_time_nonfinite)
        out$n_status_nonfinite <- as.integer(diag$n_status_nonfinite)
        out$n_time_negative <- as.integer(diag$n_time_negative)
        out$data_fail_reasons <- as.character(diag$reasons_str)
        
        return(out)
      }
      
      ladder <- list(
        list(mu = 0L, step = "BASELINE",
             transform = function(X, time, status) X,
             fit = function(time, status, X) try_coxnet(time, status, X, alpha = 1, nlambda = 100, lambda_min_ratio = 1e-4)),
        list(mu = 1L, step = "FILTER_NZV",
             transform = function(X, time, status) {
               keep <- .nzv_mask(X, var_eps = 1e-12)
               X[, keep, drop = FALSE]
             },
             fit = function(time, status, X) try_coxnet(time, status, X, alpha = 1, nlambda = 80, lambda_min_ratio = 1e-3)),
        list(mu = 2L, step = "CONSTRAIN_PATH_ELASTIC",
             transform = function(X, time, status) X,
             fit = function(time, status, X) try_coxnet(time, status, X, alpha = 0.5, nlambda = 60, lambda_min_ratio = 1e-2)),
        list(mu = 3L, step = "PRESCREEN_TOPK",
             transform = function(X, time, status) {
               e <- sum(status == 1, na.rm = TRUE)
               K <- min(ncol(X), K_cap, 20L * e)
               scores <- .univariate_cox_score(time, status, X)
               ord <- order(scores, decreasing = TRUE)
               X[, ord[seq_len(K)], drop = FALSE]
             },
             fit = function(time, status, X) {
               keep2 <- .collinear_prune(X, cor_thresh = 0.999)
               X2 <- X[, keep2, drop = FALSE]
               try_coxnet(time, status, X2, alpha = 1, nlambda = 80, lambda_min_ratio = 1e-3)
             }),
        list(mu = 4L, step = "RIDGE_STABILIZE",
             transform = function(X, time, status) X,
             fit = function(time, status, X) try_coxnet(time, status, X, alpha = 0, nlambda = 80, lambda_min_ratio = 1e-2))
      )
      
      for (L in ladder) {
        
        # PATCH: track the maximum μ attempted and last step label seen
        out$mu_attempted_max <- as.integer(L$mu)
        out$last_step_tried <- as.character(L$step)
        
        Xk <- L$transform(X, time, status)
        
        # PATCH: record smallest feature count encountered (even if we fail later)
        # Robust to p_min_tried being NA on entry
        if (is.matrix(Xk) && ncol(Xk) >= 1) {
          out$p_min_tried <- if (is.na(out$p_min_tried)) {
            as.integer(ncol(Xk))
          } else {
            as.integer(min(out$p_min_tried, ncol(Xk)))
          }
        }
        
        if (!is.matrix(Xk) || ncol(Xk) < 2) {
          out$last_reason <- "INSUFFICIENT_P_AFTER_TRANSFORM"
          if (verbose) message("μ", L$mu, " ", L$step, ": insufficient predictors after transform.")
          next
        }
        
        fit <- L$fit(time, status, Xk)
        if (!check_fit_feasible(fit, Xk)) {
          out$last_reason <- "FIT_INFEASIBLE"
          if (verbose) message("μ", L$mu, " ", L$step, ": fit infeasible.")
          next
        }
        
        beta <- as.matrix(stats::coef(fit))
        nnz <- colSums(beta != 0)
        idx <- if (any(nnz > 0)) which(nnz > 0)[1] else ncol(beta)
        
        risk <- drop(Xk %*% beta[, idx])
        if (risk_is_degenerate(risk)) {
          out$last_reason <- "DEGENERATE_RISK"
          if (verbose) message("μ", L$mu, " ", L$step, ": degenerate risk.")
          next
        }
        
        out$feasible <- TRUE
        out$mu_level <- L$mu
        out$reason <- "PASS"
        out$fit <- fit
        out$X_used <- Xk
        out$policy <- list(step = L$step, lambda_index = idx, nnz = nnz[idx])
        out$p_used <- ncol(Xk)
        
        # PATCH: success reason bookkeeping
        out$last_reason <- "PASS"
        
        return(out)
      }
      
      out$reason <- "ALL_MU_EXHAUSTED"
      
      # ---------------------------------------------------------------------------
      # PATCH: for μ-exhaustion, report p_used as best-effort compression
      # (smallest feature count reached by transforms), not the original p0.
      # This makes 'retention' meaningful even on failure.
      # ---------------------------------------------------------------------------
      out$p_used <- as.integer(out$p_min_tried)
      out$last_reason <- if (!is.null(out$last_reason) && nzchar(out$last_reason)) out$last_reason else "ALL_MU_EXHAUSTED"
      out
    }
      
    #---------------------------
    # 2) Algorithm-specific feasibility code framework (dynamic feasibility)
    #---------------------------
    feasibility_code_coxnet <- function(gate_obj) {
      # Explicit, algorithm-specific codes (do not reuse across algorithms)
      if (isTRUE(gate_obj$feasible)) return("COXNET_PASS")
      if (identical(gate_obj$reason, "DATA_INFEASIBLE")) return("COXNET_FAIL_DATA")
      if (identical(gate_obj$reason, "ALL_MU_EXHAUSTED")) return("COXNET_FAIL_MU_EXHAUSTED")
      "COXNET_FAIL_OTHER"
    }
    
    # Placeholder stubs to illustrate "dynamic feasibility applies to specific ML algorithms"
    # These are intentionally minimal; each algorithm must define its OWN code space and checks.
    feasibility_code_rsf <- function(n, M, p) {
      if (n < 50) return("RSF_FAIL_LOW_N")
      if (M < 20) return("RSF_FAIL_LOW_EVENTS")
      if (p < 2)  return("RSF_FAIL_LOW_P")
      "RSF_PASS"
    }
    
    #---------------------------
    # 3) Stratum extractor: build (time, status, X) from a loaded dfXXX object
    #---------------------------
    # IMPORTANT: Replace this with your project’s canonical schema mapping.
    # This function must:
    #   - isolate rows for cancer_type and endpoint (metric)
    #   - define time/status variables for that endpoint
    #   - build predictor-only matrix X (NO IDs, NO survival vars)
    extract_stratum_data <- function(df_obj, cancer_type, metric,
                                     time_col_map, status_col_map,
                                     cancer_col = "type",
                                     drop_cols_regex = "^(patient|sample|id|barcode)$") {
      
      stopifnot(is.data.frame(df_obj))
      
      if (!cancer_col %in% colnames(df_obj)) {
        stop("extract_stratum_data: missing cancer_type column: ", cancer_col, call. = FALSE)
      }
      
      df_s <- df_obj[df_obj[[cancer_col]] == cancer_type, , drop = FALSE]
      
      # PATCH: explicit empty-stratum guard (audit-friendly)
      if (nrow(df_s) == 0L) {
        stop("extract_stratum_data: no rows for ", cancer_col, "='", cancer_type, "'.", call. = FALSE)
      }
      
      tcol <- time_col_map[[metric]]
      scol <- status_col_map[[metric]]
      if (is.null(tcol) || is.null(scol)) {
        stop("No time/status mapping for metric='", metric, "'.", call. = FALSE)
      }
      if (!(tcol %in% colnames(df_s)) || !(scol %in% colnames(df_s))) {
        stop("Missing time/status columns for metric='", metric, "': ", tcol, ", ", scol, call. = FALSE)
      }
      
    # ---- survival coercion (factor/character safe) ----
    time   <- suppressWarnings(as.numeric(as.character(df_s[[tcol]])))
    status <- suppressWarnings(as.numeric(as.character(df_s[[scol]])))
    
    # ---- FAIL-FAST survival schema assertions (TAR/Phase II contract) ----
    # status must be {0,1,NA} only
    if (!all(is.na(status) | status %in% c(0, 1))) {
      bad <- unique(status[!is.na(status) & !(status %in% c(0, 1))])
      stop(
        "extract_stratum_data: invalid status values for metric='", metric,
        "' (expected {0,1,NA}). Observed: ", paste(head(bad, 20), collapse = ", "),
        call. = FALSE
      )
    }
    
    # time must be >= 0 when observed (schema-level); negative is invalid
    time <- assert_time_schema_nonneg(time, metric = metric)
    
    # Predictor-only frame: drop survival vars, cancer_type, and obvious IDs
    drop_explicit <- c(cancer_col, tcol, scol)
    keep_cols <- setdiff(colnames(df_s), drop_explicit)
    keep_cols <- keep_cols[!grepl(drop_cols_regex, keep_cols, ignore.case = TRUE)]
    Xdf <- df_s[, keep_cols, drop = FALSE]
    
    # -------------------------------------------------------------------------
    # Phase II canonical predictor construction (token-aware; glmnet-safe)
    # -------------------------------------------------------------------------
    pattern <- paste0("^", cancer_type, "-")
    preds <- colnames(Xdf)
    preds <- preds[grepl(pattern, preds)]
    
    if (length(preds) == 0L) {
      stop(
        "extract_stratum_data: no predictors found with prefix ",
        shQuote(paste0(cancer_type, "-")),
        " after exclusions.",
        call. = FALSE
      )
    }
    
    # Ensure row identity exists (required by build_numeric_X alignment propagation)
    df_s <- assert_row_ids(df_s)  # sets rownames(df_s) = sample
    
    # ⬇️ ADD THESE TWO LINES (audit-only, no behavioral change)
    names(time)   <- rownames(df_s)
    names(status) <- rownames(df_s)
    
    stopifnot(length(time) == nrow(df_s))
    stopifnot(length(status) == nrow(df_s))
    
    # Build X from the FULL df_s using the selected predictor names
    X <- build_numeric_X(df_sub = df_s, preds = preds)
    
    stopifnot(nrow(X) == nrow(df_s))
        
        # Optional: drop columns that are entirely non-finite AFTER token coercion
        ok <- colSums(is.finite(X)) > 0
        X <- X[, ok, drop = FALSE]
        
        stopifnot(is.matrix(X))
        stopifnot(nrow(X) == length(time))
        stopifnot(nrow(X) == length(status))
        stopifnot(identical(rownames(X), rownames(df_s)))
        
        if (!is.matrix(X) || ncol(X) < 2L) {
          stop("extract_stratum_data: insufficient usable predictors after token-safe coercion.", call. = FALSE)
        }
        
        list(time = time, status = status, X = X)
        }
    
    #---------------------------
    # 4) Maps for endpoints (EXAMPLE; adjust to your naming)
    #---------------------------
    # Use your project’s actual column names for OS/PFI/DFI/DSS.
    time_col_map <- list(
      OS  = "OS.time",
      DSS = "DSS.time",
      DFI = "DFI.time",
      PFI = "PFI.time"
    )
    status_col_map <- list(
      OS  = "OS",
      DSS = "DSS",
      DFI = "DFI",
      PFI = "PFI"
    )
    
    #---------------------------
    # 5) Logging table (per stratum)
    #---------------------------
    log_rows <- list()
    
    #---------------------------
    # 6) Main (c,m,d) loop
    #---------------------------
    unique_strata <- dfinput %>%
      distinct(cancer_type, metric, df) %>%
      arrange(cancer_type, metric, df)
    
    message("Unique strata (cancer_type × metric × df): ", nrow(unique_strata))
    if (nrow(unique_strata) == 0L) {
      stop("unique_strata is empty. No strata to evaluate. dfinput may be empty or filtered upstream.", call. = FALSE)
    }
    
    #---------------------------
    # PARALLELIZATION PATCH:
    # - Define DF_ROOT once (avoid setwd in workers)
    # - Wrap the original loop body into a worker function
    # - Execute with future_lapply, leaving 2 cores free
    #---------------------------
    DF_ROOT <- "D:/Pré-artigo 5-optosis model/Machine learning CancerRCDShiny_prediction/Enrique version/PHASE_II/dfXXX_series"
    
    run_one_stratum <- function(i, unique_strata, DF_ROOT,
                                time_col_map, status_col_map) {
      ctype    <- unique_strata$cancer_type[i]
      metric_i <- unique_strata$metric[i]   # PATCH: avoid collision with τ feasibility concept
      dff      <- unique_strata$df[i]
      
      stopifnot(is.character(metric_i), length(metric_i) == 1L, nzchar(metric_i))
      
      # -------------------------------------------------------------------------
      # PATCH: Per-stratum parallel provenance (TAR-friendly)
      # - Captures which worker executed this stratum + wall time
      # - Proves the loop actually ran in parallel (not only that workers exist)
      # -------------------------------------------------------------------------
      worker_pid  <- as.integer(Sys.getpid())
      worker_host <- as.character(Sys.info()[["nodename"]])
      t0 <- proc.time()[["elapsed"]]
    
      # Load dfXXX object
      # NOTE: set your root path; this assumes df files are directly readable.
      # PATCH: do NOT setwd inside workers; resolve full path deterministically
      df_path <- file.path(DF_ROOT, dff)
      
      load_err <- NULL
      df_obj <- tryCatch(
        readRDS(df_path),
        error = function(e) { load_err <<- conditionMessage(e); NULL }
      )
      
      if (is.null(df_obj)) {
        
        return(as_feas_log_row(
          cancer_type = ctype,
          metric      = metric_i,
          df_file     = dff,
          algorithm   = "CoxNet",
          feasibility_code = "LOAD_FAIL",
          
          n = NA_integer_,
          p0 = NA_integer_,
          p_used = NA_integer_,
          
          feature_retention_ratio = NA_real_,
          sample_retention_ratio  = NA_real_,
          
          E = NA_integer_,
          
          nnz_selected = NA_integer_,
          
          # IMPORTANT: schema-consistent types
          mu_selected = NA_real_,     # double, not integer
          mu_step     = NA_integer_,  # integer, not character
          
          dropped_features_count = NA_integer_,
          
          p_min_tried      = NA_integer_,
          mu_attempted_max = NA_integer_,
          last_step_tried  = NA_character_,
          last_reason      = NA_character_,
          
          N_min = NA_integer_,
          E_min = NA_integer_,
          n_time_nonfinite   = NA_integer_,
          n_status_nonfinite = NA_integer_,
          n_time_negative    = NA_integer_,
          data_fail_reasons  = NA_character_,
          
          n_raw   = NA_integer_,
          E_raw   = NA_integer_,
          n_clean = NA_integer_,
          E_clean = NA_integer_,
          
          n_dropped_invalid = NA_integer_,
          n_drop_time_nonfinite_row   = NA_integer_,
          n_drop_status_nonfinite_row = NA_integer_,
          n_drop_time_negative_row    = NA_integer_,
          
          worker_pid  = as.integer(worker_pid),
          worker_host = as.character(worker_host),
          elapsed_sec = as.numeric(proc.time()[["elapsed"]] - t0),
          
          error_message = if (!is.null(load_err)) paste0("readRDS error: ", load_err) else "readRDS returned NULL (unknown failure).",
          PHASE_III_logic = NA_character_
        ))
      }
      
          # Extract stratum
      stratum <- tryCatch(
        extract_stratum_data(df_obj, ctype, metric_i, time_col_map, status_col_map, cancer_col = "type"),
        error = function(e) list(error = conditionMessage(e))
      )
  
      if (!is.null(stratum$error)) {
        
        return(as_feas_log_row(
          cancer_type = ctype,
          metric      = metric_i,
          df_file     = dff,
          algorithm   = "CoxNet",
          feasibility_code = "EXTRACT_FAIL",
          
          n = NA_integer_,
          p0 = NA_integer_,
          p_used = NA_integer_,
          
          feature_retention_ratio = NA_real_,
          sample_retention_ratio  = NA_real_,
          
          E = NA_integer_,
          
          nnz_selected = NA_integer_,
          
          # IMPORTANT: schema-consistent types
          mu_selected = NA_real_,     # double
          mu_step     = NA_integer_,  # integer
          
          dropped_features_count = NA_integer_,
          
          p_min_tried      = NA_integer_,
          mu_attempted_max = NA_integer_,
          last_step_tried  = NA_character_,
          last_reason      = NA_character_,
          
          N_min = NA_integer_,
          E_min = NA_integer_,
          n_time_nonfinite   = NA_integer_,
          n_status_nonfinite = NA_integer_,
          n_time_negative    = NA_integer_,
          data_fail_reasons  = NA_character_,
          
          n_raw   = NA_integer_,
          E_raw   = NA_integer_,
          n_clean = NA_integer_,
          E_clean = NA_integer_,
          
          n_dropped_invalid = NA_integer_,
          n_drop_time_nonfinite_row   = NA_integer_,
          n_drop_status_nonfinite_row = NA_integer_,
          n_drop_time_negative_row    = NA_integer_,
          
          worker_pid  = as.integer(worker_pid),
          worker_host = as.character(worker_host),
          elapsed_sec = as.numeric(proc.time()[["elapsed"]] - t0),
          
          error_message = as.character(stratum$error),
          PHASE_III_logic = NA_character_
        ))
      }    
      
      time_vec   <- stratum$time
      status_vec <- stratum$status
      X          <- stratum$X
    
      # -------------------------------------------------------------------------
      # PATCH (audit-preserving cleaning):
      # - Compute raw counts for logging
      # - Drop invalid survival rows deterministically
      # - Recompute counts post-cleaning
      # - Proceed to CoxNet gate with cleaned data
      # -------------------------------------------------------------------------
      n_raw <- length(time_vec)
      E_raw <- sum(status_vec == 1, na.rm = TRUE)
      
      clean <- clean_survival_rows_with_audit(time_vec, status_vec, X)
      
      time_clean   <- clean$time_clean
      status_clean <- clean$status_clean
      X_clean      <- clean$X_clean
  
      # CLEAN counts (model-ready; must exist for logging + retention)
      n_clean <- length(time_clean)
      E_clean <- sum(status_clean == 1, na.rm = TRUE)
      
      
      stopifnot(
        length(time_clean) == length(status_clean),
        is.matrix(X_clean),
        nrow(X_clean) == length(time_clean),
        nrow(X_clean) == length(status_clean)
      )
      
      # -------------------------------------------------------------------------
      # PATCH: sample retention ratio is purely survival-row cleaning
      # -------------------------------------------------------------------------
      sample_retention_ratio <- if (is.finite(n_raw) && n_raw > 0) as.numeric(n_clean / n_raw) else NA_real_
      
      # Core predictor stats (pre-gate, feature-level)
      p0 <- if (is.matrix(X_clean)) ncol(X_clean) else NA_integer_
      
      # CoxNet dynamic feasibility (μ-ladder) on CLEAN data
      gate <- mu_ladder_coxnet_gate(time_clean, status_clean, X_clean,
                                    E_min = 20, N_min = 50, verbose = FALSE)
      fcode <- feasibility_code_coxnet(gate)
      
      p_used <- gate$p_used
      
      # -------------------------------------------------------------------------
      # PATCH: rename retention -> feature_retention_ratio (feature-level)
      # -------------------------------------------------------------------------
      feature_retention_ratio <- if (is.finite(p0) && p0 > 0 && is.finite(p_used)) as.numeric(p_used / p0) else NA_real_
      
      dropped <- if (is.finite(p0) && is.finite(p_used)) as.integer(p0 - p_used) else NA_integer_
      
      mu_sel  <- if (isTRUE(gate$feasible)) gate$mu_level else NA_integer_
      mu_step <- if (isTRUE(gate$feasible) && !is.null(gate$policy$step)) gate$policy$step else NA_character_
      
      # PATCH (logging enrichments): surface diagnostics from the gate object
      p_min_tried      <- if (!is.null(gate$p_min_tried)) as.integer(gate$p_min_tried) else NA_integer_
      mu_attempted_max <- if (!is.null(gate$mu_attempted_max)) as.integer(gate$mu_attempted_max) else NA_integer_
      last_step_tried  <- if (!is.null(gate$last_step_tried)) as.character(gate$last_step_tried) else NA_character_
      last_reason      <- if (!is.null(gate$last_reason)) as.character(gate$last_reason) else NA_character_
      
      # -------------------------------------------------------------------------
      # PATCH (data-feasibility diagnostics): surface explicit data-failure reasons
      # NOTE: these reflect feasibility on CLEAN data, not raw data
      # -------------------------------------------------------------------------
      N_min <- if (!is.null(gate$N_min)) as.integer(gate$N_min) else NA_integer_
      E_min <- if (!is.null(gate$E_min)) as.integer(gate$E_min) else NA_integer_
      n_time_nonfinite   <- if (!is.null(gate$n_time_nonfinite)) as.integer(gate$n_time_nonfinite) else NA_integer_
      n_status_nonfinite <- if (!is.null(gate$n_status_nonfinite)) as.integer(gate$n_status_nonfinite) else NA_integer_
      n_time_negative    <- if (!is.null(gate$n_time_negative)) as.integer(gate$n_time_negative) else NA_integer_
      data_fail_reasons  <- if (!is.null(gate$data_fail_reasons)) as.character(gate$data_fail_reasons) else NA_character_
      
      # -------------------------------------------------------------------------
      # PATCH: nnz_selected from gate$policy$nnz (PASS only; otherwise NA)
      # -------------------------------------------------------------------------
      nnz_selected <- NA_integer_
      if (isTRUE(gate$feasible) && !is.null(gate$policy) && !is.null(gate$policy$nnz)) {
        nnz_selected <- as.integer(gate$policy$nnz)
      }
      
      return(as_feas_log_row(
        cancer_type = ctype,
        metric      = metric_i,
        df_file     = dff,
        algorithm   = "CoxNet",
        feasibility_code = fcode,
        
        # CLEAN stats (model-ready rows)
        n     = as.integer(n_clean),
        p0    = as.integer(p0),
        p_used= as.integer(p_used),
        
        feature_retention_ratio = as.numeric(feature_retention_ratio),
        sample_retention_ratio  = as.numeric(sample_retention_ratio),
        
        E = as.integer(E_clean),
        
        nnz_selected = as.integer(nnz_selected),
        
        # IMPORTANT: schema-consistent types
        mu_selected = if (isTRUE(gate$feasible)) as.numeric(gate$mu_level) else NA_real_,  # double
        mu_step     = NA_integer_,  # integer; keep step label in last_step_tried
        
        dropped_features_count = as.integer(dropped),
        
        # logging enrichments
        p_min_tried      = as.integer(p_min_tried),
        mu_attempted_max = as.integer(mu_attempted_max),
        last_step_tried  = as.character(last_step_tried),
        last_reason      = as.character(last_reason),
        
        # data-feasibility diagnostics (CLEAN data)
        N_min = as.integer(N_min),
        E_min = as.integer(E_min),
        n_time_nonfinite   = as.integer(n_time_nonfinite),
        n_status_nonfinite = as.integer(n_status_nonfinite),
        n_time_negative    = as.integer(n_time_negative),
        data_fail_reasons  = as.character(data_fail_reasons),
        
        # raw vs clean counts (audit trail)
        n_raw   = as.integer(n_raw),
        E_raw   = as.integer(E_raw),
        n_clean = as.integer(n_clean),
        E_clean = as.integer(E_clean),
        
        n_dropped_invalid = as.integer(clean$n_dropped_total),
        n_drop_time_nonfinite_row   = as.integer(clean$n_drop_time_nonfinite),
        n_drop_status_nonfinite_row = as.integer(clean$n_drop_status_nonfinite),
        n_drop_time_negative_row    = as.integer(clean$n_drop_time_negative),
        
        # provenance
        worker_pid  = as.integer(worker_pid),
        worker_host = as.character(worker_host),
        elapsed_sec = as.numeric(proc.time()[["elapsed"]] - t0),
        
        error_message = NA_character_,
        PHASE_III_logic = NA_character_
      ))
      
    # ---------------------------
    # CHECKPOINT A (MUST be placed immediately BEFORE the future_lapply call)
    # Location: right after unique_strata is created (and after DF_ROOT/run_one_stratum are defined),
    #           and immediately before:
    #           rows_list <- future.apply::future_lapply(...)
    # ---------------------------
    cat("\n==================== CHECKPOINT A ====================\n")
    cat("About to launch future_lapply\n")
    cat("exists(dfinput, inherits=FALSE) =", exists("dfinput", inherits = FALSE), "\n")
    if (exists("dfinput", inherits = FALSE)) {
      cat("class(dfinput) =", paste(class(dfinput), collapse = ","), "\n")
      cat("nrow(dfinput)  =", nrow(dfinput), "\n")
      cat("ncol(dfinput)  =", ncol(dfinput), "\n")
      cat("names(dfinput) =", paste(names(dfinput), collapse = ", "), "\n")
    }
    cat("exists(unique_strata, inherits=FALSE) =", exists("unique_strata", inherits = FALSE), "\n")
    if (exists("unique_strata", inherits = FALSE)) {
      cat("class(unique_strata) =", paste(class(unique_strata), collapse = ","), "\n")
      cat("nrow(unique_strata)  =", nrow(unique_strata), "\n")
      cat("ncol(unique_strata)  =", ncol(unique_strata), "\n")
      cat("head(unique_strata)  =\n")
      print(utils::head(unique_strata, 3))
    }
    cat("======================================================\n\n")
    
    # HARD GUARD (recommended): stop immediately if there is nothing to run
    if (!exists("unique_strata", inherits = FALSE) || nrow(unique_strata) == 0L) {
      stop("CHECKPOINT A STOP: unique_strata is empty (no strata to evaluate).", call. = FALSE)
    }
    # Run in parallel and collect rows
    rows_list <- future.apply::future_lapply(
      X = seq_len(nrow(unique_strata)),
      FUN = function(i) {
        run_one_stratum(
          i = i,
          unique_strata = unique_strata,
          DF_ROOT = DF_ROOT,
          time_col_map = time_col_map,
          status_col_map = status_col_map
        )
      },
      future.seed = TRUE
    )
    
    # Bind rows into the same structure you had before
    feas_log <- dplyr::bind_rows(rows_list)
    
    # -------------------------------------------------------------------------
    # PATCH: Post-run parallelization certificate (TAR-friendly)
    # - Fixes misleading export: certificate file now contains the single-row cert
    # - PID distribution table is exported to its own file
    # -------------------------------------------------------------------------
    parallel_certificate <- function(feas_log,
                                     expected_workers = n_workers,
                                     cert_outfile = "parallelization_certificate.tsv",
                                     pid_outfile  = "parallelization_pid_table.tsv",
                                     verbose = TRUE) {
      
      if (!("worker_pid" %in% colnames(feas_log))) {
        stop("parallel_certificate: missing column 'worker_pid'. Did you add the per-stratum provenance patch?",
             call. = FALSE)
      }
      if (!("worker_host" %in% colnames(feas_log))) {
        stop("parallel_certificate: missing column 'worker_host'. Did you add the per-stratum provenance patch?",
             call. = FALSE)
      }
      
      pid_tbl <- feas_log %>%
        dplyr::filter(is.finite(worker_pid)) %>%
        dplyr::mutate(
          worker_pid  = as.integer(worker_pid),
          worker_host = as.character(worker_host)
        ) %>%
        dplyr::count(worker_pid, worker_host, name = "n_strata") %>%
        dplyr::arrange(dplyr::desc(n_strata))
      
      uniq_pid <- nrow(pid_tbl)
      
      # Capture actual strategy safely
      plan_class_local <- tryCatch(as.character(class(future::plan())[1]), error = function(e) NA_character_)
      
      cert <- data.frame(
        timestamp = as.character(Sys.time()),
        future_strategy = plan_class_local,
        expected_workers = as.integer(expected_workers),
        unique_worker_pids_in_loop = as.integer(uniq_pid),
        stringsAsFactors = FALSE
      )
      
      # -------------------------------------------------------------------------
      # Correct exports: cert -> cert_outfile; pid_tbl -> pid_outfile
      # -------------------------------------------------------------------------
      rio::export(cert, cert_outfile)
      rio::export(pid_tbl, pid_outfile)
      
      if (verbose) {
        message("Parallel certificate: strategy=", cert$future_strategy,
                " | unique_pids_in_loop=", cert$unique_worker_pids_in_loop,
                " | cert_file=", cert_outfile,
                " | pid_table_file=", pid_outfile)
      }
      
      # Strict: if expected_workers > 1, require at least 2 unique PIDs in the actual loop
      if (is.finite(expected_workers) && expected_workers > 1L) {
        if (!is.finite(uniq_pid) || uniq_pid < 2L) {
          stop("Parallel certificate failed: expected >1 worker in loop but observed unique_worker_pids_in_loop=",
               uniq_pid, ". This indicates sequential fallback or a worker collapse.",
               call. = FALSE)
        }
      }
      
      list(certificate = cert, pid_table = pid_tbl,
           cert_outfile = cert_outfile, pid_outfile = pid_outfile)
    }
    
    # -------------------------------------------------------------------------
    # PHASE III: Eligibility-gated algorithm routing label
    # - Adds column: PHASE_III_logic
    # - Deterministic mapping from Phase II feasibility_code
    # - Explicitly separates DATA, GEOMETRY, and OTHER CoxNet failures
    # -------------------------------------------------------------------------
    
    stopifnot(exists("feas_log", inherits = FALSE))
    stopifnot(is.data.frame(feas_log))
    stopifnot("feasibility_code" %in% names(feas_log))
    
    suppressPackageStartupMessages({
      library(dplyr)
    })
    
    feas_log <- feas_log %>%
      mutate(
        PHASE_III_logic = case_when(
          
          # ---------------------------------------------------------------
          # Regime A: Sparse PH geometry certified
          # ---------------------------------------------------------------
          feasibility_code == "COXNET_PASS" ~
            "ELIGIBLE_FULL_SUITE__COXNET_BACKBONE",
          
          # ---------------------------------------------------------------
          # Regime C: Geometry exists, but sparse PH fails (μ-ladder exhausted)
          # ---------------------------------------------------------------
          feasibility_code == "COXNET_FAIL_MU_EXHAUSTED" ~
            "ELIGIBLE_NONCOX__GEOMETRY_MU_EXHAUSTED",
    ## Structurally PH-incompatible but modelable strata
    ## (internal code: ELIGIBLE_NONCOX__GEOMETRY_MU_EXHAUSTED)
    
    ## Data-infeasible strata due to insufficient survival information
    ## (internal code: INELIGIBLE__INSUFFICIENT_SURVIVAL_INFORMATION)      
    
          # ---------------------------------------------------------------
          # Regime B: Statistical identifiability failure (no modeling)
          # ---------------------------------------------------------------
          feasibility_code == "COXNET_FAIL_DATA" ~
            "INELIGIBLE__INSUFFICIENT_SURVIVAL_INFORMATION",
          
          # ---------------------------------------------------------------
          # Engineering / pipeline failures (non-biological)
          # ---------------------------------------------------------------
          feasibility_code == "LOAD_FAIL" ~
            "INELIGIBLE__PIPELINE_FAILURE_LOAD",
          
          feasibility_code == "EXTRACT_FAIL" ~
            "INELIGIBLE__PIPELINE_FAILURE_EXTRACT",
          
          # ---------------------------------------------------------------
          # Catch-all: other CoxNet failures (must remain visible)
          # ---------------------------------------------------------------
          TRUE ~
            "INELIGIBLE__COXNET_FAILURE_OTHER"
        )
      )
    
    # Optional audit check
    print(table(feas_log$PHASE_III_logic, useNA = "ifany"))
    
    print(table(feas_log$feasibility_code, useNA = "ifany"))
    feas_log %>% count(metric, feasibility_code) %>% arrange(metric, feasibility_code)
    
    parallel_cert_out <- parallel_certificate(
      feas_log = feas_log,
      expected_workers = n_workers,
      cert_outfile = "parallelization_certificate.tsv",
      pid_outfile  = "parallelization_pid_table.tsv",
      verbose = TRUE
    )
    
    # Optional: write the feasibility audit
      rio::export(feas_log, "CoxNet_phaseII_feasibility_log.tsv")
    
    head(feas_log)
    
    feas_log <- rio::import("CoxNet_phaseII_feasibility_log.tsv")
    table(feas_log$feasibility_code)
    # COXNET_FAIL_DATA means: “This stratum does not satisfy minimum statistical identifiability conditions for CoxNet — before any modeling is attempted.”
    head(feas_log$error_message, 10) # NA here means “no execution error occurred.”
  
  gc()
  
  ###
  ###
  ###
  ###
  ###
  
  ###############################################################################
  # Create canonical terminology table for manuscript (Phase I–II definitions)
  # Output: TSV file and xlxs for Supplementary Methods or internal documentation
  # Table 1. Canonical terminology used across Phase I–III analyses
  ###############################################################################
  
  # Define the terminology table explicitly (no inference, no transformation)
  terminology_tbl <- data.frame(
  Term = c(
    "CANARY outcome",
    "Candidate preprocessing regime",
    "Mu-Exhausted / Algorithmically Admitted",
    "Phase II execution unit",
    "Quadripartite ML Ensemble",
    "Signature omic layer identifier",
    "TAR-admissible preprocessing regime",
    "Target Scarcity / Tier 2 Attrition",
    "Tier 1 Structural Viability",
    "Viable Survival Pathway",
    "df005"
  ),
  Meaning = c(
    "Structural feasibility classification derived from CoxNet CANARY probing",
    "Any Phase I preprocessing variant in the df006–df377 series",
    "Execution units that passed basic geometric constraints but failed linear convergence (CoxNet), mandating non-linear ML resolution.",
    "(cancer type, survival endpoint, preprocessing regime, algorithm)",
    "The final architecture executed sequentially across viable pathways (Survival-Boruta, RSF, XGBoost, MTLR).",
    "Second token identifier (Omic Layer Origin): Seven layers were represented: protein abundance (.1), somatic mutation status (.2), copy number variation (.3), microRNA expression (.4), transcript isoform abundance (.5), mRNA expression (.6), and CpG methylation (.7).",
    "Subset of df006–df377 classified as Unchanged or Improved by TAR",
    "Execution units rejected by CANARY due to insufficient absolute samples (N < 50) or survival events (E < 20).",
    "Baseline clinical/omic availability requirement; excludes datasets mechanically devoid of multi-omic resolution (e.g., DLBC).",
    "The final 96 structural combinations that survived Phase I/II attrition and proceeded to Phase III ML modeling.",
    "Fixed pre-imputation reference baseline"
  ),
  Scope = c(
    "Phase II",
    "Phase I output",
    "Phase II Outcome",
    "Phase II",
    "Phase III",
    "Global",
    "Cancer × endpoint specific",
    "Phase II Outcome",
    "Phase I to II Transition",
    "Phase II to III Transition",
    "Global"
  ),
  stringsAsFactors = FALSE
)
  
  # Write to TSV (deterministic, publication-safe)
  write.table(
    terminology_tbl,
    file = "Table S1. Canonical terminology used across Phase I–III analyses.tsv",
    sep = "\t",
    row.names = FALSE,
    quote = FALSE
  )
  
  rio::export(terminology_tbl, "Table S1. Canonical terminology used across Phase I–III analyses.xlsx")
  
  ###############################################################################
  # End of snippet
  ###############################################################################
  
  ###############################################################################
  # Phase II CoxNet CANARY — tables + figures from feasibility log (feas_log)
  # Data-fidelity first: do NOT rename columns; validate schema strictly.
  ###############################################################################
  
  suppressPackageStartupMessages({
    library(readr)
    library(dplyr)
    library(tidyr)
    library(stringr)
    library(ggplot2)
  })
  
  
  # ----------------------------
  # 0) Input
  # ----------------------------
  infile_tsv <- "CoxNet_phaseII_feasibility_log.tsv"
  infile_rds <- sub("\\.tsv$", ".rds", infile_tsv)
  
  # Prefer canonical typed RDS if present; fall back to TSV otherwise
  if (file.exists(infile_rds)) {
    feas_log <- readRDS(infile_rds)
  } else {
    feas_log <- readr::read_tsv(infile_tsv, show_col_types = FALSE, progress = FALSE)
  }
  
  # ----------------------------
  # 1) Canonical schema (single source of truth)
  # ----------------------------
  FEAS_LOG_SCHEMA <- list(
    chr = c(
      "cancer_type","metric","df_file","algorithm","feasibility_code",
      "last_step_tried","last_reason","data_fail_reasons","worker_host","PHASE_III_logic",
      "error_message"
    ),
    int = c(
      "n","p0","p_used","E",
      "nnz_selected","mu_step",
      "dropped_features_count","p_min_tried","mu_attempted_max",
      "N_min","E_min",
      "n_time_nonfinite","n_status_nonfinite","n_time_negative",
      "n_raw","E_raw","n_clean","E_clean",
      "n_dropped_invalid",
      "n_drop_time_nonfinite_row","n_drop_status_nonfinite_row","n_drop_time_negative_row",
      "worker_pid"
    ),
    dbl = c(
      "feature_retention_ratio","sample_retention_ratio","elapsed_sec","mu_selected"
    )
  )
  
  # ----------------------------
  # 2) Coerce + assert (covers ALL columns)
  # ----------------------------
  coerce_and_assert_feas_log <- function(x, schema = FEAS_LOG_SCHEMA, strict_extra_cols = TRUE) {
    stopifnot(is.data.frame(x))
    
    expected <- c(schema$chr, schema$int, schema$dbl)
    
    # Required columns must exist
    missing <- setdiff(expected, names(x))
    if (length(missing) > 0L) {
      stop("feas_log is missing required columns: ", paste(missing, collapse = ", "))
    }
    
    # Unexpected columns: error (strict) or warning
    extra <- setdiff(names(x), expected)
    if (length(extra) > 0L) {
      msg <- paste0("feas_log has unexpected columns: ", paste(extra, collapse = ", "))
      if (isTRUE(strict_extra_cols)) stop(msg) else warning(msg, call. = FALSE)
    }
    
    # Deterministic coercion
    for (cc in schema$chr) x[[cc]] <- as.character(x[[cc]])
    for (cc in schema$int) x[[cc]] <- as.integer(x[[cc]])
    for (cc in schema$dbl) x[[cc]] <- as.numeric(x[[cc]])
    
    # No list columns
    stopifnot(!any(vapply(x, is.list, logical(1))))
    
    # Post-coercion type assertions
    for (cc in schema$chr) stopifnot(typeof(x[[cc]]) == "character")
    for (cc in schema$int) stopifnot(typeof(x[[cc]]) == "integer")
    for (cc in schema$dbl) stopifnot(typeof(x[[cc]]) == "double")
    
    x
  }
  
  feas_log <- coerce_and_assert_feas_log(feas_log, strict_extra_cols = TRUE)
  
  # ----------------------------
  # 3) Persist canonical typed copy (source of truth for future imports)
  # ----------------------------
  # Always write the RDS after coercion (idempotent and safe)
  saveRDS(feas_log, infile_rds)
  
  feas_log <- readRDS("CoxNet_phaseII_feasibility_log.rds")

# ----------------------------
# 1) Strict schema validation
# ----------------------------
required_cols <- c(
  "cancer_type", "metric", "df_file", "algorithm",
  "feasibility_code", "last_reason",
  "n_raw", "E_raw", "n_clean", "E_clean",
  "n_dropped_invalid",
  "n_drop_time_nonfinite_row", "n_drop_status_nonfinite_row", "n_drop_time_negative_row",
  "n_time_nonfinite", "n_status_nonfinite", "n_time_negative",
  "p0", "p_used",
  "feature_retention_ratio", "sample_retention_ratio",
  "mu_selected", "mu_step", "mu_attempted_max",
  "last_step_tried", "data_fail_reasons",
  "elapsed_sec"
)

missing <- setdiff(required_cols, names(feas_log))
if (length(missing) > 0) {
  stop(
    "<- is missing required columns: ",
    paste(missing, collapse = ", "),
    call. = FALSE
  )
}

# Soft checks: ensure expected CANARY codes are present (do not force additional ones)
observed_codes <- sort(unique(feas_log$feasibility_code))
observed_reasons <- sort(unique(feas_log$last_reason))

message("Observed feasibility_code: ", paste(observed_codes, collapse = " | "))
message("Observed last_reason: ", paste(observed_reasons, collapse = " | "))

# ----------------------------
# 2) Helper summaries
# ----------------------------
# These summaries are designed to reflect the locked manuscript statements:
# - FAIL_DATA ↔ data infeasibility / insufficient survival information
# - FAIL_MU_EXHAUSTED ↔ fit infeasibility after exhausting deterministic mu-ladder
#
# NOTE: We do NOT infer "success" classes unless present in feasibility_code.

summ_canary_core <- function(df) {
  df %>%
    summarise(
      strata = dplyr::n(),
      cancers = dplyr::n_distinct(cancer_type),
      endpoints = dplyr::n_distinct(metric),
      regimes = dplyr::n_distinct(df_file),
      
      # Cohort / event audit (cleaned = endpoint-scoped cohort used for fitting checks)
      n_clean_median = suppressWarnings(stats::median(n_clean, na.rm = TRUE)),
      n_clean_IQR = suppressWarnings(stats::IQR(n_clean, na.rm = TRUE)),
      E_clean_median = suppressWarnings(stats::median(E_clean, na.rm = TRUE)),
      E_clean_IQR = suppressWarnings(stats::IQR(E_clean, na.rm = TRUE)),
      
      # Invalid survival schema accounting
      dropped_invalid_total = sum(n_dropped_invalid, na.rm = TRUE),
      drop_time_nonfinite_total = sum(n_drop_time_nonfinite_row, na.rm = TRUE),
      drop_status_nonfinite_total = sum(n_drop_status_nonfinite_row, na.rm = TRUE),
      drop_time_negative_total = sum(n_drop_time_negative_row, na.rm = TRUE),
      
      # Predictor dimensionality / retention
      p0_median = suppressWarnings(stats::median(p0, na.rm = TRUE)),
      p_used_median = suppressWarnings(stats::median(p_used, na.rm = TRUE)),
      feature_retention_median = suppressWarnings(stats::median(feature_retention_ratio, na.rm = TRUE)),
      sample_retention_median = suppressWarnings(stats::median(sample_retention_ratio, na.rm = TRUE)),
      
      # mu-ladder diagnostics
      mu_selected_median = suppressWarnings(stats::median(mu_selected, na.rm = TRUE)),
      mu_attempted_max_median = suppressWarnings(stats::median(mu_attempted_max, na.rm = TRUE)),
      mu_step_median = suppressWarnings(stats::median(mu_step, na.rm = TRUE)),
      
      # Runtime
      elapsed_sec_median = suppressWarnings(stats::median(elapsed_sec, na.rm = TRUE))
    )
}

# ----------------------------
# 3) CANARY tables
# ----------------------------

# 3A) Primary CANARY table: by feasibility_code × last_reason
canary_tbl_primary <- feas_log %>%
  dplyr::group_by(feasibility_code, last_reason) %>%
  summ_canary_core() %>%
  dplyr::ungroup() %>%
  dplyr::arrange(feasibility_code, dplyr::coalesce(last_reason, ""))

# 3B) Secondary: stratify by endpoint metric
canary_tbl_by_metric <- feas_log %>%
  dplyr::group_by(metric, feasibility_code, last_reason) %>%
  summ_canary_core() %>%
  dplyr::ungroup() %>%
  dplyr::arrange(metric, feasibility_code, dplyr::coalesce(last_reason, ""))

# 3C) Optional: “dominant CANARY outcome” per cancer × metric (ties labeled)
dominant_canary_by_cancer_metric <- feas_log %>%
  dplyr::count(cancer_type, metric, feasibility_code, name = "n_strata") %>%
  dplyr::group_by(cancer_type, metric) %>%
  dplyr::mutate(
    max_n  = max(n_strata),
    is_max = (n_strata == max_n)
  ) %>%
  dplyr::filter(is_max) %>%
  dplyr::summarise(
    dominant_code = paste(sort(unique(feasibility_code)), collapse = " + "),
    dominant_n    = unique(max_n),
    .groups       = "drop"
  ) %>%
  dplyr::arrange(metric, cancer_type)

# ----------------------------
# 4) Export tables (TSV) for manuscript / supplement
# ----------------------------
out_primary <- "CANARY_Table_primary_by_code_reason.tsv"
out_metric  <- "CANARY_Table_by_metric_code_reason.tsv"
out_dom     <- "CANARY_Table_dominant_code_by_cancer_metric.tsv"

readr::write_tsv(canary_tbl_primary, out_primary)
readr::write_tsv(canary_tbl_by_metric, out_metric)
readr::write_tsv(dominant_canary_by_cancer_metric, out_dom)

message("Wrote: ", out_primary)
message("Wrote: ", out_metric)
message("Wrote: ", out_dom)

# ----------------------------
# 5) Figures (ggplot2) — aligned to CANARY concept
# ----------------------------

# Figure A: overall distribution of CANARY outcomes
figA_df <- feas_log %>%
  count(feasibility_code, name = "n_strata") %>%
  mutate(frac = n_strata / sum(n_strata))

pA <- ggplot(figA_df, aes(x = feasibility_code, y = n_strata)) +
  geom_col() +
  labs(
    title = "CoxNet CANARY outcomes across all strata",
    x = "Feasibility code (CANARY outcome)",
    y = "Number of strata"
  ) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 20, hjust = 1))

ggsave("FIG_A_CANARY_outcome_counts.png", pA, width = 8, height = 4.5, dpi = 300)

# Figure B: distribution by endpoint (metric)
figB_df <- feas_log %>%
  count(metric, feasibility_code, name = "n_strata") %>%
  group_by(metric) %>%
  mutate(frac = n_strata / sum(n_strata)) %>%
  ungroup()

pB <- ggplot(figB_df, aes(x = metric, y = n_strata, fill = feasibility_code)) +
  geom_col(position = "stack") +
  labs(
    title = "CoxNet CANARY outcomes by survival endpoint",
    x = "Endpoint (metric)",
    y = "Number of strata",
    fill = "CANARY outcome"
  ) +
  theme_bw()

ggsave("FIG_B_CANARY_by_metric.png", pB, width = 8, height = 4.8, dpi = 300)

# Figure C (optional but powerful): tile plot of dominant CANARY code per cancer × metric
# This is explicitly diagnostic: it visualizes where strata are data-infeasible vs mu-exhausted.
pC <- ggplot(dominant_canary_by_cancer_metric,
             aes(x = cancer_type, y = metric, fill = dominant_code)) +
  geom_tile(color = "grey85", linewidth = 0.2) +
  labs(
    title = "Dominant CoxNet CANARY outcome by cancer type and endpoint",
    x = "Cancer type",
    y = "Endpoint (metric)",
    fill = "Dominant CANARY code"
  ) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
    panel.grid = element_blank()
  )

ggsave("FIG_C_CANARY_tile_by_cancer_metric.png", pC, width = 14, height = 5, dpi = 300)

# --- INJECTED: Publication-ready formatting for Figure C ---
pC_pub <- pC
pC_pub$data$dominant_code <- gsub("_", " ", pC_pub$data$dominant_code)
pC_pub <- pC_pub + 
  labs(title = NULL) + 
  theme(legend.position = "bottom")

ggsave("FIG_C_CANARY_tile_by_cancer_metric_600dpi.tiff", pC_pub, width = 14, height = 5, dpi = 600, device = "tiff", compression = "lzw")
# -----------------------------------------------------------


message("Figures written: FIG_A_*.png, FIG_B_*.png, FIG_C_*.png")

names(feas_log)
unique(feas_log$feasibility_code)
unique(feas_log$last_reason)
###############################################################################
# End.
###############################################################################


###
###
###
###
###
###
###############################################################################
# PHASE III — REQUIRED FUNCTIONS (AUTHORITATIVE, SINGLE DEFINITIONS)
# -----------------------------------------------------------------------------
# IMPORTANT POLICY NOTE (NO DELETIONS OF COMMENTARY):
# Your pasted script contains MULTIPLE re-definitions of the same functions
# (e.g., certify_df_schema_phaseIII, extract_phaseIII_unit_data, fit_predict_rsf_unit,
# stable_seed_from_key, fit_rsf_backend). In R, the *last* definition silently wins,
# which makes the run non-auditable and brittle.
#
# This block is a CONTRACT-SAFE consolidation that:
#   - Preserves your policy commentary (kept here and expanded where needed)
#   - Eliminates name collisions by providing ONE authoritative definition per name
#   - Aligns endpoint naming to your dfXXX canonical schema: OS / OS.time etc.
#   - Aligns the driver to call fit_predict_rsf_unit_safe() (one unit = one runlog row)
#   - Uses explicit, reason-coded predictor exclusions (ledger)
#   - Enforces “NO predictor-driven row deletion” (only endpoint survival masking)
#   - Keeps determinism via unit-key seed
#
# DROP-IN INSTRUCTION:
# Replace *all* Phase III function definitions in your script with THIS block.
# Then, in your driver loop, call fit_predict_rsf_unit_safe(...) exactly as shown
# in the amended run_phaseIII_rsf_parallel_by_df() below.
###############################################################################

suppressPackageStartupMessages({
  library(data.table)
  library(survival)
  library(digest)
  # Backends are optional at load-time; we check requireNamespace() at call-time
})

###############################################################################
# 0) Global Phase III output roots (deterministic, auditable)
###############################################################################
PHASEIII_ROOT <- "PHASEIII_RSF"
PHASEIII_RUNLOG_PATH <- file.path(PHASEIII_ROOT, "PHASEIII_RSF_RUNLOG.tsv")
PHASEIII_LEDGER_PATH <- file.path(PHASEIII_ROOT, "PHASEIII_RSF_predictor_exclusion_ledger.tsv")

if (!dir.exists(PHASEIII_ROOT)) dir.create(PHASEIII_ROOT, recursive = TRUE, showWarnings = FALSE)

###############################################################################
# 1) Endpoint schema map (explicit, canonical) — dfXXX schema: OS / OS.time etc.
###############################################################################
.phaseIII_endpoint_map <- function(endpoint) {
  endpoint <- as.character(endpoint)
  if (endpoint == "OS")   return(list(event = "OS",  time = "OS.time"))
  if (endpoint == "DSS")  return(list(event = "DSS", time = "DSS.time"))
  if (endpoint == "DFI")  return(list(event = "DFI", time = "DFI.time"))
  if (endpoint == "PFI")  return(list(event = "PFI", time = "PFI.time"))
  stop("Phase III: unsupported endpoint '", endpoint, "'. Expected one of OS, DSS, DFI, PFI.", call. = FALSE)
}

###############################################################################
# 2) Deterministic seed from execution-unit key (order-invariant)
###############################################################################
stable_seed_from_key <- function(key_string) {
  stopifnot(is.character(key_string), length(key_string) == 1L, nzchar(key_string))
  # xxhash32 -> 32-bit stable seed
  h <- digest::digest(key_string, algo = "xxhash32", serialize = FALSE)
  x <- as.integer(strtoi(substr(h, 1, 8), base = 16L))
  if (is.na(x)) x <- 1L
  x <- abs(x)
  if (x == 0L) x <- 1L
  x <- (x %% .Machine$integer.max)
  if (x == 0L) x <- 1L
  as.integer(x)
}

.phaseIII_seed_for_unit <- function(cancer_type, endpoint, dfid, df_file, backend, seed_salt = "PHASEIII_RSF_V1") {
  key <- paste("RSF", backend, seed_salt, cancer_type, endpoint, dfid, df_file, sep = "||")
  stable_seed_from_key(key)
}

###############################################################################
# 3) Append-only TSV writer (creates header if missing)
###############################################################################
`%||%` <- function(x, y) if (!is.null(x)) x else y

.phaseIII_append_tsv <- function(path, dt, col_order) {
  stopifnot(is.data.table(dt))
  stopifnot(is.character(path), length(path) == 1L, nzchar(path))
  stopifnot(is.character(col_order), length(col_order) >= 1L)
  
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  
  missing <- setdiff(col_order, names(dt))
  if (length(missing) > 0L) {
    stop("Phase III: missing required columns for TSV write: ",
         paste(missing, collapse = ", "), call. = FALSE)
  }
  dt <- dt[, ..col_order]
  
  new_file <- !file.exists(path) || isTRUE(file.info(path)$size == 0)
  if (new_file) {
    data.table::fwrite(dt, path, sep = "\t", quote = FALSE)
  } else {
    data.table::fwrite(dt, path, sep = "\t", quote = FALSE, append = TRUE, col.names = FALSE)
  }
  invisible(TRUE)
}

###############################################################################
# 4) Central Phase III runlog appender (one TSV row per execution unit)
###############################################################################
append_phaseIII_runlog_tsv <- function(runlog_path, row_named_list) {
  stopifnot(is.character(runlog_path), length(runlog_path) == 1L, nzchar(runlog_path))
  stopifnot(is.list(row_named_list), length(names(row_named_list)) > 0L)
  
  cols <- c(
    "timestamp_utc",
    "dfid","df_file","cancer_type","endpoint",
    "PHASE_III_logic",
    "backend",
    "algorithm",
    "n_raw_cancer","n_used_endpoint","E_used_endpoint",
    "p_prefix_matched","p_final",
    "pred_prefix_hash","pred_final_hash",
    "seed",
    "workers_planned","worker_pid","worker_host",
    "elapsed_sec",
    "status",
    "error_message",
    "out_dir","model_path","pred_path","meta_path"
  )
  
  out <- setNames(as.list(rep(NA_character_, length(cols))), cols)
  out$timestamp_utc <- format(as.POSIXct(Sys.time(), tz = "UTC"), "%Y-%m-%dT%H:%M:%SZ")
  
  for (nm in intersect(names(row_named_list), cols)) {
    v <- row_named_list[[nm]]
    if (length(v) == 0L) v <- NA
    if (is.numeric(v) || is.integer(v)) out[[nm]] <- as.character(v)
    else if (is.logical(v)) out[[nm]] <- ifelse(is.na(v), NA_character_, ifelse(v, "TRUE","FALSE"))
    else out[[nm]] <- as.character(v)
  }
  
  dt <- as.data.table(out)
  
  .phaseIII_append_tsv(
    path = runlog_path,
    dt = dt,
    col_order = cols
  )
  invisible(TRUE)
}

###############################################################################
# 5) Dataset-level schema certification (hard gate, dfXXX canonical)
###############################################################################
certify_df_schema_phaseIII <- function(df) {
  if (!is.data.frame(df)) stop("Phase III schema error: df is not a data.frame.", call. = FALSE)
  
  required_base <- c("sample", "patient", "type",
                     "OS", "OS.time", "DSS", "DSS.time", "DFI", "DFI.time", "PFI", "PFI.time")
  missing <- setdiff(required_base, names(df))
  if (length(missing) > 0L) {
    stop("Phase III schema error: df is missing required columns: ",
         paste(missing, collapse = ", "), call. = FALSE)
  }
  
  df$sample  <- trimws(as.character(df$sample))
  df$patient <- trimws(as.character(df$patient))
  df$type    <- trimws(as.character(df$type))
  
  if (anyNA(df$sample) || any(df$sample == "")) {
    stop("Phase III schema error: 'sample' contains NA/empty values.", call. = FALSE)
  }
  if (anyNA(df$type) || any(df$type == "")) {
    stop("Phase III schema error: 'type' contains NA/empty values.", call. = FALSE)
  }
  
  # Contract-compatible: 'sample' is the immutable row identity key (global uniqueness)
  if (data.table::uniqueN(df$sample) != nrow(df)) {
    stop("Phase III schema error: 'sample' is not unique (row identity violation).", call. = FALSE)
  }
  
  invisible(TRUE)
}

###############################################################################
# 6) Strict cancer subset + endpoint-scoped survival mask (ONLY allowed row reduction)
###############################################################################
.phaseIII_subset_to_cancer <- function(df, cancer_type) {
  cancer_type <- as.character(cancer_type)
  df_ct <- df[as.character(df$type) == cancer_type, , drop = FALSE]
  if (nrow(df_ct) == 0L) {
    stop("Phase III unit error: no rows found for cancer_type='", cancer_type, "'.", call. = FALSE)
  }
  df_ct
}

.phaseIII_mask_survival <- function(df_ct, endpoint, allow_time_zero = TRUE) {
  map <- .phaseIII_endpoint_map(endpoint)
  event_col <- map$event
  time_col  <- map$time
  
  event <- df_ct[[event_col]]
  time  <- df_ct[[time_col]]
  
  # time numeric/coercible
  if (!is.numeric(time)) {
    time2 <- suppressWarnings(as.numeric(as.character(time)))
    if (sum(is.na(time2)) > sum(is.na(time))) {
      stop("Phase III survival schema error: time column '", time_col, "' is not numeric/coercible.", call. = FALSE)
    }
    time <- time2
  }
  
  # event numeric/coercible
  if (!is.numeric(event)) {
    event2 <- suppressWarnings(as.numeric(as.character(event)))
    if (sum(is.na(event2)) > sum(is.na(event))) {
      stop("Phase III survival schema error: event column '", event_col, "' is not numeric/coercible.", call. = FALSE)
    }
    event <- event2
  }
  
  # Validate time
  bad_time_nonfinite <- which(!is.na(time) & !is.finite(time))
  if (length(bad_time_nonfinite) > 0L) {
    stop("Phase III survival schema error: non-finite survival times detected for endpoint '", endpoint, "'.", call. = FALSE)
  }
  if (allow_time_zero) {
    bad_time_neg <- which(!is.na(time) & time < 0)
    if (length(bad_time_neg) > 0L) {
      stop("Phase III survival schema error: negative survival times detected for endpoint '", endpoint, "'.", call. = FALSE)
    }
  } else {
    bad_time_nonpos <- which(!is.na(time) & time <= 0)
    if (length(bad_time_nonpos) > 0L) {
      stop("Phase III survival schema error: non-positive survival times detected for endpoint '", endpoint, "'.", call. = FALSE)
    }
  }
  
  # Validate event in {0,1}
  bad_event <- which(!is.na(event) & !(event %in% c(0, 1)))
  if (length(bad_event) > 0L) {
    stop("Phase III survival schema error: non-binary event values detected for endpoint '", endpoint, "'.", call. = FALSE)
  }
  
  # Mask rows: keep only where both time and event observed and schema-valid
  keep <- which(!is.na(time) & !is.na(event))
  df_kept <- df_ct[keep, , drop = FALSE]
  
  list(
    df = df_kept,
    event_col = event_col,
    time_col = time_col,
    n_raw = nrow(df_ct),
    n_kept = nrow(df_kept),
    E_kept = sum(df_kept[[event_col]] == 1, na.rm = TRUE)
  )
}

###############################################################################
# 7) Predictors by cancer-prefix + explicit, reason-coded exclusions (ledger)
###############################################################################
.phaseIII_select_predictors_by_prefix <- function(df_u, cancer_type) {
  prefix <- paste0("^", as.character(cancer_type), "-")
  grep(prefix, names(df_u), value = TRUE)
}

.phaseIII_reason_codes <- function() {
  c(
    TOKEN_PARSE_FAIL         = "TOKEN_PARSE_FAIL",
    NON_NUMERIC              = "NON_NUMERIC__COERCION_FAILED_OR_ILLEGAL_TYPE",
    COERCION_INTRODUCED_NA   = "COERCION_INTRODUCED_NA",
    ALL_MISSING              = "ALL_MISSING",
    ANY_INFINITE             = "CONTAINS_INFINITE",
    ZERO_VARIANCE            = "ZERO_VARIANCE",
    CNV_INVALID_LEVELS       = "CNV_INVALID_LEVELS",
    MUT_BINARIZE_FAIL        = "MUT_BINARIZE_FAIL"
  )
}

.phaseIII_token_from_name <- function(varname) {
  # Expected token: ^CTAB-\d+\.(\d+)\.
  # Example: BRCA-12345.2.SOMEFEATURE -> token "2"
  tok <- sub("^[^-]+-[0-9]+\\.([0-9]+)\\..*$", "\\1", varname)
  if (!grepl("^[0-9]+$", tok)) return(NA_character_)
  tok
}

.phaseIII_coerce_mut_to_binary <- function(x) {
  if (is.factor(x) || is.character(x)) x <- suppressWarnings(as.numeric(as.character(x)))
  if (!is.numeric(x)) return(list(ok = FALSE, x = NULL))
  x_bin <- ifelse(is.na(x), NA_real_, ifelse(x == 0, 0, 1))
  ux <- sort(unique(x_bin), na.last = TRUE)
  if (!all(ux %in% c(0, 1, NA))) return(list(ok = FALSE, x = NULL))
  list(ok = TRUE, x = as.numeric(x_bin))
}

.phaseIII_coerce_cnv_to_numeric <- function(x) {
  # Accept numeric CNV directly, else map Deleted/Normal/Duplicated -> -1/0/1
  if (is.numeric(x)) return(list(ok = TRUE, x = as.numeric(x)))
  x_chr <- as.character(x)
  allowed <- c("Deleted", "Normal", "Duplicated", NA_character_)
  ux <- sort(unique(x_chr), na.last = TRUE)
  if (!all(ux %in% allowed)) return(list(ok = FALSE, x = NULL))
  out <- rep(NA_real_, length(x_chr))
  out[x_chr == "Deleted"]    <- -1
  out[x_chr == "Normal"]     <-  0
  out[x_chr == "Duplicated"] <-  1
  list(ok = TRUE, x = out)
}

.phaseIII_coerce_continuous_numeric <- function(x) {
  if (is.numeric(x)) return(list(ok = TRUE, x = as.numeric(x)))
  if (is.logical(x)) return(list(ok = TRUE, x = as.numeric(x)))
  if (is.factor(x) || is.character(x)) {
    x_num <- suppressWarnings(as.numeric(as.character(x)))
    # If coercion adds NA beyond original NA count => illegal
    if (sum(is.na(x_num)) > sum(is.na(x))) return(list(ok = FALSE, x = NULL, introduced_na = TRUE))
    return(list(ok = TRUE, x = x_num, introduced_na = FALSE))
  }
  list(ok = FALSE, x = NULL)
}

.phaseIII_exclude_predictors_numeric_legal <- function(df_u, predictors) {
  # Returns: list(predictors_final, ledger_dt, p_before, p_after, pred_prefix_hash, pred_final_hash)
  rc <- .phaseIII_reason_codes()
  
  ledger <- data.table(
    predictor = character(),
    reason_code = character()
  )
  
  keep <- character()
  coerced_cache <- list()  # predictor -> numeric vector (for model frame)
  
  for (p in predictors) {
    tok <- .phaseIII_token_from_name(p)
    if (is.na(tok)) {
      ledger <- rbind(ledger, data.table(predictor = p, reason_code = rc["TOKEN_PARSE_FAIL"]))
      next
    }
    
    x <- df_u[[p]]
    
    # Token-specific coercions (your Phase I/II semantics preserved)
    if (tok == "2") {
      cm <- .phaseIII_coerce_mut_to_binary(x)
      if (!isTRUE(cm$ok)) {
        ledger <- rbind(ledger, data.table(predictor = p, reason_code = rc["MUT_BINARIZE_FAIL"]))
        next
      }
      x_num <- cm$x
    } else if (tok == "3") {
      cc <- .phaseIII_coerce_cnv_to_numeric(x)
      if (!isTRUE(cc$ok)) {
        ledger <- rbind(ledger, data.table(predictor = p, reason_code = rc["CNV_INVALID_LEVELS"]))
        next
      }
      x_num <- cc$x
    } else {
      c0 <- .phaseIII_coerce_continuous_numeric(x)
      if (!isTRUE(c0$ok)) {
        # Distinguish introduced NA vs outright non-numeric
        if (isTRUE(c0$introduced_na %||% FALSE)) {
          ledger <- rbind(ledger, data.table(predictor = p, reason_code = rc["COERCION_INTRODUCED_NA"]))
        } else {
          ledger <- rbind(ledger, data.table(predictor = p, reason_code = rc["NON_NUMERIC"]))
        }
        next
      }
      x_num <- c0$x
    }
    
    # All missing
    if (all(is.na(x_num))) {
      ledger <- rbind(ledger, data.table(predictor = p, reason_code = rc["ALL_MISSING"]))
      next
    }
    
    # Infinite values
    if (any(is.infinite(x_num), na.rm = TRUE)) {
      ledger <- rbind(ledger, data.table(predictor = p, reason_code = rc["ANY_INFINITE"]))
      next
    }
    
    # Zero variance ignoring NA
    x_obs <- x_num[!is.na(x_num)]
    if (length(x_obs) > 0L && stats::var(x_obs) == 0) {
      ledger <- rbind(ledger, data.table(predictor = p, reason_code = rc["ZERO_VARIANCE"]))
      next
    }
    
    keep <- c(keep, p)
    coerced_cache[[p]] <- as.numeric(x_num)
  }
  
  list(
    predictors_final = keep,
    ledger_dt = ledger,
    p_before = length(predictors),
    p_after = length(keep),
    coerced_cache = coerced_cache
  )
}

###############################################################################
# 8) RSF backend (ranger or randomForestSRC) — version-safe NA policy injection
###############################################################################
fit_rsf_backend <- function(time, event, X_df,
                            backend = c("ranger", "randomForestSRC"),
                            num_trees = 1000L,
                            mtry = NULL,
                            min_node_size = NULL,
                            seed = 1L) {
  
  backend <- match.arg(backend)
  stopifnot(is.numeric(time), is.numeric(event))
  stopifnot(is.data.frame(X_df))
  stopifnot(length(time) == nrow(X_df), length(event) == nrow(X_df))
  
  # Thread safety valve (execution-only)
  Sys.setenv(OMP_NUM_THREADS = "1", MKL_NUM_THREADS = "1", OPENBLAS_NUM_THREADS = "1")
  
  dat <- data.frame(time = time, event = event, X_df, check.names = FALSE)
  
  if (backend == "ranger") {
    if (!requireNamespace("ranger", quietly = TRUE)) {
      stop("Package 'ranger' is required but not installed.", call. = FALSE)
    }
    
    y <- survival::Surv(dat$time, dat$event)
    dat2 <- dat
    dat2$y <- y
    dat2$time <- NULL
    dat2$event <- NULL
    
    form <- stats::as.formula("y ~ .")
    
    fmls <- names(formals(ranger::ranger))
    args <- list(
      formula       = form,
      data          = dat2,
      num.trees     = as.integer(num_trees),
      seed          = as.integer(seed),
      write.forest  = TRUE
    )
    
    if (!is.null(mtry)) args$mtry <- as.integer(mtry)
    if (!is.null(min_node_size)) args$min.node.size <- as.integer(min_node_size)
    
    # Prefer policy-expressive NA learning if supported by installed version
    if ("na.action" %in% fmls) {
      args$na.action <- "na.learn"
    }
    
    fit <- do.call(ranger::ranger, args)
    pred <- ranger::predict(fit, data = dat2)
    
    return(list(fit = fit, pred = pred, backend = "ranger"))
  }
  
  if (backend == "randomForestSRC") {
    if (!requireNamespace("randomForestSRC", quietly = TRUE)) {
      stop("Package 'randomForestSRC' is required but not installed.", call. = FALSE)
    }
    
    form <- stats::as.formula("survival::Surv(time, event) ~ .")
    fmls <- names(formals(randomForestSRC::rfsrc))
    
    args <- list(
      formula = form,
      data    = dat,
      ntree   = as.integer(num_trees),
      seed    = as.integer(seed)
    )
    
    if (!is.null(mtry) && "mtry" %in% fmls) args$mtry <- as.integer(mtry)
    if (!is.null(min_node_size) && "nodesize" %in% fmls) args$nodesize <- as.integer(min_node_size)
    
    # Prefer explicit NA policy if supported
    if ("na.action" %in% fmls) {
      args$na.action <- "na.impute"
    }
    
    fit <- do.call(randomForestSRC::rfsrc, args)
    pred <- randomForestSRC::predict(fit, newdata = dat)
    
    return(list(fit = fit, pred = pred, backend = "randomForestSRC"))
  }
}

###############################################################################
# 9) RSF unit executor (Contract-grade) + explicit safe wrapper (NO ellipses)
###############################################################################
fit_predict_rsf_unit <- function(df,
                                 cancer_type,
                                 endpoint,
                                 dfid,
                                 df_file,
                                 workers_planned,
                                 backend = c("ranger", "randomForestSRC"),
                                 out_root = PHASEIII_ROOT,
                                 num_trees = 1000L,
                                 seed_salt = "PHASEIII_RSF_V1",
                                 allow_time_zero = TRUE) {
  
  t0 <- proc.time()[["elapsed"]]
  worker_pid  <- as.integer(Sys.getpid())
  worker_host <- as.character(Sys.info()[["nodename"]])
  
  backend <- match.arg(backend)
  
  # Hard gate
  certify_df_schema_phaseIII(df)
  
  cancer_type <- as.character(cancer_type)
  endpoint    <- as.character(endpoint)
  dfid        <- as.character(dfid)
  df_file     <- as.character(df_file)
  
  # Contract 1: cancer subset
  df_ct <- .phaseIII_subset_to_cancer(df, cancer_type)
  
  # Contract 2+3: endpoint survival mask ONLY
  surv <- .phaseIII_mask_survival(df_ct, endpoint = endpoint, allow_time_zero = allow_time_zero)
  df_u <- surv$df
  n_after_surv <- nrow(df_u)
  
  if (n_after_surv == 0L) {
    stop("Phase III unit error: no survival-feasible samples for (", cancer_type, ",", endpoint, ",", dfid, ").", call. = FALSE)
  }
  
  # Contract 1: predictors by prefix
  preds0 <- .phaseIII_select_predictors_by_prefix(df_u, cancer_type)
  if (length(preds0) == 0L) {
    stop("Phase III unit error: no predictors found with prefix '^", cancer_type, "-' in df.", call. = FALSE)
  }
  
  # Contract 4: explicit predictor exclusions (no row deletion)
  ex <- .phaseIII_exclude_predictors_numeric_legal(df_u, preds0)
  preds <- ex$predictors_final
  if (length(preds) == 0L) {
    stop("Phase III unit error: all predictors excluded as numerically inadmissible for (", cancer_type, ",", endpoint, ",", dfid, ").", call. = FALSE)
  }
  
  # Contract 3 invariant: predictor handling must not change row count
  stopifnot(nrow(df_u) == n_after_surv)
  
  # Determinism
  seed <- .phaseIII_seed_for_unit(cancer_type, endpoint, dfid, df_file, backend, seed_salt = seed_salt)
  set.seed(seed)
  
  # Build numeric X data.frame using cached coercions (no implicit apply() surprises)
  X_df <- as.data.frame(setNames(vector("list", length(preds)), preds), check.names = FALSE)
  for (p in preds) X_df[[p]] <- ex$coerced_cache[[p]]
  stopifnot(nrow(X_df) == nrow(df_u))
  
  # Hashes for audit
  pred0_hash <- digest::digest(preds0, algo = "xxhash64", serialize = FALSE)
  pred_hash  <- digest::digest(preds,  algo = "xxhash64", serialize = FALSE)
  
  # Output layout (unit-scoped, deterministic)
  unit_tag <- paste(cancer_type, endpoint, dfid, sep = "__")
  out_dir  <- file.path(out_root, backend, dfid, cancer_type, endpoint)
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  
  model_path <- file.path(out_dir, paste0("rsf_model__", unit_tag, ".rds"))
  pred_path  <- file.path(out_dir, paste0("rsf_pred__",  unit_tag, ".rds"))
  meta_path  <- file.path(out_dir, paste0("rsf_meta__",  unit_tag, ".tsv"))
  
  # Fit
  rsf <- fit_rsf_backend(
    time = as.numeric(df_u[[.phaseIII_endpoint_map(endpoint)$time]]),
    event = as.numeric(df_u[[.phaseIII_endpoint_map(endpoint)$event]]),
    X_df = X_df,
    backend = backend,
    num_trees = as.integer(num_trees),
    seed = as.integer(seed)
  )
  
  # Persist artifacts
  saveRDS(rsf$fit,  model_path)
  saveRDS(rsf$pred, pred_path)
  
  # Predictor exclusion ledger (append-only; only excluded predictors recorded)
  if (nrow(ex$ledger_dt) > 0L) {
    ledger_out <- copy(ex$ledger_dt)
    ledger_out[, `:=`(
      unit_tag    = unit_tag,
      cancer_type = cancer_type,
      endpoint    = endpoint,
      dfid        = dfid,
      df_file     = df_file,
      backend     = backend
    )]
    .phaseIII_append_tsv(
      path = PHASEIII_LEDGER_PATH,
      dt = ledger_out,
      col_order = c("unit_tag","cancer_type","endpoint","dfid","df_file","backend","predictor","reason_code")
    )
  }
  
  # Meta (unit-local)
  meta <- data.table(
    unit_tag          = unit_tag,
    cancer_type       = cancer_type,
    endpoint          = endpoint,
    dfid              = dfid,
    df_file           = df_file,
    backend           = backend,
    algorithm         = paste0("RSF_", backend),
    n_raw_cancer      = as.integer(nrow(df_ct)),
    n_used_endpoint   = as.integer(surv$n_kept),
    E_used_endpoint   = as.integer(surv$E_kept),
    p_prefix_matched  = as.integer(length(preds0)),
    p_final           = as.integer(length(preds)),
    pred_prefix_hash  = pred0_hash,
    pred_final_hash   = pred_hash,
    seed              = as.integer(seed),
    workers_planned   = as.integer(workers_planned),
    worker_pid        = as.integer(worker_pid),
    worker_host       = as.character(worker_host),
    elapsed_sec       = as.numeric(proc.time()[["elapsed"]] - t0),
    status            = "OK",
    error_message     = "",
    out_dir           = out_dir,
    model_path        = model_path,
    pred_path         = pred_path,
    meta_path         = meta_path
  )
  data.table::fwrite(meta, meta_path, sep = "\t")
  
  # Central runlog append (OK path)
  append_phaseIII_runlog_tsv(PHASEIII_RUNLOG_PATH, as.list(meta))
  
  list(
    status = "OK",
    unit_tag = unit_tag,
    cancer_type = cancer_type,
    endpoint = endpoint,
    dfid = dfid,
    df_file = df_file,
    backend = backend,
    algorithm = paste0("RSF_", backend),
    n_raw_cancer = as.integer(nrow(df_ct)),
    n_used_endpoint = as.integer(surv$n_kept),
    E_used_endpoint = as.integer(surv$E_kept),
    p_prefix_matched = as.integer(length(preds0)),
    p_final = as.integer(length(preds)),
    pred_prefix_hash = pred0_hash,
    pred_final_hash = pred_hash,
    seed = as.integer(seed),
    workers_planned = as.integer(workers_planned),
    worker_pid = as.integer(worker_pid),
    worker_host = as.character(worker_host),
    elapsed_sec = as.numeric(proc.time()[["elapsed"]] - t0),
    out_dir = out_dir,
    model_path = model_path,
    pred_path = pred_path,
    meta_path = meta_path
  )
}

fit_predict_rsf_unit_safe <- function(df,
                                      cancer_type,
                                      endpoint,
                                      dfid,
                                      df_file,
                                      workers_planned,
                                      backend = c("ranger", "randomForestSRC"),
                                      out_root = PHASEIII_ROOT,
                                      num_trees = 1000L,
                                      seed_salt = "PHASEIII_RSF_V1",
                                      allow_time_zero = TRUE,
                                      PHASE_III_logic = "ELIGIBLE_NONCOX__GEOMETRY_MU_EXHAUSTED") {
  
  backend <- match.arg(backend)
  
  worker_pid  <- as.integer(Sys.getpid())
  worker_host <- as.character(Sys.info()[["nodename"]])
  t0 <- proc.time()[["elapsed"]]
  
  tryCatch(
    fit_predict_rsf_unit(
      df = df,
      cancer_type = cancer_type,
      endpoint = endpoint,
      dfid = dfid,
      df_file = df_file,
      workers_planned = workers_planned,
      backend = backend,
      out_root = out_root,
      num_trees = num_trees,
      seed_salt = seed_salt,
      allow_time_zero = allow_time_zero
    ),
    error = function(e) {
      
      elapsed <- as.numeric(proc.time()[["elapsed"]] - t0)
      
      # Central runlog append (FAIL path) — one row per unit on failure
      append_phaseIII_runlog_tsv(PHASEIII_RUNLOG_PATH, list(
        dfid = as.character(dfid),
        df_file = as.character(df_file),
        cancer_type = as.character(cancer_type),
        endpoint = as.character(endpoint),
        PHASE_III_logic = PHASE_III_logic,
        backend = backend,
        algorithm = paste0("RSF_", backend),
        workers_planned = as.integer(workers_planned),
        worker_pid = as.integer(worker_pid),
        worker_host = as.character(worker_host),
        elapsed_sec = elapsed,
        status = "FAIL",
        error_message = as.character(conditionMessage(e))
      ))
      
      list(
        status = "FAIL",
        cancer_type = as.character(cancer_type),
        endpoint = as.character(endpoint),
        dfid = as.character(dfid),
        df_file = as.character(df_file),
        backend = backend,
        algorithm = paste0("RSF_", backend),
        workers_planned = as.integer(workers_planned),
        worker_pid = as.integer(worker_pid),
        worker_host = as.character(worker_host),
        elapsed_sec = elapsed,
        error_message = as.character(conditionMessage(e))
      )
    }
  )
}

###############################################################################
# 10) DRIVER INJECTION (AMENDED): df-batched parallel execution uses SAFE wrapper
#     - Manifest column is "metric" but Phase III unit uses "endpoint".
#     - Therefore: endpoint <- units$metric[j]
###############################################################################
run_phaseIII_rsf_parallel_by_df <- function(phaseIII_by_df,
                                            df_root,
                                            workers,
                                            seed_base = 1L,
                                            backend = c("ranger","randomForestSRC"),
                                            out_root = PHASEIII_ROOT,
                                            num_trees = 1000L,
                                            seed_salt = "PHASEIII_RSF_V1",
                                            allow_time_zero = TRUE) {
  
  backend <- match.arg(backend)
  
  stopifnot(is.data.table(phaseIII_by_df))
  stopifnot(dir.exists(df_root))
  stopifnot(is.numeric(workers), length(workers) == 1L, workers >= 1)
  
  # Deterministic parallel RNG (Windows multisession safe)
  RNGkind("L'Ecuyer-CMRG")
  set.seed(as.integer(seed_base))
  
  suppressPackageStartupMessages({
    library(future)
    library(future.apply)
    library(parallelly)
  })
  
  future::plan(future::multisession, workers = as.integer(workers))
  
  res <- future.apply::future_lapply(seq_len(nrow(phaseIII_by_df)), function(i) {
    
    df_file <- phaseIII_by_df$df_file[i]
    dfpath  <- normalizePath(file.path(df_root, df_file), winslash = "/", mustWork = FALSE)
    if (!file.exists(dfpath)) {
      stop(sprintf("Phase III input error: missing df file '%s' resolved to '%s'.", df_file, dfpath), call. = FALSE)
    }
    
    dfid <- sub("^.*(df[0-9]+).*$", "\\1", basename(df_file))
    
    # Load once per worker
    df <- readRDS(dfpath)
    
    # Hard schema certification (once per df load)
    certify_df_schema_phaseIII(df)
    
    units <- phaseIII_by_df$units[[i]]
    out <- vector("list", nrow(units))
    
    for (j in seq_len(nrow(units))) {
      out[[j]] <- fit_predict_rsf_unit_safe(
        df = df,
        cancer_type = units$cancer_type[j],
        endpoint = units$metric[j],     # <-- CRITICAL ALIGNMENT
        dfid = dfid,
        df_file = df_file,
        workers_planned = as.integer(workers),
        backend = backend,
        out_root = out_root,
        num_trees = as.integer(num_trees),
        seed_salt = seed_salt,
        allow_time_zero = allow_time_zero,
        PHASE_III_logic = "ELIGIBLE_NONCOX__GEOMETRY_MU_EXHAUSTED"
      )
    }
    
    out
  }, future.seed = TRUE)
  
  future::plan(future::sequential)
  invisible(res)
}

###############################################################################
# END: AUTHORITATIVE PHASE III DEFINITIONS
###############################################################################



# =============================================================================
# STANDALONE FIG C RE-GENERATION (EXECUTE THIS BLOCK SOLO IF NEEDED)
# =============================================================================
# If you ever need to re-generate the publication-grade Figure C without running
# the entire Phase II sweep, you can just highlight and run this block below:

suppressPackageStartupMessages({
  library(ggplot2)
  library(dplyr)
  library(stringr)
})

# Path to the already computed feasibility log
feas_log_path <- "CoxNet_phaseII_feasibility_log.rds"

if(file.exists(feas_log_path)){
  feas_log <- readRDS(feas_log_path)
  
  # Re-create the dominant outcome table mathematically
  dominant_canary_by_cancer_metric <- feas_log %>%
    count(cancer_type, metric, feasibility_code, name = "n_strata") %>%
    group_by(cancer_type, metric) %>%
    mutate(
      max_n  = max(n_strata),
      is_max = (n_strata == max_n)
    ) %>%
    filter(is_max) %>%
    summarise(
      dominant_code = paste(sort(unique(feasibility_code)), collapse = " + "),
      dominant_n    = unique(max_n),
      .groups       = "drop"
    ) %>%
    arrange(metric, cancer_type)
    
  pC <- ggplot(dominant_canary_by_cancer_metric,
             aes(x = cancer_type, y = metric, fill = dominant_code)) +
      geom_tile(color = "grey85", linewidth = 0.2) +
      labs(
        x = "Cancer type",
        y = "Endpoint (metric)"
      ) +
      theme_bw() +
      theme(
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        panel.grid = element_blank()
      )
  
  # Publication-grade formatting injection
  pC_pub <- pC +
    scale_fill_discrete(
      name = "Dominant CANARY code",
      labels = function(x) stringr::str_replace_all(x, "_", " ")
    ) +
    theme(legend.position = "bottom")

  ggsave("FIG_C_CANARY_tile_by_cancer_metric_600dpi.tiff", pC_pub, width = 14, height = 5, dpi = 600, device = "tiff", compression = "lzw")
  cat("Successfully regenerated publication-ready FIG C!\n")
} else {
  cat("Could not find feasibility log to regenerate plot locally.\n")
}
# =============================================================================

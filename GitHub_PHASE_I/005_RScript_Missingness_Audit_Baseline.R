################################################################################
# ===========================================================================
# MODULE: Strict Groupwise Missingness Audit by Cancer Type and Omic Prefix
# ===========================================================================
################################################################################
#
# PURPOSE:
#   Compute variable-level missingness strictly per cancer type for multi-omic datasets
#   (here: df005), applying demographic/clinical and omic-layer constraints before
#   downstream imputation.
#
#   Output serves as a pre-pipeline audit to determine imputation eligibility based on
#   a per-type threshold (≤ 35%), preserving biological/clinical integrity by:
#     - Restricting computations to rows of a single cancer type at a time.
#     - Always including demographic/clinical columns (1:62).
#     - Including only omic columns (63:ncol) whose names start with "{type}-" (e.g., "ACC-").
#
# ELIGIBILITY RULE (rounded decision):
#   eligible_for_imputation = TRUE if  round(n_missing / n_total, DECIMALS) ≤ THRESHOLD.
#   We also report the unrounded proportion (prop_missing_raw) for audit transparency.
#
# OUTPUTS:
#   - df005_missingness_by_type_STRICT.tsv
#       Full audit with raw & rounded proportions, counts, and eligibility flags, by type/variable.
#   - Console: compact eligibility counts per type and group (clinical/omic).
#   - df005_missingness_boundary_EQ_threshold.tsv
#       Variables with rounded prop_missing == THRESHOLD (i.e., “equal to” the threshold).
#
# USAGE CONTEXT:
#   Run before the main imputation pipeline to produce eligibility masks/lists:
#     - Gate variables for imputation modules (only eligible variables pass).
#     - Flag ineligible variables for downstream ML with native NA handling.
#
################################################################################

# ---- Commentary on eligibility results based on df005 ----
# Most cancer types show higher ineligibility among clinical variables than omic ones.
# Notable exceptions are GBM, READ, and COAD, where >94% of omic variables exceed the
# 0.35 missingness threshold (GBM ~97.81%, READ ~95.31%, COAD ~94.03%).
# For these, omic-layer imputation should be avoided to preserve biology and prevent leakage.

# 📦 Safe import function
#### ============================================================================
#### 📦 Master Wrapper Functions for rio Import/Export with NA Safety
#### ============================================================================

library(rio)

safe_import_tsv <- function(file, format = NULL, ...) {
  import(file, format = format, na.strings = "NA", ...)
}

# ---- Safe TSV writer (define once) ----
if (!exists("safe_export_tsv", mode = "function")) {
  safe_export_tsv <- function(x, file, na = "NA") {
    stopifnot(is.character(file), length(file) == 1L)
    dir.create(dirname(file), recursive = TRUE, showWarnings = FALSE)
    data.table::fwrite(
      data.table::as.data.table(x),
      file = file,
      sep = "\t",
      na = na,
      quote = FALSE
    )
  }
}

df020_Emanuell <- safe_import_tsv("df020_Emanuell.tsv", format = "tsv")

####
####
#### -----------------------------------------------------
#### Converting empty values (== "") to NA missing values
#### -----------------------------------------------------
#### 
### It does not put the string "NA" into the cell. 
### Instead, it converts empty string values ("") to actual R missing values, i.e., 
### NA of type logical, character, or factor, depending on the column's class.
# Count empty string ("") values in each column
empty_string_counts <- sapply(df020_Emanuell, function(col) sum(col == "", na.rm = TRUE))

# Show columns with at least one empty value
empty_string_counts[empty_string_counts > 0]

df020_Emanuell[df020_Emanuell == ""] <- NA

# Count empty string ("") values in each column
empty_string_counts <- sapply(df020_Emanuell, function(col) sum(col == "", na.rm = TRUE))

# Show columns with at least one empty value
empty_string_counts[empty_string_counts > 0]

df005 <- df020_Emanuell

# ---- Harmonization: DSS := 0 when OS == 0 and DSS is NA (deterministic rule) ----
norm01 <- function(x){
  if (is.logical(x)) return(as.integer(x))
  if (is.factor(x))  x <- as.character(x)
  x <- tolower(as.character(x))
  out <- ifelse(x %in% c("1","true","t","yes","y"), 1L,
                ifelse(x %in% c("0","false","f","no","n"), 0L, suppressWarnings(as.integer(x))))
  as.integer(out)
}

OSi  <- norm01(df005$OS)
DSSi <- norm01(df005$DSS)

changed_idx <- which(OSi == 0L & is.na(DSSi))
message(sprintf("Harmonized DSS:=0 in %d rows (OS==0 & DSS==NA).", length(changed_idx)))

if (length(changed_idx)) {
  df005$DSS[changed_idx] <- 0L
  # (optional) provenance flag
  df005$DSS_harmonized_from_OS0 <- FALSE
  df005$DSS_harmonized_from_OS0[changed_idx] <- TRUE
}

# replace the offending line with a local, read-only cast
dss_int <- suppressWarnings(as.integer(df005$DSS))
# …use dss_int in whatever summary/check follows…


## Reorder specific variables in df005
## Objective:
## 1. Move "PFI.2.cr" and "PFI.time.e.cr" immediately after "PFI.time.2"
## 2. Move "DSS_harmonized_from_OS0" immediately after "DSS.time"

library(dplyr)

df005 <- df005 %>%
  relocate(PFI.2.cr, PFI.time.2.cr, .after = PFI.time.2) %>%
  relocate(DSS_harmonized_from_OS0, .after = DSS.time)

# 💾 Save the object as RDS in the working directory
saveRDS(df005, file = "df005.rds")

# 🔄 Reload dataset (restores the exact same object)
df005 <- readRDS("df005.rds")

# ------------------ Configuration ------------------
DECIMALS  <- 4        # Reporting precision for proportions AND decision boundary
THRESHOLD <- 0.35     # Threshold used in the rounded decision and boundary report

# ------------------ Packages -----------------------
# install.packages("data.table")  # if needed
# install.packages("readr")       # if needed
library(data.table)
library(readr)   # fwrite with sep = "\t"
library(rio)

if (!exists("safe_export_tsv", mode = "function")) {
  safe_export_tsv <- function(x, path, na = "NA") {
    tryCatch({
      dir.create(dirname(path), showWarnings = FALSE, recursive = TRUE)
      write.table(x, file = path, sep = "\t", quote = FALSE, row.names = FALSE, col.names = TRUE, na = na)
    }, error = function(e) stop(sprintf("Failed to write TSV '%s': %s", path, conditionMessage(e)), call. = FALSE))
  }
}

# ------------------ Input checks -------------------
stopifnot(exists("df005"))
stopifnot(ncol(df005) >= 63)
stopifnot("type" %in% names(df005))

# ------------------ Prep ---------------------------
DT <- as.data.table(df005)

# Clinical block: columns 1:62 (guard bounds)
clinical_idx  <- 1:min(62L, ncol(DT))
clinical_vars <- names(DT)[clinical_idx]
omic_vars     <- names(DT)[setdiff(seq_len(ncol(DT)), clinical_idx)]

# Ensure `type` is not treated as clinical
if ("type" %in% clinical_vars) clinical_vars <- setdiff(clinical_vars, "type")

# ------------------ Helpers ------------------------
# Compute missingness within a type-subset for selected variables.
# Decision: ROUNDED rule (≤ THRESHOLD after rounding to DECIMALS); also report raw proportions.
.compute_missingness <- function(subDT, vars, type_label, group_label,
                                 decimals = DECIMALS, threshold = THRESHOLD) {
  if (length(vars) == 0L) {
    return(data.table(
      type = character(0), variable = character(0),
      group = character(0), n_total = integer(0),
      n_missing = integer(0),
      prop_missing_raw = numeric(0),
      prop_missing = numeric(0),
      eligible_for_imputation = logical(0)
    ))
  }
  n_total   <- nrow(subDT)
  n_missing <- colSums(is.na(subDT[, ..vars]))
  prop_raw  <- n_missing / n_total
  prop_rnd  <- round(prop_raw, decimals)
  
  # ---- Rounded decision (changed from integer rule) ----
  eligible <- prop_rnd <= threshold
  
  data.table(
    type = type_label,
    variable = vars,
    group = group_label,                 # "clinical" or "omic"
    n_total = n_total,
    n_missing = as.integer(n_missing),
    prop_missing_raw = prop_raw,         # unrounded (audit)
    prop_missing     = prop_rnd,         # rounded (reporting & decision)
    eligible_for_imputation = eligible
  )
}

# ------------------ Main loop per type --------------
types_vec <- unique(stats::na.omit(DT$type))

out_list <- lapply(types_vec, function(ti) {
  subDT <- DT[type == ti]
  
  # Strict prefix pattern "{TYPE}-" with regex escaping
  prefix_pat <- paste0("^", gsub("([.|()\\^$?*+{}\\[\\]\\\\-])", "\\\\\\1", ti), "-")
  omic_vars_for_ti <- omic_vars[grepl(prefix_pat, omic_vars, perl = TRUE)]
  
  res_clin <- .compute_missingness(subDT, clinical_vars, ti, "clinical")
  res_omic <- .compute_missingness(subDT, omic_vars_for_ti, ti, "omic")
  
  rbind(res_clin, res_omic, use.names = TRUE)
})

missingness_long_strict <- rbindlist(out_list, use.names = TRUE)

#### Block outcome variables (OS, DSS, DFI, PFI) from incorrect gating simply based on the budnig elegibility of <= 0.35 na_burdem
#### REASON: WE DO NOT WANT TO FABRICATE EVENTS 0,1 that are not reference trustable
# --- Freeze event outcomes in missingness_long_strict: never impute OS/DSS/DFI/PFI ---
## Freeze outcome variables (OS, DSS, DFI, PFI) in missingness_long_strict
## Rationale: never fabricate event labels; gates must not re-open them.

stopifnot(exists("missingness_long_strict"))
MLS <- missingness_long_strict

# --- required columns
req <- c("type","variable","eligible_for_imputation")
miss <- setdiff(req, names(MLS))
if (length(miss)) stop("missingness_long_strict is missing: ", paste(miss, collapse=", "))

# --- normalize columns
MLS$variable <- as.character(MLS$variable)
if (!is.logical(MLS$eligible_for_imputation)) {
  x <- MLS$eligible_for_imputation
  MLS$eligible_for_imputation <- if (is.numeric(x)) {
    !is.na(x) & x != 0
  } else {
    y <- tolower(as.character(x))
    ifelse(y %in% c("true","t","1","yes","y"),  TRUE,
           ifelse(y %in% c("false","f","0","no","n"), FALSE, NA))
  }
}

EVENTS <- c("OS","DSS","DFI","PFI")

# --- 1) force-block events
idx_ev <- MLS$variable %in% EVENTS
before <- MLS$eligible_for_imputation[idx_ev]
MLS$eligible_for_imputation[idx_ev] <- FALSE

message(sprintf(
  "Event eligibility flip: TRUE->FALSE=%d, NA->FALSE=%d, already FALSE=%d",
  sum(before %in% TRUE,  na.rm = TRUE),
  sum(is.na(before)),
  sum(before %in% FALSE, na.rm = TRUE)
))

# hard assertion: every event row is FALSE
stopifnot(all(MLS$eligible_for_imputation[idx_ev] == FALSE))

# --- 2) keep the original invariant ONLY for non-events
DECIMALS  <- get0("DECIMALS",  ifnotfound = 4)
THRESHOLD <- get0("THRESHOLD", ifnotfound = 0.35)

idx_ne <- !idx_ev

# choose prop-missing column (support either raw or pre-rounded)
prop_col <- if ("prop_missing" %in% names(MLS)) "prop_missing" else
  if ("prop_missing_round" %in% names(MLS)) "prop_missing_round" else
    stop("missingness_long_strict needs prop_missing or prop_missing_round")

prop_ne <- if (prop_col == "prop_missing_round") {
  MLS$prop_missing_round[idx_ne]
} else {
  round(MLS$prop_missing[idx_ne], DECIMALS)
}

rhs <- prop_ne <= round(THRESHOLD, DECIMALS)
lhs <- MLS$eligible_for_imputation[idx_ne]

# if your pipeline requires a min non-NA count, uncomment this block:
# MIN_NONNA <- get0("MIN_NONNA", ifnotfound = 5L)
# nn_col <- intersect(names(MLS), c("n_non_na","n_nonNA","n_non_na_time","n_non_na_obs"))
# if (length(nn_col)) rhs <- rhs & (MLS[[nn_col[1]]][idx_ne] >= MIN_NONNA)

stopifnot(!any(is.na(lhs)), !any(is.na(rhs)))
stopifnot(all(lhs == rhs))

# --- write back + export
missingness_long_strict <- MLS
rm(MLS)

if (exists("safe_export_tsv", mode = "function")) {
  safe_export_tsv(missingness_long_strict, "df005_missingness_by_type_STRICT.tsv")
} else if (requireNamespace("data.table", quietly = TRUE)) {
  data.table::fwrite(missingness_long_strict, "df005_missingness_by_type_STRICT.tsv",
                     sep = "\t", na = "NA", quote = FALSE)
} else {
  write.table(missingness_long_strict, "df005_missingness_by_type_STRICT.tsv",
              sep = "\t", na = "NA", quote = FALSE, row.names = FALSE)
}

# ------------------ Compact counts with proportions -------------------
eligibility_counts <- missingness_long_strict[
  , .(N = .N), by = .(type, group, eligible_for_imputation)
][
  , total_vars := sum(N), by = .(type, group)
][
  , proportion := round(N / total_vars, 4)
]

print(eligibility_counts)
safe_export_tsv(as.data.frame(eligibility_counts), "df005_eligibility_counts.tsv")  # corrected spelling

# ------------------ Diagnostics & Checks ------------
# Boundary diagnostics: exactly THRESHOLD after rounding (for auditing only)
boundary_hits <- missingness_long_strict[prop_missing == THRESHOLD]
if (nrow(boundary_hits) > 0L) {
  message(sprintf("[Info] %d (type,variable) pairs at the %.2f boundary; all should be eligible by the rounded rule.",
                  nrow(boundary_hits), THRESHOLD))
  fwrite(boundary_hits, "df005_missingness_boundary_EQ_threshold.tsv",
         sep = "\t", na = "NA", quote = FALSE)  # "EQ" == equal to
}

# Proportion range checks
stopifnot(all(missingness_long_strict$prop_missing_raw >= 0 &
                missingness_long_strict$prop_missing_raw <= 1))
stopifnot(all(missingness_long_strict$prop_missing >= 0 &
                missingness_long_strict$prop_missing <= 1))

# Eligibility must reflect the rounded rule (≤ THRESHOLD) for NON-event variables only.
# Events (OS/DSS/DFI/PFI) are hard-frozen to FALSE, regardless of missingness.
{
  stopifnot(exists("missingness_long_strict"))
  EVENTS   <- c("OS","DSS","DFI","PFI")
  DECIMALS <- get0("DECIMALS",  ifnotfound = 4)
  
  # Coerce eligible_for_imputation to logical for a clean comparison
  if (!is.logical(missingness_long_strict$eligible_for_imputation)) {
    x <- missingness_long_strict$eligible_for_imputation
    missingness_long_strict$eligible_for_imputation <- if (is.numeric(x)) {
      !is.na(x) & x != 0
    } else {
      y <- tolower(as.character(x))
      ifelse(y %in% c("true","t","1","yes","y"),  TRUE,
             ifelse(y %in% c("false","f","0","no","n"), FALSE, NA))
    }
  }
  
  # Hard-freeze events to FALSE
  ev_mask <- missingness_long_strict$variable %in% EVENTS
  missingness_long_strict$eligible_for_imputation[ev_mask] <- FALSE
  
  # Rounded missingness for the assertion
  prop_round <- if ("prop_missing_round" %in% names(missingness_long_strict)) {
    missingness_long_strict$prop_missing_round
  } else {
    round(missingness_long_strict$prop_missing, DECIMALS)
  }
  thr_round <- round(THRESHOLD, DECIMALS)
  
  # Expected eligibility for NON-events only (events always FALSE)
  ne_mask  <- !ev_mask
  expected <- rep(FALSE, nrow(missingness_long_strict))
  expected[ne_mask] <- prop_round[ne_mask] <= thr_round
  
  # Treat NA as FALSE on both sides before asserting
  expected[is.na(expected)] <- FALSE
  efi <- missingness_long_strict$eligible_for_imputation
  efi[is.na(efi)] <- FALSE
  
  stopifnot(identical(efi, expected))
}

# ========================================================================
# PATCH: Survival Pair Gating and Dependency-Aware Constraints (Per Type)
# ========================================================================
################################################################################
#
# Inputs:
#   - df: data.frame with columns:
#       type, OS, OS.time, DSS, DSS.time, DFI, DFI.time, PFI, PFI.time,
#       initial_pathologic_dx_year, age_at_initial_pathologic_diagnosis
#   - elig_tbl: strict per-type eligibility table with:
#       type, variable, eligible_for_imputation (≤35% rule already enforced upstream)
#
# Policies implemented:
#   1) Type-wise gate for each pair (OS/DSS/DFI/PFI):
#        - Both variables eligible in elig_tbl (≤35% within type, by rounded decision)
#        - Each has ≥ MIN_NONNA non-missing rows within type
#        - Event prevalence within [PREV_MIN, PREV_MAX] to avoid degenerate fits
#   2) Row-wise gate:
#        - If both event and time are NA after harmonization => do not impute (never fabricate both)
#        - If event observed & time NA => allow time imputation iff anchors present
#        - If event NA & time observed => do not impute event
#   3) Constraints for time (per row):
#        - Always: initial_pathologic_dx_year + (time/365) ≤ CUTOFF_YEAR
#        - If age_col provided and event==1: (time/365) + age ≤ MAX_AGE_YRS
#        - Non-negativity enforced only for events (event==1): time < 0 → 0
#
# Outputs:
#   - df005_survival_patch_gate_typepair.tsv        (per-type gating table)
#   - df005_survival_patch_<PAIR>_rowwise_gate.tsv  (row-wise flags per pair)
#   - df_out (in-memory): constrained df (time columns normalized if exceeding constraints)
################################################################################

# ------------------ Configuration ------------------
CUTOFF_YEAR <- 2017L
MAX_AGE_YRS <- 100
DAYS_PER_YR <- 365
MIN_NONNA   <- 5L
PREV_MIN    <- 0.05
PREV_MAX    <- 0.95

# ------------------ Pair definitions ----------------
SURV_PAIRS <- list(
  OS  = list(event = "OS",  time = "OS.time",  age_col = "age_at_initial_pathologic_diagnosis"),
  DSS = list(event = "DSS", time = "DSS.time", age_col = NULL),
  DFI = list(event = "DFI", time = "DFI.time", age_col = "age_at_initial_pathologic_diagnosis"),
  PFI = list(event = "PFI", time = "PFI.time", age_col = "age_at_initial_pathologic_diagnosis")
)

YEAR_COL <- "initial_pathologic_dx_year"

# ------------------ Helpers ------------------------
# Normalize events to 0/1 (robust to logical/num/factor/char)
# ------------------ Helpers (amended) ------------------------
# ------------------ Helpers (amended) ------------------------
# Normalize events to 0/1 (robust to logical/num/factor/char)
.to01_safe <- function(x) {
  if (is.logical(x)) return(as.integer(x))
  if (is.numeric(x)) return(as.integer(x))
  if (is.factor(x))  x <- as.character(x)
  x <- trimws(tolower(as.character(x)))
  out <- ifelse(x %in% c("1","true","t","yes","y"), 1L,
                ifelse(x %in% c("0","false","f","no","n"), 0L, NA_integer_))
  as.integer(out)
}

.is_eligible_var <- function(ET, tp, var) {
  hit <- ET[type == tp & variable == var]
  if (nrow(hit) == 0L) return(FALSE)
  isTRUE(hit$eligible_for_imputation[1])
}

.typewise_gate_for_pair <- function(DT, ET, tp, ev, tim,
                                    min_nonNA = MIN_NONNA,
                                    prev_min  = PREV_MIN,
                                    prev_max  = PREV_MAX) {
  # Work on normalized copies (no side effects on DT)
  dsub <- DT[type == tp, .(
    ev  = .to01_safe(get(ev)),                       # 0/1/NA
    tim = suppressWarnings(as.numeric(get(tim)))     # numeric/NA
  )]
  
  # counts after normalization
  n_nonNA_event <- sum(!is.na(dsub$ev))
  n_nonNA_time  <- sum(!is.na(dsub$tim))
  
  # eligibility flags from strict table
  eligible_ev  <- .is_eligible_var(ET, tp, ev)
  eligible_tim <- .is_eligible_var(ET, tp, tim)
  
  # prevalence on normalized event
  prev    <- if (n_nonNA_event > 0L) mean(dsub$ev == 1L, na.rm = TRUE) else NA_real_
  prev_ok <- !is.na(prev) && prev >= prev_min && prev <= prev_max
  
  # reasons (empty => gate TRUE)
  reasons <- character(0)
  if (!eligible_ev)              reasons <- c(reasons, "event_ineligible(>35%)")
  if (!eligible_tim)             reasons <- c(reasons, "time_ineligible(>35%)")
  if (n_nonNA_event < min_nonNA) reasons <- c(reasons, sprintf("n_nonNA_event<%d", min_nonNA))
  if (n_nonNA_time  < min_nonNA) reasons <- c(reasons, sprintf("n_nonNA_time<%d",  min_nonNA))
  if (!prev_ok)                  reasons <- c(reasons, sprintf("prevalence_out_of_bounds[%.2f,%.2f]", prev_min, prev_max))
  
  gate <- length(reasons) == 0L
  
  data.frame(
    type = tp,
    pair = paste(ev, tim, sep = "/"),
    event_var = ev,
    time_var  = tim,
    n_nonNA_event = n_nonNA_event,
    n_nonNA_time  = n_nonNA_time,
    event_prevalence = ifelse(is.na(prev), NA_real_, round(prev, 4)),
    eligible_event_by_threshold = eligible_ev,
    eligible_time_by_threshold  = eligible_tim,
    prevalence_ok = prev_ok,
    gate_imputation = gate,
    reason_blocked = if (gate) "OK" else paste(reasons, collapse = ";"),
    stringsAsFactors = FALSE
  )
}

.rowwise_gate_and_constraints <- function(DT, pair_name, ev, tim, year_col, age_col = NULL,
                                          cutoff_year = CUTOFF_YEAR,
                                          max_age_yrs = MAX_AGE_YRS,
                                          days_per_year = DAYS_PER_YR) {
  stopifnot(all(c("type", ev, tim, year_col) %in% names(DT)))
  if (!is.null(age_col)) stopifnot(age_col %in% names(DT))
  
  # normalize event once (robust to logical/num/factor/char)
  ev01 <- .to01_safe(DT[[ev]])
  
  # gates based on normalized event
  DT[, (paste0(pair_name, "_pair_missing_both")) := is.na(ev01) & is.na(get(tim))]
  only_ev_obs  <- !is.na(ev01) &  is.na(DT[[tim]])
  only_tim_obs <-  is.na(ev01) & !is.na(DT[[tim]])
  
  anchors_ok <- !is.na(DT[[year_col]])
  if (!is.null(age_col)) {
    anchors_ok <- anchors_ok & !(ev01 == 1L & is.na(DT[[age_col]]))
  }
  
  allow_time_impute <- only_ev_obs & anchors_ok
  DT[, (paste0(pair_name, "_allow_time_impute")) := allow_time_impute]
  DT[, (paste0(pair_name, "_disallow_all"))      := get(paste0(pair_name, "_pair_missing_both")) |
       only_tim_obs | (!allow_time_impute & only_ev_obs)]
  
  # year cutoff and (optional) age cap
  max_days_year <- pmax(0, (cutoff_year - DT[[year_col]]) * days_per_year)
  max_days_age  <- if (is.null(age_col)) {
    rep(Inf, nrow(DT))
  } else {
    ifelse(ev01 == 1L, pmax(0, (max_age_yrs - DT[[age_col]]) * days_per_year), Inf)
  }
  admissible_max_days <- pmin(max_days_year, max_days_age, na.rm = TRUE)
  
  # non-negativity only when event==1
  idx_neg <- which(!is.na(DT[[tim]]) & ev01 == 1L & DT[[tim]] < 0)
  if (length(idx_neg) > 0L) DT[idx_neg, (tim) := 0]
  
  # cap times that exceed admissible maxima
  idx_cap <- which(!is.na(DT[[tim]]) & !is.na(admissible_max_days) & DT[[tim]] > admissible_max_days)
  if (length(idx_cap) > 0L) DT[idx_cap, (tim) := admissible_max_days[idx_cap]]
  
  gate_cols <- c(paste0(pair_name, "_pair_missing_both"),
                 paste0(pair_name, "_allow_time_impute"),
                 paste0(pair_name, "_disallow_all"))
  list(DT = DT, gate_cols = gate_cols)
}

run_survival_pair_patch <- function(df, elig_tbl,
                                    surv_pairs = SURV_PAIRS,
                                    year_col = YEAR_COL,
                                    min_nonNA = MIN_NONNA,
                                    prev_min = PREV_MIN, prev_max = PREV_MAX,
                                    cutoff_year = CUTOFF_YEAR,
                                    max_age_yrs = MAX_AGE_YRS,
                                    days_per_year = DAYS_PER_YR,
                                    export_prefix = "df005_survival_patch") {
  req <- c("type", year_col)
  stopifnot(all(req %in% names(df)))
  
  DT <- as.data.table(df)
  ET <- as.data.table(elig_tbl)
  types <- unique(DT$type)
  
  # ---- Type-wise gating across all pairs
  gate_typepair <- rbindlist(lapply(types, function(tp) {
    rbindlist(lapply(names(surv_pairs), function(pn) {
      ev  <- surv_pairs[[pn]]$event
      tim <- surv_pairs[[pn]]$time
      .typewise_gate_for_pair(DT, ET, tp, ev, tim, min_nonNA, prev_min, prev_max)
    }))
  }), use.names = TRUE)
  
  write_tsv(as.data.frame(gate_typepair),
            paste0(export_prefix, "_gate_typepair.tsv"))
  
  # ---- Row-wise gating + constraints (constraints safe globally; imputation only where gate_imputation == TRUE)
  for (pn in names(surv_pairs)) {
    ev  <- surv_pairs[[pn]]$event
    tim <- surv_pairs[[pn]]$time
    age <- surv_pairs[[pn]]$age_col
    
    res <- .rowwise_gate_and_constraints(DT, pn, ev, tim, year_col, age,
                                         cutoff_year, max_age_yrs, days_per_year)
    DT  <- res$DT
    
    gate_cols <- c("type", ev, tim, res$gate_cols)
    write_tsv(as.data.frame(DT[, ..gate_cols]),
              paste0(export_prefix, "_", pn, "_rowwise_gate.tsv"))
  }
  
  list(
    gate_typepair = as.data.frame(gate_typepair),
    df_out        = as.data.frame(DT)
  )
}

# ---- Policy Commentary -------------------------------------------------------
# Type-wise gating requires both variables in a survival pair to be eligible (≤ 35% missing,
# by the rounded decision) and sufficiently observed (≥ 5 non-missing per variable within type),
# with non-degenerate event prevalence (e.g., 0.05 ≤ Pr(event) ≤ 0.95).
#
# Row-wise gating prohibits simultaneous fabrication of event and time; time-only imputation
# is permitted exclusively when the event is observed and required anchors exist
# (e.g., initial_pathologic_dx_year; plus age for OS when event == 1).
#
# Constraints enforce temporal coherence across pairs via a diagnosis-year cutoff and, when
# applicable, an age-based maximum; non-negativity is enforced only for events (OS by default).
#
# This patch does not perform imputation. It determines where imputation is permissible and
# normalizes candidate time values. Trigger existing imputation routines only for (type, pair)
# combinations with gate_typepair$gate_imputation == TRUE and for rows with <PAIR>_allow_time_impute == TRUE.

# ---- Run patch ---------------------------------------------------------------
res <- run_survival_pair_patch(
  df            = df005,
  elig_tbl      = missingness_long_strict,
  export_prefix = "df005_survival_patch"
)

# >>> Expose for downstream modules (Module 6) <<<
gate_typepair <- res$gate_typepair   # <-- add this line

# (optional) keep the constrained df too
df_ready <- res$df_out

# (optional) quick sanity check
stopifnot(
  is.data.frame(gate_typepair),
  all(c("type","event_var","time_var","gate_imputation") %in% names(gate_typepair))
)

# (optional) persist for later sessions
# saveRDS(gate_typepair, "gate_typepair.rds")

# Inspect
head(gate_typepair)

# =====================================================================
# Output files generated by the missingness audit + survival pair patch
# =====================================================================
#
  # df005_missingness_by_type_STRICT.tsv
#   – Full missingness audit per type and variable (clinical/omic),
#     including raw and rounded proportions, counts, and eligibility flags.
#
# df005_eligibility_counts.tsv
#   – Summary of eligibility proportions per type and group (clinical vs omic).
#
# df005_missingness_boundary_EQ_threshold.tsv
#   – Variables exactly at the 0.35 missingness threshold (for audit review).
#     "EQ" stands for "equal to".
#
# df005_survival_patch_gate_typepair.tsv
#   – Type-level imputation decision for each survival pair
#     (OS, DSS, DFI, PFI) after applying ≤35% missingness rule,
#     minimum non-missing counts, and prevalence bounds.
#
# df005_survival_patch_OS_rowwise_gate.tsv
#   – Row-level gating for OS/OS.time, including:
#       *_pair_missing_both, *_allow_time_impute, *_disallow_all flags.
#
# df005_survival_patch_DSS_rowwise_gate.tsv
#   – Row-level gating for DSS/DSS.time.
#
# df005_survival_patch_DFI_rowwise_gate.tsv
#   – Row-level gating for DFI/DFI.time.
#
# df005_survival_patch_PFI_rowwise_gate.tsv
#   – Row-level gating for PFI/PFI.time.

# ================================================================================================================
#### Excluding omic variables that failed to comply with the na_missingness  permissive burden threshold of<= 0.35
## ===============================================================================================================
#' Exclude df005 columns flagged as ineligible (omic group) in STRICT table
## Exclude df005 columns using strict table where: group == "omic" & eligible_for_imputation == FALSE
## Outputs:
##   - df005_filtered_out_OMIC_FALSE_INELIGIBLE.rds
##   - df005_excluded_variables.tsv
##   - df005_exclusion_summary.tsv
##   - (optional) df005_exclusion_flagged_not_in_df005.tsv
## =============================================================================
#' Exclude df005 columns flagged as ineligible (omic group) in STRICT table
#'
#' @description
#' Loads `df005.rds`, reads the strict eligibility table, selects rows where
#' `group == "omic"` **and** `eligible_for_imputation == FALSE`, and drops the
#' corresponding `variable`-named columns from `df005`. Emits reproducible audit
#' artifacts and saves the filtered object.
#'
#' @param strict_path Character. Path to the strict eligibility TSV.
#'   Defaults to `"df005_missingness_by_type_STRICT.tsv"`. If not found, a
#'   fallback to `..._STRIC.tsv` is attempted.
#' @param df_rds Character. Path to the source object `"df005.rds"`.
#' @param prefix Character. File prefix for outputs (e.g., `"df005"`).
#'
#' @return (Invisibly) the filtered `data.frame` (`df005_filtered`).
#'
#' @details
#' **Selection rule:** keep only features *not* listed in the strict table with
#' `group == "omic"` and `eligible_for_imputation == FALSE`. The `variable`
#' column must match column names in `df005` exactly; variables flagged in the
#' strict table but absent from `df005` are reported separately and ignored.
#'
#' **Outputs written (side effects):**
#' - `<prefix>_filtered.rds` — filtered object.
#' - `<prefix>_excluded_variables.tsv` — exact list of dropped variables.
#' - `<prefix>_exclusion_summary.tsv` — counts and before/after column totals.
#' - `<prefix>_exclusion_flagged_not_in_df005.tsv` — variables flagged in STRICT
#'   but not present in `df005` (emitted only if non-empty).
#'
#' **Robustness:**
#' - Tolerates common casing variants for `variable`, `group`,
#'   `eligible_for_imputation`.
#' - Coerces truthy/falsy strings/ints into logicals.
#' - Enforces deterministic behavior and clear failure modes.
#'
#' @examples
#' \dontrun{
#' run_df005_omic_ineligible_exclusion()
#' run_df005_omic_ineligible_exclusion(
#'   strict_path = "df005_missingness_by_type_STRICT.tsv",
#'   df_rds      = "df005.rds",
#'   prefix      = "df005"
#' )
#' }
run_df005_omic_ineligible_exclusion <- function(
    strict_path = "df005_missingness_by_type_STRICT.tsv",
    df_rds      = "df005.rds",
    prefix      = "df005"
) {
  # --- Dependencies kept minimal; load lazily to avoid global side effects ----
  if (!requireNamespace("rio", quietly = TRUE)) install.packages("rio")
  
  # --- Local helpers: logging, safe I/O, schema resolution, boolean coercion ---
  if (!exists("log_msg", mode = "function")) {
    log_msg <- function(...) cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "—", ..., "\n")
  }
  if (!exists("safe_import_tsv", mode = "function")) {
    safe_import <- function(file, format = NULL, ...) rio::import(file, format = format, na.strings = "NA", ...)
  }
  if (!exists("safe_export_tsv", mode = "function")) {
    safe_export_tsv <- function(object, file, ...) rio::export(object, file = file, format = "tsv", na = "NA", ...)
  }
  if (!exists("safe_readRDS", mode = "function")) {
    # Guarantees a rectangular object with unique colnames
    safe_readRDS <- function(path, expected = c("data.frame", "data.table", "tbl_df")) {
      if (!file.exists(path)) stop("File not found: ", path, call. = FALSE)
      obj <- readRDS(path)
      if (!any(expected %in% class(obj))) obj <- as.data.frame(obj)
      if (is.null(names(obj))) names(obj) <- character(ncol(obj))
      if (anyDuplicated(names(obj))) names(obj) <- make.unique(names(obj), sep = "_")
      obj
    }
  }
  pick_col <- function(tbl, ...) {
    cn <- names(tbl); opts <- c(...)
    hit <- opts[opts %in% cn][1]
    if (is.na(hit)) stop("Missing required column among: ", paste(opts, collapse = ", "))
    hit
  }
  to_logical <- function(x) {
    if (is.logical(x)) return(x)
    y <- toupper(trimws(as.character(x)))
    out <- ifelse(y %in% c("TRUE","T","1","YES"), TRUE,
                  ifelse(y %in% c("FALSE","F","0","NO"), FALSE, NA))
    as.logical(out)
  }
  
  # ========================== Load source object ===============================
  # Single source of truth: df005 from RDS. Keeps this function idempotent and
  # independent of the caller's workspace state.
  df005 <- safe_readRDS(df_rds)
  original_ncol <- ncol(df005)
  
  # ==================== Load STRICT and resolve schema ========================
  # Fallback accommodates a common filename typo without weakening reproducibility.
  if (!file.exists(strict_path)) {
    alt <- sub("STRICT", "STRIC", strict_path, fixed = TRUE)
    if (file.exists(alt)) {
      strict_path <- alt
    } else {
      stop("Strict table not found: ", strict_path, call. = FALSE)
    }
  }
  strict <- safe_import(strict_path, format = "tsv")
  
  # Resolve required columns with tolerant matching
  col_variable <- pick_col(strict, "variable", "Variable")
  col_group    <- pick_col(strict, "group", "Group")
  col_eligible <- pick_col(strict, "eligible_for_imputation", "eligible", "Eligible_for_imputation", "Eligible")
  
  # Harmonize eligibility to logical; unknown values become NA (ignored downstream)
  strict[[col_eligible]] <- to_logical(strict[[col_eligible]])
  
  # ======================= Select rows to exclude =============================
  # Only remove features that satisfy the exact governance rule.
  is_omic       <- toupper(trimws(as.character(strict[[col_group]]))) == "OMIC"
  is_ineligible <- !is.na(strict[[col_eligible]]) & !strict[[col_eligible]]
  strict_excl   <- strict[is_omic & is_ineligible, c(col_variable, col_group, col_eligible), drop = FALSE]
  names(strict_excl) <- c("variable", "group", "eligible_for_imputation")
  
  # ================== Build exclusion list present in df005 ===================
  # Prevents accidental errors when STRICT contains variables not in df005.
  vars_flagged      <- unique(as.character(strict_excl$variable))
  vars_to_remove    <- intersect(vars_flagged, names(df005))
  vars_not_in_df005 <- setdiff(vars_flagged, names(df005))
  
  # ======================== Apply exclusion and audit =========================
  df005_filtered <- df005[, setdiff(names(df005), vars_to_remove), drop = FALSE]
  remaining_ncol <- ncol(df005_filtered)
  
  excluded_tbl <- data.frame(
    variable                = sort(vars_to_remove),
    group                   = "omic",
    eligible_for_imputation = FALSE,
    stringsAsFactors = FALSE
  )
  summary_tbl <- data.frame(
    group              = "omic",
    excluded_count     = length(vars_to_remove),
    original_columns   = original_ncol,
    remaining_columns  = remaining_ncol,
    stringsAsFactors   = FALSE
  )
  
  # ============================== Persist =====================================
  safe_export_tsv(excluded_tbl, paste0(prefix, "_excluded_variables.tsv"))
  safe_export_tsv(summary_tbl,  paste0(prefix, "_exclusion_summary.tsv"))
  if (length(vars_not_in_df005)) {
    safe_export_tsv(
      data.frame(variable = sort(vars_not_in_df005),
                 note = "flagged_in_STRICT_but_not_in_df005",
                 stringsAsFactors = FALSE),
      paste0(prefix, "_exclusion_flagged_not_in_df005.tsv")
    )
  }
  saveRDS(df005_filtered, paste0(prefix, "_filtered_out_OMIC_FALSE_INELIGIBLE.rds"))
  
  # ============================== Log =========================================
  log_msg(sprintf("Excluded %d 'omic' variables. %s: %d -> %d columns.",
                  length(vars_to_remove), prefix, original_ncol, remaining_ncol))
  
  invisible(df005_filtered)
}

# Executar função com nomes padrão
run_df005_omic_ineligible_exclusion()

## ---------------------------------------------------------------------------
## Import df005_filtered_out_OMIC_FALSE_INELIGIBLE.rds (robust)
## - Verifies file presence
## - Coerces to data.frame if needed
## - Ensures unique column names
## - Logs dimensions
## ---------------------------------------------------------------------------

# Lightweight logger (define only if absent)
if (!exists("log_msg", mode = "function")) {
  log_msg <- function(...) cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "—", ..., "\n")
}

# Safe RDS reader (define only if absent)
if (!exists("safe_readRDS", mode = "function")) {
  safe_readRDS <- function(path, expected = c("data.frame", "data.table", "tbl_df")) {
    if (!file.exists(path)) stop("File not found: ", path, call. = FALSE)
    obj <- readRDS(path)
    if (!any(expected %in% class(obj))) obj <- as.data.frame(obj)
    if (is.null(names(obj))) names(obj) <- character(ncol(obj))
    if (anyDuplicated(names(obj))) {
      warning("Duplicated column names detected; enforcing uniqueness via make.unique().")
      names(obj) <- make.unique(names(obj), sep = "_")
    }
    obj
  }
}

# ---- Load into "new" fresh df005 and report fro downstream analysis
df005 <- safe_readRDS("df005_filtered_out_OMIC_FALSE_INELIGIBLE.rds")
log_msg("Loaded df005_filtered_out_OMIC_FALSE_INELIGIBLE.rds -> 'df005' with ",
        nrow(df005), " rows and ", ncol(df005), " columns.")

# rm(df005_filtered)

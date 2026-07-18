# PH Violation Audit — Independent Schoenfeld Residual Analysis

## Purpose
Independent statistical validation of the CANARY diagnostic's finding that proportional hazards (PH) assumptions are pervasively violated across pan-cancer multi-omic survival topologies. This audit applies standard unpenalized Cox PH regression and Schoenfeld residual tests (cox.zph) to all 120 CANARY-tested cancer-endpoint combinations, using the identical LiSHMOM data pipeline, as an independent statistical test beyond the CoxNet convergence-based CANARY approach.

## Data Source
- `CoxNet_phaseII_feasibility_log.rds` — CANARY Phase II audit log (120 cancer-endpoint-df combinations)
- `dfXXX_series/` — LiSHMOM imputed matrix variants (df006-df377)

## Scripts

| File | Description |
|------|------|
| `preflight_PH_audit.R` | Preflight check — verifies CANARY log, df files, R packages |
| `PH_violation_audit.R` | Main audit — replicates CANARY pipeline, runs Cox PH + Schoenfeld, generates log-log plots |
| `loglog_plots.R` | Standalone log-log plot generator (backup) |

## Output

| File | Description |
|------|------|
| `Schoenfeld_120_Strata.tsv` | Complete per-stratum results (120 rows, 11 columns) |
| `LogLog_Survival_Plots.pdf` | Log-log diagnostic plots for the four most severe PH-violating strata |
| `LogLog_Survival_Plots.tiff` | Same as above, publication-ready 600 dpi LZW compression |
| `Scaled_Schoenfeld_Plots.pdf` | Scaled Schoenfeld residual plots |
| `Scaled_Schoenfeld_Plots.tiff` | Same as above, 600 dpi LZW compression |

## Key Results Summary

- **120** CANARY-tested strata audited
- **118/120** Cox PH models converged (2 structural failures)
- **97/118** exceeded Schoenfeld computational capacity
- **21** strata yielded valid Schoenfeld p-values
- **6** demonstrated significant PH violations (p < 0.05): ESCA-PFI, LAML-OS, OV-DSS, OV-OS, OV-DFI, ACC-PFI
- Median Schoenfeld p: **0.2065**

## Methodology
Standard unpenalized Cox PH models fitted under identical CANARY gating constraints. Global Schoenfeld test applied to each converged model. Non-convergent and non-evaluable strata documented separately.

## Requirements
- R ≥ 4.5.1
- R packages: `survival`, `data.table`, `dplyr`

## Citation
Conducted in response to Reviewer 1 (Round 2), Manuscript ID 1895164, Frontiers in Artificial Intelligence, Medicine and Public Health.

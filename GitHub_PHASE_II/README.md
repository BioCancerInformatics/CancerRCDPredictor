# Phase II — TAR Admissibility and CANARY Feasibility Audit

## Execution Order

| # | Script | Input Dependencies | Role |
|---|---|---|---|
| 001 | RScript_Phase_II_Action_Policy.R | improved_unchanged_best_fullset.tsv, df005.rds | Core: TAR (Transformation Admissibility Routing) gate + CANARY (CoxNet) feasibility audit across all cancer–endpoint–dfXXX strata. Produces CoxNet_phaseII_feasibility_log.tsv. |
| 002 | RScript_Setup_Kaleido.R | — | Install kaleido rendering engine (required for plotly image export) |
| 003 | RScript_Check_Export.R | kaleido (from 002) | Verify kaleido export functionality |
| 004 | RScript_Dynamic_Sankey_Lineage_Explorer.R | improved_unchanged_best_fullset.tsv, CoxNet_phaseII_feasibility_log.tsv (from 001) | Interactive Sankey diagram with TAR and CANARY node annotations. Produces sankey_lineage_annotated_default_landscape.html. |
| 005 | RScript_Export_Sankey_Highres.R | improved_unchanged_best_fullset.tsv, CoxNet_phaseII_feasibility_log.tsv (from 001); kaleido (from 002). Cross-phase: sources ../PHASE_III/Rscript_IMPUTATION LINEAGE ENGINE.R | Export high-resolution Sankey lineage diagram (PDF, PNG, TIFF via kaleido) |
| 006 | RScript_Generate_Images.R | sankey_lineage_annotated_default_landscape.html (from 004) | Capture Sankey HTML as publication-quality images via webshot2 |

## Design Notes

- Phase II serves as a structural audit, not a predictive exercise.
- The CANARY diagnostic (CoxNet) probes proportional-hazards feasibility under auditable constraints.
- TAR classifies each preprocessing regime as Unchanged, Improved, or Degraded relative to df005.
- Only TAR-admissible (Unchanged/Improved) regimes proceed to Phase III.
- All operations are endpoint-specific (OS, DSS, DFI, PFI) and executed strictly per cancer type.
- Script 005 contains a cross-phase dependency on the Phase III imputation lineage engine for lineage data retrieval.

## Environment Replication

Generate the lock file for this phase by running the following command in R on the local workstation where Phases I\u2013II were executed:

```r
renv::snapshot()
```

Rename the resulting `renv.lock` to `renv_phase_I_II.lock` and place it in the repository root.

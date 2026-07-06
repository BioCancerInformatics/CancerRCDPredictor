# Phase I — Data Harmonization and Imputation

## Execution Order

| # | Script | Role |
|---|---|---|
| 001 | RScript_Data_Retrieval_Ucscxenashiny.R | Setup UCSCXenaShiny for TCGA data retrieval |
| 002 | RScript_Data_Harmonization.R | Harmonize raw clinical, demographic, and survival data |
| 003 | RScript_Audit_Dataset_S1.R | Validate signature catalog (Dataset S1) |
| 004 | RScript_Data_Assembly.R | Build df005 baseline multi-omic matrix |
| 005 | RScript_Missingness_Audit_Baseline.R | Audit missingness on df005 (pre-imputation) at 35% threshold |
| 006 | RScript_Cnv_Mutation_Imputation.R | CNV and mutation imputation; produces df006–df017 |
| 007 | RScript_Conditional_Imputation_Modules.R | Conditional imputation modules processing df006+ |
| 008 | RScript_Universal_Resume_Engine_Continuous.R | Continuous omic imputation with checkpointing; produces df054–df377 |
| 009 | RScript_Imputation_Lineage_Engine.R | Trace imputation provenance and lineage from df377 |
| 010 | RScript_Missingness_Audit_Final.R | Audit missingness on df377 (post-imputation) |
| 011 | RScript_Survival_Time_Audit.R | Pre- vs post-imputation survival time comparison |
| 012 | RScript_Imputation_Evaluation_Reporting.R | Cox C-index evaluation of imputation variants |
| 013 | RScript_Heatmap_Performance_Evaluation.R | Performance heatmap visualization |
| 014 | RScript_Signature_Distribution.R | Signature distribution plots |
| 015 | RScript_Sankey_Lineage.R | Sankey lineage diagram |

## Design Notes

- The pipeline is designed as a batch framework processing all 33 cancer types and 4 survival endpoints simultaneously.
- Scripts are modular: each phase sources outputs from the preceding script(s).
- Outputs include serialized `.rds` objects, audit logs (`.tsv`), performance reports, and visualizations.
- Total output: approximately 120 GB of data.

## Environment Replication

Generate the lock file for this phase by running the following command in R on the local workstation where Phases I\u2013II were executed:

```r
renv::snapshot()
```

Rename the resulting `renv.lock` to `renv_phase_I_II.lock` and place it in the repository root.

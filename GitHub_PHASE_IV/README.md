# Phase IV — CancerRCDPredictor Shiny Application

## Execution Order

| # | Script | Role |
|---|---|---|
| 001 | RScript_App.R | Main Shiny application: interactive web interface for multi-omic survival topology exploration, SHAP visualization, and LLM-powered hypothesis generation |
| 002 | RScript_Ui_Blocks.R | Modular UI components for the Shiny application |
| 003 | RScript_Pharmacogenomic_Matrix.R | Generate unified pharmacogenomic matrix integrating OncoKB, CIViC, and DGIdb evidence tiers |
| 004 | RScript_Oncokb_Actionability.R | OncoKB clinical actionability layer: Tier 0 regulatory and clinical actionability bridge |
| 005 | RScript_Validate_Crit03_Powered.R | Validation criterion 3: powered validation of clinical report generation |
| 006 | RScript_Validate_Crit03_Reproducibility.R | Two-level reproducibility validation for clinical report generation |
| 007 | RScript_Validate_Crit05_Ecological_Fallacy.R | Ecological fallacy detection validation across patient SHAP profiles |
| 008 | RScript_Audit_Governance.R | Code quality, security, error handling, and LLM governance audit |
| 009 | RScript_Inspect.R | Quick inspection and data validation utility |

## Design Notes

- Phase IV serves as the translational interface for the complete Phase I–III pipeline.
- The Shiny application provides four primary analytical modules: Global Impact (SHAP), Interaction Topologies, MVL Performance, and Precision Oncology with AI Diagnostic Synthesis.
- The LLM module operates under a 19-point governance framework (G1–G19) with a three-layer analytical validation architecture.
- All benchmark results are archived with deterministic random seeds for bitwise reproducibility.

## Environment Replication

Generate the lock file for this phase by running the following command in R on the ZIMA dedicated server where Phases III\u2013IV were executed:

```r
renv::snapshot()
```

Rename the resulting `renv.lock` to `renv_phase_III_IV.lock` and place it in the repository root.

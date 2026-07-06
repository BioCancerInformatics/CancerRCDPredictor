# Phase IV — CancerRCDPredictor Shiny Application

## R Scripts

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

## Supporting Data Files

| File | Description |
|---|---|
| MASTER_Phase_III_Performance.csv | Phase III pan-cancer performance metrics (C-index, AUC) |
| Master_ZIMA_Mathematical_Interaction_Proof_Matrix.csv | SHAP interaction classification matrix |
| Table_S11_Interpreter_12k.csv | 12,613 retained prognostic features |
| Table_S12_Golden_150.csv | 150 golden anchor signatures (4/4 quadripartite validation) |
| Table_S15_Master_ZIMA_Strict_Mathematical_Classification.csv/xlsx | Strict mathematical classification of SHAP interactions |
| Table_S11_S12_Column_Descriptors.csv | Column metadata for Tables S11 and S12 |
| Pan_Cancer_samples_ID.tsv | Pan-cancer sample identifiers |
| Merged_Cancer_Stemness.tsv | Tumor stemness indices (RNAss, DNAss) |
| tcga_stemness.rda | TCGA stemness data (R binary) |
| tcga_tmb.rda | TCGA tumor mutational burden data |
| tcga_MSI.rda | TCGA microsatellite instability data |
| oncoKB_cancerGeneList.tsv | OncoKB cancer gene list |
| OncoKB_Gene_Annotations.rds | OncoKB gene annotations |
| FDA_level_2_oncokb_biomarker_drug_associations.tsv | FDA Level 2 biomarker-drug associations |
| FDA_level_3_oncokb_biomarker_drug_associations.tsv | FDA Level 3 biomarker-drug associations |
| fda_approved_oncology_therapies.xlsx | FDA-approved oncology therapies |
| Unified_Drug_Matrix.rds | Unified pharmacogenomic drug matrix |
| Gene_Biological_Roles.csv | Gene biological role annotations |
| NCBI_gene_info.csv | NCBI gene information |
| Committee_RCD_Operational_Definitions.xlsx | RCD operational definitions |
| Extended_RCD_Operational_Definitions.xlsx | Extended RCD definitions |
| RCD_25_Forms_Reference_Table_FINAL.html | 25 RCD forms reference table |
| RCD_Biological_Context_Decoder.txt | RCD biological context decoder |
| ai_clinical_report.Rmd | AI-generated clinical report template |
| clinical_report.Rmd | Clinical report template |
| oncokb_actionable_test.html | OncoKB actionability test page |
| www/ | Shiny app static assets (CSS, JS, images) |

## Design Notes

- Phase IV serves as the translational interface for the complete Phase I–III pipeline.
- The Shiny application provides four primary analytical modules: Global Impact (SHAP), Interaction Topologies, MVL Performance, and Precision Oncology with AI Diagnostic Synthesis.
- The LLM module operates under a 19-point governance framework (G1–G19) with a three-layer analytical validation architecture.
- All benchmark results are archived with deterministic random seeds for bitwise reproducibility.

## Environment Replication

The lock file for this phase was generated on the ZIMA dedicated server using:

```r
renv::init()
```

The resulting `renv.lock` was renamed to `renv_phase_III_IV.lock` and placed in the Phase III GitHub folder.

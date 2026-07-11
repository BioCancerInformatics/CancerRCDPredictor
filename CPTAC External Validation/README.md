# CPTAC External Validation — CancerRCDPredictor

External validation of the frozen TCGA Phase III Multi-Omic SuperLearner models on the Clinical Proteomic Tumor Analysis Consortium (CPTAC) cohort.

## Overview

The TCGA-trained Quadripartite ML Ensemble (RSF, XGBoost, MTLR, Boruta) with MVL SuperLearner synthesis was applied without retraining to 1,039 CPTAC patients across 10 cancer types, all evaluated for Overall Survival.

## Key Results

| Metric | Finding |
|--------|---------|
| **Significant generalization** | KIRC: C-index 0.649, p = 0.035 |
| **Approaching significance** | LUSC: C-index 0.586, p = 0.113 |
| **Median C-index (≥20 events)** | 0.555 across 6 cancers |
| **Best temporal AUC** | PAAD at 5yr (0.802), KIRC at 3yr (0.685) |
| **Cross-cohort Spearman ρ** | −0.382 (TCGA vs CPTAC) |
| **Systematic calibration shift** | All O/E < 1.0 (expected for external cohort) |

## Files

| File | Description |
|------|-------------|
| `001_RScript_Preflight_CPTAC_Validation.R` | Pre-flight check — verifies all assets before running validation |
| `002_RScript_CPTAC_Validation_Predictor.R` | Main validation pipeline — applies frozen TCGA models to CPTAC |
| `cptac_validation_results.tsv` | Results table — per-cancer metrics (C-index, AUC, Brier, calibration) |
| `cptac_validation.log` | Execution log |

## Validation Design

- **Models**: TCGA Phase III, frozen — no retraining
- **Feature space**: CPTAC omic predictors are a strict subset of TCGA features (44–77% retention per cancer)
- **Missing data**: Handled per algorithm (XGBoost native routing, RSF proximity imputation, MTLR missForest)
- **MVL synthesis**: TCGA elastic net weights + TCGA scaling parameters applied to CPTAC predictions
- **Metrics**: C-index, permutation p-value, bootstrap 95% CI, time-dependent AUC (1/3/5yr), Brier, IBS, calibration slope, O/E ratio
- **Event-gating**: AUC/Brier/CI only for ≥20 events; calibration for ≥10 events
- **Stratified reporting**: Low-event cancers (≤10 events) flagged with caveats

## Data Sources

- **TCGA models**: `~/students/aluno0549-6/PHASE_III/PHASE_III_Megarun_5_0_complete_ZIMA_Suit_Generator_final/PHASE_III_ML_Models/`
- **CPTAC matrices**: Imputed using identical Phase I engine as TCGA, mirroring df variants 008, 017, 147, 161, 368, 377
- **Training matrices**: `~/students/aluno0549-6/dfXXX_series/`

## Supplementary Tables

| Table | Content |
|-------|---------|
| S25 | TCGA vs CPTAC cohort comparison (n, events, omic variables per cancer per layer) |
| S26 | Per-cancer CPTAC validation metrics (all statistics, all time horizons) |

## Reproducibility

The complete validation pipeline is self-contained:
1. Run `001_RScript_Preflight_CPTAC_Validation.R` to verify assets
2. Run `002_RScript_CPTAC_Validation_Predictor.R` to execute validation
3. Results are saved to `cptac_validation_results.tsv`

All random seeds are documented. Package dependencies auto-install.

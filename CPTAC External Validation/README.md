# CPTAC External Quadripartite Validation

**Manuscript:** A Pan-Cancer Multi-Omic SuperLearner for Regulated Cell Death Survival Topologies  
**Reviewer:** R3, Round 2  
**Date:** July 2026  

---

## Overview

This folder contains the complete outputs of the CPTAC external validation, extended from MVL-only (Round 1) to all four base learners and the MVL SuperLearner ensemble. Frozen TCGA Phase III model bundles were applied to the CPTAC validation cohort without retraining, re-tuning, or feature re-selection.

### Learners evaluated

| Learner | C-index source | CPTAC valid? |
|------|------|:---:|
| XGBoost | `predict(bundle$XGBoost, ...)` on CPTAC matrix | ✅ |
| Random Survival Forest (RSF) | `predict(bundle$RSF, ...)` on CPTAC matrix (OOB-trained bundle) | ✅ |
| MTLR | Manual reconstruction from `weight_matrix` (Boruta-sparse: 1-8 features per cancer, Megarun line 285) | ✅ (3 cancers) |
| Boruta-RSF | `predict(b_mod_tcga, ...)` on CPTAC matrix | ✅ |
| MVL SuperLearner | Elastic Net combination of 4 scaled columns | ✅ |

---

## Output files

| File | Description |
|------|------|
| `cptac_validation_results_quadripartite.tsv` | Per-cancer quadripartite C-indices, delta (internal − external), MVL metrics |
| `cptac_validation_quadripartite.log` | Full execution log including all 4 surprise tests |
| `cptac_km_tertiles.pdf` / `.tiff` | MVL SuperLearner KM tertile curves (CPTAC, 600 dpi) |
| `cptac_km_tertiles_XGBoost.pdf` / `.tiff` | XGBoost KM tertile curves |
| `cptac_km_tertiles_RSF.pdf` / `.tiff` | RSF KM tertile curves |
| `cptac_km_tertiles_MTLR.pdf` / `.tiff` | MTLR KM tertile curves |
| `cptac_km_tertiles_Boruta.pdf` / `.tiff` | Boruta KM tertile curves |

---

## Source code

| File | Purpose |
|------|------|
| `001_RScript_Preflight_CPTAC_Validation_Quadripartite.R` | Pre-flight check: verifies all CPTAC matrices, TCGA model bundles, MVL weights, performance files, feature alignment, and R packages |
| `002_RScript_CPTAC_Validation_Quadripartite.R` | Main pipeline: loads CPTAC data, applies frozen TCGA bundles, extracts per-learner C-indices, runs 4 surprise tests, generates KM plots |

Both scripts require ZIMA server access with the Phase III Megarun assets in place. Set working directory to:
```
~/students/aluno0549-6/PHASE_III/PHASE_III_Megarun_5_0_complete_ZIMA_Suit_Generator_final
```

---

## Key findings

### 1. RSF (OOB-based) outperforms XGBoost (in-sample) on CPTAC
RSF was the best-performing learner in **6 of 9 evaluable cancers** (median CPTAC C-index 0.617 vs. 0.546 for XGBoost).

### 2. XGBoost exhibits severe internal optimism
Median internal−external delta (Δ) for XGBoost: **+0.452**. RSF median Δ: **−0.036** (essentially zero).

### 3. MVL Elastic Net λ-CV detected the overfit signals
MVL weight correlation with CPTAC C-index: RSF ρ = **+0.287**, XGBoost ρ = **−0.050**, Boruta ρ = **+0.040**.

### 4. Internal ranking does not predict external performance
**67% of cancers** (6/9 with valid external C-indices) flipped their "best" learner between internal TCGA and external CPTAC. BRCA (2 events) excluded due to all-NA CPTAC C-indices.

### 5. MVL produces significant CPTAC risk stratification
Combined CPTAC KM log-rank test: **p < 0.000001**.

### 6. Cross-cohort rank correlation
Spearman ρ between TCGA and CPTAC MVL C-indices: **−0.250** (inverted difficulty ranking).

---

## Four surprise tests

| Test | Metric | Result |
|:---:|------|------|
| 1 | Δ per learner (TCGA − CPTAC) | d_XGB=+0.452, d_RSF=−0.036, d_MTLR=+0.047, d_Bor=+0.103, d_MVL=+0.125 |
| 2 | λ-CV leak-detector (ρ) | XGBoost −0.050, RSF +0.287, Boruta +0.040 |
| 3 | Dominance flips | 6/9 cancers (67%) |
| 4 | MVL KM tertile log-rank | p < 0.000001 |

---

## Supplementary Table S25

The updated Supplementary Table S25 (`Rodrigues_et_al_Supplementary_Tables_S1_to_S26_Round_2.XLSX`) contains the complete 26-column quadripartite panel:
- cohort, cancer, n, n_events
- c_xgb, c_rsf, c_mtlr, c_bor, c_mvl
- d_XGB, d_RSF, d_MTLR, d_Bor, d_MVL
- p_perm, c_boot, ci_low, ci_high
- cal_slope, cal_se, oe_ratio
- auc_1yr, auc_3yr, auc_5yr
- brier_1yr, ibs

---

## Technical notes

- **MTLR:** The MTLR `mtlr` package has no `predict()` method for RDS-loaded frozen objects. Predictions were reconstructed manually from the internal `weight_matrix` and feature terms stored in the bundle.
- **MTLR NA cancers:** MTLR produced valid C-indices in 3 cancers (COAD, HNSC, OV) where the Boruta-confirmed features used for MTLR training were present in the CPTAC validation matrix. For the remaining 6 cancers, the Boruta-confirmed features (typically 1–8 per cancer, intentionally sparse by pipeline design) are not among the CPTAC columns. XGBoost avoids this because it retains all 267–912 features per cancer, guaranteeing broad overlap with CPTAC. This is a known consequence of the Boruta→MTLR sparse-model path, not a code or matrix error.
- **BRCA (2 events):** All learners reported NA — below the threshold of ≥3 events required for C-index computation.
- **XGBoost warning:** Runtime warning about model serialization format (older xgboost version); harmless, does not affect predictions.

---

*Generated on ZIMA server. All code deposited in the reproducibility package at BioCancerInformatics/CancerRCDPredictor.*

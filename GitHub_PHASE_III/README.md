# Phase III — Supervised Machine Learning for Outcome Prediction

## Execution Order

### Phase IIIA — Main Quadripartite Ensemble (Megarun 5.0)

| # | Script | Input Dependencies | Role |
|---|---|---|---|
| 001 | RScript_Phase_III_Megarun_5_0.R | CoxNet_phaseII_feasibility_log.rds, improved_unchanged_best_fullset.tsv, dfXXX.rds | Core: Quadripartite ML ensemble (RSF, XGBoost, Boruta-RSF, MTLR) + MVL ElasticNet SuperLearner synthesis. Produces model_bundle_*.rds, SHAP data, MVL weights, and MASTER_Phase_III_Performance.csv per cohort. |

### Phase IIIB — Sparsity Isolation Protocol

| # | Script | Input Dependencies | Role |
|---|---|---|---|
| 014 | RScript_Sparsity_Isolation_Megarun.R | CoxNet_phaseII_feasibility_log.rds, dfXXX.rds | Run Quadripartite ensemble exclusively on sparse discrete features (Token .2 mutations and .3 CNVs), eliminating all five continuous multi-omic layers. Produces model_bundle_*.rds and MASTER_Phase_IIIB_Sparsity_Performance.csv. |
| 015 | RScript_Sparsity_Global_Synthesis.R | MASTER_Phase_IIIB_Sparsity_Performance.csv (from 014) | Synthesize sparsity isolation results, global performance rankings, and exemplar analyses. |
| 016 | RScript_Build_Table_Sparsity_Survivors.R | 014 and 015 outputs | Build sparsity survivor tables documenting genotypic markers with independent predictive capacity. |

### Post-Hoc Analysis & Visualization

| # | Script | Input Dependencies | Role |
|---|---|---|---|
| 002 | RScript_Zima_C_Suite_Generator.R | model_bundle_*.rds (from 001) | Exhaustive SHAP dependence plots and synergy matrices. Produces Master_ZIMA_Mathematical_Interaction_Proof_Matrix.csv. |
| 003 | RScript_Brier_Score_Extractor.R | model_bundle_*.rds (from 001) | Post-hoc calibration audit: Time-Dependent Brier Score and Integrated Brier Score (IBS). Produces MASTER_Phase_III_Brier_Calibration.csv. |
| 004 | RScript_Clinical_Probability_Extractor.R | model_bundle_*.rds, MVL weights (from 001) | Extract 1-, 3-, and 5-year individualized survival probabilities for all models. |
| 017 | RScript_Posthoc_Feature_Harvester.R | model_bundle_*.rds | Terminal Harvester: Quadripartite intersection and golden anchor extraction (150 features). |

### Synthesis & Reporting

| # | Script | Input Dependencies | Role |
|---|---|---|---|
| 018 | RScript_Phase_III_Global_Synthesis.R | MASTER_Phase_III_Performance.csv (from 001) | Global performance synthesis, paradigm exemplar extraction (Lush and Supreme), and cohort rankings. |
| 005 | RScript_Penetrance_Auditor.R | *_Phase_III_Probabilities.tsv (from 004) | Calculate sample retention and algorithmic penetrance per cohort. |

### Validation

| # | Script | Input Dependencies | Role |
|---|---|---|---|
| 006 | RScript_Validation_Predictor.R | model_bundle_*.rds, MVL weights (from 001); validation dataset | Dual-Track blind validation inference engine. Produces *_Blind_Predictions.tsv. |
| 007 | RScript_Validation_Probability_Extractor.R | *_Blind_Predictions.tsv (from 006); model_bundle_*.rds (from 001) | Convert validation Z-scores to clinical probabilities via Breslow baseline hazard. |
| 008 | RScript_Server_Audit_Tables.R | *_Blind_Predictions.tsv (from 006) | Generate NA Panorama and Penetrance tables for validation. |

### Visualization

| # | Script | Input Dependencies | Role |
|---|---|---|---|
| 009 | RScript_Composite_Plotter_Baseline.R | *_Phase_III_Probabilities.tsv (from 004) | Bifurcated 4-panel composite for baseline cohort. |
| 010 | RScript_Blind_Probability_Plotter.R | *_Clinical_Probabilities.tsv (from 007) | Bifurcated 4-panel composite for internal validation cohort (Figure 11). |

### Audit

| # | Script | Input Dependencies | Role |
|---|---|---|---|
| 011 | RScript_Audit_Megarun5.R | model_bundle_*.rds (from 001); Master_ZIMA (from 002) | Completeness audit of Megarun 5.0 + ZIMA Cube outputs. |
| 012 | RScript_Audit_Strict.R | model_bundle_*.rds (from 001); Master_ZIMA (from 002); Dataset S1 | Strict classification audit with decoder merge. Produces Table_S15. |
| 013 | RScript_Preflight_Shiny_Audit.R | Table_S15 (from 012); all models and figures | Deployment readiness check for CancerRCDPredictor Shiny application. |

## Execution Flow

```
001 (Megarun 5.0) ────────────────────────┐
├── 014 (Sparsity Isolation)              │
│   ├── 015 (Sparsity Synthesis)          │
│   └── 016 (Sparsity Survivors)          │
├── 002 (ZIMA C Suite)                    │
├── 003 (Brier Extractor)                 │
├── 004 (Clinical Probabilities)          │
│   ├── 005 (Penetrance)                  │
│   └── 009 (Baseline Composite)          │
├── 017 (Feature Harvester)               │
├── 018 (Global Synthesis)                │
├── 006 (Validation Predictor)            │
│   ├── 007 (Validation Probabilities)    │
│   │   └── 010 (Blind Composite)         │
│   └── 008 (Audit Tables)                │
└── 011 (Audit Megarun) ─────────────────┘
    └── 012 (Audit Strict)
        └── 013 (Preflight Shiny)
```

## Design Notes

- Phase III is the exclusive stage for predictive modeling; no preprocessing or cohort modification occurs.
- Scripts 002, 003, 004, 006, 014, 017, and 018 can execute in parallel after 001 completes.
- The MVL SuperLearner is a post-hoc synthesis/weighting framework, not a traditional stacked generalization training loop.
- All base learners are trained independently once; the MVL fits an ElasticNet Cox (α = 0.5) on the N × 4 meta-matrix.
- Cross-validation (K = 10 or K = 3) is used exclusively for λ selection on the meta-matrix.
- Phase IIIB (sparsity isolation, 014–016) runs the same Quadripartite ensemble architecture restricted to discrete genotypic features.
- Total output from Phase III exceeds 80 GB.

## Auxiliary Scripts

Additional analysis and verification scripts are located in the `Auxiliary/` subfolder:

| Script | Role |
|---|---|
| RScript_Verify_Synergism_Antagonism_Truth.R | Mathematical ground-truth verification of SHAP interaction classification (Synergism, Antagonism, Bifurcation) |
| RScript_Zima_Global_Ranker_and_Extractor.R | Global feature importance ranking across all 96 cohorts |
| RScript_Merge_Annotate_Features.R | Merge and annotate baseline prognostic features with ML target variables |

## Environment Replication

Generate the lock file for this phase by running the following command in R on the ZIMA dedicated server where Phases III\u2013IV were executed:

```r
renv::init()
```

Rename the resulting `renv.lock` to `renv_phase_III_IV.lock` and place it in the repository root.

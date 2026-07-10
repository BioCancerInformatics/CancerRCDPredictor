# Reproducibility Package — CancerRCDPredictor

This folder contains the complete reproducibility materials for the manuscript *"A Zero-Leakage Quadripartite Multi-Omic SuperLearner Framework for Pan-Cancer Regulated Cell Death Survival Prediction."*

## Contents

| File | Environment | Description |
|------|-------------|-------------|
| `sessionInfo_ZIMA.txt` | ZIMA Server | R 4.5.1, NixOS — Phase III/IV environment |
| `sessionInfo_NB.txt` | Local Notebook | R 4.2.3, Windows 10 — Phase I/II environment |
| `renv_phase_I_II.lock` | Notebook | Exact R package versions for Phase I/II |
| `renv_phase_III_IV.lock` | ZIMA | Exact R package versions for Phase III/IV |
| `feature_lists.tsv` | ZIMA | Per-stratum locked features (XGBoost, RSF, MTLR, Boruta) |
| `hyperparameters.tsv` | ZIMA | Per-stratum model hyperparameters (1,728 entries) |
| `random_seeds.txt` | ZIMA | Seed documentation for Phase III/IV |
| `random_seeds_NB.txt` | Notebook | Seed documentation for Phase I/II |
| `preprocessing_params.txt` | Notebook | Preprocessing parameters (Phase I/II) |

## Extraction Scripts

| Script | Environment |
|--------|-------------|
| `002_Rscript_Reproducibility_Extraction_ZIMA.R` | ZIMA — extracts feature lists, hyperparameters |
| `003_Rscript_Reproducibility_Extraction_NB.R` | Notebook — extracts preprocessing params |

## Script-to-Output Mapping

Figures and tables are generated within their respective phase pipelines. Each phase folder on GitHub contains a README.md documenting execution order, dependencies, and outputs.

| Phase | Environment | Contents |
|-------|-------------|----------|
| Phase I | Notebook | Data harmonization, imputation, feature gating |
| Phase II | Notebook | CANARY feasibility diagnostic |
| Phase III | ZIMA | Model training, evaluation, interpretability |
| Phase IV | ZIMA | Shiny deployment, blind validation |

## Environment Replication

To replicate the exact R environment:
```r
# For Phase I/II (Notebook):
renv::restore(lockfile = "renv_phase_I_II.lock")

# For Phase III/IV (ZIMA):
renv::restore(lockfile = "renv_phase_III_IV.lock")
```

## Global Random Seeds

| Analysis | Seed |
|----------|------|
| Model training (all phases) | 42 |
| Dummy-variable injection | 123 |
| Bootstrap CIs | 123 |
| Permutation testing | 123 + stratum offset |
| Calibration | No random component |

# CancerRCDPredictor

<div align="center">

<img src="https://img.shields.io/badge/Status-Under%20Active%20Development-orange?style=for-the-badge" alt="Status">

<br>

<img src="https://img.shields.io/badge/R-Shiny-blue?style=for-the-badge&logo=r">
<img src="https://img.shields.io/badge/Pan--Cancer-96%20Predictive%20Models-red?style=for-the-badge">
<img src="https://img.shields.io/badge/Multi--Omics-7%20Layers-success?style=for-the-badge">
<img src="https://img.shields.io/badge/Explainable%20AI-SHAP%20%7C%20LIME-purple?style=for-the-badge">

</div>


# CancerRCDPredictor

CancerRCDPredictor is an interactive precision oncology platform designed to explore individualized survival topologies associated with Regulated Cell Death (RCD) pathways across multiple cancer types. The application integrates Pan-Cancer multi-omic signatures with advanced machine learning architectures to provide interpretable, patient-oriented predictive landscapes.

The framework was developed from the methodological architecture described in the manuscript:

> **A Pan-Cancer Multi-Omic SuperLearner for Regulated Cell Death Survival Topologies**


# Overview

CancerRCDPredictor was developed as a translational extension of the CancerRCDShiny ecosystem, transforming large-scale prognostic signature catalogs into an interactive predictive and interpretability engine.

The platform enables the exploration of:

- 96 validated Pan-Cancer predictive cohorts
- 12,613 biologically filtered multi-omic signatures
- 7 omic layers
- SHAP-based survival geometries
- Cohort-level interaction topologies
- Patient-specific non-proportional hazard trajectories

The system was specifically engineered to bypass limitations of classical proportional hazards models and capture complex non-linear biological survival structures.


# Key Features

## Multi-Omic Predictive Architecture

The platform integrates seven omic layers:

- Protein abundance
- Somatic mutations
- Copy Number Variation (CNV)
- miRNA expression
- Transcript isoform-specific expression
- mRNA expression
- DNA methylation

through a tokenized multi-layer nomenclature framework optimized for large-scale Pan-Cancer analysis.


## Explainable AI Framework

CancerRCDPredictor incorporates multiple explainability modules:

### Global Explainability
- SHAP Beeswarm plots
- Global impact ranking
- Feature dominance visualization

### Local Explainability
- LIME surrogate decomposition
- Waterfall trajectories
- Patient-level force plots

### Interaction Explainability
- SHAP interaction topologies
- Synergistic lethality mapping
- Protective antagonistic trajectories
- Bifurcation survival geometries


## Educational Sandbox

The platform was also designed as a pedagogical topology explorer and educational sandbox for Explainable Artificial Intelligence in precision oncology.

Dedicated educational modules explain:

- SHAP interpretation
- Survival geometries
- Multi-omic interactions
- Precision oncology trajectories
- Non-proportional hazard dynamics


# Analytical Architecture

The platform follows a strict three-phase audit-compliant architecture:


## Phase I — Harmonization and Reconstruction

- Multi-omic harmonization
- Layer-specific preprocessing
- Missing-data auditing
- Fault-tolerant imputation engine
- Leakage prevention protocols


## Phase II — CANARY Structural Diagnostics

- CoxNet feasibility auditing
- Proportional hazards diagnostics
- Geometric admissibility gating
- Survival topology certification


## Phase III — Quadripartite Ensemble Synthesis

The final predictive framework combines:

- Random Survival Forests (RSF)
- XGBoost
- Survival-Boruta
- Multi-Task Logistic Regression (MTLR)

through a Multi-View Elastic Net SuperLearner architecture.


# Platform Modules

| Module | Description |
|---|---|
| Welcome | Platform overview and predictive architecture |
| How to Read | Educational interpretability guide |
| Methodological Integrity | Phase I–III methodological overview |
| MVL Performance | Time-dependent AUROC exploration |
| Global Impact | SHAP Beeswarm visualization |
| Interaction Topologies | Synergy and antagonism mapping |
| Precision Oncology | Individual patient trajectory decomposition |
| Signature Interpreter | Multi-omic signature exploration |


# Dataset Scope

The framework integrates:

- TCGA Pan-Cancer cohorts
- UCSCXena resources
- Multi-omic clinical matrices

Clinical survival endpoints include:

- Overall Survival (OS)
- Disease-Specific Survival (DSS)
- Disease-Free Interval (DFI)
- Progression-Free Interval (PFI)

The analytical architecture generated:

- 14,907 prognostic nomenclatures
- 17,875 biological target elements
- 372 harmonized preprocessing matrices


# Technologies

## Backend

- R
- Shiny
- survival
- glmnet
- randomForestSRC
- xgboost
- SHAP
- LIME

## Frontend

- Bootstrap 5
- bslib
- DT
- Responsive Glassmorphism UI
- Dynamic rendering pipelines


# Installation

```r
# Clone repository
git clone https://github.com/BioCancerInformatics/CancrRCDPredictor.git

# Open R
# Install dependencies

install.packages(c(
  "shiny",
  "bslib",
  "DT",
  "survival",
  "glmnet",
  "randomForestSRC",
  "xgboost",
  "lime",
  "shapviz"
))

# Run application
shiny::runApp()

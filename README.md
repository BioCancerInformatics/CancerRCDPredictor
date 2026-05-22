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

CancerRCDPredictor engineers a novel Pan-Cancer Multi-Omic SuperLearner pipeline designed to mathematically overcome the critical algorithmic bottlenecks of precision oncology—specifically extreme data sparsity, dimensional missingness, and the structural failures of traditional linear proportional-hazards models.

By mapping non-linear survival topologies across 33 tumor types and introducing a Dual-Track genotypic sparsity displacement architecture, we provide a mathematically resilient predictive framework. Crucially, our system guarantees local interpretability via N-dimensional TreeSHAP interactions, directly answering the mandate for transparent, audit-compliant AI tools capable of managing multi-modal biological complexity without sacrificing patient data.

The framework was developed from the methodological architecture described in the manuscript:

> **A Pan-Cancer Multi-Omic SuperLearner for Regulated Cell Death Survival Topologies**


# Overview

CancerRCDPredictor was developed as a translational extension of the CancerRCDShiny ecosystem, transforming large-scale prognostic signature catalogs into an interactive predictive and interpretability engine.

The platform enables the exploration of:

- 96 validated Pan-Cancer predictive cohorts
- 12,613 biologically filtered multi-omic signatures
- 7 omic layers
- SHAP-based survival geometries and LIME surrogate models
- Cohort-level interaction topologies (mapping 26,800 Synergistic, Antagonistic, and Bifurcation dependencies)
- 10,306 patient-specific non-proportional hazard trajectories
- 1,050 patient samples in a clinical blind validation cohort
- 150 elite "Golden Anchor" RCD signatures (Quadripartite-validated apex drivers)
- A Dual-Track Inference Architecture (powered by an MVL SuperLearner and XGBoost fallback)

The system was specifically engineered to bypass limitations of classical proportional hazards models and capture complex non-linear biological survival structures.

## Key Discoveries & Architectural Outcomes
Beyond serving as a predictive engine, the CancerRCDPredictor pipeline generated profound biological and algorithmic discoveries:

### The Terminal Harvester & 150 Golden Anchors: 
From an initial universe of 14,595 signatures, the Quadripartite framework forced features to survive a rigorous 4/4 algorithmic constraint (RSF VIMP, XGBoost Gain, Boruta Z-score, MTLR L2-Norm). This distilled the landscape down to exactly 150 "Golden Anchors"—the absolute highest echelon of pan-cancer prognostic reliability.

### Algorithmic Displacement & Genotypic Erasure: 
The architecture revealed a severe structural displacement during high-dimensional model competition. Continuous phenotypic layers (transcript isoforms, mRNA) monopolized 85.7% of the predictive topology, mathematically suppressing and erasing static genomic mutations and CNVs (0.0% retention in the golden anchors).

### Dynamic SuperLearner Voting (Lush vs. Supreme Exemplars): 
The SuperLearner dynamically adapts voting weights to the cohort's biological complexity. In high-entropy "Lush" environments (e.g., LGG), it distributes trust equally across all 4 base-learners (25% each) to synthesize fragmented signals. In "Supreme" deterministic environments (e.g., READ_OS), it routes up to 95.7% of trust into XGBoost to maximize resolution.

# Key Features
## Multi-Omic Predictive Architecture & Nomenclature
The platform integrates seven omic layers:

Protein abundance (.1)
Somatic mutations (.2)
Copy Number Variation (CNV) (.3)
miRNA expression (.4)
Transcript isoform-specific expression (.5)
mRNA expression (.6)
DNA methylation (.7)

These layers are tracked through an 11-Part Tokenized Nomenclature System (**CTAB-GSI.GFC.PFC.SCS.TNC.HRC.SMC.TMC.TIC.RCD**), ensuring programmatic parsing of biological function, immune landscape, and Regulated Cell Death (RCD) pathways directly from the signature ID.

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

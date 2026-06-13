# CancerRCDPredictor
<div align="center">

<br>

<img src="https://img.shields.io/badge/R-Shiny-blue?style=for-the-badge&logo=r">
<img src="https://img.shields.io/badge/Pan--Cancer-96%20Predictive%20Models-red?style=for-the-badge">
<img src="https://img.shields.io/badge/Multi--Omics-7%20Layers-success?style=for-the-badge">
<img src="https://img.shields.io/badge/Explainable%20AI-SHAP%20%7C%20LIME-purple?style=for-the-badge">
<img src="https://img.shields.io/badge/Internal%20Clinical%20Blind%20Validation%20Cohort%20 -orange?style=for-the-badge">

</div>

<br>

**Authors:**  
[Emanuell de Souza Rodrigues](https://www.researchgate.net/profile/Emanuell-Rodrigues-De-Souza)   
[Higor Almeida Cordeiro Nogueira](https://www.researchgate.net/profile/Higor-Cordeiro-Nogueira)   
[Victor dos Santos Lopes](https://www.linkedin.com/in/victor-lopes-880604377)   
[Enrique Medina-Acosta](https://www.researchgate.net/profile/Enrique-Medina-Acosta)


# Purpose

CancerRCDPredictor engineers a novel Pan-Cancer Multi-Omic SuperLearner pipeline designed to mathematically overcome the critical algorithmic bottlenecks of precision oncology—specifically extreme data sparsity, dimensional missingness, and the structural failures of traditional linear proportional-hazards models.

By mapping non-linear survival topologies across 33 tumor types and introducing a Dual-Track genotypic sparsity displacement architecture, we provide a mathematically resilient predictive framework. Crucially, our system guarantees local interpretability via N-dimensional TreeSHAP interactions, directly answering the mandate for transparent, audit-compliant AI tools capable of managing multi-modal biological complexity without sacrificing patient data.

> **CancerRCDPredictor** is an interactive Shiny application developed as part of the research associated with the pre-print **A Pan-Cancer Multi-Omic SuperLearner for Regulated Cell Death Survival Topologies**, available at **[bioRxiv]( https://www.biorxiv.org/content/10.64898/2026.05.29.728842v1).**


# Overview

<p align="center">
  <img src="https://github.com/BioCancerInformatics/CancerRCDPredictor/blob/main/Figures/Pipeline_tiny.png" width="1200">
</p>


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

- Protein abundance (.1)
- Somatic mutations (.2)
- Copy Number Variation (CNV) (.3)
- miRNA expression (.4)
- Transcript isoform-specific expression (.5)
- mRNA expression (.6)
- DNA methylation (.7)

These layers are tracked through an 11-Part Tokenized Nomenclature System (**CTAB-GSI.GFC.PFC.SCS.TNC.HRC.SMC.TMC.TIC.RCD**), ensuring programmatic parsing of biological function, immune landscape, and Regulated Cell Death (RCD) pathways directly from the signature ID.

## Explainable AI Framework

CancerRCDPredictor incorporates multiple explainability modules to prevent "black box" predictions:

# Global Explainability
- SHAP Beeswarm plots: Global impact ranking and feature dominance visualization.

<p align="center">
  <img src="https://github.com/BioCancerInformatics/CancerRCDPredictor/blob/main/Figures/Beeswarm.png" width="1200">
</p>

# Local Explainability
- LIME Surrogate Models: Point-of-care localized linear surrogates mapping individualized hazard boundaries.
- TreeSHAP Waterfall & Force Plots: Decompiling the exact predictive logic of the non-linear SuperLearner for individual patient trajectories.

<p align="center">
  <img src="https://github.com/BioCancerInformatics/CancerRCDPredictor/blob/main/Figures/Lethal_Trajectory.png" width="1200">
</p>


# Interaction Explainability
Mapping 26,800 statistically significant 3D TreeSHAP trans-signature dependencies across three mathematical archetypes:

1. Synergism: Hazard Amplification.
2. Antagonism: Functional Rescue Effect.
3. Context-Dependent Bifurcation: Topological sign-reversals.

## Educational Sandbox
The platform was also designed as a pedagogical topology explorer and educational sandbox for Explainable Artificial Intelligence in precision oncology.

Dedicated educational modules explain:

- SHAP interpretation
- Survival geometries
- Multi-omic interactions
- Precision oncology trajectories
- Non-proportional hazard dynamics

## Audit-Compliant Analytical Architecture
The platform operates on a strictly deterministically gated architecture, governed by 4 Constitutional Contracts (Groupwise isolation, endpoint-scoped cohorts, zero predictor-driven sample reduction, and explicit exclusion ledgers) alongside rigorous Identifiability Thresholds ($E_{min} \ge 20$, $N_{min} \ge 50$).

# Phase I — Harmonization and Reconstruction
- Universal Resume Engine: A fault-tolerant pipeline deploying 12 distinct imputation methods (including kNN, missForest, XGBoost, LightGBM, MICE, and iSVD) with automated .rds checkpointing for memory safety.
_ Generation of LiSHMOM: 372 Lineage-Specific Harmonized Multi-Omic Matrices.
_ Leakage prevention protocols.

# Phase II — CANARY Structural Diagnostics
- CoxNet Feasibility Auditing: Mathematically mapping proportional-hazards failures and $\mu$-ladder exhaustion.
- Geometric Admissibility Gating & Survival Topology Certification.

# Phase III — Quadripartite Ensemble Synthesis & Calibration
- The Base-Learners: Random Survival Forests (RSF), XGBoost, Survival-Boruta, and Multi-Task Logistic Regression (MTLR).
- The Meta-Learner: Synthesized via a Multi-View Elastic Net SuperLearner (MVL).
- Brier Calibration Audit: Rigorous post-hoc probability validation using Inverse Probability of Censoring Weighting (IPCW) and Time-Dependent Brier Scores (IBS) across 1-, 3-, and 5-year horizons.
- "No Cohort Stays Behind" Policy: Algebraic fallback defenses (Z-Score Mean Imputation, Micro-Jitter Variance Injection, Boruta Coerced 0.5 Resolution) preventing singular matrix crashes.

# Internal Blind Validation & Dual-Track Inference Engine
To guarantee 100% predictive penetrance against the 1,050 pristine validation records, a **Dual-Track Inference Engine** was deployed:

- Path A (SuperLearner): Synthesizes continuous risk hazard Z-scores for structurally intact records.
- Path B (Native XGBoost Fallback): Autonomously routes highly fragmented patient records through sparsity-aware split finding to prevent artificial risk escalation.


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

## 📌 Citation

If you use the **CancerRCDPredictor** Shiny App or the results generated by this application, please cite our preprint:

Rodrigues de Souza E., Almeida Cordeiro Nogueira H., dos Santos Lopes V. and Medina-Acosta E. (2026). A Pan-Cancer Multi-Omic SuperLearner for Regulated Cell Death Survival Topologies. Available at **[bioRxiv](https://www.biorxiv.org/content/10.64898/2026.05.29.728842v1).**

**Citing this work supports the continued development of this multi-omic SuperLearner Predictor**.

### BibTeX



```bibtex
@article {Rodrigues de Souza2026.05.29.728842,
	author = {Rodrigues de Souza, Emanuell and Almeida Cordeiro Nogueira, Higor and dos Santos Lopes, Victor and Medina-Acosta, Enrique},
	title = {A Pan-Cancer Multi-Omic SuperLearner for Regulated Cell Death Survival Topologies},
	elocation-id = {2026.05.29.728842},
	year = {2026},
	doi = {10.64898/2026.05.29.728842},
	publisher = {Cold Spring Harbor Laboratory},
		URL = {https://www.biorxiv.org/content/early/2026/06/02/2026.05.29.728842},
	eprint = {https://www.biorxiv.org/content/early/2026/06/02/2026.05.29.728842.full.pdf},
	journal = {bioRxiv}
}
```

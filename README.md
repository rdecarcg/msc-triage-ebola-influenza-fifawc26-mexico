# EpiTriage 2026: Epidemiological Triage & Macroeconomic Impact Simulation Pipeline

[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.20600419.svg)](https://doi.org/10.5281/zenodo.20600419)
[![Platform](https://img.shields.io/badge/r-%23276DC3.svg?style=flat&logo=r&logoColor=white)](https://www.r-project.org/)
[![Shiny](https://img.shields.io/badge/shiny-%2317a2b8.svg?style=flat)](https://shiny.posit.co/)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![ORCID](https://img.shields.io/badge/ORCID-0009--0007--1851--9547-green.svg)](https://orcid.org/0009-0007-1851-9547)

<table border="0" cellpadding="0" cellspacing="0">
  <tr>
    <td valign="top" width="80" style="border: none;">
      <img src="assets/GH_FBlack.svg#gh-light-mode-only" alt="Rodrigo De Carcer Signature" width="65" style="max-width: none;">
      <img src="assets/GH_FWhite.svg#gh-dark-mode-only" alt="Rodrigo De Carcer Signature" width="65" style="max-width: none;">
    </td>
    <td valign="top" style="border: none; padding-left: 15px;">
      <b>Author:</b> Rodrigo Abel De Cárcer Gandarilla<br>
      <b>Date:</b> June 2026<br>
      <b>Concept:</b> Executive Decision Support System & Policy Simulator<br>
      <b>Preprint:</b> <i>medRxiv (Integration Pending)</i>
    </td>
  </tr>
</table>
<br clear="left"/>

---

## Project Overview

The 2026 FIFA World Cup in Mexico represents an unprecedented mass-gathering event, introducing severe spatial-temporal mobility dynamics and substantial biosecurity challenges. This repository hosts a comprehensive, end-to-end computational pipeline designed to evaluate the healthcare system and macroeconomic risks of a hypothetical 'Black Swan' event: a simultaneous multi-pathogen incursion of **Ebola Bundibugyo (BDBV)**—modeled during an active WHO Public Health Emergency of International Concern (PHEIC)—and pandemic **Influenza H5N1** during a Southern Hemisphere winter surge vector.

Utilizing a 5-stage methodological framework, the pipeline establishes a Pathogen Incursion Probability Engine via a Poisson arrival process, synthesizes operational clinical cohorts, trains an ensemble machine learning meta-learner optimized for asymmetric misclassification costs, models discrete-time hospital capacity saturation under Crisis Standards of Care (CSC), and projects long-term societal and macroeconomic health deficits using Global Burden of Disease (GBD) and WHO-CHOICE parameters.

---

## Repository Structure

The project is organized as a modular, self-contained R project:

```text
├── msc-triage-ebola-influenza-fifawc26-mexico.Rproj  # Central RProject Anchor
├── renv.lock                                         # Reproducible Environment Lockfile
├── data_dictionary.md                                # Detailed Feature Metadata
├── README.md                                         # Project Master Documentation
├── data/
│   ├── dataset_ebola_vs_influenza_fifawc26_triage_mexico.csv # Ground-Truth Cohort
│   └── dataset_operational_triage.csv                        # Operational Dataset (With Noise)
├── src/
│   ├── config.R                        # Global Configuration & Parameter Constant Inversion
│   ├── 00-pipeline-orchestrator.R      # Master Execution Controller & Error Handling Engine
│   ├── 01-clinical-simulation.R        # Parametric Stochastic Cohort Generation (N=165,500)
│   ├── 02-exploratory-data-analysis.R  # Clinical Profiling & Multicollinearity Diagnostics (VIF)
│   ├── 03-predictive-modelling.R       # Stacked Generalization Ensemble (Ridge-RF-XGBoost)
│   ├── 04-micro-impact.R               # Discrete-Time Resource Saturation Simulation
│   └── 05-macro-impact.R               # Bounded SIR Community Spillover & Macroeconomics
├── reports/
│   ├── figures/                        # Publication-Ready Visualizations (Absolute Savings, etc.)
│   ├── models/                         # Serialized Binary Model Stacks (.rds)
│   └── results/                        # Epidemiological & WHO-CHOICE Policy Sensitivity Matrices
└── epitriage-dashboard/
    ├── app.R                           # Interactive Policy Shiny Dashboard (UI/Server)
    ├── data/                           # Synchronized Dashboard Core Datasets & Living Visual Objects
    └── www/                            # High-Resolution NATIVE Plots For Executive Views
```

---

## Methodological Framework

### 1. Stochastic Cohort Generation (`src/01-clinical-simulation.R`)
Generates a balanced clinical population (N = 165,500) calibrated against historical WHO 2007 BDBV outbreak dynamics and pandemic influenza markers. This stage operates strictly as a parametric stochastic simulation. Patient profiles incorporate demographics, right-skewed latency rates derived via Gamma distributions, log-normal multivariate hematological features (capturing severe thrombocytopenia and leukopenia), and conditional length-of-stay (LOS) metrics dependent on terminal survival outcome vectors.

### 2. Exploratory Data Analysis & Diagnostics (`src/02-exploratory-data-analysis.R`)
Calculates epidemiological characteristics stratified by etiological agent. Implements Variance Inflation Factor (VIF) diagnostic checks through an *auxiliary multivariable generalized linear model (GLM)* to validate predictive feature independence (enforcing a strict health-sector threshold of VIF < 2.5) prior to machine learning training loops.

### 3. Stacked Ensemble Meta-Learner (`src/03-predictive-modelling.R`)
Injects 10% to 15% instrument variance and recall field bias into physiological data to mimic an acute healthcare surge environment. Combines heterogeneous base learners—Regularized Logistic Regression (Ridge Bounds), Random Forests (Non-linear interactions), and Extreme Gradient Boosting (XGBoost Error Correction)—via Penalized Meta-Model Stacking. The final classifier optimizes the Precision-Recall Area Under Curve (PR-AUC) and configures an F-beta=2 decision threshold to heavily penalize fatal False Negatives (missing Ebola isolation) over managed False Positives.

### 4. Micro-Operational Impact Simulation (`src/04-micro-impact.R`)
Executes a 60-day discrete-time stochastic resource simulation. Incorporates dynamic sigmoid optimization boundaries to adjust clinical log-odds triage constraints relative to real-time hospital occupancy rates across a baseline threshold of 350 emergency-retrofitted AIIR units. Evaluates operational infrastructure elasticity over a multivariable policy parameter landscape via functional mapping.

### 5. Macroeconomic Health Economics (`src/05-macro-impact.R`)
Translates hospital overflow thresholds into community transmission matrices through a bounded SIR spillover simulation across a susceptible population envelope (N = 5.0M). Quantifies long-term multi-sectoral societal health deficits isolating premature mortality Years of Life Lost (YLL) and Disability-Adjusted Life Years (DALYs). Introduces a multi-scenario macroeconomic burden analysis applying WHO-CHOICE Value of Statistical Life (VSL) ranges (1x, 3x, and 5x GDP per capita).

---

## Interactive Shiny Dashboard (`epitriage-dashboard/app.R`)

The clinical and economic outputs are compiled into a production-grade Executive Decision Support System dashboard built with `shinydashboard`. The architecture comprises six specialized analytical views:

1. **Executive Summary:** Real-time KPI tracking showcasing Poisson arrival risks, maximum lives saved, and absolute economic burden averted. 
2. **Interactive Policy Simulator:** Active user sliders enabling real-time scenario modelling by manipulating susceptible demographic sizes, effective reproduction numbers (Rt), systemic collapse mortality metrics, and infrastructure capacity caps.
3. **Operational Triage (Micro):** Analyzes hospital infrastructure saturation dynamics and visualizes the asymmetric cost matrix of the triage algorithm.
4. **Macroeconomic Impact:** Details the WHO-CHOICE burden sensitivity across GDP and VSL variants.
5. **Clinical & ML Diagnostics:** Presents the synthetic cohort's EDA, biomarker distributions, and the PR-AUC evaluation of the stacked meta-learner.
6. **Raw Data Explorer:** Allows dynamic filtering and export of the generated datasets, efficacy tables, and sensitivity matrices.

---

## Installation & Reproducibility

This project utilizes the `renv` package management system to guarantee local environment and package version lock-in consistency across systems.

### Hardware Prerequisites & Performance Metrics
The script stack utilizes parallel processing (`doParallel`) to accelerate hyperparameter grid sweeps. Performance metrics and cross-validation loops have been natively compiled and optimized locally on Apple Silicon architecture hardware running macOS, ensuring exceptionally fast computation times during the ensemble compilation and simulation phases.

### Execution Steps
1. Clone the repository to your local directory.
2. Open the directory through RStudio using the master project anchor: `msc-triage-ebola-influenza-fifawc26-mexico.Rproj`.
3. Restore the standardized computational environment by running the following command in your R console:
   ```r
   renv::restore()
   ```
4. Execute the master pipeline controller to run all methodologies, build datasets, validate diagnostics, export statistical tables, and render graphics:
   ```r
   source('src/00-pipeline-orchestrator.R')
   ```
5. To launch the Executive Dashboard application, run:
   ```r
   shiny::runApp('epitriage-dashboard')
   ```

---

## Data Provenance & Open Science
* Comprehensive metadata, boundary ranges, and clinical variable descriptions are documented in the root directory file: `data_dictionary.md`.
* Extended analytical outputs, data structures, and serialized models are permanently cataloged and open-source certified via Zenodo under DOI: [10.5281/zenodo.20600419](https://doi.org/10.5281/zenodo.20600419).

---

## License
This project is licensed under the MIT License - see the local documentation files for complete compliance details.
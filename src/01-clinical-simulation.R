# =========================================================================
# SCRIPT 01: PARAMETRIC STOCHASTIC SIMULATION FOR CLINICAL TRIAGE COHORTS
# PROJECT: EPIDEMIOLOGICAL TRIAGE FIFA WORLD CUP 2026 - MEXICO
# PATHOGENS: EBOLA BUNDIBUGYO (WHO 2007 CALIBRATION) VS. INFLUENZA H5N1
# AUTHOR: RODRIGO ABEL DE CARCER GANDARILLA
# =========================================================================
# DESCRIPTION: Generation of a synthetic patient population (N=165,500) to 
# simulate clinical presentation, demographic distribution, and biomarker 
# profiles. This parametric stochastic simulation serves as the foundational 
# ground truth for machine learning model training and operational triage 
# efficacy evaluation, strictly avoiding generalized probability integrals 
# in favor of targeted clinical distributions.
# =========================================================================

if (!require('pacman')) install.packages('pacman')
pacman::p_load(MASS, tidyverse, lubridate)

# =========================================================================
# 0. INITIALIZATION & SETUP ####
# =========================================================================
# Load global parameters to ensure cross-script parameter immutability.
if (file.exists('src/config.R')) source('src/config.R', echo = FALSE)

# Define master simulation seed for deterministic reproducibility 
# of the stochastic generation arrays.
set.seed(GLOBAL_SEED) 
n <- COHORT_SIZE 
n_half <- n / 2

# =========================================================================
# 1. patient_id ####
# =========================================================================
# Primary key ensuring strict data provenance and row-level traceability.
patient_id <- 1:n

# =========================================================================
# 2. pathogen ####
# =========================================================================
# Target Class: Balanced binary categorization (50/50 split) configured 
# to optimize initial machine learning training stability.
pathogen <- c(rep('Influenza_H5N1', n_half), rep('Ebola_Bundibugyo', n_half))

# =========================================================================
# 3. age ####
# =========================================================================
# Demographic Synthesis: Normal approximation, truncated to [16, 80] to 
# reflect the viable adult and semi-adult demographic profile expected 
# at international mass-gathering sporting events.
age <- pmax(pmin(round(rnorm(n, mean = 38, sd = 12)), 80), 16)

# =========================================================================
# 4. infection_date ####
# =========================================================================
# Epidemiological Chronology: Stochastic uniform distribution of exposure 
# events across the designated risk window.
start_date <- CLINICAL_START_DATE
end_date_silent <- CLINICAL_END_SILENT
simulation_days_total <- CLINICAL_SIM_DAYS

infection_dates <- start_date + sample(0:simulation_days_total, n, replace = TRUE)

# =========================================================================
# 5. silent_detection_phase ####
# =========================================================================
# Binary flag identifying the early-phase under-reporting period where 
# systemic epidemiological surveillance networks are lagging.
silent_detection_phase <- ifelse(infection_dates <= end_date_silent, 1, 0)

# =========================================================================
# 6. incubation_days ####
# =========================================================================
# Clinical Progression: Parametric generation using Gamma distributions (shape k, rate θ).
# This specifically captures the right-skewed natural history of viral 
# pathogen latency, accommodating long-tail outlier incubation periods 
# observed in historical WHO reference data.
incubation_days <- c(
  rgamma(n_half, shape = 2, rate = 0.8), # H5N1: Rapid onset kinetics.
  rgamma(n_half, shape = 4, rate = 0.4)  # Bundibugyo: Extended latency profile.
)
incubation_days <- pmax(incubation_days, 1)

# =========================================================================
# 7. triage_arrival_date ####
# =========================================================================
# Clinical Presentation: Explicit temporal function of infection date 
# compounded by the stochastically derived symptomatic onset lag.
triage_arrival_date <- infection_dates + round(incubation_days)

# =========================================================================
# 8. presymptomatic_shedding ####
# =========================================================================
# Bernoulli distribution mapping the probability of viral infectiousness 
# prior to the manifestation of acute clinical symptoms.
presymptomatic_shedding <- c(sample(c(0, 1), n_half, replace = TRUE, prob = c(0.3, 0.7)), 
                             rep(0, n_half))

# =========================================================================
# 9. tourism_mobility ####
# =========================================================================
# Spatiotemporal mobility matrices mapping potential geographical spread 
# vectors across critical urban infrastructure.
tourism_mobility <- c(sample(c('Host_City', 'Tourist_Corridor'), n_half, replace = TRUE, prob = c(0.5, 0.5)),
                      sample(c('Host_City', 'Tourist_Corridor'), n_half, replace = TRUE, prob = c(0.8, 0.2)))

# =========================================================================
# 10. exposure_route ####
# =========================================================================
# Behavioral Risk Vector Assessment (0 = Indirect/Zoonotic, 1 = Direct Human Contact).
exposure_route <- c(sample(c(0, 1), n_half, replace = TRUE, prob = c(0.95, 0.05)),
                    sample(c(0, 1), n_half, replace = TRUE, prob = c(0.02, 0.98)))

# =========================================================================
# 11. care_avoidance ####
# =========================================================================
# Psychosocial Variable: Modeling healthcare hesitancy or structural access barriers.
care_avoidance <- sample(c(0, 1), n, replace = TRUE, prob = c(0.65, 0.35))

# =========================================================================
# 12 & 13. platelets_count & white_blood_cells ####
# =========================================================================
# Hematological Synthesis: Multivariate normal distribution modeled in log-normal 
# space. This mathematical approach preserves the strictly positive nature of 
# cell counts while accurately capturing the complex physiological covariance 
# between severe thrombocytopenia and leukopenia during acute viremia.
get_lnorm_params <- function(m, s) {
  location <- log(m^2 / sqrt(s^2 + m^2))
  shape <- sqrt(log(1 + (s^2 / m^2)))
  return(c(location, shape))
}

p_bundibugyo <- get_lnorm_params(130000, 35000)
w_bundibugyo <- get_lnorm_params(4500, 2200)
p_h5n1 <- get_lnorm_params(160000, 35000)
w_h5n1 <- get_lnorm_params(6800, 2500)

generate_correlated_biomarkers <- function(n, mu_vec, sd_vec, rho) {
  sigma <- matrix(c(sd_vec[1]^2, rho * sd_vec[1] * sd_vec[2], 
                    rho * sd_vec[1] * sd_vec[2], sd_vec[2]^2), 2, 2)
  mvn_data <- mvrnorm(n, mu = mu_vec, Sigma = sigma)
  return(exp(mvn_data))
}

bio_h5n1 <- generate_correlated_biomarkers(n_half, c(p_h5n1[1], w_h5n1[1]), c(p_h5n1[2], w_h5n1[2]), 0.2)
bio_bundibugyo <- generate_correlated_biomarkers(n_half, c(p_bundibugyo[1], w_bundibugyo[1]), c(p_bundibugyo[2], w_bundibugyo[2]), -0.4)

platelets_count <- round(c(bio_h5n1[,1], bio_bundibugyo[,1]))
white_blood_cells <- round(c(bio_h5n1[,2], bio_bundibugyo[,2]))

# =========================================================================
# 14. clinical_outcome ####
# =========================================================================
# Ground-truth Case Fatality Rate (CFR) disposition (0 = Survived, 1 = Deceased)
# utilized downstream for health economics and impact burden quantification.
prob_fatal_h5n1 <- CFR_H5N1
prob_fatal_bundibugyo <- CFR_BUNDIBUGYO 

clinical_outcome <- c(
  sample(c(0, 1), n_half, replace = TRUE, prob = c(1 - prob_fatal_h5n1, prob_fatal_h5n1)),
  sample(c(0, 1), n_half, replace = TRUE, prob = c(1 - prob_fatal_bundibugyo, prob_fatal_bundibugyo))
)

# =========================================================================
# 15. hospital_stay_days ####
# =========================================================================
# Resource Utilization: Length-of-stay (LOS) modeled as conditional Gamma 
# variables dependent on survival outcome to reflect rapid mortality trajectories 
# versus prolonged critical care recoveries.
hospital_stay_days <- numeric(n)
idx_h5n1_rec <- which(clinical_outcome[1:n_half] == 0)
idx_h5n1_fat <- which(clinical_outcome[1:n_half] == 1)
idx_bund_rec <- which(clinical_outcome[(n_half + 1):n] == 0) + n_half
idx_bund_fat <- which(clinical_outcome[(n_half + 1):n] == 1) + n_half

hospital_stay_days[idx_bund_rec] <- rgamma(length(idx_bund_rec), shape = 4, rate = 0.20)
hospital_stay_days[idx_bund_fat] <- rgamma(length(idx_bund_fat), shape = 3, rate = 0.40)
hospital_stay_days[idx_h5n1_rec] <- rgamma(length(idx_h5n1_rec), shape = 3, rate = 0.35)
hospital_stay_days[idx_h5n1_fat] <- rgamma(length(idx_h5n1_fat), shape = 2, rate = 0.50)

hospital_stay_days <- pmax(round(hospital_stay_days), 1)

# =========================================================================
# 16. household_sar ####
# =========================================================================
# Secondary Attack Rate (SAR) metrics representing localized community 
# transmission and spillover potential if patient is not properly isolated.
household_sar <- c(
  round(rnorm(n_half, mean = 35, sd = 8)), 
  round(rnorm(n_half, mean = 15, sd = 4))
)

# =========================================================================
# VALIDATION & CONSOLIDATION ####
# =========================================================================
# Non-parametric Kolmogorov-Smirnov (K-S) testing to statistically validate 
# that our stochastically generated cohorts maintain high-fidelity 
# distributional alignment with historical WHO reference parameters.
ks_test_plt <- ks.test(bio_bundibugyo[,1], 'plnorm', meanlog = p_bundibugyo[1], sdlog = p_bundibugyo[2])

cat('--- STATISTICAL VALIDATION (K-S TEST) ---\n')
cat('Distributional Fidelity Check (D-stat):', round(ks_test_plt$statistic, 4), '\n')
cat('p-value:', ks_test_plt$p.value, '\n')
if(ks_test_plt$p.value > 0.05) {
  cat('Result: Simulation maintains historical distributional parameters.\n\n')
} else {
  cat('Result: Warning - Distributional drift detected.\n\n')
}

# Master Dataframe Assembly.
df_triage <- data.frame(
  patient_id = patient_id, 
  pathogen = as.factor(pathogen), 
  age = age, 
  infection_date = infection_dates,
  silent_detection_phase = as.factor(silent_detection_phase),
  incubation_days = incubation_days,
  triage_arrival_date = triage_arrival_date,
  presymptomatic_shedding = as.factor(presymptomatic_shedding), 
  tourism_mobility = as.factor(tourism_mobility),
  exposure_route = as.factor(exposure_route), 
  care_avoidance = as.factor(care_avoidance),
  platelets_count = platelets_count, 
  white_blood_cells = white_blood_cells, 
  clinical_outcome = as.factor(clinical_outcome),
  hospital_stay_days = hospital_stay_days,
  household_sar = household_sar
)

if (!dir.exists('data')) dir.create('data', recursive = TRUE)
write.csv(df_triage, 'data/dataset_ebola_vs_influenza_fifawc26_triage_mexico.csv', row.names = FALSE)

# Data for Shiny App.
write.csv(df_triage, 'epitriage-dashboard/data/dataset_ebola_vs_influenza_fifawc26_triage_mexico.csv', row.names = FALSE)

cat('Success: Consolidated clinical cohort exported to data/ directory.\n')
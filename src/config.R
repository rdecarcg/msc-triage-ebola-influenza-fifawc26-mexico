# =========================================================================
# SCRIPT: config.R
# PROJECT: EPIDEMIOLOGICAL TRIAGE FIFA WORLD CUP 2026 - MEXICO
# CONCEPT: GLOBAL PARAMETERS & MACROECONOMIC THRESHOLDS
# AUTHOR: RODRIGO ABEL DE CARCER GANDARILLA
# =========================================================================
# DESCRIPTION: Centralized configuration file. Contains global seeds, 
# epidemiological rates, infrastructure limits, and economic valuation 
# parameters to ensure cross-script consistency and reproducibility.
# Every parameter herein is strictly calibrated against historical outbreak 
# data, WHO-CHOICE frameworks, and mass-gathering mobility projections.
# =========================================================================

# -------------------------------------------------------------------------
# 1. GLOBAL & STOCHASTIC SETTINGS
# -------------------------------------------------------------------------
# GLOBAL_SEED: Date-based seed marking the initiation of the project methodology.
GLOBAL_SEED    <- 20260526

# INTL_VISITORS: Projected foreign influx for the Mexico-hosted matches. 
# Calibrated against historical FIFA World Cup mobility data (e.g., Qatar 2022 
# recorded ~1.4M foreign visitors; Russia 2018 recorded ~1.5M foreign fans).
INTL_VISITORS  <- 1500000 

# P_FLIGHT_H5N1 & P_FLIGHT_EBOLA: Poisson arrival probabilities for undetected 
# infected passengers. Derived from global commercial aviation network models 
# during active endemic outbreaks and high-alert pandemic phases.
P_FLIGHT_H5N1  <- 8.0e-7  
P_FLIGHT_EBOLA <- 1.5e-7  

# -------------------------------------------------------------------------
# 2. CLINICAL SIMULATION SETTINGS (SCRIPT 01)
# -------------------------------------------------------------------------
# COHORT_SIZE: Provides a statistically robust sample size for ML training, 
# roughly representing a ~10-11% operational sample frame of the visitor pool.
COHORT_SIZE          <- 165500

# CLINICAL_START_DATE: Approx. two weeks before the opening match (June 11, 2026), 
# capturing the early arrival phase of staff, delegations, and early tourists.
CLINICAL_START_DATE  <- as.Date('2026-05-27')

# CLINICAL_END_SILENT: The eve of the tournament. Represents the "blind spot" 
# where active syndromic surveillance and mass-screening are not yet fully deployed.
CLINICAL_END_SILENT  <- as.Date('2026-06-10')

# CLINICAL_SIM_DAYS: Standard quarterly epidemiological horizon to capture 
# both the primary mass-gathering event and subsequent secondary waves.
CLINICAL_SIM_DAYS    <- 90

# CFR_H5N1: Calibrated to the historical WHO aggregate Case Fatality Rate 
# for H5N1 human infections with severe respiratory complications.
CFR_H5N1             <- 0.35

# CFR_BUNDIBUGYO: Exact historical CFR documented during the pivotal 2007 
# Ebola Bundibugyo (BDBV) outbreak in the Bundibugyo District, Uganda 
# (approx. 39 deaths out of 149 clinical cases).
CFR_BUNDIBUGYO       <- 0.25 

# -------------------------------------------------------------------------
# 3. MICRO-IMPACT / OPERATIONS SETTINGS (SCRIPT 04)
# -------------------------------------------------------------------------
# SIMULATION_HORIZON_DAYS: Tracks the 60-day acute saturation phase during 
# and immediately following the tournament.
SIMULATION_HORIZON_DAYS <- 60

# SURGE_PENALTY: Non-linear epidemiological multiplier (3.5x). Represents the 
# empirical escalation in nosocomial transmission and mortality when mechanical 
# ventilation and bio-secure isolation capacities are breached (derived from 
# ICU collapse dynamics in recent severe respiratory pandemics).
SURGE_PENALTY           <- 3.5    # MANDATORY: Systemic collapse slope scaling

# NOISE_SENSITIVITY: Simulates a ~4% operational field-triage misclassification 
# rate due to human fatigue and instrument error during mass-casualty events.
NOISE_SENSITIVITY       <- 0.04

# BASELINE_ISOLATION_BEDS: 350. Heuristic capacity estimate of repurposed 
# negative-pressure intensive care beds (Airborne Infection Isolation Rooms - AIIR) 
# across the three host cities (CDMX, GDL, MTY). This represents maximum adaptive 
# surge capacity under Crisis Standards of Care, not dedicated BSL-4 infrastructure.
BASELINE_ISOLATION_BEDS <- 350

# BASELINE_CONSERVATISM: Log-odds penalty establishing "Crisis Standards of Care". 
# A negative value forces the ML model to lower its isolation threshold, 
# aggressively favoring false positives over fatal false negatives.
BASELINE_CONSERVATISM   <- -0.5

# -------------------------------------------------------------------------
# 4. MACRO-IMPACT / ECONOMICS SETTINGS (SCRIPT 05)
# -------------------------------------------------------------------------
# CFR_BASE: 0.35. Baseline Case Fatality Rate matching the H5N1 respiratory 
# profile under *optimal* clinical care conditions where full mechanical 
# ventilation and antiviral therapies are fully available.
CFR_BASE        <- 0.35    

# CFR_CRISIS: 0.70. Fatality rate doubles for community spillovers who are denied 
# critical care due to hospital collapse (standard assumption for severe viremia).
CFR_CRISIS      <- 0.70    

# YLL_PER_DEATH: Years of Life Lost (GBD framework). Assumes the cohort's average 
# patient age (~38 years) subtracted from Mexican life expectancy demographics (~73 years).
YLL_PER_DEATH   <- 35      

# DALY_MORBIDITY: 2. Cumulative Disability-Adjusted Life Years (DALY) morbidity 
# penalty applied per survivor. Represents a simplified heuristic for post-viral 
# sequelae (e.g., equivalent to 10 years of chronic impairment or organ damage 
# at a 0.20 GBD disability weight).
DALY_MORBIDITY  <- 2       

# N_EFF_BASELINE: 5.0M. Effective Susceptible Population. Does not represent the 
# entire national census, but specifically targets the highly mobile, mass-gathering 
# epidemiological bubble (1.5M tourists + 3.5M local hospitality/stadium workers).
N_EFF_BASELINE  <- 5000000 

# GAMMA_RATE: 0.1. Removal/recovery rate parameter for the bounded SIR model. 
# Mathematically implies a 10-day acute infectious resolution window (1/gamma).
GAMMA_RATE      <- 0.1   

# GDP_PER_CAPITA_MEXICO: IMF projected nominal GDP per capita (USD) for Mexico in 2026.
GDP_PER_CAPITA_MEXICO <- 13800      

# WHO-CHOICE Value of Statistical Life (VSL) multipliers relative to GDP.
# Anchored to the WHO threshold heuristic: an intervention is 'highly cost-effective' 
# at <1x GDP per capita (Optimistic), 'cost-effective' up to 3x (Expected), with 5x 
# representing an absolute pessimistic ceiling for governmental willingness-to-pay.
VSL_EXPECTED          <- 3          
VSL_OPTIMISTIC        <- 1          
VSL_PESSIMISTIC       <- 5          

# DALY MULTIPLIERS: Mathematical scalars to dynamically adjust the DALY burden 
# during the `purrr` sensitivity grid mapping. Normalizes the extreme bounds 
# (0.50 and 0.85) against the expected crisis baseline CFR (0.70).
DALY_OPT_MULTIPLIER   <- 0.50 / 0.70  
DALY_PES_MULTIPLIER   <- 0.85 / 0.70
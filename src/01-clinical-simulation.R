# =========================================================================
# SCRIPT 01: PARAMETRIC MONTECARLO SIMULATION FOR CLINICAL TRIAGE COHORTS
# PROJECT: EPIDEMIOLOGICAL TRIAGE FIFA WORLD CUP 2026 - MEXICO
# PATHOGENS: EBOLA BUNDIBUGYO VS SEVERE SEASONAL INFLUENZA (N = 250, ALPHA = 0.01)
# =========================================================================

set.seed(20260520) # Scientific reproducibility seed (20.05.2026).
n <- 250

# 1. Target Variable: Pathogen (balanced design for logistic regression efficiency).
pathogen <- c(rep('Severe_Influenza', 125), rep('Ebola_Bundibugyo', 125))

# 2. Demographics: Age (normal distribution bounded between 16 and 80).
age <- round(rnorm(n, mean = 38, sd = 12))
age <- pmax(pmin(age, 80), 16)

# 3. Clinical: Incubation period in days (gamma distributions reflecting biology).
# Influenza: short (1-4 days). Ebola: long (2-21 days).
incubation_days <- c(round(rgamma(125, shape = 2, rate = 1)), round(rgamma(125, shape = 9, rate = 1)))
incubation_days <- pmax(incubation_days, 1)

# 4. Transmission: Presymptomatic shedding (binary: 0 = No, 1 = Yes).
# Note: Influenza sheds before symptoms; Ebola biologically does NOT.
presymptomatic_shedding <- c(sample(c(0, 1), 125, replace = TRUE, prob = c(0.3, 0.7)), rep(0, 125))

# 5. Spatiotemporal: National tourism mobility.
# 0 = World Cup Host City (CDMX/MTY/GDL), 1 = Secondary Tourist Corridor (Riviera Maya, Oaxaca, etc.).
tourism_mobility <- c(sample(c('Host_City', 'Tourist_Corridor'), 125, replace = TRUE, prob = c(0.5, 0.5)),
                      sample(c('Host_City', 'Tourist_Corridor'), 125, replace = TRUE, prob = c(0.8, 0.2)))

# 6. Behavioral: Exposure route in crowded environments.
# 0 = General aerosol exposure (stadiums/transit).
# 1 = Direct fluid contact/skin-to-skin close contact with active outbreak node vectors.
exposure_route <- c(sample(c(0, 1), 125, replace = TRUE, prob = c(0.95, 0.05)),
                    sample(c(0, 1), 125, replace = TRUE, prob = c(0.02, 0.98)))

# 7. Behavioral bias: Emergency care avoidance (0 = Immediate care, 1 = Self-medicates/avoids).
care_avoidance <- sample(c(0, 1), n, replace = TRUE, prob = c(0.65, 0.35))

# 8. Biomarkers: Rapid Complete Blood Count (CBC) at Triage Admission.
# Ebola causes severe acute thrombocytopenia (low platelets) and leukopenia (low WBC).
platelets_count <- c(round(rnorm(125, mean = 220000, sd = 40000)), round(rnorm(125, mean = 75000, sd = 20000)))
white_blood_cells <- c(round(rnorm(125, mean = 7500, sd = 1500)), round(rnorm(125, mean = 2800, sd = 800)))

# 9. Clinical Outcome Logistics: Hospital Stay Days until resolution.
# Influenza: 10-14 days in ICU. Ebola: abrupt deaths (days 6-9) or long convalescence (>21 days).
hospital_stay_days <- c(round(rnorm(125, mean = 11, sd = 3)), 
                        round(ifelse(runif(125) < 0.4, rnorm(125, 7, 1.5), rnorm(125, 24, 4))))
hospital_stay_days <- pmax(hospital_stay_days, 2)

# 10. Epidemiological Risk Indicator: Secondary Attack Rate (SAR % within household).
household_sar <- c(round(rnorm(125, mean = 42, sd = 10)), round(rnorm(125, mean = 4, sd = 2)))
household_sar <- pmax(pmin(household_sar, 100), 0)

# 11. Final Patient Outcome (0 = Recovered/Discharged, 1 = Deceased).
# Case Fatality Rate (CFR): Severe Influenza ~2-5%; Ebola Bundibugyo ~40% (WHO parameter).
clinical_outcome <- c(sample(c(0, 1), 125, replace = TRUE, prob = c(0.96, 0.04)),
                      sample(c(0, 1), 125, replace = TRUE, prob = c(0.60, 0.40)))

# Consolidate synthetic DataFrame:
df_triage <- data.frame(
  patient_id = 1:n,
  pathogen = as.factor(pathogen),
  age = age,
  incubation_days = incubation_days,
  presymptomatic_shedding = as.factor(presymptomatic_shedding),
  tourism_mobility = as.factor(tourism_mobility),
  exposure_route = as.factor(exposure_route),
  care_avoidance = as.factor(care_avoidance),
  platelets_count = platelets_count,
  white_blood_cells = white_blood_cells,
  hospital_stay_days = hospital_stay_days,
  household_sar = household_sar,
  clinical_outcome = as.factor(clinical_outcome)
)

# Export to data directory (tracked and reproducible placement):
write.csv(df_triage, 'data/dataset_ebola_vs_influenza_fifawc26_triage_mexico.csv', row.names = FALSE)
cat('Success: Dataset generated and saved at data/dataset_ebola_vs_influenza_fifawc26_triage_mexico.csv\n')
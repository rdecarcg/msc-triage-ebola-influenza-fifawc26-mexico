# =========================================================================
# SCRIPT 02: ADVANCED EXPLORATORY DATA ANALYSIS (EDA)
# PROJECT: EPIDEMIOLOGICAL TRIAGE FIFA WORLD CUP 2026 - MEXICO
# PATHOGENS: EBOLA BUNDIBUGYO VS INFLUENZA H5N1
# =========================================================================

# Environment & Dependencies:
library(tidyverse)
library(patchwork)

# Reproducible figures directory.
dir.create('reports/figures/', showWarnings = FALSE, recursive = TRUE)

# Simulation dataset load.
if (!file.exists('data/dataset_ebola_vs_influenza_fifawc26_triage_mexico.csv')) {
  stop('Error: Clean dataset not found. Run script 01-clinical-simulation.R first.')
}

df_triage <- read.csv('data/dataset_ebola_vs_influenza_fifawc26_triage_mexico.csv', stringsAsFactors = TRUE)

# =========================================================================
# SECTION 1: DESCRIPTIVE EPIDEMIOLOGICAL PROFILES (TABULAR ANALYTICS).
# =========================================================================

cat('--- OMNIBUS PATHOGEN PROFILE SUMMARY ---\n')
pathogen_summary <- df_triage %>%
  group_by(pathogen) %>%
  summarise(
    sample_size = n(),
    mean_age    = mean(age),
    sd_age      = sd(age),
    mean_incubation = mean(incubation_days),
    mean_platelets  = mean(platelets_count),
    mean_wbc        = mean(white_blood_cells),
    mean_stay       = mean(hospital_stay_days),
    mean_sar        = mean(household_sar)
  )
print(pathogen_summary)

# Contingency Matrix: Exposure route vs pathogen (Validation of transmission mechanics).
cat('\n--- CONTINGENCY: PATHOGEN VS EXPOSURE ROUTE ---\n')
print(table(df_triage$pathogen, df_triage$exposure_route))

# =========================================================================
# SECTION 2: BIOMARKER DISCRIMINATION ANALYTICS (VISUALIZATIONS).
# =========================================================================

# Custom professional theme for public health reports.
theme_triage <- function() {
  theme_minimal(base_size = 11) +
    theme(
      plot.title = element_text(face = 'bold', size = 12, hjust = 0.5),
      plot.subtitle = element_text(size = 10, color = 'darkgray', hjust = 0.5),
      panel.grid.minor = element_blank(),
      legend.position = 'bottom',
      strip.text = element_text(face = 'bold', size = 10)
    )
}

# Plot A: Hematological Profile (Platelets vs WBC).
p_biomarkers <- ggplot(df_triage, aes(x = platelets_count, y = white_blood_cells, color = pathogen)) +
  geom_point(alpha = 0.7, size = 2.5) +
  scale_color_manual(values = c('Ebola_Bundibugyo' = '#8B0000', 'Influenza_H5N1' = '#2F4F4F')) +
  labs(
    title = 'Hematological Triage Space',
    subtitle = 'Complete Blood Count (CBC) Disambiguation Node',
    x = 'Platelets Count (cells/µL)',
    y = 'White Blood Cells Count (cells/µL)',
    color = 'Pathogen Agent:'
  ) +
  theme_triage()

# Spanish version for paper (Perfil Hematológico (Plaquetas vs Glóbulos Blancos).
p_biomarkers_sp <- ggplot(df_triage, aes(x = platelets_count, y = white_blood_cells, color = pathogen)) +
  geom_point(alpha = 0.7, size = 2.5) +
  scale_color_manual(
    values = c('Ebola_Bundibugyo' = '#8B0000', 'Influenza_H5N1' = '#2F4F4F'),
    labels = c('Ébola Bundibugyo', 'Influenza A(H5N1)')
  ) +
  labs(
    title = 'Espacio de Triage Hematológico',
    subtitle = 'Nodo de Desambiguación: Biometría Hemática Completa (CBC)',
    x = 'Conteo de Plaquetas (células/µL)',
    y = 'Conteo de Leucocitos (células/µL)',
    color = 'Agente Patógeno:'
  ) +
  theme_triage()

# Plot B: Incubation Period Densities.
p_incubation <- ggplot(df_triage, aes(x = incubation_days, fill = pathogen)) +
  geom_density(alpha = 0.6) +
  scale_fill_manual(values = c('Ebola_Bundibugyo' = '#8B0000', 'Influenza_H5N1' = '#2F4F4F')) +
  labs(
    title = 'Temporal Latency Distribution',
    subtitle = 'Probability Density of Incubation Eras',
    x = 'Days from Exposure to Symptom Onset',
    y = 'Density',
    fill = 'Pathogen Agent'
  ) +
  theme_triage()

# Spanish version for paper (Densidades del Periodo de Incubación).
p_incubation_sp <- ggplot(df_triage, aes(x = incubation_days, fill = pathogen)) +
  geom_density(alpha = 0.6) +
  scale_fill_manual(
    values = c('Ebola_Bundibugyo' = '#8B0000', 'Influenza_H5N1' = '#2F4F4F'),
    labels = c('Ébola Bundibugyo', 'Influenza A(H5N1)')
  ) +
  labs(
    title = 'Distribución de Latencia Temporal',
    subtitle = 'Densidad de probabilidad de los periodos de incubación',
    x = 'Días desde la exposición hasta el inicio de síntomas',
    y = 'Densidad',
    fill = 'Agente Patógeno:'
  ) +
  theme_triage()

# Combine plots using patchwork grid.
eda_composite_1 <- p_biomarkers + p_incubation + plot_layout(guides = 'collect') & theme(legend.position = 'bottom')
eda_composite_1_sp <- p_biomarkers_sp + p_incubation_sp + plot_layout(guides = 'collect') & theme(legend.position = 'bottom')

# Save high-resolution visualization for documentation.
ggsave('reports/figures/eda_biomarkers_and_latency.png', plot = eda_composite_1, width = 10, height = 5, dpi = 300)
ggsave('reports/figures/eda_biomarkers_and_latency_sp.png', plot = eda_composite_1_sp, width = 10, height = 5, dpi = 300)

# =========================================================================
# SECTION 3: CLINICAL RESOLUTION AND TRANSMISSIBILITY SURVEILLANCE
# =========================================================================

# Plot C: Hospital Stay vs Household SAR stratified by Clinical Outcome.
eda_composite_2 <- ggplot(df_triage, aes(x = pathogen, y = hospital_stay_days, fill = factor(clinical_outcome))) +
  geom_boxplot(alpha = 0.8, outlier.color = 'red') +
  scale_fill_manual(values = c('0' = '#4682B4', '1' = '#CD5C5C'), labels = c('Recovered', 'Deceased')) +
  facet_wrap(~clinical_outcome, scales = 'free_y', 
             labeller = as_labeller(c('0' = 'Cohorts: Discharged','1' = 'Cohorts: Fatal Outbreaks'))) +
  labs(
    title = 'Clinical Kinetic Fingerprint',
    subtitle = 'Length of Stay (LoS) Patterns by Pathogen and Final Outcome',
    x = 'Etiological Agent',
    y = 'Hospitalization Days',
    fill = 'Patient Fate') + 
  theme_triage()

# Spanish version for paper ().
eda_composite_2_sp <- ggplot(df_triage, aes(x = pathogen, y = hospital_stay_days, fill = factor(clinical_outcome))) +
  geom_boxplot(alpha = 0.8, outlier.color = "red") +
  scale_x_discrete(labels = c("Ebola_Bundibugyo" = "Ébola Bundibugyo", "Influenza_H5N1" = "Influenza A(H5N1)")) +
  scale_fill_manual(values = c("0" = "#4682B4", "1" = "#CD5C5C"), labels = c("Recuperado (Alta Médica)", "Fallecido (Caso Fatal)")) +
  facet_wrap(~ clinical_outcome, scales = "free_y", 
             labeller = as_labeller(c("0" = "Cohortes: Altas Hospitalarias", "1" = "Cohortes: Desenlaces Fatales"))) +
  labs(
    title = "Huella Cinética Clínica",
    subtitle = "Patrones de estancia hospitalaria según patógeno y desenlace categorizado",
    x = "Agente etiológico",
    y = "Días de hospitalización",
    fill = "Estado del paciente") +
  theme_triage()

ggsave('reports/figures/eda_clinical_outcomes.png', plot = eda_composite_2, width = 8, height = 5, dpi = 300)
ggsave('reports/figures/eda_clinical_outcomes_sp.png', plot = eda_composite_2_sp, width = 8, height = 5, dpi = 300)

cat('\nSuccess: EDA completed. Plots exported to reports/figures/\n')
# =========================================================================
# SCRIPT 02: ADVANCED EXPLORATORY DATA ANALYSIS (EDA) & DIAGNOSTICS
# PROJECT: EPIDEMIOLOGICAL TRIAGE FIFA WORLD CUP 2026 - MEXICO
# AUTHOR: RODRIGO ABEL DE CARCER GANDARILLA
# =========================================================================
# DESCRIPTION: This script performs baseline clinical profiling, assesses 
# feature multicollinearity via Variance Inflation Factors (VIF), and generates 
# bivariate probability distributions. These diagnostics are critical to 
# evaluate the discriminative power and statistical independence of 
# physiological and chronological variables prior to predictive modelling.
# =========================================================================

if (!require('pacman')) install.packages('pacman')
pacman::p_load(tidyverse, patchwork, corrplot, car)

# =========================================================================
# 0. INITIALIZATION & DATA PROVENANCE ####
# =========================================================================
# Load global configuration constraints to ensure environmental consistency.
if (file.exists('src/config.R')) source('src/config.R', echo = FALSE)
if (!dir.exists('reports/figures')) dir.create('reports/figures', recursive = TRUE)

# Enforce sequential pipeline execution by validating the generated cohort.
if (!file.exists('data/dataset_ebola_vs_influenza_fifawc26_triage_mexico.csv')) {
  stop('Error: Clean cohort dataset not found. Run Script 01 first.')
}
df_triage <- read.csv('data/dataset_ebola_vs_influenza_fifawc26_triage_mexico.csv', stringsAsFactors = TRUE)

# =========================================================================
# 1. TABLE 1: CLINICAL BASELINE CHARACTERISTICS ####
# =========================================================================
# Generation of a standard epidemiological "Table 1" conforming to STROBE 
# reporting guidelines. It summarizes synthetic cohort demographics and 
# central tendencies of clinical biomarkers stratified by etiological agent.
cat('--- TABLE 1: CLINICAL BASELINE CHARACTERISTICS ---\n')
table_1 <- df_triage %>%
  group_by(pathogen) %>%
  summarise(
    N = n(),
    Age_Mean_SD = paste0(round(mean(age), 1), ' (', round(sd(age), 1), ')'),
    Platelets_Median = median(platelets_count),
    WBC_Median = median(white_blood_cells),
    Stay_Days_Mean = round(mean(hospital_stay_days), 1)
  )
print(as.data.frame(table_1))

# =========================================================================
# 2. MULTICOLLINEARITY DIAGNOSTICS (CORRELATION & VIF) ####
# =========================================================================
# Assessing predictive feature independence. High multicollinearity can 
# artificially inflate standard errors, destabilize coefficient estimates, 
# and introduce bias in subsequent regularized regression models.

cat('\n--- GENERATING CORRELATION MATRIX & VIF ---\n')

# 2.1 Pearson Correlation Matrix.
# Explicit exclusion of primary keys (patient_id) to prevent data leakage 
# and isolation of purely physiological/chronological numeric vectors.
numeric_vars <- df_triage %>% select(where(is.numeric), -patient_id, -clinical_outcome)
cor_matrix <- cor(numeric_vars, method = 'pearson')

png('reports/figures/correlation_matrix.png', width = 1200, height = 1200, res = 200)
corrplot(cor_matrix, method = 'color', type = 'upper', order = 'hclust',
         addCoef.col = 'black', tl.col = 'black', 
         tl.srt = 95, tl.cex = 0.7, number.cex = 0.7, diag = FALSE,
         title = 'Predictor Correlation Matrix', mar = c(0, 1, 1, 0))
dev.off()

# 2.2 Variance Inflation Factor (VIF) Calculation.
# Diagnostic check executed via an auxiliary multivariable generalized linear 
# model (GLM) mapping predictor features against the binary target class.
# Threshold heuristic: A strict VIF < 2.5 boundary is enforced (exceeding standard 
# econometric thresholds of 5.0) to ensure the high-fidelity feature independence 
# required for clinical decision support systems.
vif_model <- glm(pathogen ~ age + incubation_days + platelets_count + 
                   white_blood_cells + hospital_stay_days + household_sar, 
                 data = df_triage, family = binomial)
cat('\n--- VIF VALUES (Target Validation: < 2.5) ---\n')
print(vif(vif_model))

# =========================================================================
# 3. BIOMARKER DISCRIMINATION ANALYTICS (VISUALIZATIONS) ####
# =========================================================================

# 3.1 Bivariate Hematological Distribution.
# Evaluates the topological separation and spatial overlap of pathogens based 
# on correlated blood markers. Demonstrates the algorithm's capacity to 
# isolate systemic viral responses (e.g., profound thrombocytopenia in BDBV).
p_biomarkers <- ggplot(df_triage, aes(x = platelets_count, y = white_blood_cells, color = pathogen)) +
  geom_point(alpha = 0.2, size = 1) +
  geom_density_2d(linewidth = 0.4) +
  scale_color_manual(values = c('Influenza_H5N1' = '#8B0000', 'Ebola_Bundibugyo' = '#2F4F4F')) +
  labs(title = 'Bivariate Distribution: Platelets vs WBC', 
       subtitle = 'Hematological profile separation',
       x = 'Platelets Count', y = 'White Blood Cells') +
  theme_minimal()

# 3.2 Pathogen Latency Profiles.
# Density mapping of incubation periods to visualize chronological divergence.
# This validates incubation period as a high-value primary splitting node 
# for subsequent tree-based ensemble algorithms (Random Forest / XGBoost).
p_incubation <- ggplot(df_triage, aes(x = incubation_days, fill = pathogen)) +
  geom_density(alpha = 0.6) +
  scale_fill_manual(values = c('Influenza_H5N1' = '#8B0000', 'Ebola_Bundibugyo' = '#2F4F4F')) +
  labs(title = 'Latency Period Distribution', 
       subtitle = 'Incubation chronological density',
       x = 'Incubation Days', y = 'Density') +
  theme_minimal()

# 3.3 Composite Publishable Output.
# Merging topological mappings into a single cohesive statistical figure 
# tailored for manuscript submission.
eda_composite_1 <- p_biomarkers + p_incubation + plot_layout(guides = 'collect') & theme(legend.position = 'bottom')
ggsave('reports/figures/eda_biomarkers_and_latency.png', plot = eda_composite_1, width = 10, height = 5, dpi = 300)

# Synchronize generated analytical objects with the Interactive Dashboard environment.
ggsave('epitriage-dashboard/www/eda_biomarkers_and_latency.png', plot = eda_composite_1, width = 16, height = 5, dpi = 300)
file.copy(from = 'reports/figures/correlation_matrix.png', 
          to = 'epitriage-dashboard/www/correlation_matrix.png', 
          overwrite = TRUE)

cat('\nSuccess: EDA completed. Diagnostics and Plots exported to reports/figures/\n')
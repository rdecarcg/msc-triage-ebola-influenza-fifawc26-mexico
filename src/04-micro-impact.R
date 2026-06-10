# =========================================================================
# SCRIPT 04: OPERATIONAL TRIAGE & MICRO-IMPACT SIMULATION ENGINE
# PROJECT: EPIDEMIOLOGICAL TRIAGE FIFA WORLD CUP 2026 - MEXICO
# CONCEPT: DYNAMIC TRIAGE THRESHOLD OPTIMIZATION (CRISIS STANDARDS OF CARE)
# AUTHOR: RODRIGO ABEL DE CARCER GANDARILLA
# =========================================================================
# DESCRIPTION: This script executes a stochastic discrete-time simulation 
# across a 60-day horizon to model healthcare facility resource saturation. 
# It utilizes the predictive output of the ensemble meta-learner to dynamically 
# adjust clinical triage admission thresholds based on real-time surge 
# capacity constraints, perfectly aligning with international Crisis 
# Standards of Care (CSC) frameworks.
# =========================================================================

if (!require('pacman')) install.packages('pacman')
pacman::p_load(tidyverse, tidymodels, stacks, patchwork, ggrepel)

# =========================================================================
# 0. INITIALIZATION & AESTHETIC SETUP ####
# =========================================================================
# Load global constants to enforce parameter immutability.
if (file.exists('src/config.R')) source('src/config.R', echo = FALSE)

# Pre-defined visualization theme for publication-ready graphical outputs.
theme_triage <- function() {
  theme_minimal(base_size = 11) +
    theme(
      plot.title = element_text(face = 'bold', size = 14, hjust = 0.5),
      plot.subtitle = element_text(size = 10, color = 'dimgray', hjust = 0.5),
      panel.grid.minor = element_blank(),
      legend.position = 'bottom',
      strip.text = element_text(face = 'bold', size = 11)
    )
}

# =========================================================================
# 1. ENVIRONMENT PIPELINE INITIALIZATION ####
# =========================================================================
# Enforce sequential dependency: Validate the existence of the operational 
# dataset and the serialized ML model before initializing the simulation engine.
if (!file.exists('data/dataset_operational_triage.csv')) {
  stop('Error: Operational triage dataset missing. Run Script 03 first.')
}
if (!file.exists('reports/models/stacked_triage_model.rds')) {
  stop('Error: Stacked ensemble model binary not found. Run Script 03 first.')
}

df_triage_op <- read.csv('data/dataset_operational_triage.csv', stringsAsFactors = TRUE)
df_triage_op$is_ebola <- factor(df_triage_op$is_ebola, levels = c('Ebola', 'Influenza'))
triage_stack <- readRDS('reports/models/stacked_triage_model.rds')

# Extract out-of-sample class probability vectors and append to the operational cohort.
full_preds <- predict(triage_stack, df_triage_op, type = 'prob')
df_triage_op$prob_pred <- full_preds$.pred_Ebola
df_triage_op$is_ebola_num <- ifelse(df_triage_op$is_ebola == 'Ebola', 1, 0)

# =========================================================================
# 2. GLOBAL STOCHASTIC PARAMETERS & FUNCTIONAL ENGINE ####
# =========================================================================
set.seed(GLOBAL_SEED) 
total_patients <- nrow(df_triage_op)

# Staggered Patient Arrivals: Modeled via a Beta distribution (shape1=2, shape2=3.5)
# to accurately replicate a right-skewed acute epidemiological surge profile, 
# avoiding the flat bias of uniform distributions.
stochastic_arrivals <- rbeta(total_patients, shape1 = 2, shape2 = 3.5)
df_triage_op$arrival_day <- round(stochastic_arrivals * (SIMULATION_HORIZON_DAYS - 1)) + 1

# Core Simulation Engine: A discrete-time loop mapping resource-dependent allocation.
simulate_triage <- function(scenario_name, current_Rt, max_isolation_beds, base_conservatism) {
  
  bed_registry <- data.frame(discharge_day = numeric(0), case_type = numeric(0))
  history_bed_demand <- numeric(SIMULATION_HORIZON_DAYS)
  history_occupied_beds <- numeric(SIMULATION_HORIZON_DAYS)
  deferred_secondary_outbreaks <- rep(0, SIMULATION_HORIZON_DAYS + 30)
  daily_demand <- numeric(SIMULATION_HORIZON_DAYS)
  
  # Forward-projection of local transmission vectors to map baseline demand.
  for (d in 1:SIMULATION_HORIZON_DAYS) {
    new_cases <- sum(df_triage_op$arrival_day == d)
    projected_load <- new_cases * (current_Rt ^ (d / 10)) 
    
    # Apply non-linear penalty multiplier when infrastructural thresholds are breached.
    if (sum(daily_demand) > max_isolation_beds) {
      projected_load <- projected_load * SURGE_PENALTY
    }
    daily_demand[d] <- projected_load
  }
  
  # Daily operational simulation executing capacity-aware clinical triage.
  for (t in 1:SIMULATION_HORIZON_DAYS) {
    
    # 1. Clear hospital beds for patients who have reached their discharge day.
    if (nrow(bed_registry) > 0) {
      bed_registry <- bed_registry %>% filter(discharge_day > t)
    }
    
    outbreak_cases_today <- deferred_secondary_outbreaks[t]
    cohort_today <- df_triage_op %>% filter(arrival_day == t)
    num_influenza_today <- sum(cohort_today$is_ebola_num == 0)
    
    # 2. Real-time infrastructural constraint tracking.
    occupancy_rate <- nrow(bed_registry) / max_isolation_beds
    
    # 3. Dynamic Sigmoid Optimization: 
    # Adjusting the log-odds decision boundary dynamically based on baseline 
    # institutional conservatism, real-time bed scarcity, and diagnostic noise.
    log_odds_threshold <- base_conservatism + (SURGE_PENALTY * occupancy_rate) + 
      (NOISE_SENSITIVITY * num_influenza_today)
    
    p_optimal <- 1 / (1 + exp(-log_odds_threshold))
    
    # 4. Clinical Allocation Logic
    if (nrow(cohort_today) > 0) {
      cohort_today <- cohort_today %>% mutate(pred_ebola = ifelse(prob_pred >= p_optimal, 1, 0))
      
      # True Positives: Correctly identified and admitted to High-Level Isolation.
      tp_cases <- cohort_today %>% filter(is_ebola_num == 1 & pred_ebola == 1)
      # False Negatives: Fatal clinical misses prematurely discharged into the community.
      fp_cases <- cohort_today %>% filter(is_ebola_num == 1 & pred_ebola == 0) 
      
      if (nrow(tp_cases) > 0) {
        for (i in 1:nrow(tp_cases)) {
          # Hard limit ceiling to prevent infinite memory bounds during complete collapse.
          if (nrow(bed_registry) < max_isolation_beds * 5) {
            bed_registry <- rbind(bed_registry, data.frame(discharge_day = t + tp_cases$hospital_stay_days[i], case_type = 1))
          }
        }
      }
      
      # Trigger localized community outbreaks as a direct consequence of False Negatives.
      if (nrow(fp_cases) > 0) {
        deferred_secondary_outbreaks[t + 5] <- deferred_secondary_outbreaks[t + 5] + 
          round(nrow(fp_cases) * current_Rt * 0.1)
      }
    }
    
    # Record longitudinal timeline vectors.
    history_bed_demand[t] <- nrow(bed_registry) + outbreak_cases_today
    history_occupied_beds[t] <- nrow(bed_registry)
  }
  
  tibble(
    Scenario = scenario_name,
    Rt = current_Rt,
    Max_Beds = max_isolation_beds,
    Conservatism = base_conservatism,
    Day = 1:SIMULATION_HORIZON_DAYS,
    Occupied_Beds = history_occupied_beds,
    Bed_Demand = history_bed_demand
  )
}

# =========================================================================
# 3. SENSITIVITY GRID EVALUATION & HEATMAP (MACRO-IMPACT) ####
# =========================================================================
# 2-Way Sensitivity Landscape Exploration via Functional Programming (`purrr`).
cat('\n--- RUNNING 2-WAY SENSITIVITY GRID VIA PURRR ---\n')

param_grid <- expand_grid(
  scenario_name = 'Sensitivity_Mapping',
  current_Rt = 2.0, 
  max_isolation_beds = seq(200, 500, by = 50),
  base_conservatism = seq(-1.5, 1.5, by = 0.5)
)

df_sensitivity_raw <- pmap_dfr(param_grid, simulate_triage)

# Identifying the critical day of systemic infrastructure failure across all dimensions.
df_sensitivity <- df_sensitivity_raw %>%
  mutate(Is_Collapsed = Bed_Demand > Max_Beds) %>%
  filter(Is_Collapsed == TRUE) %>%
  group_by(Max_Beds, Conservatism) %>%
  summarise(Saturation_Day = min(Day), .groups = 'drop') %>%
  right_join(df_sensitivity_raw %>% select(Max_Beds, Conservatism) %>% distinct(), by = c('Max_Beds', 'Conservatism')) %>%
  mutate(
    Saturation_Day = replace_na(Saturation_Day, SIMULATION_HORIZON_DAYS),
    Clinical_Profile = case_when(
      Conservatism <= -1.0 ~ 'Aggressive Discharge (High Risk)',
      Conservatism == -0.5 ~ 'Standard CSC',
      Conservatism == 0.0  ~ 'Neutral Boundary',
      Conservatism >= 0.5  ~ 'Conservative Isolation (Low Risk)'
    )
  )

plot_heatmap <- ggplot(df_sensitivity, aes(x = factor(Max_Beds), y = factor(Conservatism), fill = Saturation_Day)) +
  geom_tile(color = 'white', linewidth = 1) +
  geom_text(aes(label = ifelse(Saturation_Day >= 60, 'No\nCollapse', paste('Day', Saturation_Day)),
                color = ifelse(Saturation_Day >= 13, 'white', 'black')), fontface = 'bold', size = 4) +
  scale_color_manual(values = c('black' = 'black', 'white' = 'white'), guide = 'none') +
  scale_fill_viridis_c(option = 'magma', direction = -1, name = 'Saturation Day') +
  labs(title = 'Infrastructure Elasticity vs Clinical Triage Aggressiveness',
       subtitle = 'Identifying the Critical Day of System Collapse Across Operational Parameters (Rt = 2.0)',
       x = 'Maximum Isolation Bed Capacity (Infrastructure)', y = 'Baseline Conservatism Index (Clinical Policy)') +
  theme_minimal() + theme(legend.position = 'bottom')

if (!dir.exists('reports/figures')) dir.create('reports/figures', recursive = TRUE)
ggsave('reports/figures/sensitivity_heatmap.png', plot = plot_heatmap, width = 9, height = 7, dpi = 300)

# =========================================================================
# 4. COUNTERFACTUAL SCENARIOS & SATURATION PLOTTING (MICRO-IMPACT) ####
# =========================================================================
# Simulating Baseline & Counterfactual Epidemiological Trajectories.
cat('\n--- RUNNING EPIDEMIOLOGICAL COUNTERFACTUALS ---\n')

scenario_definitions <- tibble(
  scenario_name = c('Negligence', 'Standard_Campaign', 'Max_Awareness'),
  current_Rt = c(3.5, 2.0, 1.1),
  max_isolation_beds = BASELINE_ISOLATION_BEDS,
  base_conservatism = BASELINE_CONSERVATISM
)

df_master_simulation <- pmap_dfr(scenario_definitions, simulate_triage)

# Isolate topological crossing points to annotate graphical output.
threshold_crossings <- df_master_simulation %>% filter(Bed_Demand > Max_Beds) %>% group_by(Scenario) %>% slice_min(Day, n = 1) %>% ungroup()

df_sim_en <- df_master_simulation %>%
  mutate(Scenario_Label = case_when(
    Scenario == 'Negligence' ~ 'Collective Negligence (Rt = 3.5)',
    Scenario == 'Standard_Campaign' ~ 'Standard Campaign (Rt = 2.0)',
    Scenario == 'Max_Awareness' ~ 'Adaptive Control (Rt = 1.1)'
  )) %>%
  mutate(Scenario_Label = factor(Scenario_Label, levels = c('Collective Negligence (Rt = 3.5)', 'Standard Campaign (Rt = 2.0)', 'Adaptive Control (Rt = 1.1)')))

threshold_crossings <- threshold_crossings %>% left_join(df_sim_en %>% select(Scenario, Day, Scenario_Label) %>% distinct(), by = c('Scenario', 'Day'))
peak_rt11 <- df_sim_en %>% filter(Scenario == 'Max_Awareness') %>% slice_max(Bed_Demand, n = 1)

plot_en <- ggplot(df_sim_en, aes(x = Day, y = Bed_Demand, color = Scenario_Label)) +
  geom_ribbon(aes(ymin = BASELINE_ISOLATION_BEDS, ymax = pmax(Bed_Demand, BASELINE_ISOLATION_BEDS), fill = Scenario_Label), alpha = 0.15, color = NA, show.legend = FALSE) +
  geom_line(linewidth = 1.2) +
  geom_hline(yintercept = BASELINE_ISOLATION_BEDS, linetype = 'dashed', color = 'red', linewidth = 0.8) +
  annotate('text', x = 2, y = BASELINE_ISOLATION_BEDS + 60, label = paste0('Critical Capacity Limit (', BASELINE_ISOLATION_BEDS, ' Beds)'), color = 'red', fontface = 'bold', hjust = -2.5) +
  geom_point(data = threshold_crossings, aes(x = Day, y = Bed_Demand, color = Scenario_Label), size = 4, shape = 21, fill = 'white', stroke = 2, show.legend = FALSE) +
  ggrepel::geom_label_repel(data = threshold_crossings, aes(x = Day, y = Bed_Demand, label = paste0('Saturation Day: ', Day, '\n(Beds: ', round(Bed_Demand), ')'), color = Scenario_Label), fill = 'white', fontface = 'bold', size = 3.5, nudge_y = 300, nudge_x = -5, segment.size = 0.6, show.legend = FALSE, alpha = 0.8) +
  annotate('label', x = 48, y = 950, label = paste0('SUCCESS: Adaptive Control Peak\nDay ', peak_rt11$Day, ' (', round(peak_rt11$Bed_Demand), ' Beds)'), color = '#2E4053', fill = '#EAECEE', fontface = 'bold', size = 4.2) +
  geom_curve(data = peak_rt11, aes(x = 42, y = 894, xend = Day, yend = Bed_Demand + 20), arrow = arrow(length = unit(0.2, 'cm')), color = '#2E4053', linewidth = 0.8, curvature = 0.2, inherit.aes = FALSE) +
  geom_point(data = peak_rt11, aes(x = Day, y = Bed_Demand), color = '#2E4053', size = 5, shape = 18, show.legend = FALSE) +
  scale_color_manual(values = c('Collective Negligence (Rt = 3.5)' = '#8B0000', 'Standard Campaign (Rt = 2.0)' = '#E67E22', 'Adaptive Control (Rt = 1.1)' = '#2E4053')) +
  scale_fill_manual(values = c('Collective Negligence (Rt = 3.5)' = '#8B0000', 'Standard Campaign (Rt = 2.0)' = '#E67E22', 'Adaptive Control (Rt = 1.1)' = '#2E4053')) +
  labs(title = 'Operational Impact of Triage Under Counterfactual Scenarios', subtitle = 'Identifying Infrastructure Saturation Dynamics Under Crisis Standards of Care', x = 'Simulation Horizon (Days)', y = 'Total Isolation Bed Demand', color = 'Scenario:', fill = 'Scenario:') + 
  theme_minimal() + theme(legend.position = 'bottom')

ggsave('reports/figures/saturation_scenarios.png', plot = plot_en, width = 10, height = 6, dpi = 300)

# =========================================================================
# 5. ASYMMETRIC WEIGHTED CONFUSION MATRIX EVALUATION ####
# =========================================================================
# Evaluating the algorithmic decision boundary under the paradigm of highly 
# asymmetric misclassification costs, where false negatives severely outweigh 
# false positives in terms of systemic epidemiological impact.

BASE_THRESHOLD <- 1 / (1 + exp(-BASELINE_CONSERVATISM))

df_eval <- df_triage_op %>%
  mutate(
    clinical_decision = ifelse(prob_pred >= BASE_THRESHOLD, 1, 0),
    Actual = ifelse(is_ebola_num == 1, 'Ebola (Actual)', 'Influenza (Actual)'),
    Predicted = ifelse(clinical_decision == 1, 'Isolate (Predicted)', 'Discharge (Predicted)')
  )

df_cm <- df_eval %>%
  group_by(Actual, Predicted) %>%
  summarise(Cases = n(), .groups = 'drop') %>%
  mutate(
    Proportion = round(Cases / sum(Cases) * 100, 2),
    Outcome = case_when(
      Actual == 'Ebola (Actual)' & Predicted == 'Isolate (Predicted)' ~ 'True Positive',
      Actual == 'Ebola (Actual)' & Predicted == 'Discharge (Predicted)' ~ 'False Negative\n(FATAL MISS)',
      Actual == 'Influenza (Actual)' & Predicted == 'Isolate (Predicted)' ~ 'False Positive\n(BED WASTED)',
      Actual == 'Influenza (Actual)' & Predicted == 'Discharge (Predicted)' ~ 'True Negative'
    )
  )

plot_cm <- ggplot(df_cm, aes(x = Predicted, y = Actual, fill = Outcome)) +
  geom_tile(color = 'white', linewidth = 2) +
  geom_text(aes(label = paste0(Outcome, '\n\n', format(Cases, big.mark = ','), '\n(', Proportion, '%)')), fontface = 'bold', size = 5, color = 'white') +
  scale_fill_manual(values = c('True Positive' = '#117A65', 'True Negative' = '#2E86C1', 'False Positive\n(BED WASTED)' = '#F39C12', 'False Negative\n(FATAL MISS)' = '#C0392B')) +
  scale_x_discrete(position = 'bottom') + 
  labs(title = 'Operational Confusion Matrix: Asymmetric Cost', subtitle = 'Decision Boundary Optimized for Minimizing Fatal Misses', x = 'Clinical Triage Algorithm Decision', y = 'True Pathogen Pathology') +
  theme_minimal() + theme(legend.position = 'none')

ggsave('reports/figures/weighted_confusion_matrix_grid.png', plot = plot_cm, width = 8, height = 6, dpi = 300)

# Synchronize output objects with the local Shiny Application dependencies.
write.csv(df_triage_op, file.path('epitriage-dashboard/data/dataset_operational_triage.csv'), row.names = FALSE)
saveRDS(plot_en, 'epitriage-dashboard/data/plot_saturation.rds')
ggsave('epitriage-dashboard/www/saturation_scenarios.png', plot = plot_en, width = 15, height = 6, dpi = 300)
ggsave('epitriage-dashboard/www/sensitivity_heatmap.png', plot = plot_heatmap, width = 8, height = 7, dpi = 300)
ggsave('epitriage-dashboard/www/weighted_confusion_matrix_grid.png', plot = plot_cm, width = 8, height = 6, dpi = 300)

cat('\n[SUCCESS] Script 04: Complete Macro & Micro Impact Analysis Engine Executed.\n')
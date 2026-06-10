# =========================================================================
# SCRIPT 05: MACRO-EPIDEMIOLOGICAL IMPACT & HEALTH ECONOMICS ENGINE
# PROJECT: EPIDEMIOLOGICAL TRIAGE FIFA WORLD CUP 2026 - MEXICO
# CONCEPT: LONG-TERM COMMUNITY SPILLOVER & DALY BURDEN ANALYSIS
# AUTHOR: RODRIGO ABEL DE CARCER GANDARILLA
# =========================================================================
# DESCRIPTION: Translates micro-operational hospital overflows into community 
# transmission spillovers. Quantifies long-term societal health deficits 
# using the Global Burden of Disease (GBD) framework, isolating cumulative 
# mortality and Disability-Adjusted Life Years (DALYs). Introduces a 
# bounded SIR community model driven by triage leakage and evaluates 
# macroeconomic burden via WHO-CHOICE parameter sensitivity matrices.
# =========================================================================

if (!require('pacman')) install.packages('pacman')
pacman::p_load(dplyr, tidyr, ggplot2, scales, purrr)

# Load global configuration constraints to ensure environmental consistency.
if (file.exists('src/config.R')) source('src/config.R', echo = FALSE)

# Enforce sequential pipeline dependency.
if (!exists('df_sim_en')) stop('Error: Core simulation structures missing. Execute Script 04 prior.')

# =========================================================================
# 1. AUTONOMOUS COMMUNITY SPILLOVER CALCULATOR (BOUNDED SIR) ####
# =========================================================================
# Modeling systemic failure: Community transmission is mathematically bounded 
# to only occur as a direct consequence of hospital resource exhaustion 
# (i.e., discharging highly infectious clinical false negatives or refusing 
# care due to zero bed availability).
scenarios_labels <- unique(df_sim_en$Scenario_Label)
df_impact_list <- list()

for (scen in scenarios_labels) {
  sub_df <- df_sim_en %>% filter(Scenario_Label == scen) %>% arrange(Day)
  n_days <- nrow(sub_df)
  
  # Initialize discrete-time state vectors.
  S <- numeric(n_days)
  I <- numeric(n_days)
  R_pool <- numeric(n_days)
  
  Community_Active <- numeric(n_days)
  Daily_Deaths <- numeric(n_days)
  Daily_DALYs <- numeric(n_days)
  
  # Implied R0 based on scenario label string to drive the SIR beta transmission rate.
  current_rt <- as.numeric(gsub('.*Rt = ([0-9.]+).*', '\\1', scen))
  beta_rate <- current_rt * GAMMA_RATE
  
  for (t in 1:n_days) {
    bed_demand_t <- sub_df$Bed_Demand[t]
    occupied_t   <- sub_df$Occupied_Beds[t]
    
    # Deficit represents infectious agents returned to the susceptible community.
    deficit_t    <- max(0, bed_demand_t - BASELINE_ISOLATION_BEDS)
    
    if (t == 1) {
      I[t] <- deficit_t
      S[t] <- N_EFF_BASELINE - I[t]
      R_pool[t] <- 0
      Community_Active[t] <- I[t]
      
      new_infections <- deficit_t
      resolved_cases <- 0
    } else {
      # Standard mass-action incidence mechanics.
      new_infections <- beta_rate * I[t - 1] * (S[t - 1] / N_EFF_BASELINE)
      resolved_cases <- GAMMA_RATE * I[t - 1]
      
      S[t] <- max(0, S[t - 1] - new_infections)
      I[t] <- max(0, I[t - 1] + new_infections - resolved_cases + deficit_t)
      R_pool[t] <- min(N_EFF_BASELINE, R_pool[t - 1] + resolved_cases)
      
      Community_Active[t] <- I[t]
    }
    
    resolved_hospital_cases <- occupied_t * GAMMA_RATE
    
    # Global Burden of Disease (GBD) Metrics:
    # Mortality applies base CFR to isolated patients, and Crisis CFR to community spillovers.
    Daily_Deaths[t] <- (resolved_hospital_cases * CFR_BASE) + (resolved_cases * CFR_CRISIS)
    
    # DALYs = Years of Life Lost (YLL) + Years Lived with Disability (YLD/Morbidity).
    Daily_DALYs[t]  <- (Daily_Deaths[t] * YLL_PER_DEATH) + ((resolved_hospital_cases + new_infections) * DALY_MORBIDITY)
  }
  
  sub_df$Community_Infections <- Community_Active
  sub_df$Daily_Deaths         <- Daily_Deaths
  sub_df$Daily_DALYs          <- Daily_DALYs
  sub_df$Cumulative_Deaths    <- cumsum(Daily_Deaths)
  sub_df$Cumulative_DALYs     <- cumsum(Daily_DALYs)
  
  df_impact_list[[scen]] <- sub_df
}

df_impact <- bind_rows(df_impact_list)

# =========================================================================
# 2. EVALUATION METRICS & PAIRWISE INTERACTION MATRICES ####
# =========================================================================
# Consolidating global macroeconomic epidemiological impact reports and 
# extracting cross-scenario comparative indices to evaluate policy efficacy.
impact_summary <- df_impact %>%
  group_by(Scenario_Label) %>%
  summarise(
    Total_Inferred_Unattended = round(sum(pmax(0, Bed_Demand - BASELINE_ISOLATION_BEDS))),
    Estimated_Deaths          = round(max(Cumulative_Deaths)),
    Estimated_DALYs           = round(max(Cumulative_DALYs))
  ) %>%
  arrange(desc(Estimated_Deaths))

if (!dir.exists('reports/results')) dir.create('reports/results', recursive = TRUE)
write.csv(impact_summary, 'reports/results/epidemiological_inference_summary.csv', row.names = FALSE)

d_neg <- impact_summary$Estimated_Deaths[grepl('3.5', impact_summary$Scenario_Label)]
d_std <- impact_summary$Estimated_Deaths[grepl('2.0', impact_summary$Scenario_Label)]
d_ada <- impact_summary$Estimated_Deaths[grepl('1.1', impact_summary$Scenario_Label)]

dal_neg <- impact_summary$Estimated_DALYs[grepl('3.5', impact_summary$Scenario_Label)]
dal_std <- impact_summary$Estimated_DALYs[grepl('2.0', impact_summary$Scenario_Label)]
dal_ada <- impact_summary$Estimated_DALYs[grepl('1.1', impact_summary$Scenario_Label)]

pairwise_comparison <- data.frame(
  Comparison = c('Adaptive (1.1) vs Collective Negligence (3.5)', 'Adaptive (1.1) vs Standard Campaign (2.0)', 'Standard Campaign (2.0) vs Collective Negligence (3.5)'),
  Lives_Saved             = c(d_neg - d_ada, d_std - d_ada, d_neg - d_std),
  Mortality_Reduction_Pct = round(c((d_neg - d_ada)/d_neg * 100, (d_std - d_ada)/d_std * 100, (d_neg - d_std)/d_neg * 100), 2),
  DALYs_Averted           = c(dal_neg - dal_ada, dal_std - dal_ada, dal_neg - dal_std),
  DALY_Reduction_Pct      = round(c((dal_neg - dal_ada)/dal_neg * 100, (dal_std - dal_ada)/dal_std * 100, (dal_neg - dal_std)/dal_neg * 100), 2)
)

write.csv(pairwise_comparison, 'reports/results/epidemiological_pairwise_efficacy.csv', row.names = FALSE)

# =========================================================================
# 3. HIGH-DIMENSIONAL PROGRESSION VISUALIZATION (LOGARITHMIC FACETING) ####
# =========================================================================
# Generating publication-ready facetted charts demonstrating tipping-point 
# mechanics and logarithmic growth functions post-saturation.
df_impact_long <- df_impact %>%
  select(Day, Scenario_Label, Cumulative_Deaths, Cumulative_DALYs) %>%
  pivot_longer(cols = c(Cumulative_Deaths, Cumulative_DALYs), names_to = 'Metric', values_to = 'Value') %>%
  mutate(
    Value = pmax(Value, 1), 
    Metric_Label = ifelse(Metric == 'Cumulative_Deaths', 'A. Cumulative Mortality (Log10 Scale)', 'B. Cumulative DALYs (Log10 Scale)'),
    Metric_Label = factor(Metric_Label, levels = c('A. Cumulative Mortality (Log10 Scale)', 'B. Cumulative DALYs (Log10 Scale)'))
  )

plot_impact_time <- ggplot(df_impact_long, aes(x = Day, y = Value, color = Scenario_Label)) +
  geom_line(linewidth = 1.3) + facet_wrap(~ Metric_Label, scales = 'free_y', ncol = 1) +
  scale_color_manual(values = c('Collective Negligence (Rt = 3.5)' = '#8B0000', 'Standard Campaign (Rt = 2.0)' = '#E67E22', 'Adaptive Control (Rt = 1.1)' = '#2E4053')) +
  scale_y_log10(labels = scales::label_number(scale_cut = scales::cut_short_scale()), breaks = scales::trans_breaks('log10', function(x) 10^x)) +
  annotation_logticks(sides = 'l', color = 'gray70') +
  labs(title = 'Epidemiological Progression: Autonomous Community Outbreak', x = 'Simulation Horizon (Days)', y = 'Cumulative Societal Deficit (Log10 Scale)', color = 'Scenario:') + 
  theme_minimal() + theme(legend.position = 'bottom', panel.spacing = unit(1, 'lines'))

if (!dir.exists('reports/figures')) dir.create('reports/figures', recursive = TRUE)
ggsave('reports/figures/epidemiological_impact_timeseries_log.png', plot = plot_impact_time, width = 10, height = 8, dpi = 300)

# =========================================================================
# 4. MACROECONOMIC SENSITIVITY & WHO-CHOICE BURDEN TABLE ####
# =========================================================================
# Translating DALYs into absolute financial loss leveraging the WHO-CHOICE 
# Value of Statistical Life (VSL) index, anchored to national GDP markers.
df_economics <- impact_summary %>%
  mutate(
    Expected_Loss_BUSD = (Estimated_DALYs * (GDP_PER_CAPITA_MEXICO * VSL_EXPECTED)) / 1e9,
    Optimistic_Loss_BUSD = ((Estimated_DALYs * DALY_OPT_MULTIPLIER) * (GDP_PER_CAPITA_MEXICO * VSL_OPTIMISTIC)) / 1e9,
    Pessimistic_Loss_BUSD = ((Estimated_DALYs * DALY_PES_MULTIPLIER) * (GDP_PER_CAPITA_MEXICO * VSL_PESSIMISTIC)) / 1e9
  ) %>%
  arrange(desc(Expected_Loss_BUSD))

baseline_expected <- max(df_economics$Expected_Loss_BUSD)
baseline_optimistic <- max(df_economics$Optimistic_Loss_BUSD)
baseline_pessimistic <- max(df_economics$Pessimistic_Loss_BUSD)

df_economics_sensitivity <- df_economics %>%
  mutate(Expected_Savings_BUSD = baseline_expected - Expected_Loss_BUSD, Optimistic_Savings_BUSD = baseline_optimistic - Optimistic_Loss_BUSD, Pessimistic_Savings_BUSD = baseline_pessimistic - Pessimistic_Loss_BUSD) %>%
  select(Scenario_Label, Expected_Loss_BUSD, Optimistic_Loss_BUSD, Pessimistic_Loss_BUSD, Expected_Savings_BUSD, Optimistic_Savings_BUSD, Pessimistic_Savings_BUSD)

write.csv(df_economics_sensitivity, 'reports/results/macroeconomic_sensitivity_matrix.csv', row.names = FALSE)

# Functional programmatic engine to generate continuous macroeconomic topology.
calc_macro_impact_grid <- function(n_val, rt_val, cfr_crisis_val, vsl_val) {
  peak_I <- n_val * max(0, (1 - (1/rt_val))) 
  total_deaths <- peak_I * cfr_crisis_val
  total_dalys <- (total_deaths * YLL_PER_DEATH) + (peak_I * DALY_MORBIDITY)
  loss_busd <- (total_dalys * (GDP_PER_CAPITA_MEXICO * vsl_val)) / 1e9
  return(loss_busd)
}

sensitivity_grid <- expand_grid(
  N_EFF_Scenario = c(2500000, 5000000, 7500000), 
  Rt = seq(1.1, 3.5, by = 0.4),
  CFR_Crisis = seq(0.50, 0.85, by = 0.05),
  VSL = c(VSL_OPTIMISTIC, VSL_EXPECTED, VSL_PESSIMISTIC)
) %>% mutate(Economic_Loss_BUSD = pmap_dbl(list(N_EFF_Scenario, Rt, CFR_Crisis, VSL), calc_macro_impact_grid))

write.csv(sensitivity_grid, 'reports/results/multivariable_policy_sensitivity.csv', row.names = FALSE)

# =========================================================================
# 5. VISUALIZATION: ABSOLUTE ECONOMIC BURDEN LILLIPOP CHART ####
# =========================================================================
df_plot_econ <- df_economics_sensitivity %>% mutate(Label_With_Range = paste0(Scenario_Label, '\n[VSL Range: $', format(round(Optimistic_Loss_BUSD, 1), big.mark = ',', trim = TRUE), 'B - $', format(round(Pessimistic_Loss_BUSD, 1), big.mark = ',', trim = TRUE), 'B]'))
max_x_boundary <- max(df_plot_econ$Pessimistic_Loss_BUSD) * 1.03

plot_econ <- ggplot(df_plot_econ, aes(y = reorder(Label_With_Range, Expected_Loss_BUSD))) +
  geom_vline(xintercept = baseline_expected, linetype = 'dotted', color = 'gray50', linewidth = 0.8) +
  geom_segment(aes(x = Optimistic_Loss_BUSD, xend = Pessimistic_Loss_BUSD, yend = reorder(Label_With_Range, Expected_Loss_BUSD), color = Scenario_Label), linewidth = 7, alpha = 0.22) +
  geom_segment(aes(x = 0, xend = Expected_Loss_BUSD, yend = reorder(Label_With_Range, Expected_Loss_BUSD)), color = 'gray60', linewidth = 1.1) +
  geom_point(aes(x = Expected_Loss_BUSD, color = Scenario_Label), size = 9) +
  geom_text(aes(x = Expected_Loss_BUSD, label = paste0('$', format(round(Expected_Loss_BUSD, 2), big.mark = ',', trim = TRUE), ' B')), vjust = -2.0, fontface = 'bold', size = 4.2, color = '#2E4053') +
  geom_segment(data = filter(df_plot_econ, Expected_Savings_BUSD > 0), aes(x = baseline_expected, xend = Expected_Loss_BUSD + (max_x_boundary * 0.02), yend = reorder(Label_With_Range, Expected_Loss_BUSD)), arrow = arrow(length = unit(0.25, 'cm'), ends = 'last', type = 'closed'), color = '#27AE60', linewidth = 0.9, linejoin = 'mitre') +
  geom_label(data = filter(df_plot_econ, Expected_Savings_BUSD > 0), aes(x = baseline_expected - (Expected_Savings_BUSD / 2), label = paste0('Averted Burden: $', format(round(Expected_Savings_BUSD, 2), big.mark = ','), ' B')), color = 'white', fill = '#27AE60', fontface = 'bold', size = 3.8, vjust = 1.6, label.r = unit(0.12, 'lines')) +
  scale_color_manual(values = c('Collective Negligence (Rt = 3.5)' = '#8B0000', 'Standard Campaign (Rt = 2.0)' = '#E67E22', 'Adaptive Control (Rt = 1.1)' = '#2E4053')) +
  scale_x_continuous(labels = scales::dollar_format(suffix = ' B'), limits = c(0, max_x_boundary), expand = expansion(mult = c(0.05, 0.05))) + 
  labs(title = stringr::str_wrap('Macroeconomic Burden: Expected Cost & Savings of Triage Interventions', width = 60), x = 'Absolute Economic Loss (Billions USD)', y = '') + theme_minimal() + theme(legend.position = 'bottom')

ggsave('reports/figures/who_choice_economic_absolute_savings.png', plot = plot_econ, width = 11, height = 6.5, dpi = 300)

# =========================================================================
# 6. VISUALIZATION: MULTIVARIABLE POLICY SENSITIVITY FACETED GRAPH ####
# =========================================================================
df_plot_sens <- sensitivity_grid %>% filter(N_EFF_Scenario == 5000000)

plot_sens_grid <- ggplot(df_plot_sens, aes(x = Rt, y = Economic_Loss_BUSD, color = factor(CFR_Crisis))) +
  geom_line(linewidth = 1.2) + geom_point(size = 2) + facet_wrap(~ paste('VSL Multiplier:', VSL), scales = 'free_y', ncol = 1) +
  scale_color_viridis_d(option = 'plasma', name = 'Crisis CFR:') + scale_y_continuous(labels = scales::dollar_format(suffix = ' B')) +
  labs(title = 'Macroeconomic Sensitivity Landscape: Policy vs Pathogen Dynamics', x = 'Effective Reproduction Number (Rt)', y = 'Projected Absolute Economic Loss (Billions USD)') +
  theme_minimal() + theme(legend.position = 'bottom', panel.spacing = unit(1, 'lines'))

ggsave('reports/figures/multivariable_macro_sensitivity_grid.png', plot = plot_sens_grid, width = 10, height = 9, dpi = 300)

# =========================================================================
# 7. VISUALIZATION: EXPOSED POPULATION (N) SENSITIVITY CHART ####
# =========================================================================
df_pop_sens <- sensitivity_grid %>% filter(VSL == 3, CFR_Crisis == 0.70) %>% mutate(N_Factor = factor(N_EFF_Scenario, levels = c(2500000, 5000000, 7500000), labels = c('2.5M (High Containment)', '5.0M (Baseline)', '7.5M (Extended Spillover)')))

plot_pop <- ggplot(df_pop_sens, aes(x = factor(Rt), y = Economic_Loss_BUSD, fill = N_Factor)) +
  geom_col(position = 'dodge', color = 'black', alpha = 0.85) +
  scale_fill_manual(name = 'Susceptible Population (N)', values = c('2.5M (High Containment)' = '#2E4053', '5.0M (Baseline)' = '#E67E22', '7.5M (Extended Spillover)' = '#8B0000')) +
  scale_y_continuous(labels = scales::dollar_format(suffix = ' B')) +
  labs(title = 'Demographic Impact on Macroeconomic Burden', x = 'Effective Reproduction Number (Rt)', y = 'Expected Economic Loss (Billions USD)') + theme_minimal() + theme(legend.position = 'bottom')

ggsave('reports/figures/population_sensitivity.png', plot = plot_pop, width = 9, height = 7, dpi = 300)

# Data for Shiny App.
write.csv(impact_summary, 'epitriage-dashboard/data/epidemiological_inference_summary.csv', row.names = FALSE)
write.csv(pairwise_comparison, 'epitriage-dashboard/data/epidemiological_pairwise_efficacy.csv', row.names = FALSE)
write.csv(df_economics_sensitivity, 'epitriage-dashboard/data/macroeconomic_sensitivity_matrix.csv', row.names = FALSE)
write.csv(sensitivity_grid, 'epitriage-dashboard/data/multivariable_policy_sensitivity.csv', row.names = FALSE)
saveRDS(plot_impact_time, 'epitriage-dashboard/data/plot_impact_time.rds')
saveRDS(plot_pop, 'epitriage-dashboard/data/plot_pop.rds')
ggsave('epitriage-dashboard/www/multivariable_macro_sensitivity_grid.png', plot = plot_sens_grid, width = 16, height = 8, dpi = 300)
plot_econ_shiny <- plot_econ + labs(title = NULL, subtitle = NULL) + theme(text = element_text(size = 20), legend.position = 'none')
ggsave('epitriage-dashboard/www/who_choice_economic_absolute_savings.png', plot = plot_econ_shiny, width = 14, height = 8, dpi = 300)

cat('\n[SUCCESS] Script 05: Macro-Impact Analysis, Multi-Sensitivity & Visualizations Complete.\n')
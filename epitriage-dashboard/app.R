#' @title SHINY DASHBOARD: EPIDEMIOLOGICAL TRIAGE FIFA WORLD CUP 2026
#' @concept EXECUTIVE DECISION SUPPORT SYSTEM & POLICY SIMULATOR
#' @author RODRIGO ABEL DE CARCER GANDARILLA
# =========================================================================
# DESCRIPTION: Production-grade Interactive Policy Simulator for the 2026
# FIFA World Cup mass-gathering event. This dashboard integrates the 
# outputs of the machine learning triage algorithm and the macroeconomic 
# SIR spillover models, providing real-time sensitivity analysis for 
# hospital saturation and WHO-CHOICE economic burden projections.
# =========================================================================

if (!require('pacman')) install.packages('pacman')
pacman::p_load(shiny, shinydashboard, dplyr, readr, DT, ggplot2, plotly)

# =========================================================================
# 0. INITIALIZATION & CONFIG
# =========================================================================
# Global environment standardization ensuring the dashboard utilizes the 
# exact mathematical constants established during backend model training.
if (file.exists('../src/config.R')) {
  source('../src/config.R', echo = FALSE)
} else {
  # Secure fallback baseline constants for cloud container deployments.
  GLOBAL_SEED <- 20260526
  SIMULATION_HORIZON_DAYS <- 60
  SURGE_PENALTY <- 3.5
  NOISE_SENSITIVITY <- 0.04
}

# =========================================================================
# 1. GLOBAL DATA INGESTION
# =========================================================================
# 1.1 Import pre-computed epidemiological matrices and efficacy tables.
df_epi_summary    <- read_csv('data/epidemiological_inference_summary.csv', show_col_types = FALSE)
df_pairwise       <- read_csv('data/epidemiological_pairwise_efficacy.csv', show_col_types = FALSE)
df_macro_sens     <- read_csv('data/macroeconomic_sensitivity_matrix.csv', show_col_types = FALSE)
df_multi_policy   <- read_csv('data/multivariable_policy_sensitivity.csv', show_col_types = FALSE)

# 1.2 Import serialized living ggplot objects for interactive rendering.
p_saturation  <- readRDS('data/plot_saturation.rds')
p_impact_time <- readRDS('data/plot_impact_time.rds')
p_pop         <- readRDS('data/plot_pop.rds')

# 1.3 Extract static KPI baseline vectors for the Executive Summary view.
lives_saved_max <- df_pairwise %>% filter(grepl('Adaptive', Comparison) & grepl('Negligence', Comparison)) %>% pull(Lives_Saved)
econ_saved_max  <- df_macro_sens %>% filter(grepl('1.1', Scenario_Label)) %>% pull(Expected_Savings_BUSD)
unattended_std  <- df_epi_summary %>% filter(grepl('2.0', Scenario_Label)) %>% pull(Total_Inferred_Unattended)

# 1.4 Operational dataset ingestion and stochastic configuration.
df_triage_op      <- read_csv('data/dataset_operational_triage.csv', show_col_types = FALSE)
df_triage_op$is_ebola <- factor(df_triage_op$is_ebola, levels = c('Ebola', 'Influenza'))
df_triage_op$is_ebola_num <- ifelse(df_triage_op$is_ebola == 'Ebola', 1, 0)

# Staggered Patient Arrivals: Beta distribution generating the stochastic acute surge profile.
set.seed(GLOBAL_SEED)
total_patients <- nrow(df_triage_op)
stochastic_arrivals <- rbeta(total_patients, shape1 = 2, shape2 = 3.5)
df_triage_op$arrival_day <- round(stochastic_arrivals * (SIMULATION_HORIZON_DAYS - 1)) + 1

# Core simulation engine function: Executed reactively by user inputs.
simulate_triage <- function(scenario_name, current_Rt, max_isolation_beds, base_conservatism) {
  
  # Bed registry tracking (stores discharge days).
  bed_registry <- data.frame(discharge_day = numeric(0))
  history_bed_demand <- numeric(SIMULATION_HORIZON_DAYS)
  deferred_outbreaks <- numeric(SIMULATION_HORIZON_DAYS + 35)
  
  for (t in 1:SIMULATION_HORIZON_DAYS) {
    # 1. Bed clearance: Retain only patients whose discharge day is strictly after the current day.
    if (nrow(bed_registry) > 0) {
      bed_registry <- bed_registry[bed_registry$discharge_day > t, , drop = FALSE]
    }
    
    # 2. Cohort analysis and dynamic threshold calculation.
    cohort_today <- df_triage_op[df_triage_op$arrival_day == t, ]
    num_influenza_today <- sum(cohort_today$is_ebola_num == 0)
    
    # Calculate the operational log-odds threshold based on real-time occupancy and conservatism.
    occupancy_rate <- nrow(bed_registry) / max_isolation_beds
    log_odds_threshold <- base_conservatism + (SURGE_PENALTY * occupancy_rate) + (NOISE_SENSITIVITY * num_influenza_today)
    p_optimal <- 1 / (1 + exp(-log_odds_threshold))
    
    # 3. Clinical triage and resource allocation.
    if (nrow(cohort_today) > 0) {
      pred_ebola <- ifelse(cohort_today$prob_pred >= p_optimal, 1, 0)
      
      # True Positives: Correctly identified, bed allocated.
      tp_cases <- cohort_today[cohort_today$is_ebola_num == 1 & pred_ebola == 1, ]
      if (nrow(tp_cases) > 0) {
        new_admissions <- data.frame(discharge_day = t + tp_cases$hospital_stay_days)
        bed_registry <- rbind(bed_registry, new_admissions)
      }
      
      # False Negatives: Fatal clinical misses prematurely discharged into the community.
      fp_cases <- cohort_today[cohort_today$is_ebola_num == 1 & pred_ebola == 0, ]
      if (nrow(fp_cases) > 0) {
        deferred_outbreaks[t + 5] <- deferred_outbreaks[t + 5] + round(nrow(fp_cases) * current_Rt * 0.1)
      }
    }
    
    # 4. Total demand tracking: Actual hospital occupancy plus deferred community outbreaks.
    history_bed_demand[t] <- nrow(bed_registry) + deferred_outbreaks[t]
  }
  
  tibble(Day = 1:SIMULATION_HORIZON_DAYS, Bed_Demand = history_bed_demand, Scenario = scenario_name)
}

# =========================================================================
# 2. USER INTERFACE (UI)
# =========================================================================
# Application frontend built on shinydashboard with encapsulated CSS constraints.
ui <- dashboardPage(
  skin = 'black',
  
  dashboardHeader(
    title = tags$span(
      style = "display: flex; align-items: center; margin-left: -2px;",
      tags$img(src = 'GH_FBlack.svg', height = '35px', style = "margin-right: 10px; margin-bottom: 2px;"),
      "EpiTriage 2026: Mexico"
    ),
    titleWidth = 330,
    tags$li(class = 'dropdown', tags$a(href = 'https://github.com/rdecarcg/msc-triage-ebola-influenza-fifawc26-mexico', icon('github'), ' GitHub', target = '_blank', style = 'color: #333 !important; font-weight: bold;')),
    tags$li(class = 'dropdown', tags$a(href = 'https://doi.org/10.5281/zenodo.20600419', icon('database'), ' Zenodo: 10.5281/zenodo.20600419', target = '_blank', style = 'color: #333 !important; font-weight: bold;')),
    tags$li(class = 'dropdown', tags$a(href = 'https://medrxiv.org', icon('file-medical'), ' medRxiv', target = '_blank', style = 'color: #333 !important; font-weight: bold;')),
    tags$li(class = 'dropdown', tags$a(href = '#', icon('balance-scale'), ' License: MIT', style = 'color: #333 !important; font-weight: bold; pointer-events: none;')),
    tags$li(class = 'dropdown',
            tags$a(href = '#', 'Author: Rodrigo De Cárcer (Independent Researcher) | June 2026', 
                   style = 'font-weight: bold; pointer-events: none; color: gray;')
    )
  ),
  
  dashboardSidebar(
    width = 300,
    sidebarMenu(
      menuItem('Executive Summary', tabName = 'executive', icon = icon('globe')),
      menuItem('Interactive Policy Simulator', tabName = 'simulator', icon = icon('sliders-h')),
      menuItem('Operational Triage (Micro)', tabName = 'operations', icon = icon('hospital')),
      menuItem('Macroeconomic Impact', tabName = 'macro', icon = icon('chart-line')),
      menuItem('Clinical & ML Diagnostics', tabName = 'clinical', icon = icon('microscope')),
      menuItem('Raw Data Explorer', tabName = 'data_explorer', icon = icon('database'))
    )
  ),
  
  dashboardBody(
    tags$head(tags$style(HTML('
  .main-header { position: fixed !important; width: 100%; top: 0; z-index: 9999; }
  .wrapper { margin-top: 100px !important; }
  .main-sidebar { position: fixed; top: 50px; width: 100% !important; height: 50px !important; min-height: 50px !important; max-height: 50px !important; padding-top: 0 !important; z-index: 1000; overflow: hidden; }
  .sidebar-menu { display: flex; flex-direction: row; width: 100%; overflow: visible; }
  .sidebar-menu > li { width: auto; }
  .sidebar-toggle { display: none !important; }
  .wrapper { height: auto !important; min-height: 100% !important; overflow-x: hidden; }
  .content-wrapper { margin-left: 0 !important; min-height: calc(100vh - 100px) !important; background-color: #ecf0f5; }
  .row { display: -webkit-flex; display: -ms-flexbox; display: flex; flex-wrap: wrap; }
  .box { display: flex; flex-direction: column; height: calc(100% - 20px); margin-bottom: 20px;}
  .box-body { flex-grow: 1; display: flex; flex-direction: column; justify-content: center; }
  .img-responsive { max-width: 100%; height: auto; display: block; margin: auto; }
'))),
    
    tabItems(
      tabItem(tabName = 'executive',
              fluidRow(
                valueBox('14.08%', 'Poisson Probability of Joint Outbreak', icon = icon('dice'), color = 'purple'),
                valueBox(paste0(format(lives_saved_max, big.mark = ','), ' Lives'), 
                         'Maximum Potential Lives Saved', icon = icon('heartbeat'), color = 'green'),
                valueBox(paste0('$', format(round(econ_saved_max, 1), big.mark = ','), ' B'), 
                         'Economic Burden Averted (Expected USD)', icon = icon('money-bill-wave'), color = 'aqua')
              ),
              fluidRow(
                box(title = 'Scenario Definitions: Modeling Behavioral & Policy Interventions', status = 'info', solidHeader = TRUE, width = 12,
                    HTML('
                    <div style="display: flex; gap: 20px;">
                      <div style="flex: 1;">
                        <h5 style="color: #8B0000; font-weight: bold;">Collective Negligence (Rt = 3.5)</h5>
                        <p>Represents a worst-case baseline where no extraordinary public health measures are enacted. Mass-gatherings continue unimpeded, masking compliance is zero, and the healthcare system operates under standard daily protocols until total collapse.<br><br>In other words, it means doing nothing.</p>
                      </div>
                      <div style="flex: 1; border-left: 2px solid #ccc; border-right: 2px solid #ccc; padding: 0 15px;">
                        <h5 style="color: #E67E22; font-weight: bold;">Standard Campaign (Rt = 2.0)</h5>
                        <p><b>The Reactive Baseline:</b> Mirrors traditional, reactive mitigation protocols seen in early 2020 (COVID-19) or the 2009 H1N1 outbreaks. It includes basic public awareness campaigns (e.g., handwashing posters), voluntary social distancing, and delayed isolation only <i>after</i> clinical outbreaks are widespread. It heavily relies on syndromic screening (fever checks) which critically fails against pathogens with overlapping prodromes or silent incubation periods.</p>
                      </div>
                      <div style="flex: 1;">
                        <h5 style="color: #2E4053; font-weight: bold;">Adaptive Control (Rt = 1.1)</h5>
                        <p>The optimal target scenario. Achieved strictly through the aggressive, pre-emptive deployment of the stratified policy recommendations outlined below, including ML-driven triage (tolerating false positive isolations), AIIR bed expansion <i>prior</i> to collapse, and stringent enclosed transit protection.</p>
                      </div>
                    </div>
                    ')
                )
              ),
              fluidRow(
                box(title = 'Long-Term Societal Deficits (Log10)', status = 'primary', solidHeader = TRUE, width = 6,
                    plotlyOutput('plotly_impact_time', height = '450px')), 
                box(title = 'Macroeconomic Absolute Savings', status = 'success', solidHeader = TRUE, width = 6,
                    tags$img(src = 'who_choice_economic_absolute_savings.png', class = 'img-responsive', style='max-height: 450px;')) 
              ),
              fluidRow(
                box(title = 'Context, Assumptions & Methodological Plausibility', status = 'warning', width = 6, solidHeader = TRUE,
                    HTML('
                    <p><b>The Core Problem & Mobility:</b> The 2026 FIFA World Cup will trigger unprecedented global mobility. Although matches are shared across North America, Mexico\'s host cities, tourist corridors, and mass-gathering sites (e.g., official Fan Fests, Ángel de la Independencia celebrations) will maintain extreme density throughout the entire event and its subsequent tourist phase. Benchmarked against historical FIFA milestones—where Russia 2018 and Qatar 2022 generated massive cross-border clusters—Mexico anticipates a single cohesive wave of <u>1.5M international visitors</u>.<br> On one hand, this includes an immediate influx from the Southern Hemisphere (e.g., Argentina, Brazil) traveling directly during their active winter season. This introduces a severe epidemiological risk due to the hemispheric discrepancy in seasonal vaccination campaigns, leaving travelers highly vulnerable to adaptive respiratory strains like H5N1, for which global proactive vaccine deployment remains critically limited.<br> On the other hand, BDBV could be covertly imported by individuals within the <u>21-day silent incubation period</u> explicitly defined by WHO guidelines. These vectors can originate directly from active PHEIC zones in Central/East Africa (e.g., DRC, Uganda) or contract the pathogen via secondary exposure chains within congested international airport transit hubs across Europe and North America before entry.</p>
                    <p><b>Pathogen Mutation & Generalizability:</b> While H5N1 currently shows limited human-to-human (H2H) spread, this model stress-tests a worst-case <i>gain-of-function mutation</i> enabling sustained aerosol transmission, mimicking the catastrophic 1918 H1N1 pandemic. Notably, this ML-triage architecture is fully generalizable and can be rapidly redeployed for any future "Disease X" or other mass-gathering events (e.g., the Olympics) by adjusting the underlying parameters.</p>
                    <p><b>Diagnostic Ambiguity & Biosecurity Windows:</b> Ebola Bundibugyo (BDBV) presents a catastrophic biosecurity threat since <u>there is currently no approved vaccine or specific antiviral therapy available</u> for this particular strain. Compounding this, during the first 48 hours of symptom onset, both BDBV and H5N1 present with indistinguishable febrile prodromes (e.g., fever, malaise), making purely syndromic triage highly unreliable and necessitating ML-driven analysis of hematological biomarkers (leukocytes and platelets).</p>
                    <p><b>Demographics, Temporal & Economic Baselines:</b> The <b>60-day simulation</b> brackets the tournament\'s acute phase. The Effective Susceptible Population (<u>N = 5.0M</u>) isolates the tourist influx plus the local service sector. The ML training cohort (<u>N = 165,500</u>) captures a statistically robust ~11% high-risk sample frame. Economic impact utilizes a baseline GDP per capita of <u>$13,800 USD</u> (IMF 2026 projections). For context, utilizing this same WHO-CHOICE VSL framework, the COVID-19 pandemic\'s excess mortality in Mexico equates to nearly <u>$1.0 Trillion USD</u> in lost statistical life value; this system aims to preemptively avert multi-billion dollar VSL losses during a localized surge.</p>
                    <p><b>Evaluating Quarantines & Adaptive Capacity:</b> Unjustified societal lockdowns cause devastating economic paralysis and human rights friction. Sweeping quarantines should only be evaluated as a last resort. They remain mathematically and ethically unjustified <i>until</i> the Adaptive Surge Capacity (initially modeled at the 350-bed limit for emergency-retrofitted AIIR units across CDMX, GDL, MTY, but dynamically expandable if required) and Crisis Standards of Care are absolutely exhausted. This dashboard identifies the exact tipping point where such extreme measures must be considered.</p>
                    ')
                ),
                box(title = 'Stratified Policy Recommendations (Target Rt = 1.1)', status = 'success', width = 6, solidHeader = TRUE,
                    HTML('
                    <p>To achieve the "Adaptive Control" scenario, interventions must be stratified by sector and prioritized by their critical impact on breaking transmission chains:</p>

                    <h5 style="color: #8B0000; font-weight: bold; margin-top: 15px;">I. Government & Public Health (Systemic Level)</h5>
                    <ul style="margin-bottom: 10px; padding-left: 20px;">
                      <li><b>CSC Legal Activation:</b> Executive authorization for hospitals to deploy ML-based triage (tolerating high False Positive isolation rates) without liability for standard care delays.</li>
                      <li><b>Dynamic Bed Expansion:</b> Proactively retrofitting additional AIIR capacity the moment existing isolation unit occupancy reaches a 70% threshold, ensuring infrastructure stays ahead of exponential surge trends.</li>
                      <li><b>Preemptive Vaccination Campaign:</b> Triggering an extraordinary, proactive H5N1 vaccination protocol for frontline personnel and high-risk demographics immediately upon localized early-warning detection (e.g., wastewater signals), strictly prior to widespread clinical outbreaks and avoiding reactive delays.</li>
                      <li><b>Strategic MCM Stockpiling & HCW Protection:</b> Pre-positioning of Medical Countermeasures (BSL-4 PPE, Intravenous fluids for BDBV, and Oseltamivir for H5N1) strictly within host cities. Guaranteeing rigorous biosecurity training and adequate specialized PPE allocation for all healthcare workers to prevent catastrophic nosocomial infections.</li>
                    </ul>

                    <h5 style="color: #E67E22; font-weight: bold;">II. Institutional & Venue Operations (Tactical Level)</h5>
                    <ul style="margin-bottom: 10px; padding-left: 20px;">
                      <li><b>Decentralized Pre-Hospital Triage:</b> Deploying mobile screening units at stadiums, Fan Fests and other mass-gathering sites to intercept suspected cases and prevent nosocomial amplification in standard ERs.</li>
                      <li><b>Facility Airflow & Screening Mandates:</b> Enforcing MERV-13 or HEPA filtration standards in high-density indoor stadium zones (VIP lounges, media centers) to mitigate aerosolized H5N1 transmission. Implementing mandatory non-contact temperature screening at all venue and workplace access points.</li>
                      <li><b>Workplace & Office Density:</b> Implementing temporary remote or hybrid work mandates for non-essential corporate and on-site office sectors across host cities to drastically reduce metropolitan transit loads and mitigate occupational transmission.</li>
                    </ul>

                    <h5 style="color: #2E4053; font-weight: bold;">III. Civilians & Attendees (Behavioral Level)</h5>
                    <ul style="padding-left: 20px;">
                      <li><b>Stringent Fluid & Fomite Hygiene (WHO Guidelines):</b> Absolute prohibition of sharing drinks, food, e-cigarettes, or vapes (critical vectors for BDBV saliva transmission and H5N1). Enforcing rigorous hand washing and avoiding touching mucosal membranes (eyes, nose, mouth).</li>
                      <li><b>Targeted Care-Seeking (Safe ER Transit):</b> Assuming early symptoms are "just the flu" can be fatal. Symptomatic individuals (fever, malaise) must immediately proceed to designated emergency rooms rather than waiting at home. They should utilize N95 masks during transit, avoid crowded public transport if possible, and immediately notify triage staff of their symptom onset to expedite evaluation and bio-secure extraction to designated AIIR facilities.</li>
                      <li><b>Enclosed Transit Protection:</b> Proactive adherence to N95 masking specifically within high-density indoor transit (metros, airport shuttles).</li>
                    </ul>
                    ')
                )
              )
      ),
      
      tabItem(tabName = 'simulator',
              fluidRow(
                box(title = 'Macro Policy & Pathogen Parameters', status = 'warning', width = 4, solidHeader = TRUE,
                    helpText('Adjust levers to calculate real-time epidemiological and economic consequences.'),
                    selectInput('sim_n', 'Susceptible Population (N):',
                                choices = c('2.5M (High Containment)' = 2500000,
                                            '5.0M (Baseline)' = 5000000,
                                            '7.5M (Extended Spillover)' = 7500000), selected = 5000000),
                    sliderInput('sim_rt', 'Effective Reproduction No. (Rt):', min = 1.1, max = 3.5, value = 2.0, step = 0.1),
                    sliderInput('sim_cfr', 'System Collapse Lethality (CFR):', min = 0.50, max = 0.85, value = 0.70, step = 0.05),
                    radioButtons('sim_vsl', 'Value of Statistical Life (VSL):', 
                                 choices = c('Optimistic (x1)' = 1, 'Expected (x3)' = 3, 'Pessimistic (x5)' = 5), selected = 3)
                ),
                box(title = 'Real-Time Macro Impact Calculator', status = 'danger', width = 8, solidHeader = TRUE,
                    fluidRow(
                      valueBoxOutput('dyn_deaths_box', width = 4),
                      valueBoxOutput('dyn_dalys_box', width = 4),
                      valueBoxOutput('dyn_loss_box', width = 4)
                    ),
                    hr(),
                    plotOutput('dynamic_plot', height = '280px')
                )
              ),
              fluidRow(
                box(title = 'Micro-Operational Parameters (Triage)', status = 'warning', width = 4, solidHeader = TRUE,
                    HTML('<p class="help-block">Adjust hospital capacities and clinical triage aggressiveness.<br><br><br><br> Note: The background stochastic arrival curve (patient influx) reflects the pre-computed beta-distribution baseline. Adjusting these sliders triggers the real-time execution of the discrete-time operational simulation to dynamically re-evaluate infrastructure saturation and threshold crossings.<br><br><br><br></p>'),
                    sliderInput('sim_beds', 'Max Isolation Bed Capacity (AIIR):', min = 0, max = 1000, value = 350, step = 50),
                    sliderInput('sim_conservatism', 'Baseline Conservatism Index:', min = -5, max = 5, value = -0.5, step = 0.5)
                ),
                box(title = 'Infrastructure Saturation Dynamics', status = 'danger', width = 8, solidHeader = TRUE,
                    fluidRow(
                      valueBoxOutput('col_rt35_box', width = 4),
                      valueBoxOutput('col_rt20_box', width = 4),
                      valueBoxOutput('col_rt11_box', width = 4)
                    ),
                    hr(),
                    plotlyOutput('plotly_sim_saturation', height = '350px')
                )
              )
      ),
      
      tabItem(tabName = 'operations',
              fluidRow(
                box(title = 'Operational Parameters & Asymmetric Costs', status = 'warning', solidHeader = TRUE, width = 12,
                    HTML('
                    <div style="display: flex; gap: 20px;">
                      <div style="flex: 1;">
                        <b>Why 350 Beds? (Threshold):</b> Native negative-pressure Airborne Infection Isolation Rooms (AIIR) are extremely scarce. This figure represents an Adaptive Surge Capacity, assuming the emergency retrofitting of existing Intensive Care Units (ICUs) using portable HEPA exhaust systems across the host cities. Exceeding this limit signifies the absolute exhaustion of physical space, ventilators, and intensivist personnel.
                      </div>
                      <div style="flex: 1;">
                        <b>Baseline Conservatism Index (-0.5):</b> A log-odds penalty hardcoded into the ML algorithm. By shifting the sigmoid probability curve to the left, it forces the system to operate under "Crisis Standards of Care". This proactively lowers the clinical threshold for isolation, acting as an aggressive safety buffer to admit patients even if their ML probability score is moderately low.
                      </div>
                      <div style="flex: 1;">
                        <b>Type I vs Type II Errors:</b> In epidemiological triage, errors are profoundly asymmetric. A False Positive (Type I) means isolating a patient with Influenza—this merely consumes a wasted bed and resources. A False Negative (Type II) means discharging a highly infectious Ebola patient back into a mass-gathering event. This triggers exponential spillover, catastrophic mortality, and massive macroeconomic collapse.
                      </div>
                    </div>
                    ')
                )
              ),
              fluidRow(
                box(title = 'Infrastructure Saturation Dynamics', status = 'danger', solidHeader = TRUE, width = 12,
                    tags$img(src = 'saturation_scenarios.png', class = 'img-responsive', style='max-height: 600px;'))
              ),
              fluidRow(
                box(title = 'Operational Triage Matrix', status = 'warning', solidHeader = TRUE, width = 5,
                    tags$img(src = 'weighted_confusion_matrix_grid.png', class = 'img-responsive', style='max-height: 450px;')),
                box(title = 'System Collapse Sensitivity Heatmap (Beds vs Policy)', status = 'primary', solidHeader = TRUE, width = 7,
                    tags$img(src = 'sensitivity_heatmap.png', class = 'img-responsive', style='max-height: 450px;')) 
              )
      ),
      
      tabItem(tabName = 'macro',
              fluidRow(
                box(title = 'Macroeconomic Model Assumptions', status = 'warning', solidHeader = TRUE, width = 12,
                    HTML('
                    <div style="display: flex; gap: 20px;">
                      <div style="flex: 1;">
                        <b>Value of Statistical Life (VSL):</b> Following WHO-CHOICE framework guidelines, the VSL is bounded between 1x (Optimistic), 3x (Expected), and 5x (Pessimistic) the GDP per capita. This captures the varying global economic valuations of premature mortality and provides a robust sensitivity band for policymakers.
                      </div>
                      <div style="flex: 1;">
                        <b>Exposed Population (N = 5.0M Baseline):</b> The Effective Susceptible Universe (N_EFF) targets the highly mobile demographic explicitly interacting within the World Cup host cities and immediate tourist corridors. The 2.5M and 7.5M bounds stress-test aggressive early containment vs. extended geographical spillover.
                      </div>
                      <div style="flex: 1;">
                        <b>Disability-Adjusted Life Years (DALYs):</b> A universal metric of overall disease burden, expressed as the number of years lost due to ill-health, disability, or early death. It combines Years of Life Lost (YLL) due to premature mortality and Years Lived with Disability (YLD) for survivors with chronic post-viral sequelae.
                      </div>
                    </div>
                    ')
                )
              ),
              fluidRow(
                box(title = 'Demographic Impact on Macroeconomic Burden', status = 'info', solidHeader = TRUE, width = 12,
                    plotlyOutput('plotly_pop', height = '450px'))
              ),
              fluidRow(
                box(title = 'Multivariable Sensitivity Grid', status = 'info', solidHeader = TRUE, width = 12,
                    tags$img(src = 'multivariable_macro_sensitivity_grid.png', class = 'img-responsive', style='max-height: 1000px;'))
              )
      ),
      
      tabItem(tabName = 'clinical',
              fluidRow(
                box(title = 'Clinical Dataset Provenance & Synthesis', status = 'warning', width = 12, solidHeader = TRUE,
                    HTML('
                    <div style="display: flex; gap: 20px;">
                      <div style="flex: 1;">
                        <b>Synthetic Cohort Formulation:</b> The machine learning models are trained on a robust synthetic cohort (N=165,500).
                      </div>
                      <div style="flex: 1;">
                        <b>The Stacked Meta-Learner (Ridge-RF-XGBoost):</b> This specific combination is optimal for clinical triage.
                      </div>
                      <div style="flex: 1;">
                        <b>Diagnostic Architecture:</b> Stacking these base models via a Meta-Learner optimizes the Precision-Recall Area Under Curve (PR-AUC).
                      </div>
                    </div>
                    ')
                )
              ),
              fluidRow(
                box(title = 'Hematological & Latency Separation', status = 'primary', solidHeader = TRUE, width = 12,
                    tags$img(src = 'eda_biomarkers_and_latency.png', class = 'img-responsive', style='max-height: 600px;'))
              ),
              fluidRow(
                box(title = 'Pathogen Dynamics: Hematological & Chronological Observations', status = 'warning', solidHeader = TRUE, width = 12,
                    HTML('
                    <p><b>Hematological Separation (Platelets vs WBC):</b> The bivariate density distributions reveal distinct physiological signatures. Ebola Bundibugyo (BDBV) induces profound thrombocytopenia (dangerously low platelets) and leukopenia (low white blood cells) characteristic of severe viral hemorrhagic fevers, tightly clustering in the lower-left quadrant. Conversely, Influenza H5N1 presents with relatively higher counts, reflecting a different immune response and cytokine cascade. This distinct spatial separation validates these biomarkers as highly discriminative features for the ML model.</p>
                    <p><b>Chronological Divergence (Latency):</b> The incubation period distributions further differentiate the pathogens. H5N1 exhibits rapid onset kinetics (peaking within 2-5 days), typical of severe respiratory viruses. BDBV displays a prolonged, right-skewed latency tail (extending up to 21 days, aligning strictly with WHO official parameters). This chronobiological divergence makes the <code>incubation_days</code> variable a critical primary splitting node for the decision tree algorithms.</p>
                    ')
                )
              ),
              fluidRow(
                box(title = 'Biomarker Correlation Matrix', status = 'primary', solidHeader = TRUE, width = 6,
                    tags$img(src = 'correlation_matrix.png', class = 'img-responsive', style='max-height: 450px;')),
                box(title = 'Ensemble Meta-Learner PR-Curve', status = 'primary', solidHeader = TRUE, width = 6,
                    tags$img(src = 'pr_curve_stacked_test.png', class = 'img-responsive', style='max-height: 450px;'))
              ),
              fluidRow(
                box(title = 'Algorithmic Architecture: The Stacked Generalization Meta-Learner', status = 'success', solidHeader = TRUE, width = 12,
                    HTML('
                    <p><b>Composition (Ridge + RF + XGBoost):</b> Our predictive engine eschews single-algorithm reliance in favor of a Stacked Generalization ensemble. It integrates three orthogonal learning strategies:</p>
                    <ul style="margin-bottom: 10px;">
                      <li><b>Regularized Logistic Regression (Ridge):</b> Applies L2 penalization to prevent coefficient inflation, capturing strict linear decision boundaries and establishing a baseline probability.</li>
                      <li><b>Random Forest (RF):</b> Utilizes bootstrap aggregated decision trees to capture complex, non-linear physiological feature interactions (e.g., specific platelet-to-WBC ratios) while naturally resisting overfitting.</li>
                      <li><b>Extreme Gradient Boosting (XGBoost):</b> Executes sequential gradient descent to aggressively correct residual errors from the weak prediction zones left by the previous models.</li>
                    </ul>
                    <p><b>Why this is the chosen solution:</b> In epidemiological triage, the cost matrix is profoundly imbalanced. As established, a False Negative (missing Ebola) is catastrophic, while a False Positive (isolating Influenza) merely consumes a bed. By fusing these diverse models and explicitly optimizing the Meta-Learner for the <b>Precision-Recall Area Under Curve (PR-AUC)</b> instead of standard accuracy, the architecture maximizes sensitivity for the minority class (BDBV) under extreme field-laboratory noise. This guarantees superior out-of-sample robustness and strictly minimizes fatal clinical misses.</p>
                    ')
                )
              )
      ),
      
      tabItem(tabName = 'data_explorer',
              fluidRow(
                box(title = 'Export Simulation Datasets', status = 'primary', width = 12,
                    selectInput('data_choice', 'Select Dataset:', 
                                choices = c('Macroeconomic Sensitivity' = 'macro',
                                            'Epidemiological Efficacy (Pairwise)' = 'pairwise',
                                            'Multivariable Policy Sensitivity' = 'multi_policy',
                                            ' Operational Triage Data (Simulated)' = 'triage_op'),
                                selected = 'triage_op'),
                    hr(),
                    DT::dataTableOutput('table_explorer'),
                    hr(),
                    conditionalPanel(
                      condition = "input.data_choice == 'triage_op'",
                      box(title = 'Data Dictionary & Feature Provenance', status = 'info', solidHeader = TRUE, width = 12,
                          HTML('
                          <div style="column-count: 2; column-gap: 40px;">
                            <p><b>patient_id:</b> Primary key ensuring row-level traceability and provenance.</p>
                            <p><b>pathogen:</b> Target Class representing the infectious agent (Influenza_H5N1 vs Ebola_Bundibugyo).</p>
                            <p><b>age:</b> Simulated patient demographic ranging from 16 to 80 years.</p>
                            <p><b>incubation_days:</b> Parametric Gamma-distributed latency period.</p>
                            <p><b>presymptomatic_shedding:</b> Binary flag (1 = contagious prior to symptom onset).</p>
                            <p><b>platelets_count:</b> Hematological biomarker derived from a Log-normal distribution.</p>
                            <p><b>white_blood_cells:</b> Leukocyte quantification derived from a Log-normal distribution.</p>
                            <p><b>hospital_stay_days:</b> Conditional length-of-stay stochastically mapped to clinical survival.</p>
                            <p><b>prob_pred (Operational):</b> Output probability for BDBV generated by the ML Meta-Learner.</p>
                            <p><b>arrival_day (Operational):</b> Assigned day of hospital presentation based on acute stochastic surge profile (Beta distribution).</p>
                          </div>
                          ')
                      )
                    )
                )
              )
      )
    )
  )
)

# =========================================================================
# 3. SERVER LOGIC
# =========================================================================
server <- function(input, output, session) {
  
  # Render Plotly objects with a clean layout format.
  output$plotly_impact_time <- renderPlotly({ 
    ggplotly(p_impact_time, tooltip = 'y') %>% 
      layout(
        legend = list(orientation = 'h', xanchor = 'center', x = 0.5, yanchor = 'top', y = -0.2),
        margin = list(l = 50, r = 20, t = 50, b = 80), xaxis = list(automargin = FALSE), yaxis = list(automargin = FALSE)
      )
  })
  
  # --- CENTRAL REACTIVE BLOCK: MICRO-SIMULATION ENGINE ---
  sim_micro_data <- reactive({
    req(input$sim_beds, input$sim_conservatism)
    sim_rt_11 <- simulate_triage('Adaptive Control (Rt = 1.1)', 1.1, input$sim_beds, input$sim_conservatism)
    sim_rt_20 <- simulate_triage('Standard Campaign (Rt = 2.0)', 2.0, input$sim_beds, input$sim_conservatism)
    sim_rt_35 <- simulate_triage('Collective Negligence (Rt = 3.5)', 3.5, input$sim_beds, input$sim_conservatism)
    
    df_combined <- bind_rows(sim_rt_11, sim_rt_20, sim_rt_35)
    df_combined$Scenario <- factor(df_combined$Scenario, levels = c('Collective Negligence (Rt = 3.5)', 'Standard Campaign (Rt = 2.0)', 'Adaptive Control (Rt = 1.1)'))
    df_combined
  })
  
  # --- DYNAMIC DAY CALCULATION (VERTICAL FORMAT) ---
  get_scenario_metrics <- function(df, scenario_pattern) {
    sub_df <- df %>% filter(grepl(scenario_pattern, Scenario))
    
    # System collapse day.
    collapse_day <- sub_df %>% filter(Bed_Demand > input$sim_beds) %>% pull(Day) %>% head(1)
    
    # Peak demand day.
    peak_day <- sub_df %>% slice(which.max(Bed_Demand)) %>% pull(Day)
    
    # Vertical format using HTML line breaks.
    txt_collapse <- ifelse(length(collapse_day) == 0 || is.infinite(collapse_day), 'None', collapse_day)
    HTML(paste('Collapse Day:', txt_collapse, '<br/>Peak Day:', peak_day))
  }
  
  output$col_rt35_box <- renderValueBox({ valueBox(get_scenario_metrics(sim_micro_data(), '3.5'), 'Rt 3.5 (Collective Negligence)', icon = icon('hospital'), color = 'red') })
  output$col_rt20_box <- renderValueBox({ valueBox(get_scenario_metrics(sim_micro_data(), '2.0'), 'Rt 2.0 (Standard Campaign)', icon = icon('hospital'), color = 'orange') })
  output$col_rt11_box <- renderValueBox({ valueBox(get_scenario_metrics(sim_micro_data(), '1.1'), 'Rt 1.1 (Adaptive Control)', icon = icon('hospital'), color = 'navy') })
  
  # --- CORRECTED INTERACTIVE PLOTLY RENDERER ---
  output$plotly_sim_saturation <- renderPlotly({
    df_data <- sim_micro_data()
    validate(need(nrow(df_data) > 0, 'Calculando...'))
    
    # Ensure factor ordering to prevent graphical jitter during reactive updates.
    df_data$Scenario <- factor(df_data$Scenario, levels = c('Collective Negligence (Rt = 3.5)', 'Standard Campaign (Rt = 2.0)', 'Adaptive Control (Rt = 1.1)'))
    
    p <- ggplot(df_data, aes(x = Day, y = Bed_Demand, color = Scenario, fill = Scenario, group = Scenario)) +
      # Layer 1: Base total area shading.
      geom_ribbon(aes(ymin = 0, ymax = Bed_Demand), alpha = 0.1, color = NA) +
      # Layer 2: Saturation shading (excess demand only).
      # Use the same grouping aesthetic to avoid layer rendering conflicts.
      geom_ribbon(data = df_data %>% filter(Bed_Demand > input$sim_beds), aes(ymin = input$sim_beds, ymax = Bed_Demand), alpha = 0.4, color = NA) +
      geom_line(linewidth = 1) +
      geom_hline(yintercept = input$sim_beds, linetype = 'dashed', color = 'black', linewidth = 0.8) +
      scale_color_manual(values = c('Collective Negligence (Rt = 3.5)' = '#8B0000', 'Standard Campaign (Rt = 2.0)' = '#E67E22', 'Adaptive Control (Rt = 1.1)' = '#2E4053')) +
      scale_fill_manual(values = c('Collective Negligence (Rt = 3.5)' = '#8B0000', 'Standard Campaign (Rt = 2.0)' = '#E67E22', 'Adaptive Control (Rt = 1.1)' = '#2E4053')) +
      labs(title = 'Real-time Saturation Curve', x = 'Days', y = 'Bed Demand') +
      theme_minimal() + theme(legend.position = 'none')
    
    ggplotly(p, tooltip = c('x', 'y', 'color')) %>% layout(margin = list(l = 20, r = 20, t = 30, b = 20), xaxis = list(automargin = TRUE), yaxis = list(automargin = TRUE))
  })
  
  # --- REMAINING MACRO & CLINICAL PLOTS ---
  sim_results <- reactive({
    n_val <- as.numeric(input$sim_n)
    rt_val <- as.numeric(input$sim_rt)
    cfr_val <- as.numeric(input$sim_cfr)
    vsl_val <- as.numeric(input$sim_vsl)
    
    peak_I <- n_val * max(0, (1 - (1/rt_val))) 
    total_deaths <- peak_I * cfr_val
    total_dalys <- (total_deaths * 35) + (peak_I * 2)
    loss_busd <- (total_dalys * (13800 * vsl_val)) / 1e9
    
    list(deaths = total_deaths, dalys = total_dalys, loss = loss_busd)
  })
  
  output$dyn_deaths_box <- renderValueBox({ valueBox(format(round(sim_results()$deaths), big.mark = ','), 'Cumulative Deaths', icon = icon('skull'), color = 'red') })
  output$dyn_dalys_box <- renderValueBox({ valueBox(format(round(sim_results()$dalys), big.mark = ','), 'Total DALYs Burden', icon = icon('wheelchair'), color = 'orange') })
  output$dyn_loss_box <- renderValueBox({ valueBox(paste0('$', format(round(sim_results()$loss, 2), big.mark = ','), ' B'), 'Economic Loss', icon = icon('chart-line'), color = 'purple') })
  
  output$dynamic_plot <- renderPlot({
    df_plot <- df_multi_policy %>% filter(N_EFF_Scenario == as.numeric(input$sim_n), VSL == as.numeric(input$sim_vsl))
    
    ggplot(df_plot, aes(x = Rt, y = Economic_Loss_BUSD, group = factor(CFR_Crisis), color = factor(CFR_Crisis))) +
      geom_line(linewidth = 1, alpha = 0.4) +
      geom_line(data = df_plot %>% filter(round(CFR_Crisis, 2) == round(as.numeric(input$sim_cfr), 2)), linewidth = 1.5) +
      geom_point(aes(x = as.numeric(input$sim_rt), y = sim_results()$loss), color = 'black', size = 6, shape = 18) +
      scale_color_viridis_d(option = 'plasma', name = 'System CFR:') +
      scale_y_continuous(labels = scales::dollar_format(suffix = ' B')) +
      labs(title = 'Policy Trajectory Tracking', x = 'Effective Reproduction Number (Rt)', y = '') +
      theme_minimal(base_size = 14) + theme(legend.position = 'bottom')
  })
  
  output$plotly_pop <- renderPlotly({ 
    ggplotly(p_pop, tooltip = c('y', 'fill')) %>% 
      layout(legend = list(orientation = 'h', xanchor = 'center', x = 0.5, yanchor = 'top', y = -0.25), margin = list(l = 80, r = 20, t = 50, b = 100), xaxis = list(automargin = FALSE), yaxis = list(automargin = FALSE))
  })
  
  output$table_explorer <- DT::renderDataTable({
    data_to_show <- switch(input$data_choice, 'macro' = df_macro_sens %>% mutate_if(is.numeric, round, 2), 'pairwise' = df_pairwise, 'multi_policy' = df_multi_policy %>% mutate_if(is.numeric, round, 2), 'triage_op' = df_triage_op)
    datatable(data_to_show, extensions = 'Buttons', filter = 'top', options = list(pageLength = 10, scrollX = TRUE, dom = 'Bfrtip', buttons = c('csv', 'excel')), rownames = FALSE)
  })
}

shinyApp(ui, server)
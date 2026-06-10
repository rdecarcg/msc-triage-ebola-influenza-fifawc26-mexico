# =========================================================================
# SCRIPT 00: MASTER PIPELINE ORCHESTRATOR
# PROJECT: EPIDEMIOLOGICAL TRIAGE FIFA WORLD CUP 2026 - MEXICO
# CONCEPT: END-TO-END AUTOMATION & REPRODUCIBILITY CONTROLLER
# AUTHOR: RODRIGO ABEL DE CARCER GANDARILLA
# =========================================================================
# DESCRIPTION: Master execution controller for the epidemiological and 
# macroeconomic impact simulation pipeline. This script guarantees strict 
# deterministic reproducibility by enforcing sequential module execution, 
# managing global memory states, and dynamically calculating the baseline 
# Poisson incursion probabilities prior to triggering the parametric 
# stochastic clinical simulations.
# =========================================================================

# =========================================================================
# 1. Environment Initialization & Memory Management.
# =========================================================================
# Purge the global environment to prevent hidden data leakage across runs.
rm(list = ls())

# Force garbage collection to optimize RAM allocation before heavy matrix operations.
gc() 

message('==========================================================')
message('INITIALIZING EPIDEMIOLOGICAL PIPELINE ORCHESTRATOR')
message('==========================================================')
start_time <- Sys.time()

# Load Global Configuration constraints.
if (!file.exists('src/config.R')) {
  stop('\n[FATAL ERROR] Configuration file "src/config.R" not found. Halting pipeline.')
}
source('src/config.R', echo = FALSE)
message('>>> Global configuration and parameters loaded successfully.')

# =========================================================================
# 2. Pathogen Incursion Probability Engine (Poisson Arrival Process)
# =========================================================================
# Evaluates the mathematical plausibility of the underlying Black Swan event.

# Poisson Lambda (Expected number of infected arrivals)
lambda_h5n1  <- INTL_VISITORS * P_FLIGHT_H5N1
lambda_ebola <- INTL_VISITORS * P_FLIGHT_EBOLA

# Probability of AT LEAST ONE incursion: P(X >= 1) = 1 - P(X = 0) = 1 - e^(-lambda).
prob_incursion_h5n1  <- 1 - exp(-lambda_h5n1)
prob_incursion_ebola <- 1 - exp(-lambda_ebola)

# Joint probability of a simultaneous multi-pathogen scenario (assuming independent vectors).
prob_joint_scenario <- prob_incursion_h5n1 * prob_incursion_ebola

message('\n>>> PATHOGEN INCURSION PLAUSIBILITY ASSESSMENT (POISSON MODEL)')
message('    Context: June 2026 WHO PHEIC (BDBV) & Southern Hemisphere Winter Surge')
message(sprintf('    International Visitors Simulated: %s', format(INTL_VISITORS, big.mark = ',')))
message(sprintf('    P(Incursion | Influenza H5N1):    %.2f%%', prob_incursion_h5n1 * 100))
message(sprintf('    P(Incursion | Ebola Bundibugyo):  %.2f%%', prob_incursion_ebola * 100))
message(sprintf('    P(Joint Simultaneous Scenario):   %.2f%%', prob_joint_scenario * 100))
message('    Note: The pipeline will now execute under the counterfactual condition')
message('    that the joint scenario has occurred (P = 1.0) to assess impact.\n')

# =========================================================================
# 3. Sequential Module Definition.
# =========================================================================
# Strict array defining the dependency execution order.
pipeline_scripts <- c(
  'src/01-clinical-simulation.R',
  'src/02-exploratory-data-analysis.R',
  'src/03-predictive-modelling.R',
  'src/04-micro-impact.R',
  'src/05-macro-impact.R'
)

# =========================================================================
# 4. Execution Engine with Robust Error Handling.
# =========================================================================
# Sandboxed execution loop utilizing tryCatch for strict upstream dependency gating.
run_pipeline <- function(scripts) {
  for (script in scripts) {
    if (!file.exists(script)) {
      stop(sprintf('\n[FATAL ERROR] File not found: "%s". Halting pipeline.', script))
    }
    
    message(sprintf('\n>>> [%s] Executing module: %s ...', format(Sys.time(), '%H:%M:%S'), script))
    
    execution_status <- tryCatch({
      source(script, echo = FALSE)
      TRUE 
    }, error = function(e) {
      message(sprintf('\n[FATAL ERROR] Execution failed in module "%s".', script))
      message(sprintf('Traceback/Details: %s', e$message))
      FALSE 
    })
    
    if (!execution_status) {
      stop('\nPipeline aborted due to upstream module failure.')
    }
    
    message(sprintf('<<< [%s] Module complete: %s', format(Sys.time(), '%H:%M:%S'), script))
  }
}

# =========================================================================
# 5. Trigger Execution.
# =========================================================================
run_pipeline(pipeline_scripts)

# =========================================================================
# 6. Pipeline Conclusion & Performance Metrics.
# =========================================================================
end_time <- Sys.time()
execution_duration <- difftime(end_time, start_time, units = 'mins')

message('\n==========================================================')
message(sprintf('[SUCCESS] Full pipeline executed perfectly in %.2f minutes.', execution_duration))
message('All synthetic arrays, diagnostic plots, and spatial-economic matrices are ready.')
message('==========================================================')
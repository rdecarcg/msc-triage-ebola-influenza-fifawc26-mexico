# =========================================================================
# SCRIPT 03: STACKED ENSEMBLE PREDICTIVE MODELLING & DECISION THRESHOLDING
# PROJECT: EPIDEMIOLOGICAL TRIAGE FIFA WORLD CUP 2026 - MEXICO
# AUTHOR: RODRIGO ABEL DE CARCER GANDARILLA
# =========================================================================
# DESCRIPTION: Development of a Stacked Generalization ensemble meta-learner 
# to classify etiological agents (Ebola Bundibugyo vs. Influenza H5N1) under 
# extreme diagnostic uncertainty. Incorporates in-silico stochastic variance 
# to simulate field-laboratory degradation, and mathematically optimizes 
# decision boundaries using Precision-Recall Area Under Curve (PR-AUC) and 
# F-beta scoring to strictly penalize fatal false negatives.
# =========================================================================

if (!require('pacman')) install.packages('pacman')
pacman::p_load(tidyverse, tidymodels, stacks, ranger, xgboost, glmnet, doParallel)

# =========================================================================
# 0. INITIALIZATION & SETUP ####
# =========================================================================
# Load global constants to enforce parameter immutability across the pipeline.
if (file.exists('src/config.R')) source('src/config.R', echo = FALSE)
if (!dir.exists('reports/models')) dir.create('reports/models', recursive = TRUE)
if (!dir.exists('data')) dir.create('data', recursive = TRUE)

# =========================================================================
# 1. DATA INGESTION & OPERATIONAL NOISE INJECTION ####
# =========================================================================
# Load the baseline, idealized clinical cohort generated in Script 01.
df_triage <- read.csv('data/dataset_ebola_vs_influenza_fifawc26_triage_mexico.csv', stringsAsFactors = TRUE)

# In-Silico Degradation of Field-Laboratory Conditions: 
# Injecting stochastic variance into physiological measurements to emulate 
# instrument tolerance limits, human operational error, and chronological 
# recall bias expected during an acute mass-casualty surge.
set.seed(GLOBAL_SEED)
df_triage_op <- df_triage %>%
  mutate(
    # Introduce 15% standard deviation noise to platelet quantification.
    platelets_count = round(platelets_count * rnorm(n(), mean = 1, sd = 0.15)),
    # Introduce 10% standard deviation noise to leukocyte quantification.
    white_blood_cells = round(white_blood_cells * rnorm(n(), mean = 1, sd = 0.10)),
    # Recall bias in latency reporting (stochastically shifted by -3 to +3 days).
    incubation_days = pmax(1, incubation_days + sample(c(-3:-1, 0, 1:3), n(), replace = TRUE, 
                                                       prob = c(0.05, 0.15, 0.2, 0.2, 0.2, 0.15, 0.05))),
    # Target Variable Binarization: Explicit definition for logistic constraints.
    is_ebola = factor(ifelse(pathogen == 'Ebola_Bundibugyo', 'Ebola', 'Influenza'), levels = c('Ebola', 'Influenza'))
  )
write.csv(df_triage_op, file.path('data/dataset_operational_triage.csv'), row.names = FALSE)

# =========================================================================
# 2. PARTITIONING & PREPROCESSING RECIPE ####
# =========================================================================
# 80/20 stratified split to preserve proportional class representation 
# across both training and hold-out evaluation spaces.
set.seed(123)
data_split <- initial_split(df_triage_op, prop = 0.8, strata = is_ebola)
df_train <- training(data_split)
df_test  <- testing(data_split)

# k-Fold Cross-Validation (k=5) to ensure out-of-sample robustness 
# and prevent algorithm overfitting during the hyperparameter grid search.
folds <- vfold_cv(df_train, v = 5, strata = is_ebola)

# Feature Engineering Recipe: Standard scalar normalization.
# NOTE: 'patient_id' is implicitly excluded from the formula to strictly 
# prevent data leakage into the learning environment.
triage_rec <- recipe(is_ebola ~ platelets_count + white_blood_cells + incubation_days, data = df_train) %>% 
  step_normalize(all_numeric_predictors())

ctrl_grid <- control_stack_grid()
ctrl_res <- control_stack_resamples()

# =========================================================================
# 3. BASE LEARNERS FORMULATION (PARALLELIZED) ####
# =========================================================================
# Initialize multicore parallel processing to accelerate hyperparameter sweeps.
all_cores <- parallel::detectCores(logical = FALSE)
cl <- makePSOCKcluster(all_cores)
registerDoParallel(cl)

# 3.1 Regularized Logistic Regression (Ridge).
# Strategy: Captures strictly linear decision boundaries and applies L2 
# penalty to prevent coefficient inflation.
ridge_spec <- logistic_reg(penalty = tune(), mixture = 0) %>% 
  set_engine('glmnet') %>% 
  set_mode('classification')
ridge_wf <- workflow() %>% add_recipe(triage_rec) %>% add_model(ridge_spec)
ridge_res <- tune_grid(ridge_wf, resamples = folds, grid = 4, control = ctrl_grid)

# 3.2 Random Forest (Recursive Partitioning).
# Strategy: Captures complex non-linear feature interactions and high-order 
# physiological thresholding via bootstrap aggregated decision trees.
rf_spec <- rand_forest(trees = 300, min_n = tune()) %>% 
  set_engine('ranger', num.threads = 1) %>% 
  set_mode('classification')
rf_wf <- workflow() %>% add_recipe(triage_rec) %>% add_model(rf_spec)
rf_res <- tune_grid(rf_wf, resamples = folds, grid = 3, control = ctrl_grid)

# 3.3 Extreme Gradient Boosting (XGBoost).
# Strategy: Sequential error correction through gradient descent to 
# optimize residual learning across weak prediction zones.
xgb_spec <- boost_tree(trees = 300, learn_rate = tune()) %>% 
  set_engine('xgboost', nthread = 1) %>% 
  set_mode('classification')
xgb_wf <- workflow() %>% add_recipe(triage_rec) %>% add_model(xgb_spec)
xgb_res <- tune_grid(xgb_wf, resamples = folds, grid = 3, control = ctrl_grid)

stopCluster(cl)
registerDoSEQ()

# =========================================================================
# 4. META-LEARNER STACKING & BLENDING ####
# =========================================================================
# Blending base learners via a penalized meta-model optimized for PR-AUC.
# Rationale: PR-AUC evaluates model precision across minority class thresholds,
# making it significantly superior to standard ROC-AUC in environments with 
# highly asymmetric misclassification costs.
triage_data_stack <- stacks() %>% 
  add_candidates(ridge_res) %>% 
  add_candidates(rf_res) %>% 
  add_candidates(xgb_res)

set.seed(2026)
triage_model_stack <- blend_predictions(triage_data_stack, metric = metric_set(pr_auc))
triage_final_fit <- fit_members(triage_model_stack)

saveRDS(triage_final_fit, file.path('reports/models/stacked_triage_model.rds'))

# =========================================================================
# 5. DECISION THRESHOLD OPTIMIZATION (F-BETA & PR-AUC) ####
# =========================================================================
# Generating out-of-sample predictions mapped against the hold-out test set.
test_preds <- predict(triage_final_fit, df_test, type = 'prob') %>% bind_cols(df_test)

# Precision-Recall Evaluation.
pr_auc_val <- pr_auc(test_preds, truth = is_ebola, .pred_Ebola, event_level = 'first')
pr_curve_data <- pr_curve(test_preds, truth = is_ebola, .pred_Ebola, event_level = 'first')

# F-Beta Score Optimization (Asymmetric Cost Function).
# Setting Beta = 2 weights Recall (Sensitivity) twice as heavily as Precision. 
# Clinical Rationale: A False Negative (discharging an Ebola patient) triggers 
# a catastrophic and lethal community outbreak. Conversely, a False Positive 
# (isolating an Influenza patient) merely consumes a finite hospital resource.
beta_val <- 2
pr_curve_data <- pr_curve_data %>% 
  mutate(f_beta = ifelse((precision == 0 & recall == 0), 0, 
                         (1 + beta_val^2) * (precision * recall) / ((beta_val^2 * precision) + recall)))

optimal_threshold_data <- pr_curve_data %>% drop_na() %>% arrange(desc(f_beta)) %>% slice(1)
p_threshold_opt <- optimal_threshold_data$.threshold

# Visualization of the PR Curve and the mathematically derived decision boundary.
png(file.path('reports/figures/pr_curve_stacked_test.png'), width = 1800, height = 1800, res = 300)
plot(pr_curve_data$recall, pr_curve_data$precision, type = 'l', col = '#8B0000', lwd = 3, 
     xlim = c(0, 1), ylim = c(0, 1), 
     main = 'Precision-Recall Curve: Ensemble Meta-Learner',
     xlab = 'Recall (Sensitivity)', ylab = 'Precision (Positive Predictive Value)')
grid()
points(optimal_threshold_data$recall, optimal_threshold_data$precision, col = '#2E4053', pch = 19, cex = 1.5)
legend('bottomleft', legend = paste('Optimal Threshold (F-Beta=', beta_val, '): ', round(p_threshold_opt, 3)), 
       col = '#2E4053', pch = 19, bty = 'n')
dev.off()

# Synchronize generated analytical objects with the Interactive Dashboard environment.
file.copy(from = 'reports/figures/pr_curve_stacked_test.png', 
          to = 'epitriage-dashboard/www/pr_curve_stacked_test.png', 
          overwrite = TRUE)

cat('\n[SUCCESS] Script 03 executed. Stacked Meta-Learner saved and PR metrics evaluated.\n')
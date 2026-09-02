# Shared configuration for stages 03 and 04
#
# Sourced automatically at the top of 00_functions_data.R so the
# proxy-building and modelling scripts use the same settings.


# ---- design decisions -----------------------------------------------

# 1. Both proxies use six-month scripts only.
# 2. Northern Cape is excluded from Proxy 2 only.
# 3. Both proxies use the same predictor roster and 60/20/20 stratified split.
# 4. Enrolment history uses enrolment_tenure_years and registered_after_index.
# 5. Age is restricted to 5-100 years before outcome and feature derivation.
# 6. Models use the full dataset where feasible; nnet and multinom use
#    stratified samples.
# 7. Standard R packages are used for metrics and model diagnostics.
# 8. Validation and test predictions are saved by each model script.


# ---- paths ----------------------------------------------------------

# Main data folder is defined once in 00_paths.R.
source(file.path(if (exists("shared_dir")) shared_dir else ".", "00_paths.R"))

final_data_dir <- ccmdd_data_dir

# Person-level modelling tables created in stage 03 and used in stage 04.
proxy1_csv <- file.path(final_data_dir,"proxy1_person_level_renewal.csv")

proxy2_csv <- file.path(final_data_dir,"proxy2_person_level_collection.csv")

# Folder for modelling outputs, figures and saved model objects.
results_dir <- file.path(final_data_dir, "modelling_results")

dir.create(results_dir,showWarnings = FALSE,recursive = TRUE)

results_path <- function(...) {
  file.path(results_dir, ...)
}

# ---- seed -----------------------------------------------------------

# Shared seed used throughout the pipeline
seed <- 2025L


# ---- cohort rules ---------------------------------------------------

# Six-month scripts only
valid_lengths <- 6L

# Prescription timing
months_to_days <- 28L
grace_days <- 21L

# Age restriction
min_age <- 5L
max_age <- 100L

# Earliest plausible registration date 
reg_lo <- as.Date("2014-01-01")

# Proxy 2 replacement window
replacement_gap_days <- 21L

# Northern Cape is excluded from Proxy 2 only
nc_label <- "Northern Cape"

# FALSE means expected follow-up collections are F_j = E_j - 1.
# We assume initiation is not in dispense data (median first scan = 60 days; 0% within 7 days)
assume_initiation_recorded <- FALSE


# ---- split ----------------------------------------------------------

frac_train <- 0.60
frac_valid <- 0.20
# Remaining 0.20 is the test set.

ord_levels <- c(
  "0_none",
  "1_low",
  "2_partial",
  "3_full"
)


# ---- model training sizes ------------------------------------------

# Inf means the full training pool is used.
tree_train_n <- Inf
xgb_train_n <- Inf
glmnet_train_n <- Inf
polr_train_n <- Inf

# multinom uses a large stratified sample because the full fit is
# memory intensive.
multinom_train_n <- 750000L


# ---- neural network -------------------------------------------------

# Samples used for tuning.
nn_train_n_p1 <- 150000L
nn_train_n_p2 <- 120000L

# Hidden-layer sizes considered during tuning.
nn_size_grid <- c(8, 20, 40)

# Maximum number of weights allowed by nnet.
nn_max_wts <- 5000L

# Larger samples used for the final neural-network fits.
nn_final_train_n_p1 <- 500000L
nn_final_train_n_p2 <- 500000L


# ---- random forest --------------------------------------------------

# Each tree draws from the full training pool using this sample fraction.
rf_sample_fraction <- 0.25

rf_num_trees_tune <- 200L
rf_num_trees_final <- 500L


# ---- stability analysis --------------------------------------------

# Three rounds of four disjoint blocks gives 12 replicate fits.
rep_blocks <- 4L
rep_rounds <- 3L

rep_models_p1 <- c(
  "logistic",
  "tree",
  "rf",
  "xgb",
  "nn"
)

rep_models_p2 <- c(
  "polr",
  "multinom",
  "tree",
  "rf",
  "xgb",
  "nn"
)

# Neural-network sample sizes used in the stability analysis
rep_nn_n_p1 <- 150000L
rep_nn_n_p2 <- 120000L
# Deterministic seeds for each round of the replicate fits.
# Each round is spaced 1,000 seeds apart so that separate rounds use
# different random-number sequences while still being reproducible.
rep_round_seed <- function(round) {
  seed + 1000L * as.integer(round)
}

# Deterministic seed for each individual fit/block within a round.
# The block offset ensures that multiple fits in the same round do not
# restart from the same random-number sequence.
rep_fit_seed <- function(round, block) {
  seed +
    1000L * as.integer(round) +
    as.integer(block)
}


# ---- SHAP -----------------------------------------------------------

# Use the full validation set for XGBoost SHAP.
shap_n_p1 <- Inf
shap_n_per_class <- Inf

# Process SHAP calculations in chunks to limit memory use.
shap_chunk <- 100000L

# Reuse saved SHAP matrices when available.
shap_reuse <- TRUE

# Smaller sample used only for the additional beeswarm rendering.
# Reported SHAP values still use the full validation set.
shap_beeswarm_sample_n <- 50000L

# Random-forest SHAP is an optional directional cross-check.
shap_rf_n <- 300L


# ---- output paths ---------------------------------------------------

# Console scoreboard, one row per model. Scripts 03 and 08 read it back to
# rank the regression family. Not a source for any thesis table: script 16
# recomputes every reported metric from the saved predictions.
leaderboard_path <- function(proxy) v3_path(sprintf("%s_leaderboard.csv", proxy))

# The 'best' hyperparameters per model family, so the tuning search is never
# repeated. The stability suites refit at exactly these settings.
params_path <- function(proxy) v3_path(sprintf("%s_champion_params.rds", proxy))

# Per-patient validation and test predictions. Scripts 16 and 17 rebuild every
# table and figure from these instead of refitting, and they are the single
# source of truth for reported metrics.
preds_path <- function(proxy, model_key) v3_path(sprintf("%s_preds_%s.rds", proxy, model_key))

# One row per (model, replicate) from the 12 stability refits. Appended after
# each fit and re-read at the start, so an interrupted run resumes rather than
# refitting. Script 17 draws the spread figures from it.
stability_runs_path <- function(proxy) v3_path(sprintf("%s_stability_runs.csv", sub("proxy", "p", proxy)))

# Regression-champion coefficients per replicate. Script 17 checks whether each
# odds ratio keeps its sign across all 12 refits.
stability_coefs_path <- function(proxy) v3_path(sprintf("%s_stability_coefs.csv", sub("proxy", "p", proxy)))
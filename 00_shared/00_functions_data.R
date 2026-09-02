# Shared data preparation for stages 03 and 04
#
# Sourced by the modelling scripts so they use the same predictors,
# factor handling and train/validation/test split. No models are fitted here.
#
# Inputs built in stage 03:
# - proxy1_person_level_renewal.csv
# - proxy2_person_level_collection.csv

suppressPackageStartupMessages({
  library(data.table)
})


# ---- shared configuration ------------------------------------------

# 00_config.R is in the same shared folder as this file.
# Use shared_dir from the calling script, or default to ../00_shared
if (!exists("shared_dir")) shared_dir <- file.path("..", "00_shared")

config_file <- file.path(shared_dir, "00_config.R")

if (!file.exists(config_file)) {
  stop("00_config.R not found at:\n  ",
       normalizePath(config_file, mustWork = FALSE),
       "\nRun stage scripts with the working directory set to their own folder.")
}

source(config_file)


# ---- disease-group names -------------------------------------------

group_map <- c(
  "idx_Cardiovascular conditions"          = "cvd",
  "idx_Central nervous system conditions"  = "cns",
  "idx_Ear, nose and throat conditions"    = "ent",
  "idx_Emergencies and injuries"           = "emergencies",
  "idx_Endocrine conditions"               = "endocrine",
  "idx_Eye conditions"                     = "eye",
  "idx_Family planning"                    = "family_planning",
  "idx_Gastro-intestinal conditions"       = "gastro",
  "idx_HIV and AIDS"                       = "hiv",
  "idx_Infections and related conditions"  = "infections",
  "idx_Kidney and urological disorders"    = "renal",
  "idx_Mental health conditions"           = "mental_health",
  "idx_Musculoskeletal conditions"         = "msk",
  "idx_Nutrition and anaemia"              = "nutrition",
  "idx_Obstetrics and Gynaecology"         = "obgyn",
  "idx_Pain"                               = "pain",
  "idx_Respiratory conditions"             = "respiratory",
  "idx_Skin conditions"                    = "skin"
)


# ---- predictor names ------------------------------------------------

# Rename the source fields for the modelling tables.
base_map <- c(
  "patient_id"             = "patient_id",
  "age_years"              = "age",
  "idx_Gender"             = "gender",
  "log_dist"               = "log_dist",
  "collects_at_facility"   = "collects_at_facility",
  "n_drugs_index"          = "n_drugs_index",
  "n_med_groups"           = "n_med_groups",
  "multimorbidity"         = "multimorbidity",
  "index_year"             = "index_year",
  "index_month"            = "index_month",
  "index_dow"              = "index_dow",
  "enrolment_tenure_days"  = "enrolment_tenure_days",
  "registered_after_index" = "registered_after_index",
  "idx_Province"           = "province"
)


# ---- Proxy 2 leakage guard -----------------------------------------

# Outcome and follow-up fields that must never enter the Proxy 2 predictors.
forbidden_proxy2 <- c(
  "patient_id", "y_collection_cat", "y_ord", "y_nom", "y_label",
  "collection_ratio", "n_scripts", "first_rx_date", "last_rx_date",
  "n_expected_followup_in_window", "n_collected_adjusted", "n_expected_total",
  "n_dispensed_raw", "n_rows_observed", "any_return", "n_returns_total",
  "n_dispensed_then_returned", "n_return_only", "return_rate",
  "n_replacement", "n_rx_with_returns"
)

# Also catch one-hot encoded variables whose names start with a forbidden field.
assert_no_leakage <- function(vars, label) {
  hit <- vapply(vars, function(v) {
    any(v == forbidden_proxy2 | startsWith(v, forbidden_proxy2))
  }, logical(1))

  if (any(hit)) {
    stop(sprintf("LEAKAGE in %s: %s", label, paste(vars[hit], collapse = ", ")))
  }
  invisible(TRUE)
}


# ---- split assignment ----------------------------------------------

# Add a train/valid/test split, stratified by the outcome.
assign_split <- function(dt, strat_col) {
  set.seed(seed)  # make the split reproducible
  dt[, split := {
    n <- .N
    o <- sample(n)  # randomly order rows within each outcome group
    # Start by assigning all rows to test.
    out <- rep("test", n)
    # Assign the required proportion to training.
    out[o <= floor(frac_train * n)] <- "train"
    # Assign the next proportion to validation; remaining rows stay in test.
    out[o > floor(frac_train * n) &
          o <= floor((frac_train + frac_valid) * n)] <- "valid"
    out
  }, by = strat_col]  # repeat the split separately within each outcome group
  invisible(dt)
}


# ---- text and factor handling --------------------------------------

# Convert blank character values back to NA after reading the CSV files.
blank_to_na <- function(dat) {
  char_cols <- setdiff(names(dat)[sapply(dat, is.character)], "patient_id")

  for (cc in char_cols) {
    blank <- !is.na(dat[[cc]]) & dat[[cc]] == ""
    if (any(blank)) dat[blank, (cc) := NA_character_]
  }
  invisible(dat)
}

drop_unused_levels <- function(dat) {
  fac_cols <- names(dat)[sapply(dat, is.factor)]
  for (cc in fac_cols) dat[, (cc) := droplevels(dat[[cc]])]
  invisible(dat)
}

# Apply the reference levels used
apply_factors <- function(dat) {
  dat[, gender := factor(gender)]
  if ("Female" %in% levels(dat$gender)) {
    dat[, gender := relevel(gender, ref = "Female")]
  }

  dat[, province := factor(province)]
  if ("Eastern Cape" %in% levels(dat$province)) {
    dat[, province := relevel(province, ref = "Eastern Cape")]
  }

  if ("index_year" %in% names(dat)) {
    dat[, index_year := factor(index_year)]
    if ("2021" %in% levels(dat$index_year)) {
      dat[, index_year := relevel(index_year, ref = "2021")]
    }
  }

  if ("index_month" %in% names(dat)) {
    dat[, index_month := factor(index_month, levels = 1:12)]
  }

  if ("index_dow" %in% names(dat)) {
    dat[, index_dow := factor(index_dow,
      levels = c("Fri", "Mon", "Tue", "Wed", "Thu", "Sat", "Sun"))]
  }

  stopifnot("registered_after_index" %in% names(dat))
  dat[, registered_after_index := as.integer(registered_after_index)]

  # Express enrolment tenure in years for easier interpretation.
  dat[, enrolment_tenure_years := enrolment_tenure_days / 365]
  dat[, enrolment_tenure_days := NULL]

  invisible(dat)
}


# ---- predictor sets -------------------------------------------------

# Define the predictor variables used in the models.
predictor_sets <- function(dat) {
  
  # individual disease indicator variables
  disease_flags <- intersect(unname(group_map), names(dat))
  
  # continuous predictors measured on a numeric scale.
  preds_cont <- intersect(
    c("age", "log_dist", "n_drugs_index", "n_med_groups",
      "enrolment_tenure_years"),
    names(dat)
  )
  
  # binary 0/1 predictors
  # add the individual disease flags to this group as they are also binary
  preds_bin <- intersect(
    c("collects_at_facility", "multimorbidity", "registered_after_index"),
    names(dat)
  )
  preds_bin <- c(preds_bin, disease_flags)
  
  # categorical predictors that will be represented by factor levels in the models.
  preds_cat <- intersect(
    c("gender", "index_year", "index_month", "index_dow", "province"),
    names(dat)
  )
  
  # full predictor set used by most models.
  predictors <- c(preds_cont, preds_bin, preds_cat)
  
  # n_med_groups is calculated from the disease flags.
  # Remove it from unpenalised regression models to avoid including both a total and all of the variables that make up that total.
  predictors_lm <- setdiff(predictors, "n_med_groups")
  
  # Return the full set and the grouped versions for use by later scripts.
  list(
    predictors = predictors,
    predictors_lm = predictors_lm,
    preds_cont = preds_cont,
    preds_bin = preds_bin,
    preds_cat = preds_cat,
    disease_flags = disease_flags
  )
}


# ---- prepare Proxy 1 data ------------------------------------------

prepare_proxy1_data <- function(verbose = TRUE) {
  
  # Check that the tabel exists
  if (!file.exists(proxy1_csv)) {
    stop("Input not found:\n  ", proxy1_csv,
         "\nRun 01_build_proxy1_table.R first.")
  }
  
  # Read it.
  dt <- fread(proxy1_csv)
  setDT(dt)
  
  # Keep only the outcome and variables needed for modelling.
  # Rename source variables to the standard names used across model scripts.
  keep_map <- c(base_map, group_map)
  keep_map <- keep_map[names(keep_map) %in% names(dt)]
  
  need <- c("y_active", names(keep_map))
  dat  <- dt[, ..need]
  setnames(dat, names(keep_map), unname(keep_map))
  rm(dt)
  
  # Clean missing values and apply the required factor/reference levels.
  blank_to_na(dat)
  apply_factors(dat)
  
  # build the different types of predictor sets
  ps <- predictor_sets(dat)
  stopifnot(length(ps$predictors) == 31L)
  
  # remove patients missing either the outcome or any model predictor (just as a fail safe)
  n0 <- nrow(dat)
  dat <- dat[complete.cases(
    dat[, c("y_active", ps$predictors), with = FALSE]
  )]
  drop_unused_levels(dat)
  
  # Proxy 1 outcome: 0 = did not renew, 1 = renewed.
  stopifnot(all(dat$y_active %in% c(0L, 1L)))
  dat[, y_fac := factor(y_active, levels = c(0L, 1L))]
  
  # Text labels required by caret for binary classification
  dat[, y_class := factor(
    fifelse(y_active == 1L, "yes", "no"),
    levels = c("no", "yes")
  )]
  
  # Create the stratified train/validation/test split
  assign_split(dat, "y_active")
  
  # Print a short check of the final modelling table and split
  if (verbose) {
    cat(sprintf("Proxy 1 modelling table: %d patients (%d dropped as incomplete)\n",
                nrow(dat), n0 - nrow(dat)))
    cat(sprintf("Predictors: %d (%d continuous, %d binary, %d categorical)\n",
                length(ps$predictors), length(ps$preds_cont),
                length(ps$preds_bin), length(ps$preds_cat)))
    
    print(dat[, .(
      n = .N,
      renewal_rate = round(mean(y_active), 3)
    ), by = split][
      order(factor(split, levels = c("train", "valid", "test")))
    ])
  }
  
  # Return the prepared data together with the predictor sets.
  c(list(data = dat, n_dropped_incomplete = n0 - nrow(dat)), ps)
}


# ---- prepare Proxy 2 data ------------------------------------------

prepare_proxy2_data <- function(verbose = TRUE) {
  
  # Check that the Proxy 2 table exists
  if (!file.exists(proxy2_csv)) {
    stop("Input not found:\n  ", proxy2_csv,
         "\nRun 02_build_proxy2_table.R first.")
  }
  
  # Read it
  dt <- fread(proxy2_csv)
  setDT(dt)
  
  # Keep only the outcome and variables needed for modelling.
  # Rename source variables to the standard names used across model scripts.
  keep_map <- c(base_map, group_map)
  keep_map <- keep_map[names(keep_map) %in% names(dt)]
  
  need <- c("y_collection_cat", names(keep_map))
  dat  <- dt[, ..need]
  setnames(dat, names(keep_map), unname(keep_map))
  rm(dt)
  
  # Clean missing values and apply the required factor/reference levels.
  blank_to_na(dat)
  apply_factors(dat)
  
  # Build the predictor sets and confirm that no outcome-derived fields are still present
  ps <- predictor_sets(dat)
  assert_no_leakage(ps$predictors, "Proxy 2 predictor set")
  stopifnot(length(ps$predictors) == 31L)
  
  # Northern Cape is excluded from Proxy 2 because dispense capture is incomplete.
  stopifnot(!nc_label %in% unique(dat$province))
  
  # Remove patients missing either the outcome or any model predictor.
  n0 <- nrow(dat)
  dat <- dat[complete.cases(
    dat[, c("y_collection_cat", ps$predictors), with = FALSE]
  )]
  drop_unused_levels(dat)
  
  # Create ordered and unordered versions of the four-category outcome.
  stopifnot(all(dat$y_collection_cat %in% ord_levels))
  dat[, y_ord := factor(y_collection_cat, levels = ord_levels, ordered = TRUE)]
  dat[, y_nom := factor(y_collection_cat, levels = ord_levels)]
  
  # Numeric outcome required by XGBoost: 0_none = 0 through 3_full = 3.
  dat[, y_label := as.integer(y_nom) - 1L]
  
  # Create the stratified train/validation/test split.
  assign_split(dat, "y_collection_cat")
  
  # Print a short check of the final modelling table and class balance.
  if (verbose) {
    cat(sprintf("Proxy 2 modelling table: %d patients (%d dropped as incomplete)\n",
                nrow(dat), n0 - nrow(dat)))
    cat(sprintf("Predictors: %d (%d continuous, %d binary, %d categorical)\n",
                length(ps$predictors), length(ps$preds_cont),
                length(ps$preds_bin), length(ps$preds_cat)))
    
    print(dcast(
      dat[, .N, by = .(split, y_ord)][
        , pct := round(100 * N / sum(N), 1), by = split
      ],
      y_ord ~ factor(split, levels = c("train", "valid", "test")),
      value.var = "pct"
    ))
  }
  
  # Return the prepared data together with the predictor sets.
  c(list(data = dat, n_dropped_incomplete = n0 - nrow(dat)), ps)
}

# ---- stratified sampling -------------------------------------------

# Used only for models that are fitted on a documented stratified sample --
sample_stratified <- function(dt, strat_col, n_total, seed_value = seed) {
  if (n_total >= nrow(dt)) return(copy(dt))

  # Work out what proportion of each outcome group should be sampled.
  frac <- n_total / nrow(dt)
  set.seed(seed_value)  # make the sample reproducible
  
  # Randomly sample the same proportion from each outcome group.
  idx <- dt[, .I[sample(.N, max(1L, floor(frac * .N)))],
            by = strat_col]$V1
  
  dt[idx]
}


# ---- sparse design matrix ------------------------------------------

# Build one reference-coded design matrix for all factors.
# Keeping the intercept during model.matrix() and dropping it afterwards
# gives consistent reference coding across train, validation and test data.
sparse_matrix <- function(dat, predictors) {
  old_na <- getOption("na.action")
  options(na.action = "na.pass")
  on.exit(options(na.action = old_na), add = TRUE)

  X <- Matrix::sparse.model.matrix(reformulate(predictors), data = dat)
  X[, colnames(X) != "(Intercept)", drop = FALSE]
}


# ---- saved predictions ---------------------------------------------

# Save validation/test predictions so reporting scripts can rebuild
# metrics, tables and figures without refitting the models.
#
# Proxy 1: p is the predicted probability of renewal, P(y = 1).
# Proxy 2: p contains the predicted probabilities for all four outcome classes.
save_preds <- function(proxy, model_key, split, y, p, threshold = NA_real_) {
  # Store the predictions together with the information needed to evaluate them.
  obj <- list(proxy = proxy, model_key = model_key, split = split,
              y = y, p = p, threshold = threshold,
              saved_at = format(Sys.time()))
  saveRDS(obj, preds_path(proxy, model_key))
  invisible(obj)
}

# Load previously saved predictions for a model.
load_preds <- function(proxy, model_key) {
  f <- preds_path(proxy, model_key)
  # Stop if the model has not yet produced its prediction file.
  if (!file.exists(f)) {
    stop("Missing predictions: ", f,
         "\nRun the corresponding model script first.")
  }
  readRDS(f)
}

# ---- selected model parameters -------------------------------------

# Save the selected parameter settings for one model and proxy.
# Existing settings for other models are kept in the same RDS file.
save_params <- function(proxy, key, value) {
  f <- params_path(proxy)
  store <- if (file.exists(f)) readRDS(f) else list()
  store[[key]] <- value
  saveRDS(store, f)
  invisible(store)
}

# Load saved parameter settings for a proxy.
# If key is NULL, return the full parameter store.
load_params <- function(proxy, key = NULL) {
  f <- params_path(proxy)
  if (!file.exists(f)) stop("No champion parameter store yet: ", f)
  store <- readRDS(f)
  if (is.null(key)) return(store)
  if (is.null(store[[key]])) stop(sprintf("No frozen '%s' champion for %s.", key, proxy))
  store[[key]]
}
  
# ---- leaderboard ----------------------------------------------------

record_result <- function(proxy, model_id, source_file, metrics_row) {
  # Use a local copy because model_id is also a leaderboard column name.
  mid <- model_id   # local alias: inside [.data.table, 'model_id' is the column
  row <- as.data.table(metrics_row)
  row[, `:=`(model_id = mid, source_file = source_file,
             recorded_at = format(Sys.time()))]
  f <- leaderboard_path(proxy)
  lb <- if (file.exists(f)) fread(f) else data.table()
  if (nrow(lb) > 0 && "model_id" %in% names(lb)) {
    lb <- lb[lb$model_id != mid]   # re-recording a model_id replaces its row
  }
  lb <- rbind(lb, row, fill = TRUE)
  fwrite(lb, f)
  invisible(lb)
}

# Models are ranked using validation performance only.
# Test metrics are kept for final reporting.
show_leaderboard <- function(proxy) {
  f <- leaderboard_path(proxy)
  if (!file.exists(f)) { cat("No leaderboard yet for", proxy, "\n"); return(invisible(NULL)) }
  lb <- fread(f)
  ord <- if (proxy == "proxy1") "valid_auc" else "valid_macro_f1"
  if (ord %in% names(lb)) {
    setorderv(lb, ord, order = -1L, na.last = TRUE)
  } else {
    cat(sprintf("Note: '%s' absent from %s - leaderboard left unsorted. ", ord, basename(f)),
        "Re-run the model scripts to record it.\n")
  }
  print(lb)
  invisible(lb)
}


# ---- stability replicates ------------------------------------------

# Three rounds of four blocks gives the 12 planned replicate fits.
replicate_plan <- function(n_blocks = rep_blocks, n_rounds = rep_rounds) {
  plan <- CJ(round = seq_len(n_rounds), block = seq_len(n_blocks))

  plan[, replicate_id := .I]
  plan[, fit_seed := rep_fit_seed(round, block)]

  setcolorder(plan, c("replicate_id", "round", "block", "fit_seed"))
  plan[]
}

replicate_rows <- function(train_dt, strat_col, round, block,
                           n_blocks = rep_blocks) {
  stopifnot(block >= 1L, block <= n_blocks)
  set.seed(rep_round_seed(round))

  assign_dt <- train_dt[, {
    o <- sample(.N)
    .(row = .I, blk = ((o - 1L) %% n_blocks) + 1L)
  }, by = strat_col]

  sort(assign_dt[blk == block, row])
}

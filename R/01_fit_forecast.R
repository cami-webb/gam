# GAM forecast — tree growth and mortality
# Called by run_forecast.R or directly: Rscript R/01_fit_forecast.R

.libPaths(c("/projectnb/dietzelab/cwebb16/R_libs", .libPaths()))

library(aws.s3)
library(yaml)
library(readr)
library(dplyr)
library(mgcv)

cfg  <- read_yaml("config.yml")
tcfg <- cfg$targets[[cfg$target]]
set.seed(cfg$model$random_seed)

# ── S3 setup ───────────────────────────────────────────────────────────────────
Sys.setenv(
  AWS_ACCESS_KEY_ID     = Sys.getenv("OSN_KEY"),
  AWS_SECRET_ACCESS_KEY = Sys.getenv("OSN_SECRET"),
  AWS_DEFAULT_REGION    = ""
)
base_url <- gsub("https://", "", cfg$s3$endpoint)

# ── Load covariates from S3 ───────────────────────────────────────────────────
message("Reading covariates from S3...")
covariate_url <- paste0(cfg$s3$endpoint, "/", cfg$s3$read_bucket, "/", tcfg$covariate_s3_key)
covs <- readr::read_csv(covariate_url, show_col_types = FALSE) |>
  mutate(log_dbh_init = log(dbh_init)) |>
  filter(dbh_init > 0, is.finite(log_dbh_init))

# Species grouping (parametric factor term in GAM)
common_species <- covs |> count(species, sort = TRUE) |>
  filter(n >= 20) |> pull(species)
covs <- covs |> mutate(species_grp = factor(
  if_else(species %in% common_species, species, "Other"),
  levels = c(sort(common_species), "Other")
))

# ── Build GAM formulas ────────────────────────────────────────────────────────
# Continuous covariates get s() smooth terms; species_grp is parametric
k      <- cfg$model$k
method <- cfg$model$method

num_covs <- tcfg$covariates  # species_grp excluded — added separately as parametric

smooth_terms <- paste(
  sapply(num_covs, function(v) sprintf("s(%s, k=%d)", v, k)),
  collapse = " + "
)

growth_formula <- as.formula(paste(
  "growth_cm_yr ~ s(log_dbh_init, k=", k, ") + species_grp +", smooth_terms
))

mort_formula <- as.formula(paste(
  "dead_0621 ~ s(log_dbh_init, k=", k, ") + species_grp +", smooth_terms
))

# ── Fit and forecast for each interval ────────────────────────────────────────
all_forecasts <- list()

for (iv in tcfg$intervals) {
  message(sprintf("\n=== Interval %s (%d -> %d) ===", iv$name, iv$start_year, iv$end_year))

  train_g <- covs |>
    filter(!is.na(growth_cm_yr), is.na(dead_0621) | dead_0621 == 0L) |>
    filter(!is.na(log_dbh_init)) |>
    filter(if_all(all_of(intersect(num_covs, names(covs))), ~ !is.na(.)))

  train_m <- covs |>
    filter(!is.na(dead_0621)) |>
    filter(!is.na(log_dbh_init)) |>
    filter(if_all(all_of(intersect(num_covs, names(covs))), ~ !is.na(.)))

  # --- Growth GAM ---
  message("  Fitting growth GAM (n=", nrow(train_g), ")")
  gam_growth <- tryCatch(
    mgcv::gam(growth_formula, data = train_g, method = method),
    error = function(e) { message("  Growth GAM failed: ", e$message); NULL }
  )

  # --- Mortality GAM ---
  message("  Fitting mortality GAM (n=", nrow(train_m), ")")
  gam_mort <- tryCatch(
    mgcv::gam(mort_formula, data = train_m, family = binomial, method = method),
    error = function(e) { message("  Mortality GAM failed: ", e$message); NULL }
  )

  target_datetime <- as.Date(paste0(iv$end_year,   "-01-01"))
  ref_datetime    <- as.Date(paste0(iv$start_year, "-01-01"))

  rows <- list()

  if (!is.null(gam_growth)) {
    pred_growth <- predict(gam_growth, newdata = train_g, type = "response")
    message(sprintf("  Growth R²: %.3f", cor(train_g$growth_cm_yr, pred_growth)^2))
    rows$growth <- data.frame(
      model_id           = tcfg$model_id,
      datetime           = target_datetime,
      reference_datetime = ref_datetime,
      site_id            = train_g$tree_id,
      family             = "normal",
      parameter          = "mu",
      variable           = "growth_cm_yr",
      prediction         = pred_growth,
      project_id         = cfg$project_id,
      duration           = tcfg$duration,
      interval           = iv$name
    )
  }

  if (!is.null(gam_mort)) {
    pred_mort <- predict(gam_mort, newdata = train_m, type = "response")
    message(sprintf("  Mortality deviance explained: %.1f%%", summary(gam_mort)$dev.expl * 100))
    rows$mort <- data.frame(
      model_id           = tcfg$model_id,
      datetime           = target_datetime,
      reference_datetime = ref_datetime,
      site_id            = train_m$tree_id,
      family             = "bernoulli",
      parameter          = "prob",
      variable           = "dead_0621",
      prediction         = pred_mort,
      project_id         = cfg$project_id,
      duration           = tcfg$duration,
      interval           = iv$name
    )
  }

  if (length(rows) > 0) all_forecasts[[iv$name]] <- bind_rows(rows)
}

# ── Combine and write to S3 ───────────────────────────────────────────────────
combined      <- bind_rows(all_forecasts)
forecast_file <- paste0("tree-", Sys.Date(), "-gam.csv")

write_csv(combined, forecast_file)

s3_key <- paste0(cfg$s3$submissions_path, "/", forecast_file)
message("\nUploading to s3://", cfg$s3$write_bucket, "/", s3_key)

aws.s3::put_object(
  file      = forecast_file,
  object    = s3_key,
  bucket    = cfg$s3$write_bucket,
  base_url  = base_url,
  use_https = TRUE,
  region    = ""
)

unlink(forecast_file)
message("Done. Rows written: ", nrow(combined))

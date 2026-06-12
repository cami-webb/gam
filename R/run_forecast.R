# Entry point — dispatches to the correct target script
# Rscript R/run_forecast.R

.libPaths(c("/projectnb/dietzelab/cwebb16/R_libs", .libPaths()))
library(yaml)

cfg    <- read_yaml("config.yml")
target <- cfg$target
message("Running GAM forecast for target: ", target)

if (target == "tree") {
  source("R/01_fit_forecast.R")
} else if (target == "coastal") {
  stop("Coastal GAM not yet implemented — add R/01_fit_forecast_coastal.R")
} else if (target == "urban") {
  stop("Urban GAM not yet implemented — add R/01_fit_forecast_urban.R")
} else {
  stop("Unknown target: ", target)
}

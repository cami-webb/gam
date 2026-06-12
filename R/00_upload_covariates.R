# Upload tree_covariates.csv to S3 read bucket metadata folder
# Run once manually from SCC when covariate file is updated
#
# Before running, set your S3 keys in the shell:
#   export OSN_KEY=your_key
#   export OSN_SECRET=your_secret
#
# Usage: Rscript R/00_upload_covariates.R

.libPaths(c("/projectnb/dietzelab/cwebb16/R_libs", .libPaths()))

library(aws.s3)
library(yaml)

cfg <- read_yaml("config.yml")

Sys.setenv(
  AWS_ACCESS_KEY_ID     = Sys.getenv("OSN_KEY"),
  AWS_SECRET_ACCESS_KEY = Sys.getenv("OSN_SECRET"),
  AWS_DEFAULT_REGION    = ""
)

local_file <- "/projectnb/dietzelab/cwebb16/FRP/Urban/Tree/data/processed/tree_covariates.csv"
s3_key     <- cfg$targets$tree$covariate_s3_key
bucket     <- cfg$s3$read_bucket
base_url   <- gsub("https://", "", cfg$s3$endpoint)

message("Uploading ", local_file)
message("  -> s3://", bucket, "/", s3_key)

aws.s3::put_object(
  file      = local_file,
  object    = s3_key,
  bucket    = bucket,
  base_url  = base_url,
  use_https = TRUE,
  region    = ""
)

message("Done.")

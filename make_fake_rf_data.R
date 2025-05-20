# ==============================================================================
# project setup
# ==============================================================================

# ------------------------------------------------------------------------------
# paths
# ------------------------------------------------------------------------------

proj_root <- here::here()

# scripts
script_dir <- fs::path(proj_root, "R")

# data
data_dir <- fs::path(proj_rootm, "data")
data_meta <- fs::path(data_dir, "00_qnr_metadata")
data_micro <- fs::path(data_dir, "01_microdata")

# ------------------------------------------------------------------------------
# environment
# ------------------------------------------------------------------------------

# set a random seed for reproducibility
base::set.seed(8765309)

# ------------------------------------------------------------------------------
# load utility functions
# ------------------------------------------------------------------------------

util_fun_paths <- fs::dir_ls(
  path = script_dir,
  type = "file",
  regexp = "utils_"
)

purrr::walk(
  .x = util_fun_paths,
  .f = ~ source(file = .x)
)

# ==============================================================================
# generate random data, orchestrating script execution
# ==============================================================================

scripts <- c(
  "00_ingest_qnr_metadata.R",
  "01_create_hhold.R",
  "02_create_members.R"
)

purrr::walk(
  .x = scripts,
  .f = ~ source(file = fs::path(script_dir, .x))
)

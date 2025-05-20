# ==============================================================================
# ingest data
# ==============================================================================

# questionnaire in JSON
qnr_df <- susometa::parse_questionnaire(
  path = here::here("data", "00_qnr_metadata", "01_raw", "document.json")
)

# categories in Excel
reusable_categories_df <- susometa::parse_categories(
  dir = here::here("data", "00_qnr_metadata", "01_raw", "Categories")
)

# ==============================================================================
# write processed data to disk
# ==============================================================================

# questionnaire metadata
saveRDS(
  object = qnr_df,
  file = here::here(
    "data", "00_qnr_metadata", "02_processed",
    "qnr_df.rds"
  )
)

# reusable categories
saveRDS(
  object = reusable_categories_df,
  file = here::here(
    "data", "00_qnr_metadata", "02_processed",
    "reusable_categories_df"
  )
)

# ==============================================================================
# get answer options from metadata
# ==============================================================================

# ------------------------------------------------------------------------------
# list questions by section
# ------------------------------------------------------------------------------

housing_vars <- c(
  "s11q2", # dwelling ownership
  "s11q7", # has occupancy document
  "s11q8", # type of occupancy document
  "s11q16", # roof material
  "s11q17", # floor material
  "s11q15" # wall material
)

wash_vars <- c(
  "s13q11", # toilet type
  "s13q2", # drinking water
  "s13q3", # where water source is located
  "s13q4", # water collection time
  "s13q5", # treat water
  "s13q6", # water treatment type
  "s13q19" # refuse disposal type
)

digital_vars <- c(
  "s11q19" # access internet hhold
)

# shocks
shocks_var <- "s16aq1"

# ------------------------------------------------------------------------------
# extract answer options and labels for target questions
# ------------------------------------------------------------------------------

val_lbls <- c(
  housing_vars,
  wash_vars,
  digital_vars
) |>
  # set names of vector as its character value
  purrr::set_names() |>
  purrr::map(
    .f = ~ susometa::get_answer_options(
      qnr_df = qnr_df,
      categories_df = reusable_categories_df,
      varname = !!rlang::sym(.x)
    )
  )


# ==============================================================================
# construct data
# ==============================================================================

# create household-level data with SuSo IDs
hholds_core <- survey_sample_w_probabilties |>
  # expand PSU-level data into hhold-level data
  # creating a number of hhold obs per PSU equal to `n_hholds`
	tidyr::uncount(weights = n_hholds) |>
  # add a SuSo-style interview ID as the first column
	dplyr::mutate(
    interview__id = uuid::UUIDgenerate(n = dplyr::n()),
    .before = 1
  )

# compute the number of households for later use
n_obs <- nrow(hholds_core)

# construct
hhold_vars_df <- val_lbls |>
  # create named lists of sampled values
  # where the names are variable names from `val_lbls`
  purrr::imap(
    .f = ~ sample(
      x = unname(.x),
      size = n_obs,
      replace = TRUE
    )
  ) |>
  tibble::as_tibble()

# ------------------------------------------------------------------------------
# create shocks
# ------------------------------------------------------------------------------

# construct a list of shocks
shocks <- tibble::tribble(
  ~ name, ~ code,
  "Drought", 101,
  "Floods", 102,
  "Torrential/excessive rains", 103,
  "Tropical cyclones/hurricanes/typhoons/tornados", 104,
  "Heatwaves", 105,
  "Cold waves", 106,
  "Landslides", 201,
  "Earthquakes", 202,
  "Volcano eruptions", 203,
  "Wildfires", 204,
  "Insect infestations (e.g., locust)", 205,
  "Pest and disease outbreaks", 206,
  "Animal disease outbreaks (e.g., bird flu, swine flu, bovine tuberculosis, brucellosis)", 301,
  "Human health crisis, epidemic/pandemic (e.g., ebola, cholera, Covid-19)", 302,
  "Conflict, local unrest/violence", 401,
  "Theft/robbery/kidnapping", 402,
  "Cyber-attacks (e.g. phishing attacks, password attacks, malware, ransomware)", 403,
  "Substantial increase in food prices", 501,
  "Substantial increase in fuel/transport prices", 502,
  "Substantial increase in price of production inputs (excluding fuel)", 503,
  "Substantial drop in price of production output", 504,
  "Job loss of household member (not related to death, illness, or disability)", 505,
  "Household (non-agricultural) business failure or closure", 506,
  "Death, income lost due to illness, cost of illness or disability of income-earning household member", 601,
  "Death, cost of illness or disability of other (non-income-earning) household member", 602,
  "Departure of income-earning household member (e.g.,  due to separation, divorce, changing to another household)", 603,
  "End of regular assistance, aid, or remittances from outside the household", 604,
)

shocks_yes_no <- shocks |>
  # construct name of desired variables
  # drawing on root name and code
	dplyr::mutate(varname = paste0(shocks_var, "__", code)) |>
  # extract this from the data frame
	dplyr::pull(varname) |>
	purrr::map_dfc(
    # for each variable, create a data frame of fake data
    .f = ~ tibble::tibble(
      !!rlang::sym(.x) := sample(
        x = c(1, 0),
        size = n_obs,
        replace = TRUE
      )
    )
  )

# combine data and apply variable labels
hholds <- hholds_core |>
	dplyr::bind_cols(
    hhold_vars_df, shocks_yes_no
  ) |>
  # label ID variables
  labelled::set_value_labels(.labels = list(
    region = region,
    urb_rur = urb_rur
  )) |>
  # label non-ID household variables
	labelled::set_value_labels(.labels = val_lbls) |>
	# replace values with logical missing based on skip patterns
  dplyr::mutate(
    # only ask occupancy doc if occupancy is authorized
    s11q7 = dplyr::if_else(
      condition = s11q2 %in% c(1, 2, 4),
      true = s11q7,
      false = NA
    ),
    # only get occupancy doc type if have occupancy doc
    s11q8 = dplyr::if_else(
      condition = s11q7 == 1,
      true = s11q8,
      false = NA
    ),
    # water collection time only asked for certain soruces/locations
    s13q4 = dplyr::if_else(
      condition = (
        # source located elsewhere or
        s13q3 == 3 |
        # source is piped to neighbor / water kiosk
        s13q2 %in% c(3, 13)
      ),
      true = s13q4,
      false = NA
    ),
    # water treatment method only asked if water is treated
    s13q6 = dplyr::if_else(
      condition = s13q5 == 1,
      true = s13q6,
      false = NA
    )
  )

# ==============================================================================
# write data to disk
# ==============================================================================

haven::write_dta(
  data = hholds,
  path = here::here("data", "01_microdata", "rf_hholds.dta")
)

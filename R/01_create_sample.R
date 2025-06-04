# ==============================================================================
# create sampling frame
# ==============================================================================

# ------------------------------------------------------------------------------
# compose strata and their sizes (manually)
# ------------------------------------------------------------------------------

region <- c(
  "Hauts-Bassins" = 1,
  "Boucle du Mouhoum" = 2,
  "Sahel" = 3,
  "Est" = 4,
  "Sud-Ouest" = 5,
  "Centre-Nord" = 6,
  "Centre-Ouest" = 7,
  "Plateau-Central" = 8,
  "Nord" = 9,
  "Centre-Est" = 10,
  "Centre" = 11,
  "Cascades" = 12,
  "Centre-Sud" = 13
)

urb_rur <- c(
  "Urbain" = 1,
  "Rural" = 2
)

n_hholds_by_stratum <- tidyr::crossing(region, urb_rur) |>
  dplyr::mutate(
    n_hholds = dplyr::case_when(
      # Boucle du Mouhoun (2) 358471
      region == 2 & urb_rur == 1 ~ 41333,
      region == 2 & urb_rur == 2 ~ 317138,
      # Cascades (12) 145464
      region == 12 & urb_rur == 1 ~ 34953,
      region == 12 & urb_rur == 2 ~ 110511,
      # Centre (11) 718603
      region == 11 & urb_rur == 1 ~ 582378,
      region == 11 & urb_rur == 2 ~ 136225,
      # Centre-Est (10) 292917
      region == 10 & urb_rur == 1 ~ 60239,
      region == 10 & urb_rur == 2 ~ 232678,
      # Centre-Nord (6) 318471
      region == 6 & urb_rur == 1 ~ 40773,
      region == 6 & urb_rur == 2 ~ 277698,
      # Centre-Ouest (7) 289333
      region == 7 & urb_rur == 1 ~ 64479,
      region == 7 & urb_rur == 2 ~ 224854,
      # Centre-Sud (13) 146526
      region == 13 & urb_rur == 1 ~ 19039,
      region == 13 & urb_rur == 2 ~ 127487,
      # Est (4) 316757
      region == 4 & urb_rur == 1 ~ 30704,
      region == 4 & urb_rur == 2 ~ 286053,
      # Hauts-Bassins (1) 447866
      region == 1 & urb_rur == 1 ~ 223908,
      region == 1 & urb_rur == 2 ~ 223958,
      # Nord (9) 294650
      region == 9 & urb_rur == 1 ~ 51987,
      region == 9 & urb_rur == 2 ~ 242663,
      # Plateau Central (8) 168091
      region == 8 & urb_rur == 1 ~ 20615,
      region == 8 & urb_rur == 2 ~ 147476,
      # Sahel (3) 234442
      region == 3 & urb_rur == 1 ~ 28045,
      region == 3 & urb_rur == 2 ~ 206397,
      # Sud-Ouest (5) 175503
      region == 5 & urb_rur == 1 ~ 27359,
      region == 5 & urb_rur == 2 ~ 148144,
      .default = NA
    ),
    stratum_id = dplyr::row_number()
  )

# ------------------------------------------------------------------------------
# create a simulated sampling frame of PSUs
# by transforming PSU hhold counts into a set of PSUs
# ------------------------------------------------------------------------------

sampling_frame <- n_hholds_by_stratum |>
  # rename variable to fit argument names
  dplyr::rename(stratum_size = n_hholds) |>
  # select the variables needed as arguments
  dplyr::select(region, urb_rur, stratum_size) |>
  # apply the function to each stratum
  # using the columns of the data frame as parameter values
  # creating an appropriate number of appropriately sized PSUs
	purrr::pmap_dfr(.f = create_psus)

# ==============================================================================
# select PSUs from the sampling frame
# ==============================================================================

# ------------------------------------------------------------------------------
# specify the sampling design
# ------------------------------------------------------------------------------

n_psu_per_stratum <- 20
n_hholds_per_psu <- 10

# compose a sampling design data frame
# for simplicity, select the same number of PSUs for each stratum
sampling_design <- n_hholds_by_stratum |>
  dplyr::mutate(n_psu = n_psu_per_stratum)

# ------------------------------------------------------------------------------
# select PSUs independently for each stratum using systematic PPS
# ------------------------------------------------------------------------------

selected_psus <- sampling_design |>
  # select only those variables needed as parameter values for the function
  dplyr::select(region, urb_rur, n_psu) |>
  # apply the PSU selection function to each stratum
  purrr::pmap(
    .f = select_psus,
    sampling_frame = sampling_frame
  ) |>
  # combine the list of data frames into a single data frame
  purrr::list_rbind()

# ==============================================================================
# update PSU sizes by simulating a listing exercise that results in random
# adjustments updwards and downwords of sizes from the initial frame
# ==============================================================================

selected_psus_updated_sizes <- selected_psus |>
  dplyr::mutate(
    # select a random % adjustment factor
    fraction_adjustment = sample(
      x = c(85:120),
      size = nrow(selected_psus),
      replace = TRUE
    ),
    # apply that factor to obtain an updated measure of size
    psu_size_actual = round(psu_size * (fraction_adjustment / 100))
  )

# ==============================================================================
# compute the weights for households
# ==============================================================================

survey_sample_w_probabilties <- selected_psus_updated_sizes |>
	dplyr::mutate(

    # --------------------------------------------------------------------------
    # compute probability of selection
    # --------------------------------------------------------------------------

    # 01 - PSU in the stratum
    # using PSU size from the initial frame
    p_select_psu_in_stratum = (n_psu_per_stratum * psu_size) / (stratum_size),
    # 02 - household in the PSU
    # using updated PSU size after listing
    p_select_hhold_in_psu = (n_hholds_per_psu) / (psu_size_actual),
    # 03 - household overall
    p_hhold = p_select_psu_in_stratum * p_select_hhold_in_psu,

    # --------------------------------------------------------------------------
    # 02 - compute weights
    # --------------------------------------------------------------------------

    weight_hhold = 1 / p_hhold

  ) |>
  # add a variable to "expand" the data to be household-level
  dplyr::mutate(n_hholds = n_hholds_per_psu) |>
	dplyr::select(
    # identifiers
    region, urb_rur, psu_id,
    # count varible
    n_hholds,
    # weights
    weight_hhold
  )

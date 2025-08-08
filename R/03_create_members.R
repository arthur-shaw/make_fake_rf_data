# ==============================================================================
# get answer options from metadata
# ==============================================================================

# ------------------------------------------------------------------------------
# list questions by section
# ------------------------------------------------------------------------------

demographic_vars <- c(
  # variables not covered by other operations
  "s1q6" # birth registered
)

health_vars <- c(
  "s3q5", # had health event
  "s3q8", # had health consultation
  "s3q9", # health facility visited
  "s3q24", # hospitalized
  "s3q26", # whether needed treatment but could not get it
  "s3q27" # reason no treatment
)

functioning_vars <- c(
  "s3cq1", # difficulty seeing
  "s3cq2", # difficulty hearing
  "s3cq3", # difficulty movement
  "s3cq4", # difficulty cognitive
  "s3cq5", # difficulty care
  "s3cq6" # difficulty communication
)

digital_vars <- c(
  "s5q3", # have mobile phone
  "s5q4", # access mobile phone
  "s5q6" # access internet member
)

# ------------------------------------------------------------------------------
# extract answer options and labels for target questions
# ------------------------------------------------------------------------------

val_lbls <- c(
  demographic_vars,
  health_vars,
  functioning_vars,
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

basic_member_var_lbls <- c(
  "s1q2", # member gender
  "s1q3", # relationship to head
  "s1q4alt", # member age
  "s1q7" # marital status
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

# ------------------------------------------------------------------------------
# create scaffolding of IDs and obs
# ------------------------------------------------------------------------------

# ingest household data, keeping the IDs to build on
hhold_ids <- here::here("data", "01_microdata", "rf_hholds.dta") |>
  haven::read_dta() |>
  # specify average household size by stratum
  # using/adapting figures from here: https://www.facebook.com/RGPH5/posts/rgph2019_r%C3%A9sultats_pr%C3%A9liminairesle-rgph-2019-a-d%C3%A9nombr%C3%A9-3-907-094-m%C3%A9nages-au-bur/720600455327444/
  dplyr::select(interview__id, region, urb_rur) |>
	dplyr::mutate(
    mean_hhsize = dplyr::case_when(
      # Boucle du Mouhoun (2) 5.3
      region == 2 & urb_rur == 1 ~ 5.1,
      region == 2 & urb_rur == 2 ~ 5.5,
      # Cascades (12) 5.6
      region == 12 & urb_rur == 1 ~ 5.5,
      region == 12 & urb_rur == 2 ~ 5.8,
      # Centre (11) 4.2
      region == 11 & urb_rur == 1 ~ 3.5,
      region == 11 & urb_rur == 2 ~ 4.5,
      # Centre-Est (10) 5.4
      region == 10 & urb_rur == 1 ~ 4.5,
      region == 10 & urb_rur == 2 ~ 5.6,
      # Centre-Nord (6) 5.9
      region == 6 & urb_rur == 1 ~ 5.1,
      region == 6 & urb_rur == 2 ~ 6.2,
      # Centre-Ouest (7) 5.7
      region == 7 & urb_rur == 1 ~ 5.2,
      region == 7 & urb_rur == 2 ~ 6,
      # Centre-Sud (13) 5.4
      region == 13 & urb_rur == 1 ~ 4.35,
      region == 13 & urb_rur == 2 ~ 5.7,
      # Est (4) 6.1
      region == 4 & urb_rur == 1 ~ 5.1,
      region == 4 & urb_rur == 2 ~ 6.5,
      # Hauts-Bassins (1) 5.0
      region == 1 & urb_rur == 1 ~ 4.95,
      region == 1 & urb_rur == 2 ~ 5.2,
      # Nord (9) 5.8
      region == 9 & urb_rur == 1 ~ 5,
      region == 9 & urb_rur == 2 ~ 6.3,
      # Plateau Central (8) 5.8
      region == 8 & urb_rur == 1 ~ 4.95,
      region == 8 & urb_rur == 2 ~ 6.1,
      # Sahel (3) 4.7
      region == 3 & urb_rur == 1 ~ 4.5,
      region == 3 & urb_rur == 2 ~ 4.9,
      # Sud-Ouest (5) 5.0
      region == 5 & urb_rur == 1 ~ 4.7,
      region == 5 & urb_rur == 2 ~ 5.1,
      .default = NA
    )
  )

# create the scaffolding for a member-level data frame
members_core_df <- hhold_ids |>
  # create a random household size per household
  # drawing from a truncated Poisson distribution
  # where the mean is defined by the stratum-specific values above
  # performing the coputation row-wise, for now, to avoid coding complications
  dplyr::rowwise() |>
	dplyr::mutate(
    n_members = round(truncdist::rtrunc(
      n = 1,
      spec = "pois",
      a = 1,
      b = 20,
      lambda = mean_hhsize
    ))
  ) |>
  dplyr::ungroup() |>
  # create a number of duplicate rows equal to the household size
	tidyr::uncount(
    weights = n_members,
    # .remove = FALSE,
    .id = "members__id"
  )

# ------------------------------------------------------------------------------
# members key info
# ------------------------------------------------------------------------------


age_rules <- tibble::tibble(
  rel_code = list(
    1, # HEAD (1)
    2, # SPOUSE (2)
    c(
      3, # CHILD (OWN CHILD/STEP CHILD/ ADOPTED CHILD) (3)
      6, # NIECE/NEPHEW (6)
      10 # SON-IN-LAW/DAUGHTER-IN-LAW (10)
    ),
    4, # GRANDCHILD (4)
    c(
      5, # BROTHER/SISTER (5)
      7 # BROTHER/SISTER-IN-LAW (7)
    ),
    c(
      8, # PARENT (8)
      9 # PARENT-IN-LAW (9)
    ),
    c(
      11, # DOMESTIC HELP (RESIDENT) (11)
      12, # OTHER RELATION    (12)
      13 # OTHER NON-RELATION (13)
    )
  ),
  rule = list(
    # HEAD (1)
    function(head_age) head_age,
    # SPOUSE (2)
    function(head_age) sample(c((head_age - 5):(head_age + 5)), 1),
    # CHILD (OWN CHILD/STEP CHILD/ ADOPTED CHILD) (3)
    # SON-IN-LAW/DAUGHTER-IN-LAW (10)
    # NIECE/NEPHEW (6)
    function(head_age) sample(max(0, head_age - 40):(head_age - 13), 1),
    # GRANDCHILD (4)
    function(head_age) sample(max(0, head_age - 70):(head_age - 40), 1),
    # BROTHER/SISTER (5)
    # BROTHER/SISTER-IN-LAW (7)
    function(head_age) sample((head_age - 10):(head_age + 10), 1),
    # PARENT (8)
    # PARENT-IN-LAW (9)
    function(head_age) sample((head_age + 13):(head_age + 40), 1),
    # DOMESTIC HELP (RESIDENT) (11)
    # OTHER RELATION    (12)
    # OTHER NON-RELATION (13)
    function(head_age) sample(1:105, size = 1)
  )
) |>
  # expand data set so that each relationship code has a row
	tidyr::unnest_longer(rel_code)

members_key_info <- members_core_df |>
  dplyr::group_by(interview__id) |>
  # split the data frame into a list of data frames so that functions
  # can be applied individually to each data frame
  tidyr::nest() |>
	dplyr::mutate(
    data = purrr::map(
      .x = data,
      .f = ~ .x  |>
        select_head(var = relationship, head_val = 1) |>
        select_spouse(var = relationship, head_val = 1, spouse_val = 2) |>
        assign_other_relationships(
          var = relationship,
          head_val = 1,
          spouse_val = 2,
          relationship_codes = c(3:12),
          relationship_probs = c(0.5, rep(.5/9, times = 9))
        )
    )
  ) |>
  # combine the list of data frames back into a grouped data frame
	tidyr::unnest(c = c(data)) |>
	# ungroup in order to join in age rules
  dplyr::ungroup() |>
  dplyr::left_join(
    age_rules,
    by = c("relationship" = "rel_code")
  ) |>
  # revert to a grouped data for group-level operations
	dplyr::group_by(interview__id) |>
  dplyr::mutate(
    head_age = round(
      x = truncnorm::rtruncnorm(
        n = 1,
        a = 16, b = 85,
        mean = 35, sd = 15
      ),
      digits = 0
    ),
    age = purrr::map2_dbl(
      .x = relationship,
      .y = rule,
      .f = function(relationhip = .x, rule = .y) {
        rule(head_age[1])
      }
    )
  ) |>
  # re-nest data as list of data frames so that functions return a data frame
  tidyr::nest() |>
  dplyr::mutate(
    data = purrr::map(
      .x = data,
      .f = ~ .x |>
        assign_head_sex(relationship_var = relationship, sex_var = sex) |>
        assign_member_sex(relationship_var = relationship, sex_var = sex) |>
        assign_marital_status(
          relationship_var = relationship,
          age_var = age,
          marital_var = marital
        )
    )
  ) |>
  # combine list of data frames into a single data frame
  tidyr::unnest(cols = c(data)) |>
	dplyr::ungroup() |>
  # remove age rule function column
	dplyr::select(-rule) |>
	# rename variables
  dplyr::rename(
    s1q2 = sex,
    s1q3 = relationship,
    s1q4alt = age,
    s1q7 = marital
  )

# ------------------------------------------------------------------------------
# create data frame of random values for target variables
# ------------------------------------------------------------------------------

members_vars_df <- val_lbls |>
  # create named lists of sampled values
  # where the names are variable names from `val_lbls`
  purrr::imap(
    .f = ~ sample(
      x = unname(.x),
      size = nrow(members_key_info),
      replace = TRUE
    )
  ) |>
  tibble::as_tibble()

# ------------------------------------------------------------------------------
# combine and refine the data
# ------------------------------------------------------------------------------

members <- dplyr::bind_cols(members_key_info, members_vars_df) |>
  # apply value labels
	labelled::set_value_labels(.labels = basic_member_var_lbls) |>
	labelled::set_value_labels(.labels = val_lbls) |>
  dplyr::mutate(
    # remove health consultation answer if had no health event
    s3q8 = dplyr::if_else(
      condition = s3q5 == 2,
      true = NA_integer_,
      false = s3q8
    ),
    # remove health facility visited if either had no health event or
    # had no consultation in past 30 days
    s3q9 = dplyr::if_else(
      condition = (
        # no health event
        (s3q5 == 2) |
        # no consultation in past 30 days
        (s3q8 == 2)
      ),
      true = NA_integer_,
      false = s3q9
    ),
    # remove reason could not get care if actually could
    s3q27 = dplyr::if_else(
      condition = s3q26 == 2,
      true = NA_integer_,
      false = s3q27
    ),
    # remove access to someone else's phone if own phone
    s5q4 = dplyr::if_else(
      condition = s5q3 == 1,
      true = NA_integer_,
      false = s5q4
    ),
    # remove digital access if less than 15 years old
    dplyr::across(
      .cols = c(s5q3, s5q4, s5q6),
      .fns = ~ dplyr::if_else(
        condition = s1q4alt < 15,
        true = NA_integer_,
        false = .x
      )
    )
  )


# ==============================================================================
# write data to disk
# ==============================================================================

haven::write_dta(
  data = members,
  path = here::here("data", "01_microdata", "members.dta")
)

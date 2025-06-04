#' Randomly select the household head in the household
#'
#' @description
#'
#' Process:
#' - Select the ith row
#' - Assign relationship value of `head_val` to variable `var`
#'
#' @param df Data frame
#' @param var Bare variabe name. Relationship to head.
#' @param head_val Atomic numeric. Relationship value denoting the head.
#'
#' @return Data frame
#'
#' @importFrom dplyr mutate if_else row_number
select_head <- function(
  df,
  var,
  head_val = 1
) {

  head_index <- sample(
    x = seq_len(nrow(df)),
    size = 1
  )

  df_w_head <- df |>
    dplyr::mutate(
      {{var}} := dplyr::if_else(
        condition = dplyr::row_number() == head_index,
        true = head_val,
        false = NA_integer_
      )
    )

  return(df_w_head)

}

#' Randomly select the spouse in the household
#'
#' @description
#' Process:
#' - Identify the head
#' - Select among all non-head members
#' - Assign `spouse_val` to the variable `var`
#'
#' @inheritParams select_head
#' @param spouse_val Atomic numeric.
#' Relationship value denoting the spouse of the household head.
#'
#' @importFrom rlang as_label enquo
#' @importFrom dplyr mutate if_else row_number
select_spouse <- function(
  df,
  var,
  head_val = 1,
  spouse_val = 2
) {

  if (nrow(df) > 1) {

    var_txt <- rlang::as_label(rlang::enquo(var))
    head_index <- which(df[[var_txt]] == 1)

    spouse_index <- sample(
      x = setdiff(x = seq_len(nrow(df)), y = head_index),
      size = 1
    )

    df_w_spouse <- df |>
      dplyr::mutate(
        {{var}} := dplyr::if_else(
          condition = dplyr::row_number() == spouse_index,
          true = spouse_val,
          false = {{var}}
        )
      )

  } else {

    return(df)

  }

}

#' Randomly assign relationship variables for non-head and non-spouse members.
#'
#' @inheritParams select_spouse
#' @param relationship_codes Numeric vector. Codes for relationships to head.
#' @param relationship_probs Numeric vector. Probabilities of each relationship.
#'
#' @return Data frame
#'
#' @importFrom dplyr mutate if_else
assign_other_relationships <- function(
  df,
  var,
  head_val = 1,
  spouse_val = 2,
  relationship_codes,
  relationship_probs
) {

  df_w_relationships <- df |>
    dplyr::mutate(
      {{var}} := dplyr::if_else(
        condition = is.na({{var}}),
        true = sample(
          x = relationship_codes,
          prob = relationship_probs,
          size = nrow(df)
        ),
        false = {{var}}
      )

    )

  return(df_w_relationships)

}

assign_head_sex <- function(
  df,
  relationship_var,
  head_val = 1,
  sex_var,
  male_val = 1,
  female_val = 2,
  prob_male = 0.8,
  prob_female = 0.2
) {

  df_w_head_gender <- df |>
    dplyr::mutate(
      {{sex_var}} := dplyr::if_else(
        condition = {{relationship_var}} == head_val,
        true = sample(
          x = c(male_val, female_val),
          size = 1,
          prob = c(prob_male, prob_female)
        ),
        false = NA_integer_
      )
    )

  return(df_w_head_gender)

}

assign_member_sex <- function(
  df,
  relationship_var,
  head_val = 1,
  sex_var,
  male_val = 1,
  female_val = 2
) {

  # extract the sex of the household head as an atomic vector
  head_sex <- df |>
    dplyr::filter({{relationship_var}} == 1) |>
    dplyr::pull({{sex_var}})

  df_w_member_genders <- df |>
    dplyr::mutate(
      {{sex_var}} := dplyr::case_when(
        # keep head sex the same (as previously set by another function)
        {{relationship_var}} == 1 ~ {{sex_var}},
        # set spouse sex as opposite of head's
        {{relationship_var}} == 2 ~ dplyr::if_else(
          condition = head_sex == male_val,
          true = female_val,
          false = male_val
        ),
        # select a random gender for all other relationships
        ! {{relationship_var}} %in% c(1, 2) ~ sample(
          x = c(male_val, female_val),
          replace = TRUE,
          size = nrow(df)
        )
      )
    )

  return(df_w_member_genders)

}

assign_marital_status <- function(
  df,
  relationship_var,
  head_val = 1,
  spouse_val = 2,
  age_var,
  marital_var,
  married_mono_val = 1,
  married_poly_val = 2,
  informal_union_val = 3,
  divorced_val = 4,
  separated_val = 5,
  widowed_val = 6,
  never_married_val = 7
) {

  marital_statuses <- c(
    married_mono_val,
    married_poly_val,
    informal_union_val,
    divorced_val,
    separated_val,
    widowed_val,
    never_married_val
  )

  couple_size <- df |>
    dplyr::filter({{relationship_var}} %in% c(head_val, spouse_val)) |>
    nrow()

  marital_status_couple <- sample(
    x = c(married_mono_val, informal_union_val),
    size = 1,
    prob = c(
      0.7, # married (monogamous)
      0.2 # informal union
    )
  )

  df_w_marital_status <- df |>
    dplyr::mutate(
      {{marital_var}} := dplyr::case_when(
        # couple is only the head
        (couple_size == 1) & ({{relationship_var}} == head_val) ~ sample(
          x = c(divorced_val, separated_val, widowed_val, never_married_val),
          size = 1
        ),
        # couple is the head and spouse
        (couple_size == 2) & ({{relationship_var}} %in% c(head_val, spouse_val)) ~
          marital_status_couple,
        # couple is the head and multiple spouses
        (couple_size > 2)  & ({{relationship_var}} %in% c(head_val, spouse_val)) ~
          married_poly_val,
        # all other members
        ! {{relationship_var}} %in% c(head_val, spouse_val) & {{age_var}} >= 12 ~
          # have age-variant probability spaces and probabilities
          dplyr::case_when(
            # youngest are unmarried
            {{age_var}} %in% c(12:17) ~ never_married_val,
            # younger are more apt not to be married
            {{age_var}} %in% c(18:30) ~ sample(
              x = c(divorced_val, separated_val, widowed_val, never_married_val),
              size = nrow(df),
              replace = TRUE,
              prob = c(
                0.05, # divorced
                0.05, # separated
                0.05, # widowed
                0.85 # never married
              )
            ),
            # middle age are more likely to be divorced or separated
            {{age_var}} %in% c(31:60) ~ sample(
              x = c(divorced_val, separated_val, widowed_val, never_married_val),
              size = nrow(df),
              replace = TRUE,
              prob = c(
                0.4, # divorced
                0.2, # separated
                0.3, # widowed
                0.1 # never married
              )
            ),
            # older are more apt to be widowed
            {{age_var}} > 60 ~ sample(
              x = c(divorced_val, separated_val, widowed_val, never_married_val),
              size = nrow(df),
              replace = TRUE,
              prob = c(
                0.3, # divorced
                0.1, # separated
                0.6, # widowed
                0.0 # never married
              )
            )
          ),
        # all other cases
        .default = NA_integer_
      )
    )

  return(df_w_marital_status)

}

#' Create a data frame of PSUs for the target stratum
#'
#' @param region Integer, atomic. Region code.
#' @param urb_rur Integer, atomic. Urban/rural code.
#' @param stratum_size Integer, atomic. Total number of households in the target
#' stratum.
#' @param min_psu_size Integer, atomic. Minimum expected size of a PSU.
#' @param max_psu_size Integer, atomic. Maximum expected size of a PSU.
#'
#' @return Data frame of PSUs and their measure of size (n. hholds). Columns:
#' - `region`
#' - `urb_rur`
#' - `psu_size`
#'
#' @importFrom tibble tibble
#' @importFrom dplyr mutate row_number
create_psus <- function(
  region,
  urb_rur,
  stratum_size,
  min_psu_size = 50,
  max_psu_size = 250
) {

  # initialize:
  # 1. array to contain set of PSU sizes (initally empty)
  # 2. remaining households (initially equal to stratum size)
  psu_sizes <- c()
  remaining_hholds <- stratum_size

  # create an array of PSU sizes whose values falls between the min and max size
  # repeating until all households have been allocated to a PSU
  repeat {

    # --------------------------------------------------------------------------
    # handle the final PSU
    # --------------------------------------------------------------------------

    # case 1: remaining households would fit into a PSU of desired size
    if (remaining_hholds <= max_psu_size && remaining_hholds >= min_psu_size) {
      # append remaining households to the array of PSU sizes
      psu_sizes <- c(psu_sizes, remaining_hholds)
      break
    }

    # case 2: remaining hholds less than min PSU size, put in previous PSU
    if (remaining_hholds < 2 * min_psu_size) {
      # edit the last element in the PSU size array
      # adding the remaining households to its value
      psu_sizes[length(psu_sizes)] <- psu_sizes[length(psu_sizes)] + remaining
      break
    }

    # --------------------------------------------------------------------------
    # handle non-final PSUs
    # --------------------------------------------------------------------------

    # draw a PSU size within bounds, but not more than remaining hholds
    # first, determine the maximum hholds than can be drawn
    max_draw <- min(remaining_hholds - min_psu_size, max_psu_size)
    # then, create a PSU of size ranging between min PSU size and max draw
    psu_size <- sample(
      x = min_psu_size:max_draw,
      size = 1
    )

    # add newly created PSU to array of PSU sizes
    psu_sizes <- c(psu_sizes, psu_size)
    # while updating remaining households
    remaining_hholds <- remaining_hholds - psu_size

  }

  # construct a tibble with the IDs and PSU sizes
  psus_df <- tibble::tibble(
    region = region,
    urb_rur = urb_rur,
    psu_size = psu_sizes
  ) |>
	dplyr::mutate(psu_id = dplyr::row_number())

  return(psus_df)

}

#' Select the PSU containing the target cumulative size
#'
#' @param df Data frame. Strata sampling frame, where `cum_psu_size` is the
#' cumulative measure of size.
#' @param cum_size Integer, atomic. Size of the desired selection.
#'
#' @return Data frame. Single-row data frame containing the target PSU.
#'
#' @importFrom dplyr filter slice
select_psu <- function(
  df,
  cum_size
) {

  df_filtered <- df |>
    # select PSUs with cumulative size or greater
    dplyr::filter(.data$cum_psu_size >= cum_size) |>
    # select the first record
    dplyr::slice(1)

  return(df_filtered)

}

#' Select N random PSUs from the strata's sampling frame
#'
#' @param sampling_frame Data frame. Sampling frame for the full population.
#' @param region Integer, atomic. Code for the target region
#' @param urb_rur Integer, atomic. Code for the target urban/rural zone
#' @param n_psu Integer, atomic. Number of PSUs to select from the strata
#' defined by `region` and `urb_rur`.
#'
#' @return Data frame. One row for each selected PSU.
#'
#' @importFrom 
select_psus <- function(
  sampling_frame,
  region,
  urb_rur,
  n_psu
) {

  # compose a sampling frame from the stratum
  stratum_frame <- sampling_frame |>
    # subset frame to current stratum
    dplyr::filter(.data$region == region & .data$urb_rur == urb_rur) |>
    # create cumultative size
    dplyr::mutate(cum_psu_size = base::cumsum(psu_size))

  # compute size
  stratum_total_size <- sum(stratum_frame$psu_size, na.rm = TRUE)

  # compute sampling interval
  sampling_interval <- stratum_total_size / n_psu

  # compute random start
  random_start <- sample(
    x = 1:sampling_interval,
    size = 1
  )

  # compute random selections
  selections_as_sizes <- random_start + sampling_interval * (0:(n_psu-1))

  # construct
  psus_selected_df <- selections_as_sizes |>
    # iterate over elements of the selection size array
    # returning a list of data frames
    # where each data frame corresponds to a selected PSU
    purrr::map(
      .f = ~ select_psu(
        df = stratum_frame,
        cum_size = .x
      )
    ) |>
    # combine the list of data frames into a single data frame
    purrr::list_rbind() |>
    # add info to facilitate weight calculation
    dplyr::mutate(
      stratum_size = .env$stratum_total_size,
      n_psus = .env$n_psu
    )

  return(psus_selected_df)

}

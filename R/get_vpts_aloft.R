#' Get VPTS data from aloftdata
#'
#' This function retrieves VPTS (Vertical Profile Time Series) data from the
#' aloftdata server.
#'
#' - Constructs the S3 paths for the VPTS files based on the input
#' - Performs parallel HTTP requests to fetch the vpts-csv data
#' - Parses the response bodies with some assumptions about the column classes
#' - Adds a column with the radar source
#' - Overwrites the radar column with the radar_odim_code, all other values for
#' this column are considered in error.
#'
#'
#' @param radar_odim_code The radar ODIM code.
#' @param rounded_interval The interval to fetch data for, rounded to nearest
#'   day
#' @param source The source of the data. One of baltrad, uva or ecog-04003.
#' @param coverage A data.frame containing the coverage of the aloft data
#'   repository. If not provided, it will be fetched from via the internet.
#' @return A list of vpts data.frames
#'
#' @importFrom dplyr .data
#' @importFrom lubridate %within%
#'
#' @examples
#' get_vpts_aloft("bejab",
#'                lubridate::interval("20240305", "20240307"),
#'                "baltrad")
#'
#' @noRd
get_vpts_aloft <- function(radar_odim_code,
                           rounded_interval,
                           source,
                           coverage = get_aloft_coverage()) {

  # rename source argument for readability
  selected_source <- source

  # Check that only one radar is provided (string of length 1)
  if(!rlang::is_string(radar_odim_code)) {
    cli::cli_abort(
      "Please provide (only one) radar as a character vector of length 1.",
      class = "getRad_error_radar_not_single_string")
  }

  # Check that radar_odim_code is a single 5 character string
  if (nchar(radar_odim_code) != 5) {
    cli::cli_abort(
      "Radar ODIM code must be a single 5 character string.",
      class = "getRad_error_radar_odim_code_invalid")
  }

  # Check if the requested radars are present in the coverage
  if(!all(radar_odim_code %in% coverage$radar)) {
    cli::cli_abort(
      "Radar(s) not found in ALOFT coverage:
      {radar_odim_code[!radar_odim_code %in% coverage$radar]}.",
      class = "getRad_error_aloft_radar_not_found")}

  # Check if the requested date radar combination is present in the coverage
  at_least_one_radar_date_combination_exists <-
    dplyr::filter(coverage,
                  .data$radar %in% radar_odim_code,
                  .data$date %within% rounded_interval) |>
    nrow() > 0

  if(!at_least_one_radar_date_combination_exists) {
    cli::cli_abort(
      "No data found for the requested radar(s) and date(s).",
      class = "getRad_error_date_not_found")
  }

  # Filter the coverage data to the selected radars and time interval and
  # convert into paths on the aloft s3 storage
  ## We need to use the rounded interval because coverage only has daily
  ## resolution
  s3_paths <- dplyr::filter(coverage,
                            .data$radar == radar_odim_code,
                            .data$date %within% rounded_interval,
                            .data$source == selected_source) |>
    dplyr::pull(.data$directory) |>
    # Replace hdf5 with daily to fetch vpts files instead of hdf5 files
    string_replace("hdf5", "daily") |>
    # Construct the filename using regex magic!
    string_replace(
      "(.*?)/(.....)/(\\d{4})/(\\d{2})/(\\d{2})$",
      "\\1/\\2\\/\\3/\\2_vpts_\\3\\4\\5.csv"
    )

  # Read the vpts csv files
  aloft_data_url <-"https://aloftdata.s3-eu-west-1.amazonaws.com"

  ## this could also be done by passing the vector of urls to readr::read_csv()
  ## or vroom::vroom(), but both would be slower because they are not parallel
  ## and wouldn't declare our custom user agent or allow us to set retry
  ## criteria

  paste(aloft_data_url, s3_paths, sep = "/") |>
    read_vpts_from_url() |>
    # Add a column with the radar source to not lose this information
    purrr::map2(s3_paths, ~dplyr::mutate(.x,
                                         source = string_extract(.y,
                                                                 ".+(?=\\/daily)")
    )
    ) |>
    purrr::list_rbind() |>
    # Move the source column to the front, where it makes sense
    dplyr::relocate("source", .before = "radar") |>
    # Overwrite the radar column with the radar_odim_date, all other values are
    # considered invalid for aloft
    dplyr::mutate(radar = radar_odim_code)
}

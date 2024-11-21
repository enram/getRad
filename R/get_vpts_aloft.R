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
#'   this column are considered in error.
#'
#'
#' @param radar_odim_code The radar ODIM code.
#' @param rounded_interval The interval to fetch data for, rounded to nearest day
#' @param source The source of the data. One of baltrad, uva or ecog-04003.
#' @param coverage A data.frame containing the coverage of the aloft data repository.
#'
#' @return A list of vpts data.frames
#'
#' @importFrom dplyr .data
#' @importFrom lubridate %within%
#'
#' @examples
#' get_vpts_aloft("ABC", "2022-01-01/2022-01-02", "source1", coverage_data)
#'
#' @noRd
get_vpts_aloft <- function(radar_odim_code,
                           rounded_interval,
                           selected_source,
                           coverage) {
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

  # Filter the coverage data to the selected radars and time interval and
  # convert into paths on the aloft s3 storage
  ## We need to use the rounded interval because coverage only has daily
  ## resolution
  s3_paths <- dplyr::filter(coverage,
                            .data$radar %in% radar_odim_code,
                            .data$date %within% rounded_interval,
                            .data$source %in% selected_source) |>
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
  ## and wouldn't declare our custom user agent

  paste(aloft_data_url, s3_paths, sep = "/") |>
    purrr::map(httr2::request) |>
    # Identify ourselves in the request
    purrr::map(req_user_agent_getrad) |>
    # Set retry conditions
    purrr::map(~ httr2::req_retry(
      .x,
      max_tries = 15, backoff = \(x) sqrt(x) * 2,
      is_transient = \(resp) resp_status(resp) %in% c(429),
      retry_on_failure = TRUE
    )) |>
    # Perform the requests in parallel
    httr2::req_perform_parallel() |>
    # Fetch the response bodies and parse it using vroom
    ## A helper in bioRad (validate_vpts()) that we call indirectly via
    #" bioRad::as.vpts() currently doesn't support factors: bioRad v0.8.1
    purrr::map(httr2::resp_body_string) |>
    purrr::map(~ vroom::vroom(delim = ",",
                              I(.x),
                              col_types =
                                list(
                                  radar = vroom::col_factor(),
                                  datetime = vroom::col_datetime(),
                                  height = vroom::col_integer(),
                                  u = vroom::col_double(),
                                  v = vroom::col_double(),
                                  w = vroom::col_double(),
                                  ff = vroom::col_double(),
                                  dd = vroom::col_double(),
                                  sd_vvp = vroom::col_double(),
                                  gap = vroom::col_logical(),
                                  eta = vroom::col_double(),
                                  dens = vroom::col_double(),
                                  dbz = vroom::col_double(),
                                  dbz_all = vroom::col_double(),
                                  n = vroom::col_integer(),
                                  n_dbz = vroom::col_integer(),
                                  n_all = vroom::col_integer(),
                                  n_dbz_all = vroom::col_integer(),
                                  rcs = vroom::col_double(),
                                  sd_vvp_threshold = vroom::col_double(),
                                  vcp = vroom::col_integer(),
                                  radar_longitude = vroom::col_double(),
                                  radar_latitude = vroom::col_double(),
                                  radar_height = vroom::col_integer(),
                                  radar_wavelength = vroom::col_double(),
                                  source_file = vroom::col_character()
                                ),
                              show_col_types = NULL,
                              progress = FALSE)) |>
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

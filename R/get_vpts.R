#' Title
#'
#' @param radar
#' @param date
#'
#' @return
#' @importFrom lubridate %within%
#'
#' @export
#'
#' @examples
get_vpts <- function(radar, date) {
  # Rename radar argument so it's clear that it can contain multiple radars
  selected_radars <- radar

  # Check that the provided radar argument is a character vector
  if (!is.character(selected_radars)) {
    cli::cli_abort(
      "Radar argument must be a character vector.",
      class = "getRad_error_radar_not_single_string")
  }

  # Check that the provided date argument is parsable as a date or interval
  if (!is.character(date) && !is.Date(date) && !is.Interval(date)) {
    cli::cli_abort(
      "Date argument must be a character, Date, or Interval object.",
      class = "getRad_error_date_parsable")
  }
  # Parse the provided date argument to a lubridate interval
  ## If the date is a single date, convert it to an interval
  if (class(date) != "Interval") {
    date_interval <- lubridate::interval(date, date)
  }

  # Discover what data is available for the requested radar and time interval
  coverage_url <- "https://aloftdata.s3-eu-west-1.amazonaws.com/coverage.csv"
  coverage <-
    readr::read_csv(coverage_url, progress = FALSE, show_col_types = FALSE) |>
    dplyr::mutate(source = string_extract(directory, ".+(?=\\/hdf5)"),
                  radar = string_extract(directory, "(?<=hdf5\\/)[a-z]{5}"),
                  date = as.Date(
                    string_extract(directory,
                                   "[0-9]{4}\\/[0-9]{2}\\/[0-9]{2}$")
                    )
    )

  # Filter the coverage data to the selected radars and time interval and
  # convert into paths on the aloft s3 storage
  s3_paths <- dplyr::filter(coverage,
                radar %in% selected_radars,
                date %within% date_interval) |>
    dplyr::pull(directory) |>
    # Replace hdf5 with daily to fetch vpts files instead of hdf5 files
    string_replace("hdf5", "daily") |>
    # Construct the filename using regex magic!
    string_replace(
      "(.*?)/(.....)/(\\d{4})/(\\d{2})/(\\d{2})$",
      "\\1/\\2\\/\\3/\\2_vpts_\\3\\4\\5.csv"
    )

  # Read the vpts csv files
  aloft_data_url <-"https://aloftdata.s3-eu-west-1.amazonaws.com"

  paste(aloft_data_url, s3_paths, sep = "/") |>
    purrr::map(httr2::request) |>
    httr2::req_perform_parallel()


  #https://aloftdata.s3-eu-west-1.amazonaws.com/baltrad/daily/bewid/2023/bewid_vpts_20230101.csv

}

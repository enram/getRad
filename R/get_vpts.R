#' Retrieve vertical profile time series data from the Aloft data repository
#'
#' @inheritParams get_pvol
#' @param date Either a single date or a [lubridate::interval]
#' @param source The source of the data. One of baltrad, uva or ecog-04003.
#' If not provided, data from all the available sources will be returned.
#' @param as_vpts Logical. By default the data is returned as a
#' [bioRad::summary.vpts] object. If set to FALSE, a data.frame will be
#' returned instead with an extra column for the radar source.
#' @return By default, a vpts object is returned. See [bioRad::summary.vpts]
#' for more information. When multiple radars are selected, a list of vpts
#' objects will be returned instead. When `as_vpts = FALSE`, a single
#' data.frame is returned with an extra column for the radar source.
#'
#' @importFrom dplyr .data
#'
#' @export
#'
#' @examplesIf interactive()
#' # Fetch vpts data for a single radar and date
#' get_vpts(radar = "bejab", date = "2023-01-01")
#' # Fetch vpts data for multiple radars and a single date
#' get_vpts(radar = c("dehnr", "deflg"), date = lubridate::ymd("20171015"))
#' # Fetch vpts data for a single radar and a date range
#' get_vpts(
#'     radar = "bejab",
#'     date = lubridate::interval(lubridate::ymd_hms("2023-01-01 00:00:00"),
#'                                lubridate::ymd_hms("2023-01-02 00:14:00")
#'                                  )
#'                                )
#' get_vpts("bejab", lubridate::interval("20210101","20210301"))
#' # Fetch vpts data for a single radar and a date range from a specific source
#' get_vpts(radar = "bejab", date = "2018-05-18", source = "baltrad")
#' # Return a data.frame instead of a vpts object
#' get_vpts(radar = "chlem", date = "2023-03-10", as_vpts = FALSE)
#'
get_vpts <- function(radar,
                     date,
                     source = c("baltrad", "uva", "ecog-04003"),
                     as_vpts = TRUE) {
  # Rename radar & source arguments so it's clear that it can contain multiple
  # radars
  selected_radars <- radar
  selected_sources <- source

  # Check that the provided radar argument is a character vector
  if (!is.character(selected_radars)) {
    cli::cli_abort(
      "Radar argument must be a character vector.",
      class = "getRad_error_radar_not_character")
  }

  # Check that the provided date argument is parsable as a date or interval
  if (!is.character(date) &&
      !lubridate::is.Date(date) &&
      !lubridate::is.interval(date)) {
    cli::cli_abort(
      "Date argument must be a character, Date, or Interval object.",
      class = "getRad_error_date_parsable")
  }
  # Parse the provided date argument to a lubridate interval
  ## If the date is a single date, convert it to an interval by adding a whole
  ## day, minus a second
  if (!inherits(date, "Interval")) {
    date_interval <-
      lubridate::interval(
        lubridate::as_datetime(date),
        lubridate::as_datetime(date) +
          lubridate::ddays(1) -
          lubridate::dseconds(1)
      )
  } else {
    date_interval <- date
  }


  # Discover what data is available for the requested radar and time interval
  coverage_url <- "https://aloftdata.s3-eu-west-1.amazonaws.com/coverage.csv"
  coverage <-
    vroom::vroom(coverage_url, progress = FALSE, show_col_types = FALSE) |>
    dplyr::mutate(source = string_extract(.data$directory, ".+(?=\\/hdf5)"),
                  radar = string_extract(.data$directory, "(?<=hdf5\\/)[a-z]{5}"),
                  date = as.Date(
                    string_extract(.data$directory,
                                   "[0-9]{4}\\/[0-9]{2}\\/[0-9]{2}$")
                    )
    )

  # Check if the requested radars are present in the coverage
  if(!all(selected_radars %in% coverage$radar)) {
    cli::cli_abort(
      "Radar(s) not found in ALOFT coverage:
      {selected_radars[!selected_radars %in% coverage$radar]}.",
      class = "getRad_error_radar_not_found")
  }

  # Check if the requested date radar combination is present in the coverage
  ## We need to round the interval because coverage only has daily resolution
  rounded_interval <-
    lubridate::interval(
      lubridate::floor_date(lubridate::int_start(date_interval), "day"),
      lubridate::ceiling_date(lubridate::int_end(date_interval), "day")
    )

  at_least_one_radar_date_combination_exists <-
    dplyr::filter(coverage,
                radar %in% selected_radars,
                date %within% rounded_interval) |>
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
                radar %in% selected_radars,
                date %within% rounded_interval,
                source %in% selected_sources) |>
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

  vpts_from_s3 <-
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
    # Fetch the response bodies and parse it using readr
    purrr::map(httr2::resp_body_string) |>
    purrr::map(~vroom::vroom(delim = ",",
      .x,
      col_types =
        list(
          radar = vroom::col_factor(),
          datetime = vroom::col_datetime(),
          height = vroom::col_integer(),
          u=vroom::col_double(),
          v=vroom::col_double(),
          w=vroom::col_double(),
          ff=vroom::col_double(),
          dd=vroom::col_double(),
          sd_vvp=vroom::col_double(),
          gap = vroom::col_logical(),
          eta=vroom::col_double(),
          dens=vroom::col_double(),
          dbz=vroom::col_double(),
          dbz_all=vroom::col_double(),
          n = vroom::col_integer(),
          n_dbz = vroom::col_integer(),
          n_all = vroom::col_integer(),
          n_dbz_all = vroom::col_integer(),
          rcs=vroom::col_double(),
          sd_vvp_threshold=vroom::col_double(),
          vcp = vroom::col_integer(),
          radar_longitude=vroom::col_double(),
          radar_latitude=vroom::col_double(),
          radar_height = vroom::col_integer(),
          radar_wavelength = vroom::col_double(),
          source_file = vroom::col_factor()
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
    dplyr::relocate("source", .before = "radar")


  # Drop any results outside the requested interval
  filtered_vpts <-
    vpts_from_s3 |>
    dplyr::mutate(datetime = lubridate::as_datetime(.data$datetime)) |>
    dplyr::filter(.data$datetime %within% date_interval)

  # Return the vpts data
  ## By default, return drop the source column and convert to a vpts object for
  ## usage in bioRAD
  if (as_vpts) {

    filtered_vpts_no_source <- dplyr::select(filtered_vpts, -source)

    vpts_list <- split(filtered_vpts_no_source,
                       filtered_vpts_no_source$radar) |>
      purrr::map(~bioRad::as.vpts(.x)) |>
      purrr::set_names(unique(filtered_vpts$radar))
    # If we are only returning a single radar, don't return a list
    if(length(vpts_list) == 1) {
      return(purrr::chuck(vpts_list, 1))
    }
    return(vpts_list)
  } else {
  ## If as_vpts is set to FALSE, return as a tibble with the source column
    return(filtered_vpts)
  }
}

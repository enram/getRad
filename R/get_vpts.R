#' Retrieve vertical profile time series data from the Aloft data repository
#'
#' @inheritParams get_pvol
#' @param date Either a single date or a [lubridate::interval]
#' @param source The source of the data. One of baltrad, uva or ecog-04003. Only
#'   one source can be queried at a time. This is a required argument.
#' @param as_tibble Logical. By default the data is returned as a
#'   [bioRad::summary.vpts] object. If set to TRUE, a [dplyr::tibble()] will be
#'   returned instead with an extra column for the radar source.
#' @return By default, a vpts object is returned. See [bioRad::summary.vpts] for
#'   more information. When multiple radars are selected, a list of vpts objects
#'   will be returned instead. When `as_tibble = TRUE`, a single
#'   [dplyr::tibble()] is returned with an extra column for the radar source.
#'
#' @importFrom dplyr .data
#' @importFrom lubridate %within%
#' @export
#'
#' @examplesIf interactive()
#'
#'   # Fetch vpts data for a single radar and date
#'   get_vpts(radar = "bejab", date = "2023-01-01", source = "baltrad")
#'
#'   # Fetch vpts data for multiple radars and a single date
#'
#'   get_vpts(radar = c("dehnr", "deflg"), date = lubridate::ymd("20171015"),
#'   source = "baltrad")
#'
#'   # Fetch vpts data for a single radar and a date range
#'
#'   get_vpts(radar = "bejab", date = lubridate::interval(
#'   lubridate::ymd_hms("2023-01-01 00:00:00"), lubridate::ymd_hms("2023-01-02
#'   00:14:00") ), source = "baltrad" )
#'
#'   get_vpts("bejab", lubridate::interval("20210101","20210301"), "bejab")
#'
#'   # Fetch vpts data for a single radar and a date range from a specific
#'   source
#'
#'   get_vpts(radar = "bejab", date = "2016-09-29", source = "ecog-04003")
#'
#'   # Return a tibble instead of a vpts object
#'
#'   get_vpts(radar = "chlem", date = "2023-03-10", source = "baltrad",
#'   as_tibble = TRUE)
#'
get_vpts <- function(radar,
                     date,
                     source = c("baltrad", "uva", "ecog-04003"),
                     as_tibble = FALSE) {
  # Check source argument
  ## Check if the source argument was provided, return error if not.
  if(missing(source) || is.null(source)) {
    # providing NULL isn't allowed either
    cli::cli_abort(
      glue::glue(
        "Please provide a value for the source argument:
        possible values are {possible_sources}.",
        possible_sources = glue::glue_collapse(glue::backtick(source),
                                               sep = ", ",
                                               last = " or ")
      ),
      class = "getRad_error_source_missing")
  }

  ## Only a single source can be fetched from at a time, and it must be one of
  ## the provided values in the enumeration. New sources must also be added to
  ## the enumeration in the function definition.
  if(length(source) > 1) {
    cli::cli_abort(
      "Only one source can be queried at a time.",
      class = "getRad_error_multiple_sources")
  }

  ## The provided source must be one of the supported values in the enumeration

  # Get the default value of the source arg, even if the user provided
  # a different value.
  supported_sources <- eval(formals()$source)
  if(!source %in% supported_sources) {
    cli::cli_abort(
      glue::glue(
        "Invalid source {glue::backtick(source)} provided. Possible values are:
        {possible_sources}.",
        possible_sources = glue::glue_collapse(
          glue::backtick(supported_sources),
          sep = ", ",
          last = " or ")
      ),
      class = "getRad_error_source_invalid")
  }

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

  ## We need to round the interval because coverage only has daily resolution
  rounded_interval <-
    lubridate::interval(
      lubridate::floor_date(lubridate::int_start(date_interval), "day"),
      lubridate::ceiling_date(lubridate::int_end(date_interval), "day")
    )

  # Discover what data is available for the requested radar and time interval
  coverage <- get_aloft_coverage()

  # Check if the requested radars are present in the coverage
  found_radars <-
    dplyr::filter(coverage,
                  .data$source %in% selected_sources,
                  .data$radar %in% selected_radars) |>
    dplyr::pull(radar)
  missing_radars <- setdiff(selected_radars, found_radars)

  if(!all(selected_radars %in% coverage$radar)) {
    cli::cli_abort(
      "{length(missing_radars)} Radar{?s} not found in {source} coverage:
      {glue::backtick(missing_radars)}",
      class = "getRad_error_radar_not_found")}

  # Query the selected radars and fetched coverage for aloft vpts data.
  vpts_from_s3 <-
    purrr::map(
      selected_radars,
      ~ get_vpts_aloft(
        .x,
        rounded_interval = rounded_interval,
        source = selected_sources,
        coverage = coverage
      )
    ) |>
    radar_to_name()

  # Drop any results outside the requested interval
  filtered_vpts <-
    vpts_from_s3 |>
    purrr::map(
      \(df) dplyr::mutate(df,
                          datetime = lubridate::as_datetime(.data$datetime))
      ) |>
    purrr::map(
      \(df) dplyr::filter(df,
                          .data$datetime %within% date_interval)
      )

  # Return the vpts data
  ## By default, return drop the source column and convert to a vpts object for
  ## usage in bioRAD
  if (!as_tibble) {

    filtered_vpts_no_source <-
      purrr::map(filtered_vpts, \(df) dplyr::select(df, -source))

    vpts_list <- purrr::map(filtered_vpts_no_source, bioRad::as.vpts)
    # If we are only returning a single radar, don't return a list
    if(length(vpts_list) == 1) {
      return(purrr::chuck(vpts_list, 1))
    }
    return(vpts_list)
  } else {
  ## If as_vpts is set to FALSE, return as a tibble with the source column
    purrr::list_rbind(filtered_vpts)
  }
}

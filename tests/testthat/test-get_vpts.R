test_that("get_vpts() can fetch vpts data for a single radar and time", {
  skip_if_offline()
  single_radar_single_day <-
    get_vpts(radar = "bejab",
             date = "2023-01-01",
             source = "baltrad",
             as_vpts = FALSE)
  expect_s3_class(
    # A known radar and date combination that ALOFT has data for
    single_radar_single_day,
    "data.frame"
  )

  # Expect a known length so no rows have been accidentally filtered away
  expect_identical(
    nrow(single_radar_single_day),
    7125L
  )
})

test_that("get_vpts() can fetch vpts data for multiple radars", {
  skip_if_offline()
  multiple_radars <- get_vpts(radar = c("bejab", "bewid"),
                              date = "2023-01-01",
                              source = "baltrad",
                              as_vpts = FALSE)
  expect_s3_class(
    multiple_radars,
    "data.frame"
  )

  expect_contains(
    multiple_radars$radar,
    c("bejab", "bewid")
  )
})

test_that("get_vpts() can fetch data from a single radar source", {
  skip_if_offline()
  # Only data from UVA available for this radar day
  expect_identical(
    get_vpts(radar = "bejab",
             date = "2018-02-02",
             source = "uva",
             as_vpts = FALSE)$source |>
      unique(),
    "uva"
  )
  # radar day has data both on UVA and BALTRAD
  expect_identical(
    get_vpts(radar = "bejab",
             date = "2018-05-18",
             source = "baltrad",
             as_vpts = FALSE)$source |>
      unique(),
    "baltrad"
  )

  expect_identical(
    get_vpts(radar = "bejab",
             date = "2018-05-18",
             source = "uva",
             as_vpts = FALSE)$source |>
      unique(),
    "uva"
  )
})

test_that("get_vpts() returns columns of the expected type and order", {
  skip_if_offline()

  expected_col_types <-
    list(
      source = "character",
      radar = "factor",
      datetime = c("POSIXct", "POSIXt"),
      height = "integer",
      u = "numeric",
      v = "numeric",
      w = "numeric",
      ff = "numeric",
      dd = "numeric",
      sd_vvp = "numeric",
      gap = "logical",
      eta = "numeric",
      dens = "numeric",
      dbz = "numeric",
      dbz_all = "numeric",
      n = "integer",
      n_dbz = "integer",
      n_all = "integer",
      n_dbz_all = "integer",
      rcs = "numeric",
      sd_vvp_threshold = "numeric",
      vcp = "integer",
      radar_latitude = "numeric",
      radar_longitude = "numeric",
      radar_height = "integer",
      radar_wavelength = "numeric",
      source_file = "factor"
    )

  expect_identical(
    get_vpts(radar = c("deflg"),
             date = lubridate::ymd("20171015"),
             source = "baltrad",
             as_vpts = FALSE) |>
      purrr::map(class),
    expected_col_types
  )

  # Specific radar causing trouble
  expect_identical(
    get_vpts(radar = c("dehnr"),
             date = lubridate::ymd("20171015"),
             source = "uva",
             as_vpts = FALSE) |>
      purrr::map(class),
    expected_col_types
  )
})

test_that("get_vpts() can fetch data from a specific source only", {
  skip_if_offline()

  # Data from only BALTRAD even if UVA is available for the same interval
  expect_identical(
    get_vpts(radar = "bejab",
             date = "2018-05-18",
             source = "baltrad",
             as_vpts = FALSE) |>
      dplyr::pull("source") |>
      unique(),
    "baltrad"
  )
})

test_that("get_vpts() can fetch vpts data for a date range", {
  skip_if_offline()

  radar_interval <- get_vpts(radar = "bejab",
                             lubridate::interval(
                               lubridate::ymd("2023-01-01"),
                               lubridate::ymd("2023-01-02")
                             ),
                             source = "baltrad",
                             as_vpts = FALSE
  )
  expect_s3_class(
    radar_interval,
    "data.frame"
  )

  # Check that the requested dates are present in the output
  expect_in(
    unique(as.Date((radar_interval$datetime))),
    c(as.Date("2023-01-01"), as.Date("2023-01-02"))
  )

})

test_that("get_vpts() supports date intervals with hours and minutes",{
  skip_if_offline()

  hour_minute_interval <-
    get_vpts(radar = "bejab",
             lubridate::interval(
               lubridate::ymd_hms("2023-01-01 12:00:00"),
               lubridate::ymd_hms("2023-01-01 16:59:59")
             ),
             source = "baltrad",
             as_vpts = FALSE
    )

  expect_s3_class(
    hour_minute_interval,
    "data.frame"
  )

  # The minimum returned date should be within the interval
  expect_gte(
    min(hour_minute_interval$datetime),
    lubridate::ymd_hms("2023-01-01 12:00:00")
  )
  # The maximum returned datetime should be within the interval
  expect_lte(
    max(hour_minute_interval$datetime),
    lubridate::ymd_hms("2023-01-01 16:59:59")
  )

  # The maximum should actually be rounded by a 15 minute interval
  expect_identical(
    max(hour_minute_interval$datetime),
    lubridate::ymd_hms("2023-01-01 16:45:00")
  )
})

test_that("get_vpts() can return data as a vpts object compatible with getRad",{
  skip_if_offline()

  return_as_vpts_object <- get_vpts(radar = "depro",
                                    date = "2016-03-05",
                                    source = "uva",
                                    as_vpts = TRUE)

  list(
    radar = list("bejab","depro","bejab","bejab"),
    date = list("2018-05-18","2016-03-05","2018-05-31",lubridate::interval("2018-05-31 18:00:00",
                                                     "2018-16-01 02:00:00"))
  ) |> purrr::pmap(get_vpts)

  expect_s3_class(
    return_as_vpts_object,
    "vpts"
  )
  expect_type(
    return_as_vpts_object,
    "list"
  )
  # The returned object should be identical as if created via bioRad
  expect_identical(
    get_vpts(radar = "depro", date = "2016-03-05", as_vpts = FALSE) |>
      dplyr::select(-source) |>
      bioRad::as.vpts()
    ,
    return_as_vpts_object
  )
  # This also works when multiple radars are selected
  ## In this case a list of vpts objects should be returned
  expect_type(
    get_vpts(radar = c("depro", "bejab"),
             date = "2016-03-05",
             as_vpts = TRUE),
    "list"
  )

  expect_identical(
    get_vpts(radar = c("depro", "bejab"),
             date = "2016-03-05",
             as_vpts = TRUE) |>
      purrr::map_chr(class) |>
      unname(), #only test for class, names are tested elsewhere
    c("vpts","vpts")
  )

  ## This list should be named the same as the requested radars
  requested_radars <- c("depro", "bejab")
  expect_named(
    get_vpts(radar = c("depro", "bejab"),
             date = "2016-03-05",
             as_vpts = TRUE),
    requested_radars
  )

  ## The named child objects should correspond to the correct vpts objects (bug)
  expect_identical(
    get_vpts(radar = c("depro", "bejab"),
             date = "2016-03-05",
             as_vpts = TRUE) |>
      purrr::chuck("bejab"),
    get_vpts(radar = "bejab", date = "2016-03-05", as_vpts = TRUE)
  )
})

test_that("get_vpts() returns an error when multiple sources are provided", {
  skip_if_offline()

  expect_error(
    get_vpts(radar = "bejab",
             date = "2018-05-18",
             source = c("baltrad", "uva")),
    class = "getRad_error_multiple_sources"
  )
})

test_that("get_vpts() returns an error when an invalid source is provided",{
  expect_error(
    get_vpts("bejab","20241118","not a source"),
    class = "getRad_error_source_invalid"
  )

  expect_error(
    get_vpts("bejab","20241118","baltradz"),
    class = "getRad_error_source_invalid"
  )
})

test_that("get_vpts() returns an error when no source is provided", {
  expect_error(
    get_vpts("bejab","20180501"),
    class = "getRad_error_source_missing"
  )

  expect_error(
    get_vpts("bejab","20180501", source = NULL),
    class = "getRad_error_source_missing"
  )
})

test_that("get_vpts() returns an error for a bad radar", {
  skip_if_offline()

  # Radar not found in ALOFT coverage
  expect_error(
    get_vpts(radar = "notaradar", date = "2023-01-01"),
    class = "getRad_error_radar_not_found"
  )
  # Radar is not a character vector
  expect_error(
    get_vpts(radar = 1:3, date = "2023-01-01"),
    class = "getRad_error_radar_not_character"
  )
})

test_that("get_vpts() returns an error for a bad time argument", {
  skip_if_offline()

  # Date not found in ALOFT coverage
  expect_error(
    get_vpts(radar = "bejab", date = "9000-01-02"),
    class = "getRad_error_date_not_found"
  )
  # Time is not parsable to a date or interval
  expect_error(
    get_vpts(radar = "bejab", date = 1:3),
    class = "getRad_error_date_parsable"
  )
})

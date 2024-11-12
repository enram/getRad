test_that("get_vpts() can fetch vpts data for a single radar and time", {
  single_radar_single_day <- get_vpts(radar = "bejab", date = "2023-01-01")
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
  multiple_radars <- get_vpts(radar = c("bejab", "bewid"), date = "2023-01-01")
  expect_s3_class(
    multiple_radars,
    "data.frame"
  )

  expect_contains(
    multiple_radars$radar,
    c("bejab", "bewid")
  )
})

test_that("get_vpts() can fetch data from multiple radar sources", {
  # Data from UVA
  expect_contains(
    get_vpts(radar = "bejab", date = "2018-02-02")$source,
    "uva"
  )
  # Data both on UVA and BALTRAD
  expect_contains(
    get_vpts(radar = "bejab", date = "2018-05-18")$source,
    c("uva", "baltrad")
  )
})



test_that("get_vpts() can fetch vpts data for a date range", {
  radar_interval <- get_vpts(radar = "bejab",
                             lubridate::interval(
                               lubridate::ymd("2023-01-01"),
                               lubridate::ymd("2023-01-02")
                             )
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
  hour_minute_interval <-
    get_vpts(radar = "bejab",
             lubridate::interval(
               lubridate::ymd_hms("2023-01-01 12:00:00"),
               lubridate::ymd_hms("2023-01-01 16:59:59")
             )
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

test_that("get_vpts() returns an error for a bad radar", {
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

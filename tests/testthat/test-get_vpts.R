test_that("get_vpts() can fetch vpts data for a single radar and time", {
  expect_s3_class(
    # A known radar and date combination that ALOFT has data for
    get_vpts(radar = "bejab", date = "2023-01-01"),
    "data.frame"
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

test_that("get_vpts() can fetch vpts data for a date range", {
  expect_s3_class(
    get_vpts(radar = "bejab",
             lubridate::interval(
               lubridate::ymd("2023-01-01"),
               lubridate::ymd("2023-01-02")
             )
    ),
    "data.frame"
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
    get_vpts(radar = "bejab", date = "2023-01-02"),
    class = "getRad_error_date_not_found"
  )
  # Time is not parsable to a date or interval
  expect_error(
    get_vpts(radar = "bejab", date = 1:3),
    class = "getRad_error_date_parsable"
  )
})

coverage <- get_aloft_coverage()

test_that("get_vpts_aloft() returns error on invalid odim code", {
  # Radar is not 5 character string
  expect_error(
    get_vpts_aloft(
      radar = "beja",
      rounded_interval = lubridate::interval("2023-01-01", "2023-01-02"),
      source = "uva",
      coverage
    ),
    class = "getRad_error_radar_odim_code_invalid"
  )
  # radar is not a string
  expect_error(
    get_vpts_aloft(
      radar = 12345,
      rounded_interval = lubridate::interval("2023-01-01", "2023-01-02"),
      source = "uva",
      coverage
    ),
    class = "getRad_error_radar_not_single_string"
  )
})

test_that("get_vpts_aloft() returns error when multiple radars are queried", {
  expect_error(
    get_vpts_aloft(
      radar = c("bejab", "depro"),
      rounded_interval = lubridate::interval("2023-01-01", "2023-01-02"),
      source = "uva",
      coverage
    ),
    class = "getRad_error_radar_not_single_string"
  )
})

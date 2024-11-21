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

test_that("get_vpts_aloft() returns error on invalid odim code", {
  # Radar is not 5 character string
  expect_error(
    get_vpts_aloft(radar = "beja",
                   rounded_interval = "2023-01-01",
                   selected_source = "uva",
                   coverage),
    class = "getRad_error_radar_odim_code_invalid"
  )
  # radar is not a string
  expect_error(
    get_vpts_aloft(radar = 12345,
                   rounded_interval = "2023-01-01",
                   selected_source = "uva",
                   coverage),
    class = "getRad_error_radar_not_single_string"
  )
})

test_that("get_vpts_aloft() returns error when multiple radars are queried", {
  expect_error(
    get_vpts_aloft(radar = c("bejab","depro"),
                   rounded_interval = "2023-01-01",
                   selected_source = "uva",
                   coverage),
    class = "getRad_error_radar_not_single_string"
  )
})

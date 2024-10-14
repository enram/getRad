test_that("Pvol for estonia can be downloaded", {
  testthat::skip_if_offline()
  withr::local_options(list(httr2_progress = FALSE))
  # The api frequently sends a 429 response therefore test is allowed to fail
  testthat::show_failure(expect_s3_class(pvol <- suppressMessages(get_pvol("eesur",
    time <- as.POSIXct("2024-4-4 20:00:00", tz = "Europe/Helsinki"),
    param = "all"
  )), "pvol"))
  expect_true(bioRad::is.pvol(pvol))
  expect_identical(pvol$datetime, lubridate::with_tz(time, "UTC"))
})

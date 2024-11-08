test_that("Pvol for estonia can be downloaded", {
skip_if_offline()
  withr::local_options(list(httr2_progress = FALSE))
  # The api frequently sends a 429 response therefore test is allowed to fail
  get_pvol_q <- purrr::quietly(get_pvol)
  how_failure(pvol <- expect_no_error(get_pvol_q("eesur",
    time <- as.POSIXct("2024-4-4 20:00:00", tz = "Europe/Helsinki"),
    param = "all"
  )$result))
  skip_if_not(exists("pvol"))
  expect_s3_class(pvol, "pvol")
  expect_true(bioRad::is.pvol(pvol))
  expect_identical(pvol$datetime, lubridate::with_tz(time, "UTC"))
})

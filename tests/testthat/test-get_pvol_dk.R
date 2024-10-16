test_that("Pvol for Danish can be downloaded", {
  testthat::skip("Because no key for Denmark is available in the testing environment")
  expect_s3_class(pvol <- get_pvol("dkbor", time <- lubridate::floor_date(as.POSIXct(Sys.time(), tz = "Europe/Helsinki") - lubridate::hours(10), "5 mins") - lubridate::days(90), param = "all"), "pvol")
  expect_true(bioRad::is.pvol(pvol))
  expect_identical(lubridate::floor_date(pvol$datetime, "5 mins"), lubridate::with_tz(time, "UTC"))
})

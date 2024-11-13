test_that("Pvol for finland can be downloaded", {
  skip_if_offline()
  time <- as.POSIXct("2024-4-4 20:00:00", tz = "Europe/Helsinki")
  pvol <- expect_s3_class(get_pvol("fiuta", time, param = "all"), "pvol")
  expect_true(bioRad::is.pvol(pvol))
  expect_identical(pvol$datetime, lubridate::with_tz(time, "UTC"))
})

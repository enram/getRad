test_that("Pvol for finland can be downloaded", {
  testthat::skip_if_offline()
  expect_s3_class(pvol<-get_pvol("fiuta", time<-as.POSIXct("2024-4-4 20:00:00", tz = "Europe/Helsinki"), param = "all"),"pvol")
  expect_true(bioRad::is.pvol(pvol))
  expect_identical(pvol$datetime, lubridate::with_tz(time,"UTC"))
})

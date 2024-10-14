test_that("Pvol for German can be downloaded", {
  testthat::skip_if_offline()
  expect_s3_class(pvol<-get_pvol("deess", time<-lubridate::floor_date(as.POSIXct(Sys.time(), tz = "Europe/Helsinki")-lubridate::hours(10), "5 mins"), param = "all"),"pvol")
  expect_true(bioRad::is.pvol(pvol))
  expect_identical(lubridate::floor_date(pvol$datetime,'5 mins'), lubridate::with_tz(time,"UTC"))
})

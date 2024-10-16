test_that("Pvol for the Netherlands can be downloaded", {
  testthat::skip_if_offline()
  testthat::skip_if(Sys.which("KNMI_vol_h5_to_ODIM_h5") == "")
  withr::local_options(list("keyring_backend" = "env"))
  # get public key here https://developer.dataplatform.knmi.nl/open-data-api#token
  withr::local_envvar(
    list("getRad_nl_api_key" = "eyJvcmciOiI1ZTU1NGUxOTI3NGE5NjAwMDEyYTNlYjEiLCJpZCI6ImE1OGI5NGZmMDY5NDRhZDNhZjFkMDBmNDBmNTQyNjBkIiwiaCI6Im11cm11cjEyOCJ9")
  )
  expect_s3_class(pvol <- get_pvol("nlhrw",
    time <- as.POSIXct("2024-4-4 20:00:00",
      tz = "Europe/Helsinki"
    ),
    param = "all"
  ), "pvol")
  expect_true(bioRad::is.pvol(pvol))
  expect_identical(lubridate::floor_date(pvol$datetime, "1 mins"), lubridate::with_tz(time, "UTC"))
})

test_that("failure to find converter", {
  testthat::skip_if_offline()
  testthat::skip_if_not(Sys.which("KNMI_vol_h5_to_ODIM_h5") == "")

  withr::local_options(list("keyring_backend" = "env"))
  # get public key here https://developer.dataplatform.knmi.nl/open-data-api#token
  withr::local_envvar(
    list("getRad_nl_api_key" = "eyJvcmciOiI1ZTU1NGUxOTI3NGE5NjAwMDEyYTNlYjEiLCJpZCI6ImE1OGI5NGZmMDY5NDRhZDNhZjFkMDBmNDBmNTQyNjBkIiwiaCI6Im11cm11cjEyOCJ9")
  )
  expect_error(
    get_pvol("nlhrw",
      time <- as.POSIXct("2024-4-4 20:00:00", tz = "Europe/Helsinki"),
      param = "all"
    ),
    class = "getRad_error_no_nl_converter_found"
  )
})

test_that("The Netherlands non existing radar", {
  expect_error(
    pvol <- get_pvol(
      "nlaaa",
      time <- as.POSIXct("2024-4-4 20:00:00", tz = "Europe/Helsinki")
    ),
    class = "getRad_error_netherlands_no_url_for_radar"
  )
})
test_that("Pvol for the Netherlands authenication failure", {
  testthat::skip_if_offline()
  withr::local_options(list("keyring_backend" = "env"))
  # get public key here https://developer.dataplatform.knmi.nl/open-data-api#token
  withr::local_envvar(list("getRad_nl_api_key" = "wrongkey"))
  expect_error(
    pvol <- get_pvol("nlhrw",
      time <- as.POSIXct("2024-4-4 20:00:00", tz = "Europe/Helsinki"),
      param = "all"
    ),
    class = "getRad_error_get_pvol_nl_authorization_failure"
  )
})

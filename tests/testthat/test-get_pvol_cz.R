test_that("Check if the available attributes changed", {
  skip_if_offline()
  expect_identical(
    request("http://opendata.chmi.cz/meteorology/weather/radar/sites/ska") |>
      req_perform() |>
      httr2::resp_body_html() |>
      xml2::xml_find_all("//a/@href") |>
      xml2::xml_text() |>
      tail(-1), c(
      "vol_phidp/", "vol_rhohv/", "vol_u/", "vol_v/", "vol_w/", "vol_z/",
      "vol_zdr/"
    )
  )
})
test_that("Pvol for Czechia can be downloaded", {
  skip_if_offline()
  time <- lubridate::floor_date(
    as.POSIXct(Sys.time(),
      tz = "Europe/Helsinki"
    ) - lubridate::hours(10), "5 mins"
  )
  expect_s3_class(pvol <- get_pvol("czska",
    time,
    param = "all"
  ), "pvol")
  expect_true(bioRad::is.pvol(pvol))
  expect_identical(
    lubridate::floor_date(pvol$datetime, "5 mins"),
    lubridate::with_tz(time, "UTC")
  )
})

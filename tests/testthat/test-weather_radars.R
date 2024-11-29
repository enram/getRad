test_that("weather_radars returns a tibble", {
  skip_if_offline(host = "https://raw.githubusercontent.com")

  expect_s3_class(weather_radar_metadata, "tbl_df")
})

test_that("weather_radars returns non-empty tibble", {
  skip_if_offline(host = "https://raw.githubusercontent.com")

  expect_true(nrow(weather_radar_metadata) > 0, "Expected non-empty tibble")
})

test_that("weather_radars returns a tibble with expected columns", {
  skip_if_offline(host = "https://raw.githubusercontent.com")

  expect_named(
    weather_radar_metadata,
    c(
      "number",
      "country",
      "countryid",
      "oldcountryid",
      "wmocode",
      "odimcode",
      "location",
      "status",
      "latitude",
      "longitude",
      "heightofstation",
      "band",
      "doppler",
      "polarization",
      "maxrange",
      "startyear",
      "heightantenna",
      "frequency",
      "finishyear",
      "source",
      "diameterantenna",
      "beam",
      "gain",
      "wrwp",
      "singlerrr",
      "compositerrr",
      "wigosid"
    )
  )
})

test_that("weather_radars returns tibble with correct data types", {
  skip_if_offline(host = "https://raw.githubusercontent.com")

  expect_identical(
    purrr::map(weather_radar_metadata, class),
    list(
      number = "character",
      country = "character",
      countryid = "character",
      oldcountryid = "character",
      wmocode = "character",
      odimcode = "character",
      location = "character",
      status = "character",
      latitude = "character",
      longitude = "character",
      heightofstation = "character",
      band = "character",
      doppler = "character",
      polarization = "character",
      maxrange = "character",
      startyear = "character",
      heightantenna = "character",
      frequency = "character",
      finishyear = "character",
      source = "character",
      diameterantenna = "character",
      beam = "character",
      gain = "character",
      wrwp = "character",
      singlerrr = "character",
      compositerrr = "character",
      wigosid = "character"
    )
  )
})

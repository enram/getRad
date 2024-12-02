test_that("weather_radars returns a tibble", {
  skip_if_offline(host = "raw.githubusercontent.com")
  if (!exists("weather_radar_metadata")) {
    weather_radar_metadata <- weather_radars()
  }

  expect_s3_class(weather_radar_metadata, "tbl_df")
})

test_that("weather_radars returns non-empty tibble", {
  skip_if_offline(host = "raw.githubusercontent.com")
  if (!exists("weather_radar_metadata")) {
    weather_radar_metadata <- weather_radars()
  }

  expect_true(nrow(weather_radar_metadata) > 0, "Expected non-empty tibble")
})

test_that("weather_radars returns a tibble with expected columns", {
  skip_if_offline(host = "raw.githubusercontent.com")
  if (!exists("weather_radar_metadata")) {
    weather_radar_metadata <- weather_radars()
  }

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
  skip_if_offline(host = "raw.githubusercontent.com")
  if (!exists("weather_radar_metadata")) {
    weather_radar_metadata <- weather_radars()
  }

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

test_that("weather_radars() should return a table with records from main and archive", {
  skip_if_offline(host = "eumetnet.eu")
  ## Count the number of records in both main and archive source
  n_records_main <-
    paste(
      sep = "/",
      "http://eumetnet.eu/wp-content/themes/aeron-child",
      "observations-programme/current-activities/opera/database",
      "OPERA_Database/OPERA_RADARS_DB.json"
    ) |>
    httr2::request() |>
    req_user_agent_getrad() |>
    httr2::req_retry(
      max_tries = 15,
      backoff = \(x) sqrt(x) * 2,
      is_transient = \(resp) httr2::resp_status(resp) %in% c(429),
      retry_on_failure = TRUE
    ) |>
    httr2::req_perform() |>
    # The object is actually returned as text/plain
    httr2::resp_body_json(check_type = FALSE) |>
    length()

  n_records_archive <-
    paste(
      sep = "/",
      "http://eumetnet.eu/wp-content/themes/aeron-child",
      "observations-programme/current-activities/opera/database",
      "OPERA_Database/OPERA_RADARS_ARH_DB.json"
    ) |>
    httr2::request() |>
    req_user_agent_getrad() |>
    httr2::req_retry(
      max_tries = 15,
      backoff = \(x) sqrt(x) * 2,
      is_transient = \(resp) httr2::resp_status(resp) %in% c(429),
      retry_on_failure = TRUE
    ) |>
    httr2::req_perform() |>
    # The object is actually returned as text/plain
    httr2::resp_body_json(check_type = FALSE) |>
    length()

  # Compare to output of weather_radars()
  expect_identical(
    nrow(weather_radars()),
    n_records_main + n_records_archive
  )
})

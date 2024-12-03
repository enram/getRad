#' Get weather radar data
#'
#' The `weather_radars()` function is used to retreive weather radar metadata
#' from the [OPERA network](https://www.eumetnet.eu/activities/observations-programme/current-activities/opera/).
#' The retrieved data is returned as a tibble.
#'
#' @return A tibble containing weather radar metadata
#' @export
#'
#' @examplesIf interactive()
#' weather_radars()
weather_radars <- function() {
  # Build the url where the JSON files are hosted on eumetnet

  # Read source JSON files from OPERA
  radars_main_url <-
    paste(
      sep = "/",
      "http://eumetnet.eu/wp-content/themes/aeron-child",
      "observations-programme/current-activities/opera/database",
      "OPERA_Database/OPERA_RADARS_DB.json"
    )

  radars_archive_url <-
    paste(
      sep = "/",
      "http://eumetnet.eu/wp-content/themes/aeron-child",
      "observations-programme/current-activities/opera/database",
      "OPERA_Database/OPERA_RADARS_ARH_DB.json"
    )

  urls <- list(
    c(url = radars_main_url, source = "main"),
    c(url =    radars_archive_url, source = "archive")
  )

  # Fetch the JSON file from eumetnet with similar arguments as the other
  # functions
  purrr::map(urls, \(json_url) {
    httr2::request(json_url["url"]) |>
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
      # As tibble so it displays more nicely
      purrr::map(\(list) dplyr::as_tibble(list)) |>
      # Return as a single tibble by row binding
      purrr::list_rbind() |>
      dplyr::mutate(source = json_url["source"])
  }) |>
    # Combine both sources into a single tibble
    purrr::list_rbind() |>
    # Convert empty strings into NA
    dplyr::mutate(
      dplyr::across(dplyr::where(is.character),
      \(string) dplyr::if_else(string == "",
                               NA_character_,
                               string)
        )
      ) |>
    # Move source column to end
    dplyr::relocate(source, .after = dplyr::last_col()) |>
    # convert column types to expected values, non fitting values are returned
    # as NA without warning
    dplyr::mutate(
      number = as_integer_shh(number),
      wmocode = as_integer_shh(wmocode),
      status = as_integer_shh(status),
      latitude = as_numeric_shh(latitude),
      longitude = as_numeric_shh(longitude),
      heightofstation = as_integer_shh(heightofstation),
      doppler = dplyr::case_when(
        doppler == "Y" ~ TRUE,
        doppler == "N" ~ FALSE,
        .default = NA,
        .ptype = logical()
      ),
      maxrange = as_integer_shh(maxrange),
      startyear = as_integer_shh(startyear),,
      heightantenna = as_numeric_shh(heightantenna),
      diameterantenna = as_numeric_shh(diameterantenna),
      beam = as_numeric_shh(beam),
      gain = as_numeric_shh(gain),
      frequency = as_numeric_shh(frequency),
      wrwp = dplyr::case_when(
        wrwp == "Y" ~ TRUE,
        wrwp == "N" ~ FALSE,
        .default = NA,
        .ptype = logical()
      ),
      finishyear = as_integer_shh(finishyear),
      singlerrr = dplyr::case_when(
        singlerrr == "Y" ~ TRUE,
        singlerrr == "N" ~ FALSE,
        .default = NA,
        .ptype = logical()
      ),
      compositerrr = dplyr::case_when(
        compositerrr == "Y" ~ TRUE,
        compositerrr == "N" ~ FALSE,
        .default = NA,
        .ptype = logical()
      )
    ) |>
    # Sort data for consistent git diffs
    dplyr::arrange(country, number, startyear)
}

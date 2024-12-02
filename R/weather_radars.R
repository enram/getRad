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
  purrr::map(urls, \(json_urls) {
    httr2::request(json_urls["url"]) |>
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
      dplyr::mutate(source = json_urls["source"])
  }) |>
    # Combine both sources into a single tibble
    purrr::list_rbind() |>
    # Convert the columns to the correct types
    utils::type.convert(as.is = TRUE) |>
}

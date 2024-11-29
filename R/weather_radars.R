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
  # Build the url where the JSON file is hosted on Github
  opera_radar_overview_url <-
    paste(
      sep = "/",
      "https://raw.githubusercontent.com/enram",
      "aloftdata.eu/refs/heads/main/_data",
      "OPERA_RADARS_DB.json"
    )

  # Fetch the JSON file from Github with similar arguments as the other
  # functions
  httr2::request(opera_radar_overview_url) |>
    req_user_agent_getrad() |>
    httr2::req_retry(
      max_tries = 15, backoff = \(x) sqrt(x) * 2,
      is_transient = \(resp) httr2::resp_status(resp) %in% c(429),
      retry_on_failure = TRUE
    ) |>
    httr2::req_perform() |>
    # The object is actually returned as text/plain
    httr2::resp_body_json(check_type = FALSE) |>
    # As tibble so it displays more nicely
    purrr::map(dplyr::as_tibble) |>
    # Return as a single tibble by row binding
    purrr::list_rbind()
}

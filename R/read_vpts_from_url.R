#' Reading vpts files from a vector of urls
#'
#' This is a simple helper to read a vector of urls as vpts files using vroom
#' and httr2. This wrapper makes use of purrr to create requests that can be
#' handled in parallel with our own custom retry settings and user agent.
#'
#' Apart from parallelisation and these custom settings, this could also be
#' handled by simple call to `vroom::vroom(file = urls)`
#'
#' This function also includes column specifications for the vpts csv data
#' standard. However, [bioRad::as.vpts()] currently doesn't support factors,
#' thus any fields sent to that function need to be parsed as character vectors.
#'
#' @param urls description
#'
#' @return A list of tibbles, one for each url.
#' @noRd
#'
#' @examples
#' c("https://aloftdata.s3-eu-west-1.amazonaws.com/baltrad/daily/bejab/2024/bejab_vpts_20240305.csv",
#' "https://aloftdata.s3-eu-west-1.amazonaws.com/baltrad/daily/bejab/2024/bejab_vpts_20240306.csv",
#' "https://aloftdata.s3-eu-west-1.amazonaws.com/baltrad/daily/bejab/2024/bejab_vpts_20240307.csv"
#' ) |>
#'  read_vpts_from_url()

read_vpts_from_url <- function(urls) {
  purrr::map(urls, httr2::request) |>
    # Identify ourselves in the request
    purrr::map(req_user_agent_getrad) |>
    # Set retry conditions
    purrr::map(~ httr2::req_retry(
      .x,
      max_tries = 15, backoff = \(x) sqrt(x) * 2,
      is_transient = \(resp) resp_status(resp) %in% c(429),
      retry_on_failure = TRUE
    )) |>
    # Perform the requests in parallel
    httr2::req_perform_parallel() |>
    # Fetch the response bodies and parse it using vroom
    ## A helper in bioRad (validate_vpts()) that we call indirectly via
    # " bioRad::as.vpts() currently doesn't support factors: bioRad v0.8.1
    purrr::map(httr2::resp_body_raw) |>
    purrr::map(~ vroom::vroom(
      delim = ",",
      I(.x),
      col_types =
        list(
          radar = vroom::col_factor(),
          datetime = vroom::col_datetime(),
          height = vroom::col_integer(),
          u = vroom::col_double(),
          v = vroom::col_double(),
          w = vroom::col_double(),
          ff = vroom::col_double(),
          dd = vroom::col_double(),
          sd_vvp = vroom::col_double(),
          gap = vroom::col_logical(),
          eta = vroom::col_double(),
          dens = vroom::col_double(),
          dbz = vroom::col_double(),
          dbz_all = vroom::col_double(),
          n = vroom::col_integer(),
          n_dbz = vroom::col_integer(),
          n_all = vroom::col_integer(),
          n_dbz_all = vroom::col_integer(),
          rcs = vroom::col_double(),
          sd_vvp_threshold = vroom::col_double(),
          vcp = vroom::col_integer(),
          radar_longitude = vroom::col_double(),
          radar_latitude = vroom::col_double(),
          radar_height = vroom::col_integer(),
          radar_wavelength = vroom::col_double(),
          source_file = vroom::col_character()
        ),
      show_col_types = NULL,
      progress = FALSE
    ))
}

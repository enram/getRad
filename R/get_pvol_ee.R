get_pvol_ee <- function(radar, time, ...) {
  files <- httr2::request("https://avaandmed.keskkonnaportaal.ee/_vti_bin/RmApi.svc/active/items/query") |>
    req_user_agent_getrad() |>
    httr2::req_body_json(
      list(filter = list(and = list(
        children = list(list(and = list(
          children = list(list(isEqual = list(
            field = "$contentType",
            value = "0102FB01"
          )), list(isEqual = list(
            field = "Phenomenon",
            value = "VOL"
          )))
        )), list(and = list(children = list(
          list(
            and = list(children = list(
              list(greaterThanOrEqual = list(
                field = "Timestamp",
                value = strftime(time, "%Y-%m-%dT%H:%M:%OS6%z")
              )),
              list(lessThanOrEqual = list(
                field = "Timestamp",
                value = strftime(time, "%Y-%m-%dT%H:%M:%OS6%z")
              ))
            ))
          ),
          list(isEqual = list(
            field = "Radar",
            value = dplyr::case_match(
              radar,
              "eesur" ~ "S\u00FCrgavere radar (SUR)",
              "eehar" ~ "Harku radar (HAR)"
            )
          ))
        ))))
      )))
    ) |>
    httr2::req_retry(
      max_tries = 15, backoff = \(x) sqrt(x) * 2,
      is_transient = \(resp) httr2::resp_status(resp) %in% c(429),
      retry_on_failure = TRUE
    ) |>
    httr2::req_perform() |>
    httr2::resp_body_json()
  if (files$numFound == 0 || length(files$documents) != 1) {
    cli::cli_abort("The expected number of files is not found",
      class = "getRad_error_get_pvol_ee_differing_n_files"
    )
  }
  req <- httr2::request("https://avaandmed.keskkonnaportaal.ee/_vti_bin/RmApi.svc/active/items/") |>
    req_user_agent_getrad() |>
    httr2::req_url_path_append(files$documents[[1]]$id) |>
    httr2::req_url_path_append("files/0") |>
    httr2::req_retry(
      max_tries = 15, backoff = \(x) sqrt(x) * 2,
      is_transient = \(resp) httr2::resp_status(resp) %in% c(429),
      retry_on_failure = TRUE
    ) |>
    httr2::req_perform(path = tempfile(fileext = ".h5"))
  pvol <- bioRad::read_pvolfile(req$body, ...)
  file.remove(req$body)
  return(pvol)
}

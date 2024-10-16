get_pvol_ee <- function(radar, time, ...) {
  request("https://avaandmed.keskkonnaportaal.ee/_vti_bin/RmApi.svc/active/items/query") |>
    req_user_agent_getrad() |>
    req_body_json(
      list(filter = list(and = list(children = list(list(and = list(
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
              field = "Timestamp", value = strftime(time, "%Y-%m-%dT%H:%M:%OS6%z")
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
      )))))))
    ) |>
    req_retry(
      max_tries = 15, backoff = \(x)sqrt(x) * 2,
      is_transient = \(resp) resp_status(resp) %in% c(429), retry_on_failure = T
    ) |>
    req_perform() |>
    resp_body_json() -> files
  if (files$numFound == 0 || length(files$documents) != 1) {
    cli_abort("The expected number of files is not found",
      class = "getRad_error_get_pvol_ee_differing_n_files"
    )
  }
  request("https://avaandmed.keskkonnaportaal.ee/_vti_bin/RmApi.svc/active/items/") |>
    req_user_agent_getrad() |>
    req_url_path_append(files$documents[[1]]$id) |>
    req_url_path_append("files/0") |>
    req_retry(
      max_tries = 15, backoff = \(x)sqrt(x) * 2,
      is_transient = \(resp) resp_status(resp) %in% c(429), retry_on_failure = T
    ) |>
    req_perform(path = tempfile(fileext = ".h5")) -> req
  pvol <- bioRad::read_pvolfile(req$body, ...)
  file.remove(req$body)
  return(pvol)
}

# http://opendata.chmi.cz/meteorology/weather/radar/sites/ska/vol_z/hdf5/


get_pvol_cz <- function(radar, time, ...) {
  time_chr <- time_pos <- base <- resp <- NULL
  urls <- c(
    glue::glue("http://opendata.chmi.cz/meteorology/weather/radar/sites/{substr(radar,3,5)}/vol_z/hdf5/"),
    glue::glue("http://opendata.chmi.cz/meteorology/weather/radar/sites/{substr(radar,3,5)}/vol_u/hdf5/"),
    glue::glue("http://opendata.chmi.cz/meteorology/weather/radar/sites/{substr(radar,3,5)}/vol_v/hdf5/"),
    glue::glue("http://opendata.chmi.cz/meteorology/weather/radar/sites/{substr(radar,3,5)}/vol_w/hdf5/"),
    glue::glue("http://opendata.chmi.cz/meteorology/weather/radar/sites/{substr(radar,3,5)}/vol_zdr/hdf5/"),
    glue::glue("http://opendata.chmi.cz/meteorology/weather/radar/sites/{substr(radar,3,5)}/vol_rhohv/hdf5/"),
    glue::glue("http://opendata.chmi.cz/meteorology/weather/radar/sites/{substr(radar,3,5)}/vol_phidp/hdf5/")
  )
  rlang::check_installed(
    c("lubridate", "tidyr", "xml2", "rhdf5"),
    "to read czech radar data"
  )
  res <- lapply(urls, function(x) {
    request(x) |>
      req_user_agent_getrad() |>
      req_perform() |>
      resp_body_html() |>
      xml2::xml_find_all("//a/@href") |>
      xml2::xml_text()
  })
  files_to_get <- data.frame(base = urls) |>
    dplyr::mutate(file = res) |>
    tidyr::unnest(file) |>
    dplyr::filter(file != "../") |>
    dplyr::mutate(
      time_chr = sub(".hdf", "", sub(".*_OKPR_", "", file))
    ) |>
    dplyr::mutate(
      time_pos = strptime(time_chr, "%Y%m%d%H%M%S", tz = "UTC")
    ) |>
    dplyr::filter(lubridate::`%within%`(
      time_pos,
      lubridate::interval(
        time,
        time + lubridate::minutes(5)
      )
    ))
  files_to_get <- files_to_get |> dplyr::mutate(
    req = purrr::pmap(list(x = base, y = file), function(x, y) {
      request(x) |>
        req_url_path_append(y) |>
        req_user_agent_getrad()
    })
  )


  files_to_get$resp <- files_to_get$req |>
    req_perform_parallel(
      paths = replicate(
        length(files_to_get$req),
        tempfile(fileext = ".h5")
      )
    )
  f <- files_to_get |>
    dplyr::mutate(
      tempfile = purrr::map_chr(resp, ~ .x$body),
      # add h5 how group as it seems to be missing
      mut = purrr::map(tempfile, ~ {
        a <- rhdf5::H5Fopen(.x)
        group <- rhdf5::H5Gcreate(a, "how")
        rhdf5::H5Fclose(a)
        rhdf5::H5Gclose(group)
      }),
      pvol = purrr::map(tempfile, ~ bioRad::read_pvolfile(.x)),
      remove = purrr::map(tempfile, ~ file.remove(.x))
    )
  l <- purrr::map(purrr::chuck(f, "pvol"), bioRad::attribute_table)
  if (!all(unlist(lapply(
    lapply(l[-1], dplyr::select, -"param"), all.equal,
    dplyr::select(l[[1]], -"param")
  )))) {
    cli_abort("Not all polar volumes have the same attributes",
      class = "getRad_error_differing_attributes_cz"
    )
  }
  pvol <- Reduce(
    function(x, y) {
      x$scans <- mapply(
        function(i, j) {
          i$params <- c(i$params, j$params)
          i
        },
        x$scans, y$scans,
        SIMPLIFY = FALSE
      )
      x
    },
    f$pvol
  )
  pvol
}

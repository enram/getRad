# http://opendata.chmi.cz/meteorology/weather/radar/sites/ska/vol_z/hdf5/


get_pvol_cz <- function(radar, time, ...) {
  time_chr <- time_pos <- base <- resp <- NULL
  # All parameters are retrieved from separate files
  # Here all urls are generated
  params <- c("z", "u", "v", "w", "zdr", "rhohv", "phidp")
  urls <- glue::glue("http://opendata.chmi.cz/meteorology/weather/radar/sites/{substr(radar,3,5)}/vol_{params}/hdf5/")
  rlang::check_installed(
    c("lubridate", "tidyr", "xml2", "rhdf5"),
    "to read Czech radar data"
  )
  res <- lapply(urls, function(x) {
    httr2::request(x) |>
      req_user_agent_getrad() |>
      httr2::req_perform() |>
      httr2::resp_body_html() |>
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
      httr2::request(x) |>
        httr2::req_url_path_append(y) |>
        req_user_agent_getrad()
    })
  )


  files_to_get$resp <- files_to_get$req |>
    httr2::req_perform_parallel(
      paths = replicate(
        length(files_to_get$req),
        tempfile(fileext = ".h5")
      )
    )
  polar_volumes_tibble <- files_to_get |>
    dplyr::mutate(
      tempfile = purrr::map_chr(resp, purrr::chuck, "body"),
      # add h5 how group as it seems to be missing
      mut = purrr::map(tempfile, ~ {
        hdf_connection <- rhdf5::H5Fopen(.x)
        group <- rhdf5::H5Gcreate(hdf_connection, "how")
        rhdf5::H5Fclose(hdf_connection)
        rhdf5::H5Gclose(group)
      }),
      pvol = purrr::map(tempfile, ~ bioRad::read_pvolfile(.x)),
      remove = purrr::map(tempfile, ~ file.remove(.x))
    )
  # Check if all parameter have same attributes
  list_of_attribute_tables <- purrr::map(
    purrr::chuck(polar_volumes_tibble, "pvol"),
    bioRad::attribute_table
  )
  all_params_same_attributes <- all(unlist(lapply(
    lapply(list_of_attribute_tables[-1], dplyr::select, -"param"), all.equal,
    dplyr::select(list_of_attribute_tables[[1]], -"param")
  )))
  if (!all_params_same_attributes) {
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
    polar_volumes_tibble$pvol
  )
  pvol
}

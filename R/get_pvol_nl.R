get_pvol_nl <- function(radar, time, ...) {
  url <- dplyr::case_when(
    radar == "nlhrw" ~ "https://api.dataplatform.knmi.nl/open-data/v1/datasets/radar_volume_full_herwijnen/versions/1.0/files",
    radar == "nldhl" ~ "https://api.dataplatform.knmi.nl/open-data/v1/datasets/radar_volume_denhelder/versions/2.0/files",
    .default = NA
  )
  if (is.na(url)) {
    cli::cli_abort(
      message = "No suitable url exist for the radar {radar}",
      class = "getRad_error_netherlands_no_url_for_radar"
    )
  }
  # This request generate the temporary download url where the polar volume file can be retrieved
  resp <- tryCatch(
    httr2::request(url) |>
      req_user_agent_getrad() |>
      httr2::req_url_path_append(
        glue::glue(getOption(
          "getRad.nl_file_format",
          "RAD_{c('nlhrw'='NL62','nldhl'='NL61')[radar]}_VOL_NA_{strftime(time,'%Y%m%d%H%M', tz='UTC')}.h5"
        ))
      ) |>
      httr2::req_url_path_append("/url") |>
      httr2::req_headers(Authorization = get_secret("nl_api_key")) |>
      httr2::req_perform(),
    httr2_http_403 = function(cnd) {
      cli::cli_abort(
        c("There was an authorization error, possibly this relates to using an invalid API key",
          i = "Please check if you set the correct `nl_api_key` with {.code get_secret('nl_api_key')}"
        ),
        cnd = cnd,
        class = "getRad_error_get_pvol_nl_authorization_failure"
      )
    }
  )
  # This request retrieves the file
  req <- httr2::resp_body_json(resp)$temporaryDownloadUrl |>
    httr2::req_url(req = resp$request) |>
    httr2::req_headers(Authorization = NULL) |>
    httr2::req_perform(path = tempfile(fileext = ".h5"))
  # Dutch files need to be converted to the odim format
  converter <- getOption("getRad.nl_converter", "KNMI_vol_h5_to_ODIM_h5")
  if (!file.exists(converter)) {
    converter <- Sys.which(converter)
  }
  if (converter == "") {
    cli::cli_abort(c(
      x = "The program to convert KNMI data to ODIM format is not found.",
      i = "The source code for this binary can be obtained from this location {.file {system.file('extra/KNMI_vol_h5_to_ODIM_h5.c', package='getRad')}}",
      i = "Please compile the binary and include it in the search path as a program named {.arg KNMI_vol_h5_to_ODIM_h5}",
      i = "On linux systems this can be done with the following command {.code h5cc KNMI_vol_h5_to_ODIM_h5.c -o KNMI_vol_h5_to_ODIM_h5}.",
      i = "If another name is used or the program is not in the search path use options to locate the program ({.run options(getRad.nl_converter='')})."
    ), class = "getRad_error_no_nl_converter_found")
  }
  pvol_path <- paste0(req$body, ".odim.h5")
  system(paste(converter, pvol_path, req$body))
  pvol <- bioRad::read_pvolfile(pvol_path, ...)
  file.remove(pvol_path, req$body)
  return(pvol)
}

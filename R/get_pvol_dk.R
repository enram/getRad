get_pvol_dk <- function(radar, time, ...) {
  req <- request(
    getOption(
      "getRad.dk_url",
      "https://dmigw.govcloud.dk/v1/radardata/download"
    )
  ) |>
    req_user_agent_getrad() |>
    req_url_path_append(
      glue::glue(getOption(
        "getRad.dk_file_format",
        "{radar}_{strftime(time,'%Y%m%d%H%M', tz='UTC')}.vol.h5"
      ))
    ) |>
    req_url_query(`api-key` = get_secret("dk_api_key")) |>
    req_perform(path = tempfile(fileext = ".h5"))
  pvol <- bioRad::read_pvolfile(req$body, ...)
  file.remove(req$body)
  return(pvol)
}

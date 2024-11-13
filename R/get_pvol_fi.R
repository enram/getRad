get_pvol_fi <- function(radar, time, ...) {
  rlang::check_installed("rhdf5", "to maniplate the `h5` files from the Finish data repository")
  req <- request(
    getOption(
      "getRad.fi_url",
      "http://s3-eu-west-1.amazonaws.com/fmi-opendata-radar-volume-hdf5"
    )
  ) |>
    req_user_agent_getrad() |>
    req_url_path_append(
      glue::glue(getOption(
        "getRad.fi_file_format",
        "{strftime(time,'%Y', tz='UTC')}/{strftime(time,'%m', tz='UTC')}/{strftime(time,'%d', tz='UTC')}/{radar}/{strftime(time,'%Y%m%d%H%M', tz='UTC')}_{radar}_PVOL.h5"
      ))
    )

  req <- req |>
    req_perform(path = tempfile(fileext = ".h5"))
  rlang::check_installed("rhdf5", "To adjust the polar volume files for Finish data.")
  hdf_connection <- rhdf5::H5Fopen(req$body)
  group <- rhdf5::H5Gopen(hdf_connection, "what")
  rhdf5::h5writeAttribute("PVOL", group, "object")
  rhdf5::H5Fclose(hdf_connection)
  rhdf5::H5Gclose(group)
  pvol <- bioRad::read_pvolfile(req$body, ...)
  file.remove(req$body)
  return(pvol)
}
# https://en.ilmatieteenlaitos.fi/radar-data-on-aws-s3
# http://s3-eu-west-1.amazonaws.com/fmi-opendata-radar-volume-hdf5/2021/11/09/fiuta/202111090450_fiuta_PVOL.h5
# http://fmi-opendata-radar-volume-hdf5.s3-website-eu-west-1.amazonaws.com/?prefix=2024/03/03/fianj/

get_pvol_de <- function(radar, time, ...) {
  time_pos <- base <- iter <- param <- resp <- time_chr <- NULL
  # https://opendata.dwd.de/weather/radar/sites
  # https://opendata.dwd.de/weather/radar/sites/sweep_vol_z/hnr/hdf5/filter_simple/ras07-stqual-vol5minng01_sweeph5onem_dbzh_00-2024061011155700-hnr-10339-hd5
  # https://opendata.dwd.de/weather/radar/sites/sweep_vol_z/hnr/hdf5/filter_simple/ras07-stqual-vol5minng01_sweeph5onem_dbzh_09-2024061206040300-hnr-10339-hd5
  time <- lubridate::with_tz(time, "UTC")
  rlang::check_installed(
    c("xml2", "lubridate", "tidyr"),
    "to import data from German weather radars"
  )
  urls <- c(
    glue::glue("https://opendata.dwd.de/weather/radar/sites/sweep_vol_z/{substr(radar,3,5)}/hdf5/filter_simple/"),
    glue::glue("https://opendata.dwd.de/weather/radar/sites/sweep_vol_v/{substr(radar,3,5)}/hdf5/filter_simple/")
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
    tidyr::separate_wider_delim(file,
      delim = "-", cols_remove = FALSE,
      names = c("ras", "qual", "sweep", "time_chr", "radar", "odim", "h5")
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
  if (nrow(files_to_get) != 20) {
    cli_abort("The server returned an unexpected number of files",
      class = "getRad_error_germany_unexpected_number_of_files"
    )
  }

  files_to_get <- files_to_get |> dplyr::mutate(
    req = purrr::pmap(
      list(x = base, y = file),
      function(x, y) {
        request(x) |>
          req_url_path_append(y) |>
          req_user_agent_getrad()
      }
    )
  )



  files_to_get$resp <- files_to_get$req |>
    req_perform_parallel(
      paths = replicate(
        length(files_to_get$req),
        tempfile(fileext = ".h5")
      )
    )
  files_to_get <- files_to_get |>
    dplyr::mutate(
      tempfile = purrr::map_chr(resp, ~ .x$body)
    ) |>
    dplyr::mutate(
      scan = purrr::map(tempfile, ~ read_scan(.x)),
      remove = purrr::map(tempfile, ~ file.remove(.x))
    ) |>
    tidyr::separate_wider_delim(sweep,
      delim = "_", cols_remove = FALSE,
      names = c("vol", "name", "param", "iter")
    ) |>
    dplyr::group_by(iter) |>
    dplyr::summarize(
      scan = list(scan),
      param = list(param), radar = unique(radar)
    ) |>
    dplyr::mutate(
      scan = purrr::map2(scan, param, ~ list_to_scan(.x, .y))
    )

  pvol <- list_to_pvol(files_to_get$scan, time = time, radar = radar)
  return(pvol)
}

list_to_pvol <- function(x, time, radar,
                         source = "constructed from opendata.dwd.de") {
  stopifnot(length(time) == 1)
  stopifnot(length(radar) == 1)
  stopifnot(is.list(x))
  output <- list()
  output$radar <- radar
  output$datetime <- time
  output$scans <- x

  output$attributes <- x[[1]]$attributes
  output$attributes$what$object <- "PVOL"
  output$attributes$what$source <- source
  output$geo <- attr(x[[1]]$params[[1]], "geo")

  class(output) <- "pvol"
  output
}

list_to_scan <- function(x, p) {
  xx <- x[[1]]

  xx$params <- lapply(x, function(x) x$params[[1]])
  names(xx$params) <- p
  xx
}

read_scan <- function(file, scan = "dataset1",
                      param = "all", radar = "",
                      datetime = "", geo = list(), attributes = "") {
  rlang::check_installed("rhdf5")
  h5struct <- rhdf5::h5ls(file, all = TRUE)
  groups <- h5struct[h5struct$group == paste("/", scan, sep = ""), ]$name
  groups <- groups[grep("data", groups)]
  dtypes <- h5struct[startsWith(h5struct$group, paste("/",
    scan, "/data",
    sep = ""
  )), ]
  dtypes <- dtypes[dtypes$name == "data", ]$dtype

  h5struct <- h5struct[h5struct$group == paste("/", scan, sep = ""), ]$name
  if (length(param) == 1 && param == "all") {
    allParam <- TRUE
  } else {
    allParam <- FALSE
  }
  if (!allParam) {
    quantityNames <- purrr::map_chr(groups, function(x) {
      rhdf5::h5readAttributes(file, paste(scan, "/", x, "/what",
        sep = ""
      ))$quantity
    })
    groups <- groups[quantityNames %in% param]
    dtypes <- dtypes[quantityNames %in% param]
    if (length(groups) == 0) {
      return(NULL)
    }
  }
  attribs.how <- attribs.what <- attribs.where <- NULL
  if ("how" %in% h5struct) {
    attribs.how <- rhdf5::h5readAttributes(file, paste(scan, "/how",
      sep = ""
    ))
  }
  if ("what" %in% h5struct) {
    attribs.what <- rhdf5::h5readAttributes(file, paste(scan, "/what",
      sep = ""
    ))
  }
  if ("where" %in% h5struct) {
    attribs.where <- rhdf5::h5readAttributes(file, paste(scan, "/where",
      sep = ""
    ))
  }
  geo <- rhdf5::h5readAttributes(file, "where")
  geo$elangle <- c(attribs.where$elangle)
  geo$rscale <- c(attribs.where$rscale)
  geo$ascale <- c(360 / attribs.where$nrays)
  geo$astart <- attribs.how$astart
  geo$rstart <- attribs.where$rstart * 1000
  quantities <- mapply(function(x, y) {
    rr(
      file, paste(scan, "/", x, sep = ""),
      radar, datetime, geo, y
    )
  }, x = groups, y = dtypes, SIMPLIFY = FALSE)
  quantityNames <- purrr::map_chr(quantities, ~purrr::chuck(.x, "quantityName"))
  quantities <- lapply(quantities, "[[", "quantity")
  names(quantities) <- quantityNames
  if (is.null(attribs.how$wavelength)) {
    attribs.how$wavelength <- attributes$how$wavelength
  }
  output <- list(
    radar = radar, datetime = datetime, params = quantities,
    attributes = list(
      how = attribs.how, what = attribs.what,
      where = attribs.where
    ), geo = geo
  )
  class(output) <- "scan"
  output
}
rr <- function(file, quantity = "/", radar, datetime, geo, dtype) {
  rlang::check_installed("rhdf5")
  data <- rhdf5::h5read(file, quantity)$data

  storage.mode(data) <- "numeric"
  attr <- rhdf5::h5readAttributes(file, paste(quantity, "/what", sep = ""))
  data <- replace(data, data == as.numeric(attr$nodata), NA)
  data <- replace(
    data, data == as.numeric(attr$undetect),
    NaN
  )
  data <- as.numeric(attr$offset) + as.numeric(attr$gain) * data
  conversion <- list(
    gain = as.numeric(attr$gain), offset = as.numeric(attr$offset),
    nodata = as.numeric(attr$nodata), undetect = as.numeric(attr$undetect),
    dtype = dtype
  )
  class(data) <- c("param", class(data))
  attributes(data)$radar <- radar
  attributes(data)$datetime <- datetime
  attributes(data)$geo <- geo
  attributes(data)$param <- as.character(attr$quantity)
  attributes(data)$conversion <- conversion
  list(
    quantityName = paste0(
      strsplit(file, "_")[[1]][6],
      "_", basename(dirname(file))
    ),
    quantity = data
  )
}

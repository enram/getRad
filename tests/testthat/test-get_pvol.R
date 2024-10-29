# Country specific tests are in the respective country specific `get_pvol_`
# files

test_that("get_pvol radar argument", {
  expect_error(get_pvol(), class = "getRad_error_radar_not_character")
  expect_error(
    get_pvol(1L, time = as.POSIXct(Sys.Date())),
    class = "getRad_error_radar_not_character"
  )
  expect_error(
    get_pvol("nldhlu", time = as.POSIXct(Sys.Date())),
    class = "getRad_error_radar_not_character"
  )
  expect_error(
    get_pvol(c("nlhrw", "nldhlu"), time = as.POSIXct(Sys.Date())),
    class = "getRad_error_radar_not_character"
  )
  expect_error(
    get_pvol(c("nlhrw", "nldhl", "nlhrw"), time = as.POSIXct(Sys.Date())),
    class = "getRad_error_radar_not_character"
  )
  expect_error(
    get_pvol("nnhrw", time = as.POSIXct(Sys.Date())),
    class = "getRad_error_no_function_for_radar_with_country_code"
  )
})

test_that("get_pvol time argument", {
  expect_error(get_pvol("nlhrw", time = "asdf"),
    class = "getRad_error_time_not_correct"
  )
  expect_error(
    get_pvol("nlhrw"),
    class = "getRad_error_time_not_correct"
  )
  expect_error(
    get_pvol("nlhrw", time = 1L),
    class = "getRad_error_time_not_correct"
  )
  expect_error(
    get_pvol("nlhrw", time = Sys.Date()),
    class = "getRad_error_time_not_correct"
  )
  expect_error(
    get_pvol("nlhrw", time = as.POSIXct(Sys.Date())[c(1, 1)]),
    class = "getRad_error_time_not_correct"
  )
  expect_error(
    get_pvol("nlhrw", time = as.POSIXct(Sys.Date()) + 1),
    class = "getRad_error_time_not_correct"
  )
})

test_that("multiple radars work", {
  skip_if_offline()
  multiple_radars <- c("fianj", "dehnr")
  expect_type(
    pvl <- get_pvol(
      radar = multiple_radars,
      time = as.POSIXct(Sys.Date())
    ),
    "list"
  )

  expect_true(all(unlist(lapply(pvl, bioRad::is.pvol))))
  expect_identical(
    lapply(pvl, \(x) x$radar),
    as.list(multiple_radars)
  )
})

test_that("multiple timestamps work", {
  skip_if_offline()
  multiple_timestamps <-
    lubridate::ymd_hms(
      paste(
        lubridate::today(tzone = "UTC"),
        "00:45:00"
      ),
      paste(
        lubridate::today(tzone = "UTC"),
        "00:55:00"
      )
    )
  expect_type(
    pvl <- get_pvol(
      c("fianj"),
      time = multiple_timestamps
    ),
    "list"
  )
  expect_true(all(unlist(lapply(pvl, bioRad::is.pvol))))
  expect_identical(
    lapply(pvl, \(x) x$datetime),
    as.list(multiple_timestamps)
  )
})

test_that("multiple timestamps and radars work", {
  skip_if_offline()
  multiple_radars <- c("fivim", "deess")
  multiple_timestamps <-
    lubridate::ymd_hms(
      paste(
        lubridate::today(tzone = "UTC"),
        "00:35:00"
      ),
      paste(
        lubridate::today(tzone = "UTC"),
        "00:45:00"
      )
    )
  expect_type(
    pvl <- get_pvol(
      radar = multiple_radars,
      time = multiple_timestamps
    ),
    "list"
  )
  expect_true(all(unlist(lapply(pvl, bioRad::is.pvol))))
  expect_identical(
    lapply(pvl, \(x) x$datetime),
    as.list(rep(multiple_timestamps, each = 2))
  )
  expect_identical(
    lapply(pvl, \(x) x$radar),
    as.list(rep(multiple_radars, 2))
  )
})

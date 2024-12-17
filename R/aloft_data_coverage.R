#' Fetch the coverage table from the aloft data repository
#'
#' The coverage table provides an overview of what files are available on the
#' aloft data repository. Specifically it lists the directories that are
#' available as well as the number of files in every directory.
#'
#' @return A data.frame of the coverage file on the aloft data repository
#' @export
#'
#' @examplesIf interactive()
#' aloft_data_coverage()
aloft_data_coverage <- function() {
  # Discover what data is available for the requested radar and time interval
  coverage_url <- "https://aloftdata.s3-eu-west-1.amazonaws.com/coverage.csv"
  coverage <-
    vroom::vroom(coverage_url, progress = FALSE, show_col_types = FALSE) |>
    dplyr::mutate(source = string_extract(.data$directory, ".+(?=\\/hdf5)"),
                  radar = string_extract(.data$directory, "(?<=hdf5\\/)[a-z]{5}"),
                  date = as.Date(
                    string_extract(.data$directory,
                                   "[0-9]{4}\\/[0-9]{2}\\/[0-9]{2}$")
                  )
    )

  return(coverage)
}

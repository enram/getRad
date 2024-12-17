#' Extracts a substring from a string based on a regex pattern.
#'
#' This function uses regular expressions to extract a substring from a given string
#' based on a specified pattern. This is a base replacement of stringr::str_extract()
#'
#' @param string The input string from which the substring will be extracted.
#' @param pattern The regular expression pattern used to match the substring.
#'
#' @return The extracted substring.
#' @noRd
#'
#' @examples
#' string_extract("Hello World", "o W")
#'
string_extract <- function(string, pattern) {
  regmatches(string, regexpr(pattern, text = string, perl = TRUE))
}

#' Replace a pattern in a string with a replacement.
#'
#' This function uses regular expressions to replace a pattern in a string with a specified replacement.
#' This is a base replacement of stringr::str_replace()
#'
#' @param string The input string.
#' @param pattern The pattern to search for in the string.
#' @param replacement The replacement string.
#'
#' @return The modified string with the pattern replaced.
#' @noRd
#' @examples
#' string_replace("I'm looking for radars", "radar", "bird")
#'
string_replace <- function(string, pattern, replacement) {
  sub(pattern, replacement, string, perl = TRUE)
}

#' Round a lubridate interval
#'
#' Extension of [lubridate::round_date()] to round an interval, by default by
#' day. This means that of any given interval, the function will return the
#' interval as a floor of the interval start, to the ceiling of the interal end.
#'
#' @inheritParams lubridate::round_date
#'
#' @return An interval starting with the floor of `x` and ending with the
#'   ceiling of `x`, by the chosen unit.
#' @noRd
#'
#' @examples
#' round_interval(lubridate::interval("20230104 143204", "20240402 001206"))
round_interval <- function(x, unit = "day"){
  lubridate::interval(
    lubridate::floor_date(lubridate::int_start(x), unit),
    lubridate::ceiling_date(lubridate::int_end(x), unit)
  )
}

#' Set the list names to the unique value of the radar column
#'
#'
#' @param vpts_df_list A list of vpts data.frames
#'
#' @return A list of vpts data.frames with the names set to the unique value of
#'   the radar column of the data.frames
#'
#' @noRd
#' @examples
#'
#' list(dplyr::tibble(radar = "bejab"), dplyr::tibble(radar = "bewid")) |>
#'   radar_to_name()
radar_to_name <- function(vpts_df_list) {
  purrr::set_names(
    vpts_df_list,
    purrr::map_chr(
      vpts_df_list,
      \(df) unique(dplyr::pull(df, .data$radar))
    )
  )
}
#' Convert a character vector to integer, but do not warn.
#'
#' This function does not perform coercion, but conversion. For coercion see
#' [vctrs::vec_cast()](https://vctrs.r-lib.org/reference/vec_cast.html).
#'
#'
#' @param x A character vector
#'
#' @return An integer vector
#' @seealso [as_numeric_shh()] [as_logical_shh()]
#'
#' @noRd
#'
#' @examples
#' as_integer_shh(c("1", "2", "3"))
as_integer_shh <- function(x){
  if(!is.character(x)){
    cli::cli_abort("x must be a character vector")
  }
  suppressWarnings(as.integer(x))
}

#' Convert a character vector containing `Ỳ`, `N` and `NA` to a logical vector.
#'
#' @param x A character vector only containing `Y`, `N` and `NA`. Any other
#'   values will be silenty converted to `NA`.
#'
#' @return A logical vector
#' @seealso [as_numeric_shh()] [as_integer_shh()]
#'
#' @noRd
#'
#' @examples
#' yes_no_as_logical(c("Y", "N", NA, NA, "Y"))
#' yes_no_as_logical(c("Y", "foo", "bar", "N", NA))
yes_no_as_logical <- function(x) {
  # x needs to be a character vector
  if (!is.character(x)) {
    cli::cli_abort("x must be a character vector")
  }

  # Convert `Y` to TRUE, `N` to FALSE and `NA` to NA
  converted_vector <-
    dplyr::case_when(
      x == "Y" ~ TRUE,
      x == "N" ~ FALSE,
      .default = NA,
      .ptype = logical()
    )

  return(converted_vector)
}

#' Convert a character vector to numeric, but do not warn.
#'
#' This function does not perform coercion, but conversion. For coercion see
#' [vctrs::vec_cast()](https://vctrs.r-lib.org/reference/vec_cast.html).
#'
#' @param x A character vector
#'
#' @return A numeric vector
#' @seealso [as_integer_shh()] [as_logical_shh()]
#'
#' @noRd
#'
#' @examples
#' as_double_shh(c("1.1", "2.2", "3.3"))
as_numeric_shh <- function(x){
  if(!is.character(x)){
    cli::cli_abort("x must be a character vector")
  }
  suppressWarnings(as.numeric(x))
}

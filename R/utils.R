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

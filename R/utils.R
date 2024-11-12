#' Extracts a substring from a string based on a regex pattern.
#'
#' This function uses regular expressions to extract a substring from a given string
#' based on a specified pattern. This is a base replacement of stringr::str_extract()
#'
#' @param string The input string from which the substring will be extracted.
#' @param pattern The regular expression pattern used to match the substring.
#'
#' @return The extracted substring.
#'
#' @examples
#' string_extract("Hello World", "o W")
#' # Output: "o W"
#'
string_extract <- function(string, pattern) {
  regmatches(string, regexpr(pattern, text = string, perl = TRUE))
}

#' Convert a character vector to integer, but do not warn.
#'
#' This function does not perform coercion, but conversion. For coercion see
#' [vctrs::vec_cast()](https://vctrs.r-lib.org/reference/vec_cast.html).
#'
#'
#' @param x A character vector
#'
#' @return An integer vector
#' @seealso [as_double_shh()] [as_logical_shh()]
#' @examples
#' as_integer_shh(c("1", "2", "3"))
as_integer_shh <- function(x){
  if(!is.character(x)){
    cli::cli_abort("x must be a character vector")
  }
  suppressWarnings(as.integer(x))
}

#' Convert a character vector to logical, but do not warn.
#'
#' This function does not perform coercion, but conversion. For coercion see
#' [vctrs::vec_cast()](https://vctrs.r-lib.org/reference/vec_cast.html).
#'
#' @param x A character vector
#'
#' @return A logical vector
#' @seealso [as_double_shh()] [as_integer_shh()]
#' @examples
#' as_logical_shh(c("TRUE", "FALSE", "TRUE"))
as_logical_shh <- function(x){
  if(!is.character(x)){
    cli::cli_abort("x must be a character vector")
  }
  suppressWarnings(as.logical(x))
}

#' Convert a character vector to double, but do not warn.
#'
#' This function does not perform coercion, but conversion. For coercion see
#' [vctrs::vec_cast()](https://vctrs.r-lib.org/reference/vec_cast.html).
#'
#' @param x A character vector
#'
#' @return A double vector
#' @seealso [as_integer_shh()] [as_logical_shh()]
#' @examples
#' as_double_shh(c("1.1", "2.2", "3.3"))
as_double_shh <- function(x){
  if(!is.character(x)){
    cli::cli_abort("x must be a character vector")
  }
  suppressWarnings(as.double(x))
}

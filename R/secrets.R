#' @export
#' @rdname secret
get_secret <- function(name) {
  rlang::check_installed("keyring", "to manage secrets in getRad")
  if (!is_scalar_character(name)) {
    cli_abort("{.arg name} should be a scalar character",
      class = "getRad_error_get_secret_no_scalar_character"
    )
  }
  sname <- paste0(
    getOption("getRad.key_prefix",
      default = cli_abort("The option `getRad.key_prefix` is not found",
        class = "getRad_error_key_prefix_not_found_getting"
      )
    ), name
  )
  if (!(sname %in% keyring::key_list(sname)$service)) {
    cli_abort(
      c(
        x = "The {.arg {sname}} secret is not in the keyring.",
        i = 'Please use {.code set_secret("{name}")} to store the secret.'
      ),
      class = "getRad_error_secret_not_found"
    )
  }
  keyring::key_get(sname)
}

#' Setting and retrieving secrets from the keyring
#'
#' @param name Name of the secret to set or retrieve as a scalar character (e.g. `"nl_api_key"`)
#' @param secret Optionally a character string the the secret alternatively the system will prompt the user
#'
#' When working with a cluster it might be advantageous to use a specific keyring this can be done by setting the `keyring_backend` option in R.
#'
#' The package uses the option `getRad.key_prefix` as a prefix to all keys stored. If you want to use multiple keys for the same api you can manipulate this option.
#'
#' @return `get_secret` returns a character string, `set_secret` `TRUE` if successful.
#' @rdname secret
#'
#' @export

set_secret <- function(name, secret = NULL) {
  rlang::check_installed("keyring", "to manage secrets in getRad")
  if (!is_scalar_character(name)) {
    cli_abort("{.arg name} should be a scalar character",
      class = "getRad_error_set_secret_no_scalar_character"
    )
  }
  if (rlang::is_null(secret)) {
    cli_inform(list_secrets[[name]])
    rlang::check_installed("askpass", "To securely provide a secret")
    secret <- askpass::askpass(
      glue::glue("Please provide the value for `{name}`"))
  }
  sname <- paste0(
    getOption("getRad.key_prefix",
      default = cli_abort("The option `getRad.key_prefix` is not found",
        class = "getRad_error_key_prefix_not_found_setting"
      )
    ), name
  )
  keyring::key_set_with_value(service = sname, password = secret)
  invisible(TRUE)
}


list_secrets <- list(
  "dk_api_key" = c(
    i = "To obtain an api key for danish radar data please visit {.url https://dmiapi.govcloud.dk/#!/}, here a user account can be created  ({.href [documentation](https://opendatadocs.dmi.govcloud.dk/en/Authentication)}) and a api key (called {.arg radardataAPI}) can be obtained.",
    i = "Also confirm you adhere to the terms of use {.url https://opendatadocs.dmi.govcloud.dk/Terms_of_Use})."
  ),
  "nl_api_key" = c(
    i = "To obtain an api key for the Netherlands visit {.url https://developer.dataplatform.knmi.nl/open-data-api#token}.",
    i = "On the refered page also an public token is available for exploratory use this could be a easy option."
  )
)

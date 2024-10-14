test_that("get_secret fails correctly", {
  withr::with_options(list(keyring_backend = "env"), {
    expect_error(get_secret(c("unknown_secret", "unknown_secret2")), class = "getRad_error_get_secret_no_scalar_character")
    expect_error(get_secret(1L), class = "getRad_error_get_secret_no_scalar_character")
    expect_error(get_secret("unknown_secret"), class = "getRad_error_secret_not_found")
  })
})

test_that("set_secret fails correctly", {
  expect_error(set_secret(c("unknown_secret", "unknown_secret2")), class = "getRad_error_set_secret_no_scalar_character")
  expect_error(set_secret(1L), class = "getRad_error_set_secret_no_scalar_character")
})
test_that("Fails correctly when getRad.key_prefix is not set", {
  withr::with_options(list("getRad.key_prefix" = NULL), {
    expect_error(set_secret("dk_api_key", secret = "empty"), class = "getRad_error_key_prefix_not_found_setting")
    expect_error(get_secret("dk_api_key"), class = "getRad_error_key_prefix_not_found_getting")
  })
})
test_that("set_secret informs where to get secret", {
  withr::with_options(list(keyring_backend = "env"), {
    expect_silent(set_secret("dk_api_key", "asdf"))
    expect_silent(keyring::key_delete("getRad_dk_api_key"))
  })
})


test_that("setting and retrieving keys works", {
  withr::with_options(list(keyring_backend = "env"), {
    expect_true(set_secret("nl_api_key", "asdfasdf"))
    expect_equal(get_secret("nl_api_key"), "asdfasdf")
    expect_silent(keyring::key_delete("getRad_nl_api_key"))
  })
})


test_that("setting and retrieving keys works with getRad.key_prefix", {
  withr::with_options(list(keyring_backend = "env", getRad.key_prefix = "asdf"), {
    expect_true(set_secret("nl_api_key", "asdfasdf"))
    withr::with_options(list("getRad.key_prefix" = "getRad_"), {
      expect_error(get_secret("nl_api_key"), class = "getRad_error_secret_not_found")
    })
    expect_equal(get_secret("nl_api_key"), "asdfasdf")
    expect_silent(keyring::key_delete("asdfnl_api_key"))
  })
})

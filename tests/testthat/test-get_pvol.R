# Country specific tests are in the respective country specific `get_pvol_` files

test_that("get_pvol radar argument", {

  expect_error(get_pvol(), class = "getRad_error_radar_not_character")
  expect_error(get_pvol(1L), class = "getRad_error_radar_not_character")
  expect_error(get_pvol("nldhlu"), class = "getRad_error_radar_not_character")
  expect_error(get_pvol(c("nlhrw","nldhlu")), class = "getRad_error_radar_not_character")
  expect_error(get_pvol("nnhrw"),class="getRad_error_no_function_for_radar_with_country_code")
})

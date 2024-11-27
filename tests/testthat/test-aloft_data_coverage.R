test_that("aloft_data_coverage() returns a tibble", {
  skip_if_offline()

  expect_s3_class(
    aloft_data_coverage(),
    "tbl_df"
  )
})

test_that("aloft_data_coverage() returns the expected columns", {
  skip_if_offline()

  expect_named(
    aloft_data_coverage(),
    c("directory", "file_count", "source", "radar", "date")
  )
})

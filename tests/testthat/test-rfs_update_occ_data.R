test_that("rfs_update_occ_data handles empty data", {
  empty_data <- data.frame()
  expect_error(
    rfs_update_occ_data(
      occurrence_df = empty_data,
      species_database = rFishStatus::data_template_ref
    ),
    "The occurrence dataframe or the species database is empty. Please check the input."
  )
})

test_that("rfs_update_occ_data handles custom family", {
  empty_data <- data.frame()
  temp_folder <- tempfile("occ_data_test2")
  dir.create(temp_folder)
  expect_no_error(
    rfs_update_occ_data(
      occurrence_df = rFishStatus::data_occ_crenicichla,
      species_database = rFishStatus::data_template_ref,
      family_names = "Cichlidae",
      folder = temp_folder
    )
  )
})

test_that("rfs_update_occ_data handles auto family and return a data.frame", {
  empty_data <- data.frame()
  temp_folder <- tempfile("occ_data_test2")
  dir.create(temp_folder)
  expect_no_error(
    rfs_update_occ_data(
      occurrence_df = rFishStatus::data_occ_crenicichla,
      species_database = rFishStatus::data_template_ref,
      family_names = "auto",
      folder = temp_folder
    )
  )
  testthat::expect_s3_class(
    rfs_update_occ_data(
      occurrence_df = rFishStatus::data_occ_crenicichla,
      species_database = rFishStatus::data_template_ref,
      family_names = "auto",
      folder = temp_folder
    ),
    "data.frame"
  )
})

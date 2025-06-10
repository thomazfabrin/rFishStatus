test_that("rfs_auto_occ_update returns updated occurrence data for valid input", {
  ref_df <- rFishStatus::data_template_ref

  species <- c("Crenicichla britskii")

  folder <- tempfile("occ_test_folder_")

  result <- rFishStatus::rfs_auto_occ_update(ref_df, species, folder)
  expect_true(is.data.frame(result))
  expect_true(nrow(result) > 0)
})

test_that("rfs_auto_occ_update errors with invalid reference data", {
  bad_ref_df <- data.frame(a = 1:3, b = 4:6)
  species <- "Crenicichla britskii"
  folder <- tempfile("occ_test_folder_")
  expect_error(
    rfs_auto_occ_update(bad_ref_df, species, folder),
    "Reference database does not match the required template"
  )
})

test_that("rfs_auto_occ_update errors with NULL reference data", {
  species <- "Crenicichla britskii"
  folder <- tempfile("occ_test_folder_")
  expect_error(
    rfs_auto_occ_update(NULL, species, folder),
    "Reference database is NULL"
  )
})

test_that("rfs_auto_occ_update errors with no synonyms found", {
  ref_df <- rFishStatus::data_template_ref
  species <- "Nonexistent Species"
  folder <- tempfile("occ_test_folder_")

  expect_error(
    rfs_auto_occ_update(ref_df, species, folder),
    "No synonyms found for the provided species"
  )
})


test_that("rfs_auto_occ_update errors when no occurrences downloaded", {
  ref_df <- rFishStatus::data_template_ref
  species <- "Crenicichla britskii"
  folder <- tempfile("occ_test_folder_")

  mock_occ_data <- function(scientificName, limit) {
    list(data = data.frame())
  }

  with_mocked_bindings(
    occ_data = mock_occ_data,
    .package = "rgbif",
    {
      testthat::expect_error(
        rfs_auto_occ_update(ref_df, species, folder),
        "No occurrences downloaded"
      )
    }
  )
})

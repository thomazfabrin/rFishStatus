test_that("rfs_get_synonyms works correctly", {
  ref_list <- rFishStatus::rfs_get_valid(
    rFishStatus::data_template_ref
  )

  result_single <- rFishStatus::rfs_get_synonyms(
    ref_list,
    species = "Crenicichla britskii"
  )
  expect_true(is.list(result_single))
  expect_true(length(result_single) > 0)

  result_multiple <- rFishStatus::rfs_get_synonyms(
    ref_list,
    species = c("Crenicichla britskii", "Crenicichla lacustris")
  )
  expect_true(is.list(result_multiple))
  expect_true(length(result_multiple) > 0)

  result_no_synonyms <- rFishStatus::rfs_get_synonyms(
    ref_list,
    species = "Nonexistent Species"
  )
  expect_true(is.list(result_no_synonyms))
  expect_equal(length(result_no_synonyms), 0)
})

test_that("rfs_get_synonyms handles empty input", {
  ref_list <- rFishStatus::rfs_get_valid(
    rFishStatus::data_template_ref
  )

  result_empty <- rFishStatus::rfs_get_synonyms(
    ref_list,
    species = character(0)
  )
  expect_true(is.list(result_empty))
  expect_equal(length(result_empty), 0)
})

test_that("rfs_get_synonyms handles invalid input", {
  ref_list <- rFishStatus::rfs_get_valid(
    rFishStatus::data_template_ref
  )

  expect_error(
    rFishStatus::rfs_get_synonyms(
      ref_list = NULL,
      species = "Crenicichla britskii"
    ),
    "'ref_list' must be a data frame."
  )

  invalid_ref_list <- data.frame(
    valid_scientific_name = character(),
    scientific_name = character()
  )
  expect_error(
    rFishStatus::rfs_get_synonyms(
      invalid_ref_list,
      species = "Crenicichla britskii"
    ),
    "'ref_list' is missing required columns."
  )

  expect_error(
    rFishStatus::rfs_get_synonyms(ref_list, species = 123),
    "'species' must be a character vector."
  )

  expect_error(
    rFishStatus::rfs_get_synonyms(
      ref_list,
      species = "Crenicichla britskii",
      all_species = "yes"
    ),
    "'all_synonyms' must be a single logical value."
  )

  expect_error(
    rFishStatus::rfs_get_synonyms(
      ref_list,
      species = "Crenicichla britskii",
      complete_names = "no"
    ),
    "'complete_names' must be a single logical value."
  )
})

test_that("rfs_get_synonyms handles all_species and complete_names", {
  ref_list <- rFishStatus::rfs_get_valid(
    rFishStatus::data_template_ref
  )

  result_all_species <- rFishStatus::rfs_get_synonyms(
    ref_list,
    species = "Crenicichla britskii",
    all_species = TRUE
  )
  expect_true(is.character(result_all_species))
  expect_true(length(result_all_species) > 0)

  result_complete_names <- rFishStatus::rfs_get_synonyms(
    ref_list,
    species = "Crenicichla britskii",
    complete_names = TRUE
  )
  expect_true(is.list(result_complete_names))
  expect_true(length(result_complete_names) > 0)
})

test_that("rfs_detect_invasive works as expected", {
  temp_folder <- tempfile("occ_data_test")
  result <- rFishStatus::rfs_update_occ_data(
    species_database = rFishStatus::data_template_ref,
    occurrence_df = rFishStatus::data_occ_crenicichla,
    folder = temp_folder
  ) |>
    rFishStatus::rfs_join_basins() |>
    rFishStatus::rfs_detect_invasive(
      occ_df = _,
      reference_list = rFishStatus::rfs_get_species_list(
        rFishStatus::data_template_ref
      )
    )

  expect_true(is.data.frame(result))

  expect_true(all(c("valid_as_ref", "basin", "invasive") %in% colnames(result)))
})

test_that("rfs_detect_invasive handles logical parameters ", {
  temp_folder <- tempfile("occ_data_test")

  occ_w_basins <- rFishStatus::rfs_update_occ_data(
    species_database = rFishStatus::data_template_ref,
    occurrence_df = rFishStatus::data_occ_crenicichla,
    folder = temp_folder
  ) |>
    rFishStatus::rfs_join_basins()

  species_list <- rFishStatus::rfs_get_species_list(
    rFishStatus::data_template_ref
  )

  result_keep_invasive <- rFishStatus::rfs_detect_invasive(
    occ_df = occ_w_basins,
    reference_list = species_list,
    invasive_list = rFishStatus:::pr_invasive_species,
    keep_invasive = TRUE
  )

  expect_true(is.data.frame(result_keep_invasive))

  result_invasive_only <- rFishStatus::rfs_detect_invasive(
    occ_df = occ_w_basins,
    reference_list = species_list,
    invasive_list = rFishStatus:::pr_invasive_species,
    invasive_only = TRUE
  )
  expect_true(is.data.frame(result_invasive_only))
})

test_that("rfs_detect_invasive handles wrong entries", {
  temp_folder <- tempfile("occ_data_test")
  occ_w_basins <- rFishStatus::rfs_update_occ_data(
    species_database = rFishStatus::data_template_ref,
    occurrence_df = rFishStatus::data_occ_crenicichla,
    folder = temp_folder
  ) |>
    rFishStatus::rfs_join_basins()

  species_list <- rFishStatus::rfs_get_species_list(
    rFishStatus::data_template_ref
  )

  expect_error(
    rFishStatus::rfs_detect_invasive(
      occ_df = "1",
      reference_list = species_list,
      invasive_list = rFishStatus:::pr_invasive_species
    ),
    "occ_df must be a dataframe"
  )

  expect_error(
    rFishStatus::rfs_detect_invasive(
      occ_df = occ_w_basins,
      reference_list = "1",
      invasive_list = rFishStatus:::pr_invasive_species
    ),
    "reference_list must be a dataframe"
  )

  expect_error(
    rFishStatus::rfs_detect_invasive(
      occ_df = occ_w_basins,
      reference_list = species_list,
      invasive_list = "1"
    ),
    "invasive_list must be a dataframe"
  )

  expect_error(
    rFishStatus::rfs_detect_invasive(
      occ_df = occ_w_basins,
      reference_list = species_list,
      invasive_list = rFishStatus:::pr_invasive_species,
      keep_invasive = "1"
    ),
    "keep_invasive must be a logical value"
  )

  expect_error(
    rFishStatus::rfs_detect_invasive(
      occ_df = occ_w_basins,
      reference_list = species_list,
      invasive_list = rFishStatus:::pr_invasive_species,
      invasive_only = "1"
    ),
    "invasive_only must be a logical value"
  )

  expect_error(
    rFishStatus::rfs_detect_invasive(
      occ_df = occ_w_basins,
      reference_list = species_list,
      invasive_list = rFishStatus:::pr_invasive_species,
      keep_invasive = FALSE,
      invasive_only = TRUE
    ),
    "Cannot return only invasive species while keeping non-invasive species"
  )
})

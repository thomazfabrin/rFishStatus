test_that("rfs_get_species_list handles empty dataframe", {
  df <- data.frame()
  expect_error(
    rfs_get_species_list(df),
    "Your dataset is empty. Please check your input data."
  )
})

test_that("rfs_get_species_list returns a dataframe", {
  df <- rFishStatus::data_template_ref
  result <- rfs_get_species_list(df)
  expect_s3_class(result, "data.frame")
})

test_that("rfs_get_species_list handles summary parameter", {
  df <- rFishStatus::data_template_ref
  result <- rfs_get_species_list(df, summary = TRUE)
  expect_s3_class(result, "data.frame")
  expect_true("status" %in% colnames(result))
  expect_true("n" %in% colnames(result))
})

test_that("rfs_get_species_list processes valid species correctly", {
  df <- rFishStatus::data_template_ref
  result <- rfs_get_species_list(df)
  expect_true("scientific_name" %in% colnames(result))
  expect_true("status" %in% colnames(result))
  expect_true("Valid" %in% result$status)
})

test_that("rfs_get_species_list handles uncertain species", {
  df <- rFishStatus::data_template_ref
  df$scientific_name_status <- "Uncertain"
  result <- rfs_get_species_list(df)
  expect_true("Uncertain" %in% result$status)
})

test_that("rfs_get_species_list includes habitat information", {
  df <- rFishStatus::data_template_ref
  df$scientific_name_habitat <- "Freshwater"
  result <- rfs_get_species_list(df)
  expect_true("valid_sci_name_habitat" %in% colnames(result))
  expect_true("Freshwater" %in% result$valid_sci_name_habitat)
})

test_that("rfs_update_list handles valid input correctly", {
  input_list <- data.frame(
    species = c(
      "Asd asd",
      "Awe asd",
      "Axd asd"
    )
  )

  input_list_complete_name <- data.frame(
    species = c(
      "ASD asd",
      "ASD asd Spix & asd 1831",
      "ASD asd dsa & asd 2006"
    )
  )

  reference_list <- rfs_get_species_list(
    rFishStatus::data_template_ref
  )

  result <- rfs_update_list(input_list, reference_list)

  expect_true(is.data.frame(result))
  expect_true(all(
    c("query_species", "status", "species") %in% colnames(result)
  ))
  expect_equal(nrow(result), 3)
  expect_equal(
    result$status[1],
    "Not found"
  )

  result_complete_name <- rfs_update_list(
    input_list_complete_name,
    reference_list,
    complete_name = TRUE
  )
  expect_true(is.data.frame(result_complete_name))
  expect_true(all(
    c("query_species", "status", "species") %in% colnames(result_complete_name)
  ))
  expect_equal(nrow(result_complete_name), 3)
})

test_that("rfs_update_list handles missing species column in input_list", {
  input_list <- data.frame(
    name = c("Acanthochromis polyacanthus", "Cichla monoculus")
  )
  reference_list <- data.frame(
    species = c("Acanthochromis polyacanthus", "Cichla monoculus"),
    scientific_name = c("Acanthochromis polyacanthus", "Cichla monoculus"),
    status = c("Valid", "Valid")
  )

  expect_error(
    rfs_update_list(input_list, reference_list),
    "input_list must have a column named 'species'"
  )
})

test_that("rfs_update_list handles missing mandatory columns in reference_list", {
  input_list <- data.frame(
    species = c("Acanthochromis polyacanthus", "Cichla monoculus")
  )
  reference_list <- data.frame(
    species = c("Acanthochromis polyacanthus", "Cichla monoculus")
  )

  expect_error(
    rfs_update_list(input_list, reference_list),
    "Check your reference list"
  )
})

test_that("rfs_update_list performs fuzzy matching correctly", {
  input_list <- data.frame(
    species = c("Acanthochromis polyacanthus", "Cichla monoculos")
  )
  reference_list <- rFishStatus::rfs_get_species_list(
    rFishStatus::data_template_ref
  )

  result <- rfs_update_list(input_list, reference_list, max_dist = 2)

  expect_true("Cichla monoculos" %in% result$query_species)
  expect_true(any(grepl("fuzzy matching", result$status)))
})

test_that("rfs_update_list handles empty input_list", {
  input_list <- data.frame(species = character(0))
  reference_list <- rFishStatus::rfs_get_species_list(
    rFishStatus::data_template_ref
  )

  expect_error(
    rfs_update_list(input_list, reference_list),
    "input_list is empty"
  )
})

test_that("rfs_update_list handles invalid complete_name parameter", {
  input_list <- data.frame(
    species = c("Acanthochromis polyacanthus", "Cichla monoculus")
  )
  reference_list <- data.frame(
    species = c("Acanthochromis polyacanthus", "Cichla monoculus"),
    scientific_name = c("Acanthochromis polyacanthus", "Cichla monoculus"),
    status = c("Valid", "Valid")
  )

  expect_error(
    rfs_update_list(input_list, reference_list, complete_name = "invalid"),
    "complete_name must be a logical"
  )
})

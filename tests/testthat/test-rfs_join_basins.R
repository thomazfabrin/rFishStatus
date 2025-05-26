test_that("rfs_join_basins handles empty or wrong data", {
  expect_error(
    rfs_join_basins(
      occ_df = data.frame(),
      shapefile = "br_pr_basins"
    ),
    "The occurrence dataset is empty. Please check the dataset."
  )

  expect_error(
    rfs_join_basins(
      occ_df = data.frame(
        decimalLatitude = c(1, 2, 3),
        decimalLongitude = c(4, 5, 6)
      ),
      shapefile = "br_pr_basins"
    ),
    "The occurrence dataset does not have the same columns as the output from rFishStatus::rfs_updated_occ_data()."
  )

  expect_error(
    rfs_join_basins(
      occ_df = rFishStatus:::data_occ_update_result,
      shapefile = 1
    ),
    "The shapefile format is not correct. Please check."
  )

  expect_error(
    rfs_join_basins(
      occ_df = rFishStatus:::data_occ_update_result,
      shapefile = NULL
    ),
    "The shapefile is not available. Please check the shapefile name."
  )
})

test_that("rfs_join_basins works", {
  expect_no_error(
    rfs_join_basins(
      occ_df = rFishStatus:::data_occ_update_result
    )
  )
  df <- rfs_join_basins(occ_df = rFishStatus:::data_occ_update_result)
  expect_true("basin" %in% colnames(df))
})

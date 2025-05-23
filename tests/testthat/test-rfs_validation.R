test_that("rfs_validation handles missing species_database or data", {
    expect_error(
        rfs_validation(
            species_database = data.frame(),
            data = NULL,
            data_type = "occ_data"
        ),
        "The species database or the data is empty. Please check the input."
    )
})

test_that("rfs_validation handles wrong types", {
    expect_error(
        rfs_validation(
            species_database = data.frame(species = "A"),
            data = data.frame(species = "B"),
            data_type = "wrong_data_type"
        ),
        "data_type must be either 'occ_data' or 'spp_list'."
    )
})

test_that("rfs_validation handles wrong species_database structure", {
    expect_error(
        rfs_validation(
            species_database = data.frame(species = "A"),
            data = data.frame(species = "B"),
            data_type = "occ_data"
        ),
        "The columns of species_database do not match those of rFishStatus template. Please check your input."
    )
})

test_that("rfs_validation handles wrong occ_data structure", {
    expect_error(
        rfs_validation(
            species_database = rFishStatus::data_template_ref,
            data = data.frame(species = "B"),
            data_type = "occ_data"
        ),
        "The columns of data do not match those of rFishStatus::rfs_download_occ_data. Please check your input."
    )
})

test_that("rfs_validation returns a list containing two data frames.", {
    result <- rfs_validation(
        species_database = rFishStatus::data_template_ref,
        data = rFishStatus:::data_occ_update_result,
        data_type = "occ_data",
        ref_spp_list = rFishStatus::rfs_get_species_list(
            rFishStatus::data_template_ref
        )
    )
    expect_true(is.list(result))
    expect_true("attention_df" %in% names(result))
    expect_true("correct_df" %in% names(result))
})

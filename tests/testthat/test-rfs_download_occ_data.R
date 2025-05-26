test_that("rfs_download_occ_data handles invalid email", {
  expect_error(
    rfs_download_occ_data(
      gbif_user = "user",
      gbif_pwd = "pwd",
      gbif_email = "invalid_email",
      country = "Brazil",
      state = "Paraná"
    ),
    regexp = "Invalid email"
  )
})

test_that("rfs_download_occ_data handles user", {
  expect_error(
    rfs_download_occ_data(
      gbif_user = 123,
      gbif_pwd = "pwd",
      gbif_email = "valid_email@email.com",
      country = "Brazil",
      state = "Paraná"
    ),
    regexp = "country, state, gbif_user, gbif_pwd, and gbif_email should be characters."
  )
})

test_that("rfs_download_occ_data handles user polygon", {
  skip_on_cran()
  temp_folder <- tempfile("occ_data_test2")
  dir.create(temp_folder)
  expect_no_error(
    rfs_download_occ_data(
      gbif_user = Sys.getenv("GBIF_USER"),
      gbif_pwd = Sys.getenv("GBIF_PWD"),
      gbif_email = Sys.getenv("GBIF_EMAIL"),
      folder_name = temp_folder,
      user_polygon = rnaturalearth::ne_states(
        country = "Brazil",
        returnclass = "sf"
      ) |>
        dplyr::filter(name == "Paraná"),
      reverse_polygon = TRUE,
      taxonkey = 5208140,
      max_attempts = 75,
      sleep_time = 20
    )
  )
})

test_that("rfs_download_occ_data handles non-numeric max_attempts and sleep_time", {
  expect_error(
    rfs_download_occ_data(
      gbif_user = "user",
      gbif_pwd = "pwd",
      gbif_email = "user@email.com",
      country = "Brazil",
      state = "Paraná",
      max_attempts = "ten",
      sleep_time = "sixty"
    ),
    regexp = "should be numbers"
  )
})

test_that("rfs_download_occ_data creates folder", {
  skip_on_cran()
  # dotenv::load_dot_env()
  temp_folder <- tempfile("occ_data_test2")
  dir.create(temp_folder)
  expect_error(
    rfs_download_occ_data(
      gbif_user = Sys.getenv("GBIF_USER"),
      gbif_pwd = Sys.getenv("GBIF_PWD"),
      gbif_email = Sys.getenv("GBIF_EMAIL"),
      country = "Brazil",
      state = "Paraná",
      taxonkey = 5208140,
      folder_name = temp_folder,
      max_attempts = 75,
      sleep_time = 20
    )
  )
  unlink(temp_folder, recursive = TRUE)
})

test_that("rfs_download_occ_data does not fail if folder already exists", {
  # skip_on_cran()
  # dotenv::load_dot_env()
  temp_folder <- tempfile("occ_data_test_folder2")
  dir.create(temp_folder)
  expect_true(dir.exists(temp_folder))
  expect_error(
    rfs_download_occ_data(
      gbif_user = Sys.getenv("GBIF_USER"),
      gbif_pwd = Sys.getenv("GBIF_PWD"),
      gbif_email = Sys.getenv("GBIF_EMAIL"),
      country = "Brazil",
      state = "Paraná",
      taxonkey = 5208140,
      reverse_polygon = TRUE,
      folder_name = temp_folder,
      max_attempts = 75,
      sleep_time = 20
    ),
    NA
  )
  expect_true(dir.exists(temp_folder))
})

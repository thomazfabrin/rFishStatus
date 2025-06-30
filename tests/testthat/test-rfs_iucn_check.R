test_that("rfs_iucn_check handles missing species", {
  expect_error(
    rfs_iucn_check(iucn_api_key = "dummy"),
    regexp = "Species must be provided"
  )
})

test_that("rfs_iucn_check handles missing API key", {
  temp_dir <- tempdir()
  expect_error(
    rfs_iucn_check("Pterophyllum scalare", log_path = temp_dir),
    regexp = "IUCN API key must be provided"
  )
})

test_that("rfs_iucn_check returns data frame for not found species", {
  temp_dir <- tempdir()

  # Temporarily load .env variables only for this test
  if (Sys.getenv("IUCN_API_KEY") == "" && file.exists(".env")) {
    withr::with_envvar(
      dotenv::load_dot_env(override = FALSE, verbose = FALSE),
      {
        skip_if_not(
          nzchar(Sys.getenv("IUCN_API_KEY")),
          "IUCN_API_KEY not available"
        )

        iucn_api_key <- Sys.getenv("IUCN_API_KEY")
        expect_s3_class(
          rfs_iucn_check(
            "Genus scalare",
            iucn_api_key,
            log_path = temp_dir
          ),
          "data.frame"
        )
      }
    )
  } else {
    # Use existing environment variable
    skip_if_not(
      nzchar(Sys.getenv("IUCN_API_KEY")),
      "IUCN_API_KEY not available"
    )

    iucn_api_key <- Sys.getenv("IUCN_API_KEY")
    expect_s3_class(
      rfs_iucn_check("Genus scalare", iucn_api_key, log_path = temp_dir),
      "data.frame"
    )
  }
})

test_that("rfs_iucn_check returns data frame for found species", {
  temp_dir <- tempdir()

  # Temporarily load .env variables only for this test
  if (Sys.getenv("IUCN_API_KEY") == "" && file.exists(".env")) {
    withr::with_envvar(
      dotenv::load_dot_env(override = FALSE, verbose = FALSE),
      {
        skip_if_not(
          nzchar(Sys.getenv("IUCN_API_KEY")),
          "IUCN_API_KEY not available"
        )

        iucn_api_key <- Sys.getenv("IUCN_API_KEY")
        expect_s3_class(
          rfs_iucn_check(
            "Pterophyllum scalare",
            iucn_api_key,
            log_path = temp_dir
          ),
          "data.frame"
        )
      }
    )
  } else {
    # Use existing environment variable
    skip_if_not(
      nzchar(Sys.getenv("IUCN_API_KEY")),
      "IUCN_API_KEY not available"
    )

    iucn_api_key <- Sys.getenv("IUCN_API_KEY")
    expect_s3_class(
      rfs_iucn_check("Pterophyllum scalare", iucn_api_key, log_path = temp_dir),
      "data.frame"
    )
  }
})

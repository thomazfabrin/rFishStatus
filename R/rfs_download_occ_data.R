#' @title Download occurrence data for a specific area from GBIF
#' @description This function downloads occurrence data for a specific area from
#' GBIF. Obs.: This function is unstable and may not work properly due to time
#' constraints and server limitations while downloading the data. If the first
#' execution did not work properly, please try to rerun the function using the
#' same code. Alternatively, you can use the link, showed during function
#' execution, to download the data manually.
#' @param gbif_user GBIF username.
#' @param gbif_pwd GBIF password.
#' @param gbif_email GBIF email.
#' @param country Country name. Default is Brazil.
#' @param state State name. Default is Paraná.
#' @param user_polygon User-defined polygon. Default is NULL.
#' @param taxonkey GBIF taxonKey. Default is Chordata (44).
#' @param max_attempts Maximum number of download attempts. Default is 10
#' @param sleep_time Sleep time between download attempts. Default is 120 s.
#' @param reverse_polygon Logical. If TRUE, the polygon will be reversed.
#' @param folder_name Folder name where the data will be saved. Default is
#' "occ_data".
#' @param link Download link for the occurrence data. Default is NULL.
#' @return Downloaded occurrence data.
#' @export
#' @examples
#'\dontrun{
#' rfs_download_occ_data(gbif_user = "gbif_user",
#'   gbif_pwd = "gbif_pwd",
#'   gbif_email = "gbif_email",
#'   country = "Brazil",
#'   state = "São Paulo")
#' }
rfs_download_occ_data <- function(
    gbif_user,
    gbif_pwd,
    gbif_email,
    country = "Brazil",
    state = "Paraná",
    taxonkey = 44,
    user_polygon = NULL,
    max_attempts = 10,
    sleep_time = 120,
    reverse_polygon = FALSE,
    folder_name = "occ_data",
    link = NULL
) {
    start_time <- Sys.time()

    if (!grepl("@", gbif_email)) {
        cli::cli_abort(" Invalid email.")
    }
    if (!is.numeric(max_attempts) || !is.numeric(sleep_time)) {
        cli::cli_abort(" max_attempts and sleep_time should be numbers")
    }
    if (
        !is.character(gbif_user) ||
            !is.character(gbif_pwd) ||
            !is.character(gbif_email)
    ) {
        cli::cli_abort(
            " country, state, gbif_user, gbif_pwd, and gbif_email should be characters."
        )
    }
    if (!is.null(user_polygon)) {
        country <- NULL
        state <- NULL
    }
    if (is.null(user_polygon)) {
        country_polygon <- rnaturalearth::ne_states(
            country = country,
            returnclass = "sf"
        ) |>
            dplyr::filter(name == state)
    } else if (!is.null(user_polygon) && is.null(country) && is.null(state)) {
        country_polygon <- user_polygon
        if (nrow(country_polygon) > 1)
            country_polygon <- country_polygon <- sf::st_union(country_polygon)
        is_valid <- sf::st_is_valid(country_polygon)
        if (!is_valid) sf::st_make_valid(country_polygon)
    }
    if (reverse_polygon) country_polygon <- sf::st_reverse(country_polygon)

    state_polygon <- sf::st_make_valid(country_polygon)

    geom_query <- sf::st_cast(country_polygon$geometry[[1]], "MULTIPOLYGON") |>
        sf::st_as_text()

    if (is.null((link))) {
        if (!is.null(taxonkey)) {
            occ_link <- rgbif::occ_download(
                rgbif::pred_in("geometry", geom_query),
                rgbif::pred_in("taxonKey", taxonkey),
                user = gbif_user,
                pwd = gbif_pwd,
                email = gbif_email,
                format = "SIMPLE_CSV"
            )
        } else {
            occ_link <- rgbif::occ_download(
                rgbif::pred_in("geometry", geom_query),
                user = gbif_user,
                pwd = gbif_pwd,
                email = gbif_email,
                format = "SIMPLE_CSV"
            )
        }
        download_link <- attr(occ_link, "downloadLink")
    } else {
        download_link <- link
    }

    if (is.null(download_link)) {
        cli::cli_abort(" The download link is not available.")
    } else {
        cli::cli_alert_info(paste(
            " The download link is available. \n",
            paste(download_link)
        ))
    }

    cli::cli_alert_info(" Downloading the data...")

    file_path <- paste0(folder_name, "/occ_per_area.zip")

    if (!dir.exists(folder_name)) {
        tryCatch(
            {
                cli::cli_alert_info(" Creating the folder...")
                dir.create(folder_name, showWarnings = FALSE, recursive = TRUE)
                cli::cli_alert_success("Folder created!")
            },
            error = function(e) {
                cli::cli_alert_warning("Error creating folder.")
                cli::cli_alert_warning(paste("Error: ", e))
            }
        )
    }

    pb <- progress::progress_bar$new(
        format = "  Attempting download [:bar] :percent in :elapsed. Remaining: :remaining_time",
        total = max_attempts,
        width = 80,
        clear = FALSE
    )

    attempt <- 0
    response <- NULL

    while (is.null(response) || httr::status_code(response) != 200) {
        response <- httr::GET(
            download_link
        )
        attempt <- attempt + 1

        remaining_time <- (max_attempts - attempt) * sleep_time

        pb$tick(
            tokens = list(
                remaining_time = sprintf(
                    "%02d:%02d",
                    remaining_time %/% 60,
                    remaining_time %% 60
                )
            )
        )

        if (attempt >= max_attempts) break
        if (httr::status_code(response) != 200) {
            Sys.sleep(sleep_time)
        }
    }

    httr::GET(
        download_link,
        httr::write_disk(file_path, overwrite = TRUE),
        httr::progress()
    )

    end_time <- Sys.time()
    download_time <- difftime(end_time, start_time, units = "secs")
    if (httr::status_code(response) == 200) {
        cli::cli_alert_success(paste0(
            "File downloaded successfully in ",
            round(download_time, 2) / 60,
            " minutes."
        ))
    } else {
        cli::cli_alert_danger(
            "Download failed after ",
            max_attempts,
            " attempts."
        )
    }

    if (!file.exists(file_path)) {
        cli::cli_abort("File does not exist. Try again.")
    } else {
        cli::cli_alert_info("Unzipping the file...")
    }

    if (file.exists(file_path)) {
        tryCatch(
            {
                unzip(file_path, exdir = folder_name)
                cli::cli_alert_success("File unzipped successfully!")
            },
            warning = function(w) {
                cli::cli_alert_warning(
                    "File might be corrupted. Please, try again or download the data manually using the link."
                )
                cli::cli_alert_warning(paste("Warning: ", w))
                cli::cli_alert_info(
                    "Would you like to try downloading the file again?"
                )

                answer <- readline(
                    prompt = "Enter your choice (yes/no): "
                )

                if (tolower(answer) == "yes") {
                    rFishStatus::rfs_download_occ_data(
                        gbif_user,
                        gbif_pwd,
                        gbif_email,
                        country,
                        state,
                        taxonkey,
                        user_polygon,
                        max_attempts,
                        sleep_time,
                        reverse_polygon,
                        folder_name,
                        link = download_link
                    )
                } else {
                    cli::cli_abort("Download aborted by the user.")
                }
            }
        )
    }
}

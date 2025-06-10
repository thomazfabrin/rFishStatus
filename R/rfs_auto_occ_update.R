#' @title Get Synonyms for a Given Species and Update Occurrence Data
#' @description This function retrieves synonyms for given species from a
#' reference list and updates occurrence data for each species (GBIF).
#' @param ref_df A reference dataset following the rFishStatus template (for this function,
#' the dataset should be filled with all species from a family).
#' @param species A character vector representing the species names to query.
#' @param folder A character vector representing the folder path to save the
#' @return A data frame containing updated occurrence data.
#' @examples
#' \dontrun{
#' result <- rfs_auto_occ_update(ref_df, species, folder)
#' }
#' @export
#'
rfs_auto_occ_update <- function(ref_df, species, folder) {
    cli::cli_alert_info("Obtaining reference data and species list...")

    reference_db <- tryCatch(
        {
            ref_df
        },
        error = function(e) {
            cli::cli_abort("Error obtaining reference data: {e$message}")
        }
    )

    if (is.null(reference_db)) {
        cli::cli_abort("Reference database is NULL. Please check your input.")
    }

    if (!is.data.frame(reference_db)) {
        cli::cli_abort("Reference database must be a data frame.")
    }

    if (
        !all(
            colnames(rFishStatus::data_template_ref) %in% colnames(reference_db)
        )
    ) {
        cli::cli_abort(
            "Reference database does not match the required template."
        )
    }

    cli::cli_alert_info("Getting species list from the reference database...")

    suppressMessages(
        reference_list <- tryCatch(
            {
                rFishStatus::rfs_get_species_list(reference_db)
            },
            error = function(e) {
                cli::cli_abort("Error obtaining species list: {e$message}")
            }
        )
    )

    if (is.null(reference_list)) {
        cli::cli_abort(
            "Species list is NULL. Please check your reference data."
        )
    }

    synonyms <- tryCatch(
        {
            rFishStatus::rfs_get_synonyms(
                ref_list = reference_list,
                species = species,
                all_species = TRUE
            )
        },
        error = function(e) {
            cli::cli_abort("Error obtaining synonyms: {e$message}")
        }
    )

    if (length(synonyms) == 0) {
        cli::cli_abort("No synonyms found for the provided species.")
    } else {
        cli::cli_alert_success(
            "Total species (synonyms + valid species) obtained for the provided species: {length(synonyms)}"
        )
    }

    cli::cli_alert_info("Downloading occurrences...")

    download_occ_function <- function(synonym) {
        occ_data <- tryCatch(
            {
                rgbif::occ_data(
                    scientificName = synonym,
                    limit = 99000
                )
            },
            error = function(e) {
                cli::cli_alert_warning(paste(
                    "Error downloading occurrences for",
                    synonym,
                    ":",
                    e$message
                ))
                return(NULL)
            }
        )

        if (is.null(occ_data)) {
            cli::cli_alert_warning(paste(
                "No data found for",
                synonym
            ))
            return(data.frame())
        }

        occ_data <- occ_data$data |>
            as.data.frame()

        if (nrow(occ_data) == 0) {
            cli::cli_alert_warning(paste("No data found for", synonym))
            return(data.frame())
        }

        cli::cli_alert_success(paste(
            "Occurrences downloaded for",
            synonym
        ))

        return(occ_data)
    }

    occ_df <- purrr::map_dfr(
        synonyms,
        download_occ_function,
        .progress = TRUE
    )

    occ_df <- occ_df[, !vapply(occ_df, is.list, logical(1))]

    if (nrow(occ_df) == 0) {
        cli::cli_abort(
            "No occurrences downloaded. Please check the species names."
        )
    }

    if (!dir.exists(folder)) {
        dir.create(folder, recursive = TRUE)
        cli::cli_alert_info("Created folder: {folder}")
    }

    cli::cli_alert_success("All occurrences downloaded.")
    cli::cli_alert_info("Updating occurrence data...")

    occ_updated <- tryCatch(
        {
            rFishStatus::rfs_update_occ_data(
                species_database = reference_db,
                occurrence_df = occ_df,
                folder = folder
            )
        },
        error = function(e) {
            cli::cli_abort("Error updating occurrence data: {e$message}")
        }
    )

    if (is.null(occ_updated)) {
        cli::cli_abort("No occurrence data updated. Please check your input.")
    }

    cli::cli_alert_success("Occurrence data updated.")

    return(occ_updated)
}

#' @title Detect invasive species in occurrence data
#' @description This function detects invasive species in occurrence data based
#' on a list of invasive species and a reference list. It can keep or remove
#' invasive species from the occurrence data, and can also return only the
#' invasive species from the occurrence data. The function will use a list
#' of invasive fish species from Paraná state in Brazil as default in case
#' no invasive_list is provided. The list was published by
#' @param occ_df The occurrence data as a data frame. Obtained from
#' rFishStatus::rfs_join_basins().
#' @param invasive_list A dataframe indicating species, basins and if the
#' species is invasive (1) or not (0). Dataframe must have the columns
#' 'species', 'basin' and 'invasive'.
#' @param reference_list A reference database if species obtained from
#' rFishStatus::rfs_get_species_list().
#' @param keep_invasive A logical value indicating whether to keep
#' invasive species in the occurrence data. Default is TRUE, which
#' keeps invasive species.
#' @param invasive_only A logical value indicating whether to return only
#' the invasive species in the occurrence data. Default is FALSE,
#' which returns all species.
#' @return The filtered occurrence data as a data frame.
#' @export
#' @examples
#' \dontrun{
#' # Filter invasive species and keep only non-invasive species
#' filtered_data <- rfs_detect_invasive(occ_df, invasive_list,
#'                                      reference_list, keep_invasive = FALSE)
#' }
rfs_detect_invasive <- function(
    occ_df,
    invasive_list = NULL,
    reference_list,
    keep_invasive = TRUE,
    invasive_only = FALSE
) {
    cli::cli_progress_message(
        "Detecting invasive species in occurrence data..."
    )
    if (!is.data.frame(occ_df)) {
        cli::cli_abort("occ_df must be a dataframe")
    }

    if (nrow(occ_df) == 0) {
        cli::cli_abort("occ_df must not be empty")
    }

    if (is.null(invasive_list)) {
        cli::cli_alert_info(
            "Using default invasive species list for Paraná state."
        )
        invasive_list <- rFishStatus:::pr_invasive_species
    }

    if (!is.data.frame(invasive_list)) {
        cli::cli_abort("invasive_list must be a dataframe")
    }

    if (nrow(invasive_list) == 0) {
        cli::cli_abort("invasive_list must not be empty")
    }

    if (!is.data.frame(reference_list)) {
        cli::cli_abort("reference_list must be a dataframe")
    }

    if (nrow(reference_list) == 0) {
        cli::cli_abort("reference_list must not be empty")
    }

    if (!is.logical(keep_invasive) || length(keep_invasive) != 1) {
        cli::cli_abort("keep_invasive must be a logical value")
    }

    if (!is.logical(invasive_only) || length(invasive_only) != 1) {
        cli::cli_abort("invasive_only must be a logical value")
    }

    if (invasive_only && !keep_invasive) {
        cli::cli_abort(
            "Cannot return only invasive species while keeping non-invasive species"
        )
    }

    invasive_species <- invasive_list |>
        unique() |>
        dplyr::select(species)

    check_species <- rFishStatus::rfs_update_list(
        input_list = invasive_species,
        reference_list = reference_list,
        complete_name = FALSE,
        max_dist = 5
    )

    invasive_species_checked <- invasive_list |>
        dplyr::left_join(
            x = _,
            dplyr::select(check_species, species, status),
            by = c("species" = "species")
        )

    invasive_species_checked_long <- invasive_species_checked |>
        tidyr::separate_rows(basin, sep = ";\\s")
    inv_species_checked_long_list <- invasive_species_checked_long |>
        dplyr::distinct(species) |>
        dplyr::pull(species)

    cli::cli_progress_message(
        "Joining occurrence data with invasive species by basin..."
    )
    occ_df_w_invasive <- occ_df |>
        dplyr::left_join(
            x = _,
            dplyr::select(check_species, valid_scientific_name, species),
            by = c("valid_as_ref" = "valid_scientific_name")
        ) |>
        dplyr::left_join(
            x = _,
            dplyr::select(
                invasive_species_checked_long,
                species,
                basin,
                invasive
            ),
            by = c("species" = "species", "basin" = "basin"),
            relationship = "many-to-many"
        ) |>
        dplyr::distinct() |>
        dplyr::mutate(
            invasive = dplyr::if_else(
                is.na(invasive),
                0,
                invasive
            ),
            invasive = dplyr::if_else(
                invasive == 1,
                "yes",
                as.character(invasive)
            ),
            invasive = dplyr::if_else(
                invasive == "0" &
                    valid_as_ref %in% inv_species_checked_long_list,
                "probably invasive",
                invasive
            ),
            invasive = dplyr::if_else(
                invasive == "0",
                "not listed",
                invasive
            )
        ) |>
        dplyr::select(-species)

    if (isFALSE(keep_invasive)) {
        occ_df_w_invasive <- occ_df_w_invasive |>
            dplyr::filter(invasive == "not listed")
    }
    if (invasive_only) {
        occ_df_w_invasive <- occ_df_w_invasive |>
            dplyr::filter(invasive %in% c("yes", "probably invasive"))
    }

    cli::cli_alert_success("Invasive species detected in occurrence data.")

    return(occ_df_w_invasive)
}

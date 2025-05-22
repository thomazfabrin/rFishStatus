#' @title Update occurrence data
#' @description This function checks the occurrence data against a
#' reference list and update it.
#' @param species_database Species database according to rFishStatus template
#' (see rFishStatus::data_template_ref).
#' @param occurrence_df Occurrence dataframe obtained from
#' rFishStatus::rfs_download_occ_data().
#' @param family_names Family names to be used in the filtering
#' occurrence dataframe. Default is "auto".
#' @param folder Folder name where the data will be saved.
#' Default is "data".
#' @return Occurrence dataframe with updated species names based on the reference
#' dataset (see columns with "ref" suffix) or FishBase catalog (see columns with
#' "fb" suffix).
#' @export
#' @examples
#' rfs_update_occ_data(species_database = rFishStatus::data_template_ref,
#'                     occurrence_df = occ_df)
rfs_update_occ_data <- function(
    species_database,
    occurrence_df,
    family_names = "auto",
    folder = "data"
) {
    start_time <- Sys.time()

    if (nrow(occurrence_df) == 0 || nrow(species_database) == 0) {
        cli::cli_abort(
            "The occurrence dataframe or the species database is empty. Please check the input."
        )
    }

    if (length(family_names) == 1 && family_names == "auto") {
        tryCatch(
            {
                family_names <- rFishStatus::data_fish_families
            },
            error = function(e) {
                cli::cli_alert_warning(paste("An error occurred:", e$message))
            }
        )
    } else {
        family_names <- family_names
    }

    cli::cli_alert_info("Filtering occurrences by fish families...")
    df_occ_filtered <- occurrence_df |>
        dplyr::filter(family %in% family_names & !is.na(species))

    cli::cli_alert_info(
        "Getting reference species list from Eschmeyer's Catalog of Fishes and FishBase..."
    )
    spp_list_gbif <- df_occ_filtered |>
        dplyr::filter(family %in% family_names) |>
        dplyr::select(species) |>
        dplyr::filter(!is.na(species)) |>
        dplyr::distinct()

    spp_list_ref <- rFishStatus::rfs_get_species_list(species_database)

    suppressMessages(
        fishbase_taxa <- rfishbase::load_taxa()
    )

    suppressMessages(
        fishbase_spp <- spp_list_gbif |>
            dplyr::pull(species) |>
            rfishbase::synonyms() |>
            dplyr::filter(!Status == "misapplied name") |>
            dplyr::left_join(
                dplyr::select(fishbase_taxa, SpecCode, Family),
                by = c("SpecCode" = "SpecCode")
            ) |>
            dplyr::group_by(synonym) |>
            dplyr::mutate(
                Status = paste0(Status, collapse = " OR "),
                Family = paste0(Family, collapse = " OR "),
                Species = paste0(Species, collapse = " OR "),
                n = dplyr::n()
            ) |>
            dplyr::select(
                synonym,
                Status,
                Family,
                Species,
                n
            ) |>
            dplyr::distinct() |>
            dplyr::mutate(
                Status = dplyr::if_else(
                    stringr::str_detect(
                        Status,
                        "accepted name"
                    ) &
                        stringr::str_detect(Status, "synonym"),
                    "Valid or Synonym - please check",
                    Status
                ),
                Status = dplyr::if_else(
                    stringr::str_detect(
                        Status,
                        "accepted name"
                    ) &
                        !stringr::str_detect(Status, "synonym"),
                    "Valid",
                    Status
                ),
                Status = dplyr::if_else(
                    !stringr::str_detect(
                        Status,
                        "accepted name"
                    ) &
                        stringr::str_detect(Status, "synonym"),
                    "Synonym",
                    Status
                ),
                Family = purrr::map_chr(
                    strsplit(Family, " OR "),
                    ~ paste(unique(trimws(.x)), collapse = " OR ")
                ),
                family_count = stringr::str_count(Family, " OR ") + 1,
            ) |>
            dplyr::select(
                -n,
                -family_count
            ) |>
            dplyr::rename(
                species_fb = synonym,
                family_fb = Family,
                status_fb = Status,
                valid_as_fb = Species
            )
    )

    cli::cli_alert_info("Updating occurrence dataset...")
    spp_list_verified <- rFishStatus::rfs_update_list(
        input_list = spp_list_gbif,
        complete_name = FALSE,
        reference_list = spp_list_ref,
        max_dist = 2
    )

    df_occ_verified <- df_occ_filtered |>
        dplyr::left_join(
            spp_list_verified,
            by = c("species" = "query_species"),
            keep = TRUE
        ) |>
        dplyr::rename(
            species_gbif = species.x,
            species_ref = species.y,
            family_ref = family.y,
            status_ref = status,
            valid_as_ref = valid_scientific_name
        ) |>
        dplyr::left_join(
            fishbase_spp,
            by = c("species_gbif" = "species_fb"),
            keep = TRUE
        ) |>
        dplyr::select(-query_species, -species_ref, -species_fb) |>
        dplyr::mutate(
            genus_ref = stringr::word(valid_as_ref, 1),
            genus_ref = dplyr::if_else(
                status_ref == "Not found",
                NA_character_,
                genus_ref
            ),
            genus_fb = stringr::word(valid_as_fb, 1)
        ) |>
        dplyr::relocate(
            species_gbif,
            .before = status_ref
        ) |>
        dplyr::relocate(
            genus_ref,
            .after = family_ref
        ) |>
        dplyr::relocate(
            genus_fb,
            .after = family_fb
        ) |>
        dplyr::select(-genus.y) |>
        dplyr::rename(
            genus = genus.x,
            valid_as_ref_habitat = valid_sci_name_habitat
        )

    cli::cli_alert("Checking for inconsistencies...")
    n_occ_solved <- df_occ_verified |>
        dplyr::filter(status_ref != "Not found") |>
        dplyr::distinct() |>
        nrow()
    not_found <- df_occ_verified |>
        dplyr::filter(status_ref == "Not found") |>
        dplyr::select(species_gbif) |>
        dplyr::distinct() |>
        dplyr::pull(species_gbif)
    n_occ_raw <- nrow(df_occ_filtered)

    if (round(n_occ_solved / n_occ_raw * 100, 2) < 100)
        cli::cli_alert_warning(paste0(
            abs(round((n_occ_solved) / n_occ_raw * 100 - 100, 2)),
            "% of the occurrences were not solved. Check ",
            paste(not_found, collapse = ", "),
            " species."
        ))
    if (round(n_occ_solved / n_occ_raw * 100, 2) == 100)
        cli::cli_alert_success(
            "All occurrences were solved. Obs.: NAs species from GBIF were not considered."
        )
    if (round(n_occ_solved / n_occ_raw * 100, 2) == 0)
        cli::cli_alert_danger("No occurrences were solved.")
    if (round(n_occ_solved / n_occ_raw * 100, 2) > 100)
        cli::cli_alert_warning("Bug detected. Check for duplicated results.")

    file_path <- paste0(folder, "/occ_df_corrected.csv")
    write.csv(df_occ_verified, file_path, row.names = FALSE)
    if (length(not_found) > 0) {
        writeLines(not_found, paste0(folder, "/log_species_not_found.txt"))

        nof_found_df <- df_occ_verified |>
            dplyr::filter(status_ref == "Not found") |>
            dplyr::select(
                species_gbif,
                family.x,
                genus
            ) |>
            dplyr::distinct() |>
            dplyr::rename(
                scientific_name = species_gbif,
                family = family.x
            ) |>
            dplyr::mutate(
                status = "Unkown",
                species = scientific_name,
                valid_scientific_name = "Unkown",
                sci_name_year = "Unkown",
                sci_name_authors = "Unkown",
                valid_sci_name_habitat = "Unkown",
            ) |>
            dplyr::select(
                scientific_name,
                status,
                family,
                genus,
                species,
                valid_scientific_name,
                sci_name_year,
                sci_name_authors,
                valid_sci_name_habitat
            )
        write.csv(
            nof_found_df,
            paste0(folder, "/spp_list_to_update.csv"),
            row.names = FALSE
        )
    }

    nans_year <- df_occ_verified |>
        dplyr::filter(is.na(year)) |>
        nrow()

    end_time <- Sys.time()
    execution_time <- difftime(end_time, start_time, units = "mins")
    cli::cli_alert_info(paste0(
        "Execution time: ",
        round(execution_time, 2),
        " min"
    ))

    return(df_occ_verified)
}

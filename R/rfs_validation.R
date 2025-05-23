#' @title Helper function to validate species names
#' @description This function is used to filter species nomenclature
#' from rFishStatus::rfs_update_occ_data() that should be manually checked.
#' @param species_database Species database according to rFishStatus template
#' (see rFishStatus::data_template_ref).
#' @param data A data frame representing the occurrence data to validate.
#' Obtained using rFishStatus::rfs_update_occ_data() ("occ_data") or
#' rFishStatus::rfs_update_spp_list()("spp_list").
#' @param data_type A character string representing the type of data to
#' validate. Should be either "occ_data" or "spp_list". Default is "spp_list".
#' @param ref_spp_list A data frame representing the species list to use as
#' reference. Obtained using rFishStatus::rfs_get_species_list().
#' @return A list with two data frames: attention_df and correct_df.
#' attention_df contains species that should be manually checked.
#' @export
#' @examples
#' species_database <- rFishStatus::data_template_ref
#' species_list <- rFishStatus::rfs_get_species_list(species_database)
#' ref_spp_list <- rFishStatus::rfs_get_species_list(species_database)
#' attention_list <- rfs_validate(species_database, species_list, "spp_list", ref_spp_list)

rfs_validation <- function(
    species_database,
    data,
    data_type = "spp_list",
    ref_spp_list
) {
    if (nrow(species_database) == 0 || nrow(data) == 0) {
        cli::cli_abort(
            "The species database or the data is empty. Please check the input."
        )
    }

    if (data_type != "occ_data" && data_type != "spp_list") {
        cli::cli_abort(
            "data_type must be either 'occ_data' or 'spp_list'."
        )
    }

    if (
        !identical(
            colnames(rFishStatus::data_template_ref),
            colnames(species_database)
        )
    ) {
        cli::cli_abort(
            "The columns of species_database do not match those of rFishStatus template. Please check your input."
        )
    }

    if (
        data_type == "occ_data" &&
            !identical(
                colnames(rFishStatus:::data_occ_update_result),
                colnames(data)
            )
    ) {
        cli::cli_abort(
            "The columns of data do not match those of rFishStatus::rfs_download_occ_data. Please check your input."
        )
    }

    valid_spp <- species_database |>
        dplyr::select(species_valid_as_short) |>
        dplyr::distinct() |>
        dplyr::pull()

    valid_spp_complete_name <- species_database |>
        dplyr::select(species_valid_name_complete) |>
        dplyr::distinct() |>
        dplyr::pull()

    if (data_type == "occ_data") {
        occ_data <- data |>
            dplyr::select(species_gbif) |>
            dplyr::rename(species = species_gbif) |>
            dplyr::distinct()
        spp_list <- rFishStatus::rfs_update_list(
            input_list = occ_data,
            reference_list = ref_spp_list
        )
    } else {
        spp_list <- data
    }

    validated_spp_df <- spp_list |>
        dplyr::mutate_all(~ stringr::str_replace_all(., "  +", " ")) |>
        dplyr::mutate_all(~ stringr::str_trim(.)) |>
        dplyr::mutate(
            correct_valid_one = dplyr::if_else(
                stringr::str_detect(status, "Valid|valid") &
                    stringr::str_detect(species, query_species) &
                    stringr::str_detect(valid_scientific_name, query_species),
                TRUE,
                FALSE
            ),
            correct_valid_one = dplyr::if_else(
                stringr::str_detect(status, "Valid|valid") &
                    query_species == valid_scientific_name,
                TRUE,
                correct_valid_one
            ),
            correct_valid_two = dplyr::if_else(
                stringr::str_detect(status, "Valid|valid") &
                    query_species %in% valid_spp,
                TRUE,
                FALSE
            ),
            correct_valid_two = dplyr::if_else(
                query_species %in% valid_spp_complete_name,
                TRUE,
                correct_valid_two
            ),
            correct_valid_three = dplyr::if_else(
                correct_valid_one == TRUE &
                    correct_valid_two == TRUE,
                TRUE,
                FALSE
            ),
            correct_valid_three = dplyr::if_else(
                correct_valid_three == TRUE &
                    stringr::str_detect(status, "Valid|valid"),
                TRUE,
                FALSE
            )
        )

    validation_list <- list()

    valid_to_check <- validated_spp_df |>
        dplyr::filter(
            correct_valid_three == FALSE &
                stringr::str_detect(status, "Valid|valid")
        ) |>
        dplyr::select(
            -c(correct_valid_one, correct_valid_two, correct_valid_three)
        )
    fuzzy_to_check <- validated_spp_df |>
        dplyr::filter(stringr::str_detect(status, "Fuzzy|fuzzy")) |>
        dplyr::select(
            -c(correct_valid_one, correct_valid_two, correct_valid_three)
        )
    to_check <- validated_spp_df |>
        dplyr::filter(
            correct_valid_three == FALSE &
                stringr::str_detect(status, "Valid|valid")
        ) |>
        dplyr::select(
            -c(correct_valid_one, correct_valid_two, correct_valid_three)
        )
    please_check <- validated_spp_df |>
        dplyr::filter(
            !status == "Valid" & !status == "Synonym" & !status == "Uncertain"
        ) |>
        dplyr::select(
            -c(correct_valid_one, correct_valid_two, correct_valid_three)
        )
    attention_df <- dplyr::bind_rows(
        valid_to_check,
        fuzzy_to_check,
        to_check,
        please_check
    ) |>
        dplyr::distinct()

    not_valid_queries <- attention_df |>
        dplyr::select(query_species) |>
        dplyr::distinct() |>
        dplyr::pull()

    correct_df <- validated_spp_df |>
        dplyr::filter(!query_species %in% not_valid_queries) |>
        dplyr::select(
            -c(correct_valid_one, correct_valid_two, correct_valid_three)
        )

    validation_list$correct_df <- correct_df
    validation_list$attention_df <- attention_df

    return(validation_list)
}

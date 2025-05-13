#' @title Get the species list from a dataframe based on rFishStatus template
#' @description This function uses a dataframe of scientific names from
#' organized a list of species names with their status.
#' @param df A data frame based on the rFishStatus template (rFishStatus::template_ref_data).
#' @return A dataframe with the species list.
#' @export
#' @examples
#' df <- rFishStatus::template_ref_data
#' df_list <- rfs_get_species_list(df)
#'

rfs_get_species_list <- function(df, summary = FALSE) {
    if (nrow(df) == 0) {
        cli::cli_abort("Your dataset is empty. Please check your input data.")
    }

    cli::cli_alert_info(
        "Creating species list from the input dataset."
    )

    cli::cli_inform(
        "Looking for uncertain species names."
    )
    uncertain_spp_one <- df |>
        dplyr::select(
            scientific_name_complete,
            scientific_name_status
        ) |>
        dplyr::filter(scientific_name_status == "Uncertain") |>
        dplyr::distinct() |>
        dplyr::select(scientific_name_complete) |>
        dplyr::rename(uncertain_spp = scientific_name_complete)

    uncertain_spp_two <- df |>
        dplyr::select(
            species_valid_name_complete,
            scientific_name_status
        ) |>
        dplyr::filter(scientific_name_status == "Uncertain") |>
        dplyr::distinct() |>
        dplyr::select(species_valid_name_complete) |>
        dplyr::rename(uncertain_spp = species_valid_name_complete)

    valid_spp_uncertain <- df |>
        dplyr::select(
            species_valid_as_complete
        ) |>
        dplyr::distinct() |>
        dplyr::pull(species_valid_as_complete) |>
        na.omit()

    uncertain_spp <- uncertain_spp_one |>
        dplyr::bind_rows(uncertain_spp_two) |>
        dplyr::distinct() |>
        dplyr::mutate(
            keep = dplyr::if_else(
                uncertain_spp %in% valid_spp_uncertain,
                "Yes",
                "No"
            )
        ) |>
        dplyr::filter(keep == "No") |>
        dplyr::pull(uncertain_spp)

    # All previous valid species names
    cli::cli_inform(
        "Looking for all previous valid species names."
    )
    valid_complete_df <- df |>
        dplyr::select(
            all_valid_species_complete,
            scientific_name_status,
            scientific_name_family,
            species_valid_name_complete,
            species_valid_name_author
        ) |>
        dplyr::mutate(
            all_valid_species_complete = stringr::str_replace_all(
                all_valid_species_complete,
                " ,",
                ","
            ),
            all_valid_species_complete = stringr::str_replace_all(
                all_valid_species_complete,
                "(\\d{4}),|(\\d{4}\\)),",
                "\\1\\2;"
            )
        ) |>
        tidyr::separate_rows(all_valid_species_complete, sep = "; ") |>
        dplyr::rename(
            scientific_name = all_valid_species_complete,
            status = scientific_name_status,
            valid_scientific_name = species_valid_name_complete,
            scientific_name_author = species_valid_name_author
        ) |>
        dplyr::mutate(
            status = "Synonym",
            status = dplyr::if_else(
                scientific_name == valid_scientific_name,
                "Valid",
                status
            )
        ) |>
        dplyr::distinct() |>
        dplyr::filter(!is.na(scientific_name) & scientific_name != "")

    cli::cli_inform(
        "Filtering valid species."
    )
    valid_df <- df |>
        dplyr::mutate(
            species_valid_as_short = dplyr::if_else(
                is.na(species_valid_as_short),
                scientific_name_synonym_of,
                species_valid_as_short
            )
        ) |>
        dplyr::select(
            species_valid_name_complete,
            species_valid_as_short,
            species_valid_name_author,
            species_valid_name_year,
            scientific_name_family,
            species_valid_name_genus
        ) |>
        dplyr::rename(
            species = species_valid_as_short,
            valid_scientific_name = species_valid_name_complete,
            authors = species_valid_name_author,
            year = species_valid_name_year,
            genus = species_valid_name_genus
        ) |>
        dplyr::mutate(scientific_name = valid_scientific_name) |>
        dplyr::distinct() |>
        dplyr::filter(
            !is.na(valid_scientific_name),
            !is.na(scientific_name),
            !is.na(authors),
            stringr::str_detect(authors, "\\D")
        ) |>
        dplyr::mutate(
            status = "Valid",
            year = as.character(year),
            authors = stringr::str_replace_all(authors, year, ""),
            species = dplyr::if_else(
                is.na(species),
                valid_scientific_name,
                species
            ),
            species = stringr::str_replace(species, authors, ""),
            species = stringr::str_replace(species, year, ""),
            authors = stringr::str_replace_all(
                authors,
                "[()]",
                ""
            ),
            authors = stringr::str_trim(
                authors,
                side = "both"
            )
        ) |>
        dplyr::filter(stringr::str_count(scientific_name, " ") >= 1)

    species_df <- df |>
        dplyr::select(
            scientific_name_complete,
            scientific_name_status,
            scientific_name_family,
            species_valid_name_complete,
            scientific_name_author
        ) |>
        dplyr::rename(
            scientific_name = scientific_name_complete,
            status = scientific_name_status,
            valid_scientific_name = species_valid_name_complete
        ) |>
        dplyr::bind_rows(valid_complete_df) |>
        dplyr::filter(!is.na(scientific_name) & scientific_name != "") |>
        dplyr::distinct() |>
        dplyr::mutate(
            scientific_name = stringr::str_replace_all(
                scientific_name,
                ", &",
                " &"
            ),
            valid_scientific_name = stringr::str_replace_all(
                valid_scientific_name,
                ", &",
                " &"
            )
        )

    # Extract genus and epithet from scientific_name
    valid_spp_list <- df |>
        dplyr::select(species_valid_name_complete) |>
        dplyr::filter(
            !is.na(species_valid_name_complete) &
                species_valid_name_complete != ""
        ) |>
        dplyr::distinct() |>
        dplyr::pull(species_valid_name_complete)

    final_df <- species_df |>
        dplyr::mutate(
            genus = stringr::str_extract(scientific_name, "\\w+"),
            epithet = stringr::str_extract(
                scientific_name,
                stringr::regex("(?<=\\s)[a-z]+(\\s(?![a-z]*\\.)[a-z]+)*")
            )
        ) |>
        dplyr::mutate(
            species = paste(genus, epithet, sep = " "),
            year = as.numeric(stringr::str_extract(
                scientific_name_author,
                "\\d{4}"
            )),
            authors = stringr::str_replace(
                scientific_name_author,
                paste0(" ", year),
                ""
            ),
            scientific_name = stringr::str_replace_all(
                scientific_name,
                "\\[.*?\\]",
                ""
            ),
            scientific_name = stringr::str_trim(
                scientific_name,
                side = "both"
            ),
            status = dplyr::if_else(
                scientific_name %in% valid_spp_list,
                "Valid",
                status
            ),
            status = dplyr::if_else(
                scientific_name == valid_scientific_name,
                "Valid",
                status
            )
        ) |>
        dplyr::filter(
            !(status == "Valid" &
                scientific_name != valid_scientific_name),
            year != "Invalid Number",
            year <= lubridate::year(Sys.Date()),
            !grepl("^\\w+ & \\w+ \\d{4}$", scientific_name),
            !grepl("^[a-z]", scientific_name),
            !stringr::str_detect(scientific_name, " \\[ref. "),
            !stringr::str_detect(scientific_name, "^\\["),
            !stringr::str_detect(scientific_name, ";")
        ) |>
        dplyr::distinct() |>
        dplyr::select(-c(epithet, scientific_name_author)) |>
        dplyr::relocate(
            c("genus", "species"),
            .after = "scientific_name_family"
        ) |>
        dplyr::mutate(
            authors = stringr::str_trim(
                authors,
                side = "both"
            ),
            authors = stringr::str_replace_all(
                authors,
                "[()]",
                ""
            ),
            year = stringr::str_trim(
                as.character(year),
                side = "both"
            ),
            valid_scientific_name = stringr::str_trim(
                valid_scientific_name,
                side = "both"
            ),
            filter_out_one = dplyr::if_else(
                stringr::str_detect(
                    valid_scientific_name,
                    stringr::str_escape(authors)
                ),
                "Yes",
                "No"
            ),
            filter_out_two = dplyr::if_else(
                stringr::str_detect(
                    valid_scientific_name,
                    stringr::str_escape(year)
                ),
                "Yes",
                "No"
            ),
            is_valid = dplyr::if_else(
                status == "Valid",
                "Yes",
                "No"
            ),
            authors = stringr::str_replace_all(
                authors,
                "var.\\s+",
                ""
            ),
            species = stringr::str_replace(
                species,
                "\\s(de|de la|da|von|van|van der|del|ter)$",
                ""
            ),
            status = dplyr::if_else(
                scientific_name != valid_scientific_name,
                "Synonym",
                status
            )
        ) |>
        dplyr::filter(
            (filter_out_one == "No" &
                filter_out_two == "No" &
                is_valid == "No") |
                (filter_out_one == "Yes" &
                    filter_out_two == "Yes" &
                    is_valid == "No") |
                (filter_out_one == "Yes" &
                    filter_out_two == "No" &
                    is_valid == "No")
        ) |>
        dplyr::select(-filter_out_one, -filter_out_two, -is_valid) |>
        dplyr::bind_rows(valid_df) |>
        dplyr::mutate_all(~ stringr::str_replace_all(., "  +", " ")) |>
        dplyr::mutate_all(~ stringr::str_trim(.)) |>
        dplyr::distinct()

    # Checking inconsistencies
    cli::cli_inform(
        "Checking for inconsistencies."
    )
    single_occurrences <- final_df |>
        dplyr::group_by(species, scientific_name_family) |>
        dplyr::mutate(n = dplyr::n()) |>
        dplyr::filter(n == 1)

    duplicates <- final_df |>
        dplyr::group_by(species, scientific_name_family) |>
        dplyr::mutate(
            n = dplyr::n(),
            status_check = paste(status, collapse = ", "),
            status = dplyr::if_else(
                stringr::str_detect(status_check, "Valid"),
                "Valid",
                status
            )
        ) |>
        dplyr::filter(n > 1) |>
        dplyr::distinct()

    cli::cli_inform(
        "Creating final dataset."
    )
    final_df_checked <- dplyr::bind_rows(single_occurrences, duplicates) |>
        dplyr::select(-n, -status_check) |>
        dplyr::distinct() |>
        dplyr::ungroup() |>
        dplyr::mutate(
            scientific_name = stringr::str_replace_all(
                scientific_name,
                "\\'.*?\\'",
                ""
            )
        ) |>
        dplyr::distinct()

    available_species <- final_df_checked |>
        dplyr::select(scientific_name) |>
        dplyr::distinct() |>
        dplyr::pull(scientific_name)

    unvailable_species <- df |>
        dplyr::mutate(
            scientific_name_complete = stringr::str_trim(
                scientific_name_complete,
                side = "both"
            )
        ) |>
        dplyr::filter(!(scientific_name_complete %in% available_species)) |>
        dplyr::select(
            scientific_name,
            scientific_name_complete,
            scientific_name_status,
            species_valid_as_complete,
            species_valid_name_author,
            species_valid_name_year,
            scientific_name_genus,
            scientific_name_family
        ) |>
        dplyr::mutate(
            species_valid_name_year = as.character(species_valid_name_year)
        ) |>
        dplyr::rename(
            scientific_name = scientific_name_complete,
            species = scientific_name,
            status = scientific_name_status,
            valid_scientific_name = species_valid_as_complete,
            authors = species_valid_name_author,
            year = species_valid_name_year
        ) |>
        dplyr::mutate(
            status = dplyr::if_else(
                is.na(status),
                "Unavailable",
                status
            ),
            valid_scientific_name = dplyr::if_else(
                is.na(valid_scientific_name),
                "Unavailable",
                valid_scientific_name
            ),
            authors = dplyr::if_else(
                is.na(authors),
                "Unavailable",
                authors
            ),
            authors = stringr::str_replace(
                authors,
                year,
                ""
            ),
            year = dplyr::if_else(
                is.na(year),
                "Unavailable",
                year
            )
        ) |>
        dplyr::distinct()

    # Habitat information
    cli::cli_inform(
        "Adding habitat information."
    )

    habitat_df <- df |>
        dplyr::select(species_valid_name_complete, scientific_name_habitat) |>
        dplyr::distinct() |>
        dplyr::rename(valid_scientific_name = species_valid_name_complete) |>
        dplyr::mutate(
            valid_scientific_name = stringr::str_trim(
                valid_scientific_name,
                side = "both"
            ),
            scientific_name_habitat = stringr::str_trim(
                scientific_name_habitat,
                side = "both"
            )
        ) |>
        dplyr::filter(!is.na(scientific_name_habitat)) |>
        dplyr::distinct() |>
        dplyr::group_by(valid_scientific_name) |>
        dplyr::summarise(
            n = dplyr::n(),
            scientific_name_habitat = paste(
                unique(unlist(strsplit(scientific_name_habitat, split = ", "))),
                collapse = ", "
            )
        ) |>
        dplyr::mutate(
            scientific_name_habitat = dplyr::if_else(
                n > 1,
                paste0(scientific_name_habitat, "* (please check)"),
                scientific_name_habitat
            )
        ) |>
        dplyr::select(-n) |>
        dplyr::distinct()

    final_df_checked_validation <- dplyr::bind_rows(
        final_df_checked,
        unvailable_species
    ) |>
        dplyr::mutate_all(~ stringr::str_replace_all(., "  +", " ")) |>
        dplyr::mutate_all(~ stringr::str_trim(.)) |>
        dplyr::distinct() |>
        dplyr::mutate(
            scientific_name = stringr::str_trim(scientific_name, side = "both"),
            remove = stringr::str_detect(
                scientific_name,
                "^[a-z]|^\\?|^\\(|^[0-9]|^[A-Z][^\\s]*\\s*&"
            ),
            species = stringr::str_replace_all(species, "[()]", ""),
            species = stringr::str_trim(species, "both"),
            year = stringr::str_trim(year, "both"),
            authors = stringr::str_trim(authors, "both"),
            new_scientific_name = stringr::str_replace_all(
                scientific_name,
                '\\"',
                ""
            ),
            new_scientific_name = stringr::str_replace_all(
                new_scientific_name,
                "\\'",
                ""
            ),
            new_year = stringr::str_extract(new_scientific_name, "\\d{4}"),
            new_authors = stringr::str_replace(
                new_scientific_name,
                species,
                ""
            ),
            new_authors = stringr::str_replace(new_authors, new_year, ""),
            new_authors = stringr::str_replace_all(new_authors, "\\(|\\)", "")
        ) |>
        dplyr::rename(
            sci_name_year = new_year,
            sci_name_authors = new_authors
        ) |>
        dplyr::filter(!remove) |>
        dplyr::select(-remove, -new_scientific_name, -authors, -year) |>
        dplyr::distinct() |>
        dplyr::mutate(
            status = dplyr::if_else(
                scientific_name %in% uncertain_spp,
                "Uncertain",
                status
            ),
            valid_scientific_name = dplyr::if_else(
                valid_scientific_name %in% uncertain_spp,
                paste0(valid_scientific_name, " (Uncertain status)"),
                valid_scientific_name
            ),
            new_sci_name = stringr::str_replace_all(scientific_name, '\\"', ""),
            status = dplyr::if_else(
                new_sci_name != valid_scientific_name & status == "Valid",
                "Synonym",
                status
            )
        ) |>
        dplyr::select(-new_sci_name) |>
        dplyr::arrange(scientific_name) |>
        dplyr::left_join(
            habitat_df,
            by = "valid_scientific_name"
        ) |>
        dplyr::rename(
            valid_sci_name_habitat = scientific_name_habitat
        ) |>
        dplyr::mutate(
            sci_name_check = stringr::str_extract(
                scientific_name,
                "^[A-Za-z]+\\s[A-Za-z]+(?:\\s[a-z]+)*"
            )
        ) |>
        dplyr::group_by(sci_name_check) |>
        dplyr::mutate(
            n = dplyr::n(),
            sci_name_year_count = paste(sci_name_year, " ") |>
                stringr::str_split(" ") |>
                unlist() |>
                stringr::str_unique() |>
                length() -
                1,
            keep = dplyr::case_when(
                stringr::str_detect(scientific_name, species) &
                    status == "Valid" &
                    n > 1 &
                    sci_name_year_count == 1 ~
                    "Yes",
                stringr::str_detect(scientific_name, species) &
                    status != "Valid" &
                    n > 1 &
                    sci_name_year_count == 1 ~
                    "No",
                TRUE ~ "Yes"
            )
        ) |>
        dplyr::filter(keep == "Yes") |>
        dplyr::select(-n, -keep, -sci_name_year_count) |>
        dplyr::distinct() |>
        dplyr::ungroup() |>
        dplyr::select(-sci_name_check)

    suppressMessages(
        duplicates_uncertain <- final_df_checked_validation |>
            dplyr::group_by(scientific_name, scientific_name_family) |>
            dplyr::summarise(n = dplyr::n()) |>
            dplyr::filter(n > 1) |>
            dplyr::distinct() |>
            dplyr::pull(scientific_name)
    )

    if (length(duplicates_uncertain) > 0) {
        final_df_checked_validation <- final_df_checked_validation |>
            dplyr::filter(
                !(scientific_name %in%
                    duplicates_uncertain &
                    status == "Uncertain")
            )
    }

    if (nrow(final_df_checked_validation) == 0) {
        cli::cli_abort("Check the the input dataset. Some data are missing.")
    }

    cli::cli_alert_success(
        "Species list created successfully."
    )

    if (summary) {
        status_summary <- final_df |>
            dplyr::filter(!is.na(status)) |>
            dplyr::group_by(status) |>
            dplyr::summarise(n = dplyr::n()) |>
            as.data.frame()

        return(status_summary)
    } else {
        return(final_df_checked_validation)
    }
}

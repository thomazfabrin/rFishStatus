#' @title Check input list against a reference list and perform fuzzy matching
#' @description This function takes an input list and a reference list, and
#' performs a series of checks and fuzzy matching operations to create a
#' verified list. The function checks if the input list is a data frame,
#' if it has a column named "species", if the reference list is a data frame,
#' and if the reference list has the mandatory columns. It also allows for
#' specifying the method and maximum distance for fuzzy matching.
#' The function returns a final data frame with the verified list.
#' @param input_list A data frame representing the input list to be checked.
#' Must contain a column named "species". Species name like genus + "sp."
#' should be avoided. If you want to use complete species names (genus + species
#' + authors + year) as key, you should avoid using comma to separate authors
#' and year, and use space instead. For example: 'Genus species Author 0000'.
#' @param reference_list A data frame representing the reference list to
#' compare against. This is obtained using rFishStatus::rfs_get_species_list().
#' @param complete_name A logical value indicating whether to use the
#' species + author + year nomenclature (please, avoid using comma before the
#' year), TRUE, or genus + epithet, FALSE, for joining. Default is FALSE.
#' @param method The method to use for fuzzy matching. Default is
#' "lv" (Levenshtein distance).
#' @param max_dist The maximum distance for fuzzy matching. Default is 3.
#' The user can change this value according to the desired string distance;
#' this will affect the number of matches and false positives, and will vary
#' according to the input list. Most of cases between 2 and 3 are good values
#' when using species names, and 4 to 5 when using complete names. Please,
#' check the fuzzy matching results carefully.
#' @param max_dist_2 The maximum dostance for deeper fuzzy matching. You
#' can keep the same as of "max_dist" or increase it by 1 or 2. This can
#' improve query_species solving but also increase false positives.
#' @return A data frame containing the final verified list.
#' @export
#' @examples
#' input_list <- data.frame(
#'    species = c("Acanthochromis polyacanthus", "Acanthochromis sp.", "Cichla kelberi", "Cichla piquitii",
#'                "Cichla monocolos", "Cichla monoculus", "Cicla monoculus")
#' )
#' reference_list <- rFishStatus::rfs_get_species_list(
#'    rFishStatus::data_template_ref
#' )
#' checked_list <- rfs_update_list(input_list, reference_list)
rfs_update_list <- function(
    input_list,
    reference_list,
    complete_name = FALSE,
    method = "lv",
    max_dist = 3,
    max_dist_2 = 5
) {
    # Check if input_list is a data frame
    if (!is.data.frame(input_list)) {
        cli::cli_abort(
            "input_list must be a data frame"
        )
    }

    if (!is.logical(complete_name)) {
        cli::cli_abort(
            "complete_name must be a logical"
        )
    }

    if (nrow(input_list) < 1) {
        cli::cli_abort(
            "input_list is empty"
        )
    }
    # Check if input_list has a column named "species"
    if (!"species" %in% colnames(input_list)) {
        cli::cli_abort(
            "input_list must have a column named 'species'"
        )
    }
    # Check if reference_list is a data frame
    if (!is.data.frame(reference_list)) {
        cli::cli_abort("reference_list must be a data frame")
    }
    # Check if reference_list has the mandatory columns
    if (
        !all(
            c("species", "scientific_name", "status") %in%
                colnames(reference_list)
        )
    ) {
        cli::cli_abort("Check your reference list")
    }
    # Check if complete_name is a logical
    if (!is.logical(complete_name)) {
        cli::cli_abort("complete_name must be a logical")
    }
    # Progress bar ----
    pb <- progress::progress_bar$new(total = 100)
    pb$tick(0) # Initialize progress bar
    # Join variable ----
    # complete_name_two <- FALSE
    if (complete_name) {
        # complete_name_two <- TRUE
        # complete_name <- "scientific_name"
        reference_list <- reference_list |>
            dplyr::mutate(
                species = scientific_name
            )
    }

    # Join ----
    # Join data - one
    cli::cli_alert_info("Joining data (exact matching)")
    pb$tick(33) # Update progress bar

    input_list <- input_list |>
        dplyr::mutate(
            species = stringr::str_squish(species)
        ) |>
        dplyr::distinct()

    join_complete_df <- input_list |>
        dplyr::left_join(reference_list, by = "species") |>
        dplyr::distinct()
    # Exact join df
    exact_join_df <- join_complete_df |>
        dplyr::filter(!is.na(status))
    # Fuzzy join df
    cli::cli_alert_info("Fuzzy matching")
    pb$tick(66) # Update progress bar
    fuzzy_join_df <- join_complete_df |>
        dplyr::filter(is.na(status)) |>
        dplyr::select(species) |>
        fuzzyjoin::stringdist_left_join(
            reference_list,
            by = "species",
            method = method,
            max_dist = max_dist
        ) |>
        dplyr::mutate(status = paste(status, " (fuzzy matching)", sep = ""))
    # Complete df verified
    cli::cli_alert_info("Creating verified list")

    exact_join_df$scientific_name_family <- as.character(
        exact_join_df$scientific_name_family
    )
    fuzzy_join_df$scientific_name_family <- as.character(
        fuzzy_join_df$scientific_name_family
    )

    pb$tick(100) # Update progress bar
    joined_df <- dplyr::bind_rows(exact_join_df, fuzzy_join_df) |>
        dplyr::mutate(
            species = dplyr::if_else(is.na(species), species.x, species),
            status = dplyr::if_else(
                status == "NA (fuzzy matching)",
                "Not found",
                status
            ),
            species.y = species
        ) |>
        dplyr::select(-species.x) |>
        dplyr::rename(
            species = species.y,
            query_species = species
        ) |>
        dplyr::arrange(query_species)
    not_found_df <- joined_df |>
        dplyr::filter(status == "Not found") |>
        dplyr::select(query_species) |>
        dplyr::mutate(
            species = stringr::str_extract(
                query_species,
                "^[A-Za-z]+\\s[A-Za-z]+"
            ),
            epithet_query = stringr::word(
                stringr::str_remove(query_species, "\\.$"),
                -1
            )
        ) |>
        fuzzyjoin::stringdist_left_join(
            dplyr::filter(reference_list, !is.na(species)),
            by = c("query_species" = "species"),
            method = method,
            max_dist = max_dist_2
        ) |>
        dplyr::mutate(
            is_true = stringr::str_detect(
                scientific_name,
                paste0("\\b", epithet_query, "\\b")
            ),
            status = dplyr::if_else(
                is_true,
                paste0(status, " (epithet matching)"),
                "Not found"
            )
        ) |>
        dplyr::mutate(dplyr::across(
            -c(query_species, status),
            ~ ifelse(status == "Not found", NA, .)
        )) |>
        dplyr::mutate(
            status = dplyr::if_else(
                is.na(status),
                "Not found",
                status
            )
        ) |>
        dplyr::select(-is_true, -epithet_query, -species.x) |>
        dplyr::distinct() |>
        dplyr::group_by(query_species) |>
        dplyr::mutate(n = dplyr::n()) |>
        dplyr::filter(!(is.na(scientific_name) & n > 1)) |>
        dplyr::select(-n) |>
        dplyr::distinct() |>
        dplyr::ungroup()

    suppressMessages(
        not_found_df <- not_found_df |>
            dplyr::mutate_all(as.character)
    )

    final_df <- joined_df |>
        dplyr::filter(status != "Not found") |>
        dplyr::bind_rows(not_found_df) |>
        dplyr::mutate(
            status = dplyr::if_else(is.na(status), "Not found", status),
            valid_scientific_name = dplyr::if_else(
                is.na(valid_scientific_name),
                "Not found",
                valid_scientific_name
            ),
            # species = dplyr::if_else(
            #     is.na(species),
            #     stringr::str_extract(
            #         valid_scientific_name, ".*?(?= \\([A-Z]| [A-Z])"),
            #         species),
            species = stringr::str_extract(
                valid_scientific_name,
                ".*?(?= \\([A-Z]| [A-Z])"
            ),
            species = stringr::str_replace(
                species,
                "\\bde\\b|\\bda\\b|\\bdos\\b|\\bdi\\b|\\bdo\\b|\\bvon\\b",
                ""
            ),
            species = stringr::str_replace_all(
                species,
                "\\s*\\(.*?\\)\\s*",
                ""
            )
        ) |>
        dplyr::distinct() |>
        dplyr::group_by(query_species) |>
        dplyr::mutate(n = dplyr::n()) |>
        dplyr::ungroup() |>
        dplyr::mutate(
            status = dplyr::if_else(
                n > 1,
                paste0(status, " - Multiple matches"),
                status
            )
        ) |>
        dplyr::select(-n) |>
        dplyr::arrange(query_species)

    # Final treatment ----
    final_df_not_found <- final_df |>
        dplyr::filter(status == "Not found")
    final_df_to_single_records <- final_df |>
        dplyr::mutate(
            epithet = stringr::str_extract(query_species, "\\s\\w+")
        ) |>
        dplyr::filter(stringr::str_detect(species, epithet))
    combine_final_df <- dplyr::bind_rows(
        final_df_not_found,
        final_df_to_single_records
    )
    final_solved <- combine_final_df |>
        dplyr::pull(query_species)
    final_unsolved <- final_df |>
        dplyr::filter(!query_species %in% final_solved)

    final_df_checked <- dplyr::bind_rows(combine_final_df, final_unsolved) |>
        dplyr::group_by(query_species) |>
        dplyr::mutate(
            n = dplyr::n(),
            status = dplyr::if_else(
                n == 1,
                stringr::str_replace(status, " - Multiple matches", ""),
                status
            ),
            year = sci_name_year,
            authors = paste0(sci_name_authors, collapse = "; "),
            authors = stringr::str_replace(authors, ";.*", "")
        ) |>
        dplyr::distinct() |>
        dplyr::mutate(
            n = dplyr::n(),
            status = dplyr::if_else(
                n == 1,
                stringr::str_replace(status, " - Multiple matches", ""),
                status
            )
        ) |>
        dplyr::select(-c(n, epithet, sci_name_authors, sci_name_year)) |>
        dplyr::group_by(query_species) |>
        dplyr::mutate(
            n = dplyr::n(),
            status = paste0(status, collapse = ", ")
        ) |>
        dplyr::distinct() |>
        dplyr::select(-n, -authors, -year) |>
        dplyr::group_by(query_species) |>
        dplyr::mutate(
            n = dplyr::n(),
            species = paste0(species, collapse = ", "),
            valid_scientific_name = paste0(
                valid_scientific_name,
                collapse = " or "
            ),
            scientific_name = paste0(scientific_name, collapse = " or "),
            genus = paste0(genus, collapse = " or "),
            family = paste0(scientific_name_family, collapse = " or ")
        ) |>
        dplyr::distinct() |>
        dplyr::select(-n) |>
        dplyr::group_by(query_species) |>
        dplyr::mutate(
            n = dplyr::n(),
            family = purrr::map_chr(
                strsplit(scientific_name_family, " or "),
                ~ paste(unique(trimws(.x)), collapse = " or ")
            ),
            status = purrr::map_chr(
                strsplit(status, ", "),
                ~ paste(unique(trimws(.x)), collapse = " or ")
            ),
            species = purrr::map_chr(
                strsplit(species, ", "),
                ~ paste(unique(trimws(.x)), collapse = " or ")
            ),
            valid_scientific_name = purrr::map_chr(
                strsplit(valid_scientific_name, " or "),
                ~ paste(unique(trimws(.x)), collapse = " or ")
            )
        ) |>
        dplyr::distinct() |>
        dplyr::ungroup() |>
        dplyr::mutate(
            status = stringr::str_replace_all(
                status,
                " - Multiple matches",
                ""
            ),
            status = as.character(status),
            status = stringr::str_trim(status, side = "both"),
            status = dplyr::if_else(
                stringr::str_detect(status, "Synonym or Valid"),
                "Valid or Synonym",
                status
            ),
            status = stringr::str_trim(status, side = "both"),
            status = dplyr::if_else(
                stringr::str_detect(valid_scientific_name, query_species) &
                    stringr::str_detect(status, "Valid or Synonym"),
                "Probably Valid - Multiple matches, please check",
                status
            ),
            valid_species_count = purrr::map_int(
                strsplit(valid_scientific_name, " or "),
                ~ length(trimws(.x))
            ),
            status = dplyr::if_else(
                valid_species_count == 1 &
                    stringr::str_detect(status, "Probably Valid"),
                "Valid",
                status
            )
        ) |>
        dplyr::select(-n, -valid_species_count, -species.y) |>
        dplyr::arrange(query_species) |>
        dplyr::mutate(
            status = dplyr::if_else(
                stringr::str_detect(query_species, valid_scientific_name) &
                    stringr::str_detect(status, "(fuzzy matching)"),
                stringr::str_replace(status, "(fuzzy matching)", ""),
                status
            )
        ) |>
        as.data.frame() |>
        dplyr::mutate(
            dplyr::across(
                .cols = -query_species,
                .fns = ~ ifelse(
                    stringr::str_detect(
                        query_species,
                        "\\b(aff|cf|gr|sp|spp)\\b"
                    ),
                    "Not found",
                    .
                ),
                .names = "{.col}"
            )
        ) |>
        as.data.frame() |>
        dplyr::mutate_at(
            .vars = c("family", "species", "genus"),
            .funs = ~ stringr::str_remove_all(
                .,
                "\\(\\s*\\)|\\(\\s*|\\s*\\)|\\s*\\(\\s*|\\s*\\)\\s*"
            )
        ) |>
        dplyr::mutate(
            status = stringr::str_remove_all(status, "\\(\\s*\\)"),
            status = dplyr::if_else(
                stringr::str_detect(status, "Valid") &
                    stringr::str_detect(status, "(fuzzy matching)"),
                paste(
                    status,
                    " - Please check the status and the spelling of your query"
                ),
                status
            )
        ) |>
        dplyr::distinct()

    if (complete_name) {
        final_one <- final_df_checked |>
            dplyr::filter(status != "Not found")

        not_found <- final_df_checked |>
            dplyr::filter(status == "Not found") |>
            dplyr::select(query_species) |>
            dplyr::mutate(
                query_species_copy = query_species,
                query_species = stringr::word(query_species, 1L, 2)
            ) |>
            fuzzyjoin::stringdist_left_join(
                reference_list,
                by = c("query_species" = "species"),
                method = method,
                max_dist = 2
            ) |>
            dplyr::mutate(
                status = dplyr::if_else(
                    is.na(valid_scientific_name),
                    "Not found",
                    status
                ),
                query_species = query_species_copy
            ) |>
            dplyr::select(-query_species_copy) |>
            dplyr::distinct()

        final_df_checked <- final_one |>
            dplyr::bind_rows(not_found) |>
            dplyr::arrange(query_species) |>
            dplyr::select(-c(sci_name_year, sci_name_authors))
    }

    final_df_checked <- final_df_checked |>
        dplyr::mutate(
            status = dplyr::if_else(
                stringr::str_detect(species, " OR | or ") &
                    status != "Not found",
                paste(
                    status,
                    " - Please check the status and the spelling of your query"
                ),
                status
            ),
            status = dplyr::if_else(
                !stringr::str_detect(species, " OR | or ") &
                    stringr::str_detect(scientific_name_family, " OR | or ") &
                    status != "Not found",
                paste(status, " - Please check the family"),
                status
            ),
            status = dplyr::if_else(
                stringr::str_detect(status, "epithet matching"),
                paste(
                    status,
                    " - Please check the status and the spelling of your query"
                ),
                status
            )
        ) |>
        dplyr::select(-scientific_name_family)

    if (complete_name) {
        final_df_checked <- final_df_checked |>
            dplyr::mutate(
                status = dplyr::if_else(
                    status == "Valid" &
                        query_species != valid_scientific_name,
                    paste(
                        status,
                        " - Please check the status and the spelling of your query"
                    ),
                    status
                )
            )
    } else if (complete_name == FALSE) {
        final_df_checked <- final_df_checked |>
            dplyr::mutate(
                status = dplyr::if_else(
                    status == "Valid" &
                        query_species != species,
                    paste(
                        status,
                        " - Please check the status and the spelling of your query"
                    ),
                    status
                )
            )
    }

    final_df_checked$status <- gsub("  +", " ", final_df_checked$status)

    final_df_checked <- final_df_checked |>
        dplyr::mutate(
            status = stringr::str_replace(
                status,
                "- Please check the status and the spelling of your query - Please check the status and the spelling of your query",
                "- Please check the status and the spelling of your query"
            )
        ) |>
        # dplyr::select(-scientific_name_family) |>
        dplyr::arrange(query_species) |>
        dplyr::mutate(
            scientific_name = dplyr::if_else(
                scientific_name == "NA" | is.na(scientific_name),
                "Not found",
                scientific_name
            ),
            genus = dplyr::if_else(
                genus == "NA" | is.na(genus),
                "Not found",
                genus
            ),
            valid_sci_name_habitat = dplyr::if_else(
                valid_sci_name_habitat == "NA" | is.na(valid_sci_name_habitat),
                "Not found",
                valid_sci_name_habitat
            ),
            species = dplyr::if_else(
                species == "NA" | is.na(species),
                "Not found",
                species
            ),
            family = dplyr::if_else(
                family == "NA" | is.na(family),
                "Not found",
                family
            ),
            valid_scientific_name = dplyr::if_else(
                valid_scientific_name == "NA" | is.na(valid_scientific_name),
                "Not found",
                valid_scientific_name
            )
        )

    return(final_df_checked)
}

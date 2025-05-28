#' @title Get Synonyms for Given Species
#' @description This function retrieves synonyms for given species from a
#' reference list.
#' @param ref_list A data frame containing reference species data obtained using
#' rFishStatus::rfs_get_species_list() function.
#' @param species A character vector representing the species names to query.
#' @param all_species A logical value indicating whether to return all synonyms
#' (default is FALSE). If TRUE, returns all synonyms as a vector.
#' @param complete_names A logical value indicating whether to return complete
#' scientific names (default is FALSE) in all species list. If TRUE, returns full scientific names
#' including author and year.
#' @return A list where each element is named after a valid species and contains
#' a character vector of its synonyms.
#' @export
#' @examples
#' ref_list <- rFishStatus::rfs_get_species_list(
#'    rFishStatus::template_ref_data
#' )
#' species <- "Crenicichla britskii"
#' result <- rfs_get_synonyms(ref_list, species)
#' print(result)
rfs_get_synonyms <- function(
    ref_list,
    species,
    all_species = FALSE,
    complete_names = FALSE
) {
    if (!is.data.frame(ref_list)) {
        cli::cli_abort("'ref_list' must be a data frame.")
    }

    required_cols <- c("valid_scientific_name", "scientific_name", "status")
    missing_cols <- setdiff(required_cols, names(ref_list))
    if (length(missing_cols) > 0) {
        cli::cli_abort(
            "'ref_list' is missing required columns. Check if your input was created with rFishStatus::rfs_get_species_list()."
        )
    }
    if (!is.character(species)) {
        cli::cli_abort("'species' must be a character vector.")
    }
    if (!is.logical(all_species) || length(all_species) != 1) {
        cli::cli_abort("'all_synonyms' must be a single logical value.")
    }
    if (!is.logical(complete_names) || length(complete_names) != 1) {
        cli::cli_abort("'complete_names' must be a single logical value.")
    }

    if (length(species) == 0) {
        return(list())
    }

    sp_query <- species[1]
    remaining_species <- species[-1]

    synonyms <- ref_list |>
        dplyr::filter(
            stringr::str_detect(
                stringr::str_squish(valid_scientific_name),
                stringr::str_squish(sp_query)
            ) &
                status != "Valid"
        ) |>
        dplyr::distinct() |>
        dplyr::pull(scientific_name) |>
        unique()

    if (length(synonyms) == 0) {
        valid_sp <- ref_list |>
            dplyr::filter(stringr::str_detect(
                stringr::str_squish(scientific_name),
                stringr::str_squish(sp_query)
            )) |>
            dplyr::distinct() |>
            dplyr::pull(valid_scientific_name)

        synonyms <- ref_list |>
            dplyr::filter(
                valid_scientific_name %in% valid_sp & status != "Valid"
            ) |>
            dplyr::pull(scientific_name) |>
            unique()
    } else {
        valid_sp <- ref_list |>
            dplyr::filter(
                stringr::str_detect(valid_scientific_name, sp_query) &
                    status == "Valid"
            ) |>
            dplyr::pull(valid_scientific_name)
    }

    result <- list()

    if (length(valid_sp) != 0 || length(synonyms) != 0) {
        if (length(valid_sp) == 0) valid_sp <- "No valid species found"
        if (length(synonyms) == 0) synonyms <- "No synonyms found"
        result[[valid_sp]] <- synonyms
    }

    remaining_result <- rFishStatus::rfs_get_synonyms(
        ref_list,
        remaining_species,
        all_species,
        complete_names
    )
    result <- c(result, remaining_result)

    if (all_species) {
        valid_spp <- names(result) |>
            stringr::str_squish() |>
            unique()
        valid_spp <- valid_spp[valid_spp != "No valid species found"]

        synonyms_spp <- unlist(result) |>
            stringr::str_squish() |>
            unique()
        synonyms_spp <- synonyms_spp[synonyms_spp != "No synonyms found"]

        all_spp <- c(valid_spp, synonyms_spp)
        all_spp <- all_spp[nzchar(all_spp)]

        if (!complete_names) {
            extract_binomial <- function(x) {
                stringr::str_extract(x, "^[A-Za-z]+\\s+[a-z]+(\\s+[a-z]+)?")
            }
            binomial_names <- sapply(
                all_spp,
                extract_binomial,
                USE.NAMES = FALSE
            )
            binomial_names <- binomial_names[!is.na(binomial_names)]
            return(binomial_names)
        } else {
            return(all_spp)
        }
    } else {
        return(result)
    }
}

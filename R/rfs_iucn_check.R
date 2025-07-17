#' @title Check IUCN Red List Status for a Species
#'
#' @description This function queries the IUCN Red List API for the conservation status of a given species (genus and epithet).
#' It handles errors gracefully, logs issues, and returns a data frame with the status and any error messages.
#'
#' @param species Character. The full species name in the format "Genus epithet" (e.g., "Pterophyllum scalare"). Multiple species can be checked by providing a vector of species names.
#' @param iucn_api_key Character. Your IUCN API key.
#' @param log_path Character. The path to the directory where error logs will be saved. Default is "iucn_log/".
#'
#' @return A data frame with columns:
#'   \describe{
#'     \item{species}{The full species name.}
#'     \item{red_list_status}{The IUCN Red List status or error message.}
#'     \item{error}{Any error message encountered during the query.}
#'     \item{possible_solution}{Suggested solution if an error occurred.}
#'   }
#'
#' @examples
#' \dontrun{
#' rfs_iucn_check("Pterophyllum scalare", "your_api_key")
#' }
#' @export
#'
rfs_iucn_check <- function(species, iucn_api_key, log_path = "iucn_log/") {
    if (missing(species)) {
        cli::cli_abort(
            "Species must be provided."
        )
    }

    if (missing(iucn_api_key)) {
        cli::cli_abort(
            "IUCN API key must be provided."
        )
    }

    if (!dir.exists(log_path)) {
        dir.create(log_path, recursive = TRUE)
    }

    iucn_function <- function(species, iucn_api_key, log_path) {
        genus <- stringr::str_extract(species, "^[^ ]+")
        epithet <- stringr::str_extract(species, "(?<= )[A-Za-z-]+")

        iucn_species <- tryCatch(
            {
                rredlist::rl_species(
                    genus = genus,
                    species = epithet,
                    key = iucn_api_key
                )
            },
            error = function(e) {
                cli::cli_alert_danger(
                    "Error retrieving IUCN data for {species}: {e$message}"
                )

                write(
                    paste0(
                        "Error retrieving IUCN data for ",
                        species,
                        " at ",
                        Sys.time(),
                        ": ",
                        e$message
                    ),
                    file = paste0(
                        log_path,
                        "/",
                        Sys.Date(),
                        "_iucn_error_log.txt"
                    ),
                    append = TRUE
                )

                error_message <- e$message

                return(error_message)
            }
        )

        if (!is.list(iucn_species)) {
            red_list_status <- "Not found"
            df <- data.frame(
                species = species,
                red_list_status = red_list_status,
                error = iucn_species,
                possible_solution = dplyr::if_else(
                    stringr::str_detect(iucn_species, "No results returned"),
                    "Check the spelling of the genus and epithet.",
                    NA_character_
                )
            )
            return(df)
        }

        iucn_assessment_id <- iucn_species$assessments$assessment_id[1] # Get only the most recent assessment ID

        iucn_status <- tryCatch(
            {
                rredlist::rl_assessment(
                    id = iucn_assessment_id,
                    key = iucn_api_key
                )
            },
            error = function(e) {
                cli::cli_alert_danger(
                    "Error retrieving IUCN assessment data for {species}: {e$message}"
                )

                write(
                    paste0(
                        "Error retrieving IUCN assessment data for ",
                        species,
                        " at ",
                        Sys.time(),
                        ": ",
                        e$message,
                        " - no assessment ID found"
                    ),
                    file = paste0(
                        log_path,
                        "/",
                        Sys.Date(),
                        "_iucn_error_log.txt"
                    ),
                    append = TRUE
                )

                return(NULL)
            }
        )

        if (is.null(iucn_status)) {
            red_list_status <- "Probably not assessed"
            df <- data.frame(
                species = species,
                red_list_status = red_list_status,
                error = "No assessment data found",
                possible_solution = NA_character_
            )
            return(df)
        }

        red_list_status <- iucn_status$red_list_category$code

        df <- data.frame(
            species = species,
            red_list_status = red_list_status,
            error = NA_character_,
            possible_solution = NA_character_
        )

        Sys.sleep(0.5)

        return(df)
    }

    df <- purrr::map_dfr(
        species,
        ~ iucn_function(.x, iucn_api_key, log_path),
        .progress = TRUE
    )

    if (nrow(df) == 0) {
        cli::cli_abort(
            "No data retrieved. Please check the species names and your IUCN API key."
        )
    }

    return(df)
}

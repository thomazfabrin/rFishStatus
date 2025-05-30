#' Template Reference Data
#'
#' A dataset containing a template of input data for the `rfs_get_species_list` function.
#'
#' @format A data frame with the following columns:
#' \describe{
#'   \item{scientific_name_complete}{Complete scientific name of the species.}
#'   \item{scientific_name}{Scientific name without authorship.}
#'   \item{scientific_name_author}{Authorship of the scientific name including year.}
#'   \item{scientific_name_genus}{Genus of the scientific name.}
#'   \item{scientific_name_family}{Family of the scientific name.}
#'   \item{scientific_name_status}{Current status of the scientific name (e.g., "Valid", "Synonym", "Uncertain").}
#'   \item{species_valid_name_complete}{Complete valid name of the species.}
#'   \item{species_valid_as_complete}{Valid name of the species if it is a synonym.}
#'   \item{species_valid_as_short}{Short valid name of the species (genus + epithet).}
#'   \item{species_valid_name_genus}{Genus of the valid name.}
#'   \item{species_valid_name_author}{Authorship of the valid name including year.}
#'   \item{species_valid_name_year}{Year of the valid species name.}
#'   \item{scientific_name_synonym_of}{Valid species of the synonym (only genus + epithet).}
#'   \item{all_valid_species_complete}{All valid species names associated with the scientific name. Separated by semicolons.}
#'   \item{scientific_name_habitat}{Habitat information for the scientific name (e. g., "freshwater", "marine").}
#' }
#' @source Organized according to taxonomic data on Cichlidae
"data_template_ref"

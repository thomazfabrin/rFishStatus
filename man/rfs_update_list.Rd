% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rfs_update_list.R
\name{rfs_update_list}
\alias{rfs_update_list}
\title{Check input list against a reference list and perform fuzzy matching}
\usage{
rfs_update_list(
  input_list,
  reference_list,
  complete_name = FALSE,
  method = "lv",
  max_dist = 3,
  max_dist_2 = 5
)
}
\arguments{
\item{input_list}{A data frame representing the input list to be checked.
Must contain a column named "species". Species name like genus + "sp."
should be avoided. If you want to use complete species names (genus + species
\itemize{
\item authors + year) as key, you should avoid using comma to separate authors
and year, and use space instead. For example: 'Genus species Author 0000'.
}}

\item{reference_list}{A data frame representing the reference list to
compare against. This is obtained using rFishStatus::rfs_get_species_list().}

\item{complete_name}{A logical value indicating whether to use the
species + author + year nomenclature (please, avoid using comma before the
year), TRUE, or genus + epithet, FALSE, for joining. Default is FALSE.}

\item{method}{The method to use for fuzzy matching. Default is
"lv" (Levenshtein distance).}

\item{max_dist}{The maximum distance for fuzzy matching. Default is 3.
The user can change this value according to the desired string distance;
this will affect the number of matches and false positives, and will vary
according to the input list. Most of cases between 2 and 3 are good values
when using species names, and 4 to 5 when using complete names. Please,
check the fuzzy matching results carefully.}

\item{max_dist_2}{The maximum dostance for deeper fuzzy matching. You
can keep the same as of "max_dist" or increase it by 1 or 2. This can
improve query_species solving but also increase false positives.}
}
\value{
A data frame containing the final verified list.
}
\description{
This function takes an input list and a reference list, and
performs a series of checks and fuzzy matching operations to create a
verified list. The function checks if the input list is a data frame,
if it has a column named "species", if the reference list is a data frame,
and if the reference list has the mandatory columns. It also allows for
specifying the method and maximum distance for fuzzy matching.
The function returns a final data frame with the verified list.
}
\examples{
input_list <- data.frame(
   species = c("Acanthochromis polyacanthus", "Acanthochromis sp.", "Cichla kelberi", "Cichla piquitii",
               "Cichla monocolos", "Cichla monoculus", "Cicla monoculus")
)
reference_list <- rFishStatus::rfs_get_species_list(
   rFishStatus::data_template_ref
)
checked_list <- rfs_update_list(input_list, reference_list)
}

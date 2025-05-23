#' @title Join basins to occurrence dataset
#' @description This function joins basins to an occurrence dataset based on
#' shapefiles. The return is used by rFishStatus::rfs_detect_invasive().
#' @param occ_df A data frame of occurrences obtained from
#' rFishStatus::rfs_update_occ_data().
#' @param shapefile A character string specifying the shapefiles contained in the package.
#' The default is "br_pr_basins". The user can also provide a shapefile directly.
#' Obs.: for now, only the shapefile "br_pr_basins" is available directly
#' from the package. New opstions will be added in the future.
#' @return A data frame of the joined occurrences and basins.
#' @export
#' @examples
#' occ_df <- rFishStatus:::data_occ_update_result
#' rfs_join_basins(occ_df, "br_pr_basins")
rfs_join_basins <- function(occ_df, shapefile = "br_pr_basins") {
    if (shapefile == "br_pr_basins") shapefile <- rFishStatus:::br_pr_basins

    if (is.null(shapefile)) {
        cli::cli_abort(
            c(
                "The shapefile is not available. Please check the shapefile name."
            )
        )
    }

    if (nrow(occ_df) == 0) {
        cli::cli_abort(
            c(
                "The occurrence dataset is empty. Please check the dataset."
            )
        )
    }

    if (
        !identical(
            colnames(occ_df),
            colnames(rFishStatus:::data_occ_update_result)
        )
    ) {
        cli::cli_abort(
            c(
                "The occurrence dataset does not have the same columns as the output from rFishStatus::rfs_updated_occ_data()."
            )
        )
    }

    occ_df <- occ_df |>
        dplyr::rename(
            latitude = decimalLatitude,
            longitude = decimalLongitude
        )

    occ_df$lat_copy <- occ_df$latitude
    occ_df$lon_copy <- occ_df$longitude

    occ_df_join <- sf::st_as_sf(
        occ_df,
        coords = c("longitude", "latitude"),
        crs = 4326
    )

    occ_df_join <- sf::st_transform(occ_df_join, sf::st_crs(shapefile))

    joined_df <- sf::st_join(occ_df_join, shapefile)

    joined_df$basin <- joined_df$NOME

    joined_df <- joined_df |>
        dplyr::select(-geometry) |>
        dplyr::rename(
            latitude = lat_copy,
            longitude = lon_copy
        ) |>
        as.data.frame() |>
        dplyr::select(
            -c(OBJECTID, NOME, Shape_Leng, Shape_Area, AREA_KM2, geometry)
        ) |>
        sf::st_drop_geometry()

    return(joined_df)
}

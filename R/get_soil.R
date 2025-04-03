#' get soil data from the European Soil DataBase (ESDB), using a local repository for GIS files.
#'
#' @param lat latitude of the geographical location in decimal degrees (WGS84)
#' @param lon longitude of the geographical location in decimal degrees (WGS84)
#' @param format output format:
#' * esdb : raw ESDB information
#' * sunflo : rsunflo soil parameter list (assume a fixed wilting point)
#' @param data dataframe created from ESDB GIS files as made available by the JRC lab for research usage.
#'
#' @return a dataframe of soil properties
#' @export

get_soil_esdb <- function(lat, lon, format = "raw", data=data_soil) {

  # get closer grid position
  index <- which.min(sqrt((data$lat-lat)^2 + (data$lon-lon)^2))


  switch(

    format,

    # extract ESDB features
    raw = {
      data_soil <- data %>% dplyr::slice(index) %>% dplyr::select(-(x:lat))
    },

    # build rsunflo soil parameter list (assume a fixed wilting point)
    sunflo = {

      data_soil <- data %>%
        dplyr::slice(index) %>%
        dplyr::mutate(
          awc = awc_ptf,
          wilting_point_1 = 10,
          field_capacity_1 = (awc_ptf_t / (density_t * 300) + wilting_point_1 / 100) * 100,
          wilting_point_2 = 10,
          field_capacity_2 = ifelse(
            depth == 300, 10,
            (awc_ptf_s / (density_s * (depth - 300)) + wilting_point_2 / 100) * 100),
          stone_content = mean(gravel_t, gravel_s) / 100
        ) %>%
        dplyr::select(
          awc,
          root_depth = depth,
          field_capacity_1,
          wilting_point_1,
          field_capacity_2,
          wilting_point_2,
          soil_density_1 = density_t,
          soil_density_2 = density_s,
          stone_content
        )
    },

    stop("Invalid `format` value")

  )

  return(data_soil)

}


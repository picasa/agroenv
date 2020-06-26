#' get soil data from the European Soil DataBase (ESDB), using a local repository for GIS files.
#'
#' @param lat latitude of the geographical location in decimal degrees (WGS84)
#' @param lon longitude of the geographical location in decimal degrees (WGS84)
#' @param data dataframe created from ESDB GIS files as made available by the JRC lab for research usage.
#'
#' @return a dataframe of soil properties
#' @export

get_soil_esdb <- function(lat, lon, data=data_soil) {

  # get closer grid position
  index <- which.min(sqrt((data$lat-lat)^2 + (data$lon-lon)^2))

  # extract DB features
  data_soil <- data %>% slice(index) %>% select(-(x:lat))

  return(data_soil)

}

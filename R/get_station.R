#' get the closest climate station id based on physical station network or grid cell index
#'
#' @param latitude latitude of the geographical location in decimal degrees (WGS84)
#' @param longitude logitude of the geographical location in decimal degrees (WGS84)
#' @param data list of dataframes containing stations or grid cell coordinates
#' @param network string of the stations network or gridded data to use
#' @param n number of results to return
#'
#' @return a dataframe of station(s) or grid cell closest to the geographical location
#' @export
#' @examples
#' \donttest{
#' get_station(43.5279, 1.5009, network="climbox", n=5)
#' }

get_station <- function(latitude, longitude, data=agroenv::station_index, network, n=1) {

  results <- data[[network]] %>%
    dplyr::mutate(distance=distance_haversine(lat, lon, latitude, longitude)) %>%
    dplyr::arrange(distance) %>%
    dplyr::slice(1:n) %>%
    dplyr::select(station_id, lat, lon, distance)

  return(results)
}

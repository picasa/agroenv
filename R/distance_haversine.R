#' compute distance in km between two points: http://en.wikipedia.org/wiki/Haversine_formula
#' @export
#' @param lat1 latitude of the first geographical location in decimal degrees (WGS84)
#' @param lon1 longitude of the first geographical location in decimal degrees (WGS84)
#' @param lat2 latitude of the second geographical location in decimal degrees (WGS84)
#' @param lon2 longitude of the second geographical location in decimal degrees (WGS84)
#' @param r earth diameter at the equator in km

#' @return a numeric value of the distance between the two points in km
#' @export
#' @examples distance_haversine(43.5279, 1.5009, 42.8504, -0.4333)

distance_haversine <- function(lat1, lon1, lat2, lon2, r = 6378.137){
  radians <- pi/180
  lat2 <- lat2 * radians
  lat1 <- lat1 * radians
  lon2 <- lon2 * radians
  lon1 <- lon1 * radians
  dLat <- (lat2 - lat1)
  dLon <- (lon2 - lon1)
  a <- (sin(dLat/2)^2) + (cos(lat1) * cos(lat2)) * (sin(dLon/2)^2)
  return(2 * atan2(sqrt(a), sqrt(1 - a)) * r)
}



#' Compute reference evapotranspiration according to Penman-Monteith formula and [@Wallach2006] assumption
#'
#' @param lat Latitude of the geographical location in decimal degrees (WGS84)
#' @param lon Longitude of the geographical location in decimal degrees (WGS84)
#' @param elevation Elevation of the geographical location in meters (m)
#' @param day Day of year used to compute solar irradiation factor (calendar day)
#' @param tmin Minimum Air Temperature At 2 m Above The Surface Of The Earth (degrees C)
#' @param tmax Maximum Air Temperature At 2 m Above The Surface Of The Earth (degrees C)
#' @param tdew Dew/Frost Point Temperature At 2 m (degrees C)
#' @param rad Daily Insolation Incident On A Horizontal Surface (MJ/m^2/day)
#' @param wind Wind Speed At 10 m Above The Surface Of The Earth (m/s)
#'
#' @return a numeric value for daily reference evapotranspiration (mm/day)
#' @export

et_penman_monteith <- function(lat, lon, elevation, day, tmin, tmax, tdew, rad, wind) {

  latitude <- unique(lat)*pi/180

  # psychometric Constant
  PSC <- 0.665*10^-3*101.3*((293-0.0065*unique(elevation))/293)^5.26;

  # wind speed at 2m
  ws2 <- wind*4.87/log(67.8*10-5.42)
  es <- ((0.6108 * exp(17.27*tmax/(tmax+237.3)))+(0.6108*exp(17.27*tmin/(tmin+237.3))))/2
  slope <- (0.6108*exp(17.27*((tmax+tmin)/2)/(((tmax+tmin)/2)+237.3))*4098)/((tmax+tmin)/2+237.3)^2

  # humidity
  ea <- 0.6108*exp(17.27*tdew/(tdew+237.3));

  # radiation
  SWR <- (1-0.23)*rad
  IRDES <- 1+0.033*cos(2*pi*day/365.25)
  SD <- 0.409*sin(2*pi*day/365.25-1.39)
  SSA <- acos(-tan(latitude)*tan(SD))
  extra <- 24*60*0.082/pi*IRDES * (SSA*sin(latitude)*sin(SD)+cos(latitude)*cos(SD)*sin(SSA))
  CSR <- (0.75+2*10^-5*unique(elevation))*extra
  RRAD <- rad/CSR

  # evapotranspiration
  LWR <- 4.903*10^-9*((tmax+273.16)^4+(tmin+273.16)^4)/2*(0.34-0.14*sqrt(ea))*(1.35*RRAD-0.35)
  NRAD <- SWR-LWR;
  ET <- (0.408*slope*NRAD+PSC*(900/((tmax+tmin)/2+273))*ws2*(es-ea))/(slope+PSC*(1+0.34*ws2))

  return(ET)

}

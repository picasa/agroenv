#' get climate data from the SAFRAN model, using a local repository of grid cell data archives
#'
#' @param id id for the cell grid (optional)
#' @param lat latitude of the geographical location in decimal degrees (WGS84)
#' @param lon longitude of the geographical location in decimal degrees (WGS84)
#' @param year year(s) of the climate data to be retrieved
#' @param path path to a directory containing grid cell data archives as made available by Meteo-France for research usage
#'
#' @return a dataframe of daily climate data for 9 variables
#' @export

get_climate_safran <- function(id = NULL, lat = NULL, lon = NULL, year, path) {

  # get id for the grid cell if not given
  grid_id <- ifelse(
    is.null(id),
    get_station(lat, lon, network = "safran") %>% dplyr::pull(station_id),
    id)

  # build the file name for the requested cell
  prefix <- "quotidiennes_1990_11_2021_maille"
  file <- glue::glue("{path}/{prefix}_{grid_id}.csv.gz")

  # select variables usable with crop models
  data_climate <- readr::read_delim(
    file,
    delim=";", col_types="iiicddddddddd") %>%
    dplyr::select(id=1, date, RS=5, RR=6, TN=8, TX=9, TM=7, GR=10, RH=13, PET=11) %>%
    dplyr::mutate(id=as.character(id), date=lubridate::ymd(date), RR=RS+RR, GR=GR/100) %>%
    dplyr::filter(lubridate::year(date) %in% year) %>%
    dplyr::select(id, date, TN, TX, TM, RR, GR, PET)

  return(data_climate)
}

#' get climate data from the Agri4Cast MARS dataset (Parquet, 10 km grid)
#'
#' Returns daily records for the 10 km grid cell nearest to the input coordinates.
#'
#' @param lat Numeric. WGS84 latitude of the target location.
#' @param lon Numeric. WGS84 longitude of the target location.
#' @param country Character. ISO2 country code (uppercase, e.g. `"FR"`).
#'   If `NULL`, the country is inferred from the coordinates (slower).
#' @param year Integer vector of years to return (e.g. `2000:2001`). If `NULL`, all available years.
#' @param margin Numeric. Half-width of the spatial bounding box in degrees (default 0.2).
#'   Increase if no grid cell is found near the target (e.g. coastal or border locations).
#' @param path Character. Path to the root parquet directory.
#'
#' @return A tibble with columns: `id`, `date`, `TN`, `TX`, `TM`, `WIND`, `VP`, `RR`, `PET`, `GR`.
#' @export

get_climate_agri4cast <- function(
  lat, lon,
  country = NULL,
  year    = NULL,
  margin  = 0.2,
  path
) {
  ds <- arrow::open_dataset(path, partitioning = arrow::schema(COUNTRY = arrow::utf8(), YEAR = arrow::int32()))

  yr_min <- if (!is.null(year)) min(year) else 1900L
  yr_max <- if (!is.null(year)) max(year) else 2100L

  q <- ds |>
    dplyr::filter(
      lat  >= .env$lat - margin, lat  <= .env$lat + margin,
      lon  >= .env$lon - margin, lon  <= .env$lon + margin,
      YEAR >= yr_min, YEAR <= yr_max
    )

  if (!is.null(country)) q <- q |> dplyr::filter(COUNTRY == country)

  out <- dplyr::collect(q)

  if (nrow(out) == 0) {
    warning("No grid cell found in bbox (lat=", lat, ", lon=", lon, ", margin=", margin, "). Try increasing margin.")
    return(out)
  }

  nearest_id <- out |>
    dplyr::mutate(dist = (lat - .env$lat)^2 + (lon - .env$lon)^2) |>
    dplyr::slice_min(dist, n = 1, with_ties = FALSE) |>
    dplyr::pull(cell)

  out |>
    dplyr::filter(cell == nearest_id) |>
    dplyr::mutate(id = as.character(cell)) |>
    dplyr::select(id, date, TN, TX, TM, WIND, VP, RR = PR, GR = RAD, PET = ET0) |>
    dplyr::arrange(date)
}

#' get climate data from the NASA POWER API using the R nasapower package
#' @param lat latitude of the geographical location in decimal degrees (WGS84)
#' @param lon longitude of the geographical location in decimal degrees (WGS84)
#' @param year year(s) of the climate data to be retrieved
#' @export

get_climate_nasapower <- function(lat, lon, year)

{

  data_tmp <- nasapower::get_power(
    community = "AG",
    lonlat = c(lon, lat),
    pars = c("T2M", "T2M_MAX", "T2M_MIN", "ALLSKY_SFC_SW_DWN", "PRECTOTCORR",
             "T2MDEW", "RH2M", "WS10M"),
    dates = c(paste0(min(year), "-01-01"), paste0(max(year), "-12-31")),
    temporal_api= "DAILY")

  elev <- readr::parse_number(
    stringr::str_split(attributes(data_tmp)$POWER.Elevation, "=")[[1]][2])

  data_climate <- data_tmp %>%
    dplyr::mutate_at(dplyr::vars(T2M:WS10M), dplyr::na_if, -99) %>%
    dplyr::mutate_at(dplyr::vars(T2M:WS10M), zoo::na.approx, maxgap = 4) %>%
    dplyr::mutate(
      id = paste0(sprintf("%03d", round(abs(LAT))), sprintf("%03d", round(abs(LON)))),
      YYYYMMDD = lubridate::as_date(YYYYMMDD),
      PET = et_penman_monteith(
        lat = LAT, lon = LON, elevation = elev, day = DOY,
        tmin = T2M_MIN, tmax = T2M_MAX, tdew = T2MDEW,
        rad = ALLSKY_SFC_SW_DWN, wind = WS10M)) %>%
    dplyr::select(
      id, date = YYYYMMDD, TN = T2M_MIN, TX = T2M_MAX, TM = T2M,
      RR = PRECTOTCORR, GR = ALLSKY_SFC_SW_DWN, PET)

  return(data_climate)

}
